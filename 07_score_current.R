# 07_score_current.R ------------------------------------------------------
# Score current NPB players as MLB targets.
#
# Data source: FanGraphs, in two pieces.
#
#   1. The NPB leaderboard page (server-rendered HTML) gives the full player
#      universe for the current season -- ~680 batters and ~500 pitchers at
#      qual=0 -- along with each player's FanGraphs id. Summing that whole
#      pool also gives us exact league context for free, which is what the
#      model's league-relative predictors need.
#
#   2. The per-player JSON endpoint (/api/players/stats) gives every NPB
#      season a player has, with K%, BB%, ISO, BABIP and wRC+ already
#      computed. This is how we reach *completed* seasons.
#
# Why both: the leaderboard page ignores the `season` URL parameter and
# always renders the current season. Verified 2026-08-08 -- passing
# season=2025 returned 2026 data. So the leaderboard is used for the player
# universe and current-season league context, and the player API is used
# whenever a specific (completed) season is wanted.

source(file.path("R", "00_config.R"))
source(file.path("R", "05_fit_models.R"))

FG <- "https://www.fangraphs.com"

# FanGraphs is a smaller operation than BR and these are cheap requests, but
# there is no reason to be rude about it.
FG_SLEEP <- 1.0

# --- leaderboard ---------------------------------------------------------
#' Parse one rendered leaderboard page into a tibble.
#'
#' @param type 0 = Standard (counting stats), 1 = Advanced (wRC+, ISO, FIP...).
#'   The two tabs carry different columns, so the structural fingerprint used
#'   to find the right table differs: Advanced batting has no AB, Advanced
#'   pitching has no ER.
parse_lb_page <- function(raw, stats, type = 0) {
  if (is.na(raw)) return(NULL)
  doc <- read_html(raw)

  need <- if (type == 0) {
    if (stats == "bat") c("Name", "PA", "AB") else c("Name", "IP", "ER")
  } else {
    if (stats == "bat") c("Name", "PA") else c("Name", "IP")
  }

  tbl_node <- NULL
  for (t in html_elements(doc, "table")) {
    tb <- tryCatch(html_table(t, fill = TRUE), error = function(e) NULL)
    if (!is.null(tb) && all(need %in% names(tb)) && nrow(tb) > 5) {
      tbl_node <- t
      break
    }
  }
  if (is.null(tbl_node)) return(NULL)

  tb <- html_table(tbl_node, fill = TRUE)

  # html_table drops the hrefs, so pull the player ids from the anchors in
  # the Name column separately and bind them back on by row order.
  rows <- html_elements(tbl_node, "tbody tr")
  if (length(rows) == 0) rows <- html_elements(tbl_node, "tr")

  ids <- rows |>
    purrr::map_chr(function(tr) {
      a <- html_element(tr, "td a")
      if (inherits(a, "xml_missing")) return(NA_character_)
      h <- html_attr(a, "href")
      m <- stringr::str_match(h, "/players/[^/]+/([^/]+)/")[, 2]
      if (is.na(m)) NA_character_ else m
    })

  if (length(ids) == nrow(tb)) {
    tb$fg_id <- ids
  } else {
    warning("row/link mismatch on the ", stats, " leaderboard (",
            length(ids), " links vs ", nrow(tb), " rows); fg_id left blank")
    tb$fg_id <- NA_character_
  }

  # Guarantee the optional columns the downstream code references, so a
  # FanGraphs layout tweak degrades to NA instead of erroring.
  tb <- tb |>
    ensure_cols(c("SF", "HBP", "IBB", "GS", "ISO", "ERA")) |>
    ensure_cols_chr("Team")

  # The HTML leaderboard prints IP in thirds notation ("150.1" = 150 1/3), so
  # convert it here, once, at the point of parsing. The JSON player API returns
  # true decimals instead and must NOT be run through this -- applying it twice
  # turns 150.333 into 151.
  if (stats == "pit" && "IP" %in% names(tb)) tb$IP <- ip_to_num(tb$IP)

  # The Advanced tab renders rate columns as percentage strings -- "15.5%",
  # not 0.155. Everything downstream expects proportions, so anything whose
  # column name ends in "%" gets divided by 100 here, at the single point
  # where the string becomes a number. (The JSON player API already returns
  # proportions, which is why this only applies to the scraped HTML.)
  pct_cols <- names(tb)[grepl("%$", names(tb))]
  for (cl in pct_cols) tb[[cl]] <- as_num(tb[[cl]]) / 100

  tb |>
    filter(!is.na(Name), Name != "", Name != "Name") |>
    mutate(across(any_of(c("Age", "G", "GS", "AB", "PA", "H", "1B", "2B", "3B",
                           "HR", "R", "RBI", "BB", "IBB", "SO", "HBP", "SF",
                           "SH", "GDP", "SB", "CS", "W", "L", "SV", "ER",
                           "IP", "BF")), as_num)) |>
    mutate(
      team = if ("Team" %in% names(tb)) stringr::str_squish(Team) else NA_character_,
      # A numeric FanGraphs id means the player has an MLB/MiLB record;
      # an "sa..."-style id means FanGraphs only knows him from NPB. That is
      # a free, reliable flag for "this is a foreign import or an MLB
      # returnee", who is a very different evaluation problem.
      prior_us_experience = !is.na(fg_id) & !stringr::str_starts(fg_id, "sa")
    )
}

#' Scrape the current NPB leaderboard, one team at a time.
#'
#' FanGraphs' page-size and pagination controls are both client-side only. The
#' server renders exactly 30 rows regardless of `pagesize=2000`,
#' `pagesize=Infinity`, or `pagenum=2` -- all verified 2026-08-08. So a single
#' request can never return more than the first 30 rows of a 681-row list
#' sorted by batting average, which at qual=0 is mostly players with two plate
#' appearances.
#'
#' What *is* honoured server-side is `team` and `qual`. Slicing by team keeps
#' every request comfortably under the 30-row ceiling (Hanshin returns 14
#' batters at qual=100), so twelve small requests reconstruct the pool that one
#' large request refuses to give us.
#'
#' Team ids are not published anywhere, so rather than hard-code a guessed
#' mapping we sweep a small id range and keep whatever comes back. Invalid ids
#' return nothing and cost one cheap request.
#'
#' @param stats "bat" or "pit".
#' @param qual playing-time floor passed to FanGraphs. Keeps each team's page
#'   under 30 rows while still capturing everyone worth evaluating.
fetch_npb_leaderboard <- function(stats = c("bat", "pit"), refresh = FALSE,
                                  qual = NULL, team_ids = 1:20) {
  stats <- match.arg(stats)
  qual  <- qual %||% (if (stats == "bat") 50 else 20)

  slurp <- function(type) {
    acc <- list()
    for (tm in team_ids) {
      url <- sprintf(
        "%s/leaders/international/npb?stats=%s&type=%d&qual=%d&ind=0&team=%d",
        FG, stats, type, qual, tm)
      raw <- fetch_cached(url,
                          sprintf("fg_npb_lb_%s_t%d_q%d_tm%02d", stats, type, qual, tm),
                          sleep = FG_SLEEP, refresh = refresh)
      pgd <- parse_lb_page(raw, stats, type)
      if (!is.null(pgd) && nrow(pgd) > 0) acc[[length(acc) + 1]] <- pgd
    }
    acc
  }

  got <- slurp(0)
  if (length(got) == 0) {
    stop("could not fetch the FanGraphs NPB leaderboard (", stats, ")")
  }

  out <- bind_rows(got) |> distinct(fg_id, .keep_all = TRUE)

  # The Standard tab has no wRC+/ISO (hitters) or FIP (pitchers) -- those live
  # on the Advanced tab. Pull it too and join, otherwise those columns render
  # as empty cells in the app. Failure here is non-fatal: the model's inputs
  # all come from the Standard counting stats, so a missing Advanced tab costs
  # us display columns, not projections.
  adv <- tryCatch(bind_rows(slurp(1)) |> distinct(fg_id, .keep_all = TRUE),
                  error = function(e) NULL)

  if (!is.null(adv) && nrow(adv) > 0) {
    new_cols <- setdiff(names(adv), names(out))
    if (length(new_cols)) {
      out <- left_join(out, adv |> select(fg_id, all_of(new_cols)), by = "fg_id")
    }
    # Advanced overrides Standard for the few shared rate columns it computes
    # more carefully (it carries ISO and the percentage forms directly).
    for (cl in intersect(c("ISO", "wRC+", "FIP", "BABIP", "wOBA", "xFIP"), names(adv))) {
      m <- match(out$fg_id, adv$fg_id)
      out[[cl]] <- as_num(adv[[cl]][m])
    }
  } else {
    warning("Advanced leaderboard unavailable -- wRC+/ISO/FIP columns will be ",
            "blank in the app. Projections are unaffected.", call. = FALSE)
  }

  # A team slice that hits exactly 30 rows was probably truncated. Warn rather
  # than silently dropping the back end of that team's roster.
  hit_cap <- sum(vapply(got, nrow, integer(1)) >= 30)
  if (hit_cap > 0) {
    warning(hit_cap, " team page(s) returned the full 30 rows and may be ",
            "truncated. Raise `qual` to thin them out.", call. = FALSE)
  }

  if (nrow(out) < 60) {
    warning("Only ", nrow(out), " ", stats, " players came back across all ",
            "team slices -- the team filter may have stopped working. ",
            "Check data/raw/fg_npb_lb_", stats, "_* .", call. = FALSE)
  } else {
    message("  leaderboard: ", nrow(out), " ", stats, " players from ",
            length(got), " team slices (qual >= ", qual, ")")
  }

  out
}

# --- league context ------------------------------------------------------
#' Sum a leaderboard to get league rates.
#'
#' Only valid if `lb` really is the whole league. With pagination working that
#' is ~681 batters; without it, it is the top 30 by batting average and the
#' resulting "league average" is nonsense. `npb_current_context()` below
#' enforces that check -- call that, not this.
npb_context_from_leaderboard <- function(lb, kind = c("bat", "pit")) {
  kind <- match.arg(kind)
  s <- function(x) sum(lb[[x]], na.rm = TRUE)

  if (kind == "bat") {
    tibble(
      lg_k_pct  = s("SO") / s("PA"),
      lg_bb_pct = s("BB") / s("PA"),
      lg_hr_pa  = s("HR") / s("PA"),
      lg_iso    = (s("2B") + 2 * s("3B") + 3 * s("HR")) / s("AB"),
      source    = "leaderboard"
    )
  } else {
    tibble(
      lg_k9  = 9 * s("SO") / s("IP"),
      lg_bb9 = 9 * s("BB") / s("IP"),
      lg_hr9 = 9 * s("HR") / s("IP"),
      lg_era = 9 * s("ER") / s("IP"),
      source = "leaderboard"
    )
  }
}

#' League context for the most recent NPB season, preferring Baseball-Reference.
#'
#' BR gives true league totals from the team tables, which is strictly better
#' than reconstructing them from a player leaderboard -- and it is immune to
#' the pagination problem. The leaderboard sum is kept only as a fallback, and
#' only when the pool is big enough to plausibly be the whole league.
npb_current_context <- function(lb, kind = c("bat", "pit"), season = NULL) {
  kind <- match.arg(kind)
  path <- file.path(DIR_PROC, "npb_league_context.csv")

  if (file.exists(path)) {
    ctx <- read_csv(path, show_col_types = FALSE)
    ctx <- ctx |> filter(is.finite(if (kind == "bat") npb_lg_k_pct else npb_lg_k9))

    if (nrow(ctx) > 0) {
      target <- season %||% max(ctx$season, na.rm = TRUE)
      cs <- ctx |> filter(season == target)

      if (nrow(cs) > 0) {
        # Combine Central and Pacific, weighted by playing time.
        if (kind == "bat") {
          w <- cs$bat_PA
          out <- tibble(
            lg_k_pct  = weighted.mean(cs$npb_lg_k_pct,  w, na.rm = TRUE),
            lg_bb_pct = weighted.mean(cs$npb_lg_bb_pct, w, na.rm = TRUE),
            lg_hr_pa  = weighted.mean(cs$npb_lg_hr_pa,  w, na.rm = TRUE),
            lg_iso    = weighted.mean(cs$npb_lg_iso,    w, na.rm = TRUE),
            source    = paste0("bref ", target)
          )
        } else {
          w <- cs$pit_IP
          out <- tibble(
            lg_k9  = weighted.mean(cs$npb_lg_k9,  w, na.rm = TRUE),
            lg_bb9 = weighted.mean(cs$npb_lg_bb9, w, na.rm = TRUE),
            lg_hr9 = weighted.mean(cs$npb_lg_hr9, w, na.rm = TRUE),
            lg_era = weighted.mean(cs$npb_lg_era, w, na.rm = TRUE),
            source = paste0("bref ", target)
          )
        }
        return(out)
      }
    }
  }

  # Fallback. Refuse to pretend 30 leaders are a league.
  if (nrow(lb) < 200) {
    stop("No Baseball-Reference league context available, and the leaderboard ",
         "returned only ", nrow(lb), " players -- too few to stand in for a ",
         "league average.\nRun build_npb_context() (R/03_league_context.R) first.")
  }
  warning("Falling back to leaderboard-derived league context.", call. = FALSE)
  npb_context_from_leaderboard(lb, kind)
}

# --- per-player season history ------------------------------------------
#' All NPB seasons for one player, from the FanGraphs player API.
fetch_player_seasons <- function(fg_id, position = "P", refresh = FALSE) {
  if (is.na(fg_id)) return(NULL)

  url <- sprintf("%s/api/players/stats?playerid=%s&position=%s", FG, fg_id, position)
  raw <- fetch_cached(url, paste0("fg_player_", fg_id, "_", position),
                      sleep = FG_SLEEP, refresh = refresh, ext = "json")
  if (is.na(raw)) return(NULL)

  js <- tryCatch(jsonlite::fromJSON(raw, simplifyDataFrame = TRUE),
                 error = function(e) NULL)
  if (is.null(js) || is.null(js$data) || !is.data.frame(js$data) || nrow(js$data) == 0) {
    return(NULL)
  }

  d <- js$data
  # AbbLevel == "NPB" keeps us out of MLB/AAA/farm rows for players who have
  # bounced between leagues.
  d <- d[!is.na(d$AbbLevel) & d$AbbLevel == "NPB", , drop = FALSE]
  if (nrow(d) == 0) return(NULL)

  d$fg_id  <- fg_id
  d$season <- as.integer(d$aseason)
  d$birth  <- if (!is.null(js$playerInfo$BirthDate)) js$playerInfo$BirthDate else NA
  d$fg_name <- if (!is.null(js$playerInfo$firstLastName)) js$playerInfo$firstLastName else NA
  as_tibble(d)
}

#' Fetch histories for a set of players, with progress and throttling.
fetch_histories <- function(ids, positions, refresh = FALSE) {
  out <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    if (i %% 25 == 0) message("   ...", i, "/", length(ids))
    out[[i]] <- tryCatch(
      fetch_player_seasons(ids[i], positions[i], refresh = refresh),
      error = function(e) NULL
    )
  }
  bind_rows(out)
}

# --- assemble model inputs ----------------------------------------------
#' Turn a player-season row (FanGraphs schema) into the predictor set the
#' hitter model expects.
#' Rescale anything that arrived as a percentage rather than a proportion.
#'
#' Defence in depth for the "15.5 vs 0.155" problem. Parsing is supposed to
#' handle this, but a rate above 1 is impossible for K%, BB% or BABIP, so if
#' one shows up we know exactly what happened and can fix it rather than
#' propagating a 100x error into a projection.
as_proportion <- function(x) {
  x <- as_num(x)
  ifelse(!is.na(x) & x > 1.5, x / 100, x)
}

hitter_inputs <- function(d, ctx) {
  # wRC+ and ISO come from the player API and the Advanced leaderboard but
  # not the Standard one, so make sure they always exist.
  d |>
    ensure_cols(c("wRC+", "ISO", "BABIP", "K%", "BB%", "SF", "HBP")) |>
    mutate(
      k_pct  = coalesce(as_proportion(`K%`),  SO / PA),
      bb_pct = coalesce(as_proportion(`BB%`), BB / PA),
      hr_pa  = HR / PA,
      babip  = coalesce(as_proportion(BABIP),
                        (H - HR) / pmax(AB - SO - HR + coalesce(SF, 0), 1)),
      k_rel  = k_pct  / ctx$lg_k_pct,
      bb_rel = bb_pct / ctx$lg_bb_pct,
      hr_rel = hr_pa  / ctx$lg_hr_pa,
      Age    = as_num(Age)
    ) |>
    check_rate_sanity(c("k_pct", "bb_pct", "babip"), "hitter")
}

#' Fail loudly if a rate escaped as a percentage.
#'
#' The 100x bug shipped a 271 wRC+ projection that looked like a number rather
#' than an error. A rate above 1 is impossible, so refuse to pass it on.
check_rate_sanity <- function(d, cols, label) {
  for (cl in intersect(cols, names(d))) {
    bad <- sum(!is.na(d[[cl]]) & d[[cl]] > 1, na.rm = TRUE)
    if (bad > 0) {
      warning(bad, " ", label, " rows have ", cl, " > 1, which is impossible ",
              "for a rate -- a percentage column is being read as a ",
              "proportion somewhere upstream.", call. = FALSE)
    }
  }
  d
}

pitcher_inputs <- function(d, ctx) {
  d |>
    ensure_cols(c("ERA", "GS", "G")) |>
    mutate(
      IP     = as_num(IP),
      k9     = 9 * SO / pmax(IP, 1),
      bb9    = 9 * BB / pmax(IP, 1),
      hr9    = 9 * HR / pmax(IP, 1),
      k9_rel = k9  / ctx$lg_k9,
      bb9_rel = bb9 / ctx$lg_bb9,
      hr9_rel = hr9 / ctx$lg_hr9,
      is_sp  = as.integer(coalesce(as_num(GS), 0) / pmax(as_num(G), 1) > 0.5),
      Age    = as_num(Age)
    )
}

# --- the scorer ----------------------------------------------------------
#' Rank current NPB players as MLB targets.
#'
#' @param kind "bat" or "pit".
#' @param season target NPB season, or NULL for whatever the leaderboard
#'   currently shows. Any specific season is served from the player API.
#' @param min_pa,min_ip playing-time floor for inclusion.
#' @param age_range plausible posting/free-agency age window.
#' @param proj_pa,proj_ip MLB playing time to project counting stats at.
score_current <- function(kind = c("bat", "pit"),
                          season = NULL,
                          min_pa = 300, min_ip = 80,
                          age_range = c(21, 33),
                          proj_pa = 550, proj_ip = 150,
                          refresh = FALSE) {
  kind <- match.arg(kind)

  model_file <- file.path(DIR_MODEL,
                          if (kind == "bat") "hitter_models.rds" else "pitcher_models.rds")
  if (!file.exists(model_file)) {
    stop("No fitted model at ", model_file, ".\n",
         "Run run_all.R first -- the translation models are trained on the ",
         "historical NPB->MLB sample and cannot be skipped.")
  }
  fits <- readRDS(model_file)

  message("Fetching NPB ", kind, " leaderboard...")
  lb  <- fetch_npb_leaderboard(kind, refresh = refresh)
  ctx <- npb_current_context(lb, kind)

  message(sprintf("  %d players; league %s",
                  nrow(lb),
                  if (kind == "bat")
                    sprintf("K%% %.1f%%, BB%% %.1f%%", 100 * ctx$lg_k_pct, 100 * ctx$lg_bb_pct)
                  else
                    sprintf("K/9 %.2f, ERA %.2f", ctx$lg_k9, ctx$lg_era)))

  # Narrow to a plausible target pool *before* hitting the player API, so we
  # make ~100 requests instead of ~700.
  pt <- if (kind == "bat") "PA" else "IP"
  floor_pt <- if (kind == "bat") min_pa else min_ip

  pool <- lb |>
    mutate(Age = as_num(Age), .pt = as_num(.data[[pt]])) |>
    filter(!is.na(fg_id),
           .pt >= floor_pt * 0.6,          # loose here; tighten after we have
           Age >= age_range[1],            # full-season numbers
           Age <= age_range[2])

  message("  ", nrow(pool), " players pass the age/playing-time screen")

  if (is.null(season)) {
    src <- pool
    season_used <- NA_integer_
    message("  Using current-season leaderboard numbers.")
  } else {
    message("  Fetching per-player history for season ", season, "...")
    pos <- if (kind == "bat") "3B" else "P"
    hist <- fetch_histories(pool$fg_id, rep(pos, nrow(pool)), refresh = refresh)
    if (nrow(hist) == 0) stop("no player histories returned")

    src <- hist |>
      filter(season == !!season) |>
      left_join(select(pool, fg_id, team, prior_us_experience), by = "fg_id") |>
      mutate(Name = coalesce(fg_name, ""))
    season_used <- season
    message("  ", nrow(src), " players with a ", season, " NPB season")
  }

  src <- src |> mutate(.pt = as_num(.data[[pt]])) |> filter(.pt >= floor_pt)

  if (nrow(src) == 0) {
    warning("nothing left after the playing-time filter")
    return(tibble())
  }

  if (kind == "bat") {
    inp <- hitter_inputs(src, ctx)
    proj <- purrr::map_dfr(seq_len(nrow(inp)), function(i) {
      predict_hitter(fits, as.list(inp[i, ]), pa = proj_pa)
    })
    out <- bind_cols(
      inp |> transmute(Name, team, Age,
                       npb_PA        = PA,
                       npb_wRC_plus  = as_num(`wRC+`),
                       npb_K_pct     = k_pct,
                       npb_BB_pct    = bb_pct,
                       npb_ISO       = as_num(ISO),
                       prior_us_experience, fg_id),
      proj |> select(proj_PA = PA, proj_K_pct = K_pct, proj_BB_pct = BB_pct,
                     proj_HR = HR, proj_AVG = AVG, proj_OBP = OBP,
                     proj_SLG = SLG, proj_wOBA = wOBA, proj_wRC_plus = wRC_plus)
    ) |>
      arrange(desc(proj_wRC_plus))
  } else {
    inp <- pitcher_inputs(src, ctx)
    proj <- purrr::map_dfr(seq_len(nrow(inp)), function(i) {
      predict_pitcher(fits, as.list(inp[i, ]), ip = proj_ip)
    })
    out <- bind_cols(
      inp |> transmute(Name, team, Age,
                       npb_IP  = IP,
                       npb_ERA = as_num(ERA),
                       npb_K9  = k9, npb_BB9 = bb9, npb_HR9 = hr9, is_sp,
                       prior_us_experience, fg_id),
      proj |> select(proj_IP = IP, proj_K9 = K9, proj_BB9 = BB9,
                     proj_HR9 = HR9, proj_FIP = FIP)
    ) |>
      arrange(proj_FIP)
  }

  attr(out, "season") <- season_used
  attr(out, "league_context") <- ctx

  fname <- sprintf("targets_%s%s.csv", kind,
                   if (is.na(season_used)) "_current" else paste0("_", season_used))
  write_csv(out, file.path(DIR_PROC, fname))
  message("  Wrote ", fname)

  out
}

#' Convenience: score both sides and print the top of each.
score_all_current <- function(season = NULL, n = 15, ...) {
  h <- score_current("bat", season = season, ...)
  p <- score_current("pit", season = season, ...)

  cat("\n=== Top hitting targets (projected MLB wRC+) ===\n")
  print(as.data.frame(head(h, n)), digits = 3)
  cat("\n=== Top pitching targets (projected MLB FIP) ===\n")
  print(as.data.frame(head(p, n)), digits = 3)

  cat("\nReminder: these are 'if he reaches MLB and plays, expect this line'.\n",
      "They are not signing probabilities, and they do not know who is\n",
      "actually posting-eligible. See the survivorship note in README.md.\n", sep = "")

  invisible(list(hitters = h, pitchers = p))
}

# --- model-free screen ---------------------------------------------------
#' Rank the current NPB pool without the translation model.
#'
#' This needs no Baseball-Reference scrape and no fitted model -- just the
#' FanGraphs leaderboard. It is a *screen*, not a projection: it ranks players
#' by how far above their own NPB league they are, which is the raw material
#' the model later translates. Useful for seeing the target pool before
#' committing to the full pipeline, and as a sanity check on the model's
#' ordering afterwards.
#'
#' K%- and BB%- are indexed so 100 = league average. For K%- lower is better.
screen_current <- function(kind = c("bat", "pit"),
                           min_pa = 300, min_ip = 80,
                           age_range = c(21, 33),
                           refresh = FALSE) {
  kind <- match.arg(kind)

  lb  <- fetch_npb_leaderboard(kind, refresh = refresh)
  ctx <- npb_current_context(lb, kind)

  if (kind == "bat") {
    out <- hitter_inputs(lb, ctx) |>
      filter(as_num(PA) >= min_pa,
             Age >= age_range[1], Age <= age_range[2]) |>
      transmute(
        Name, team, Age, PA = as_num(PA),
        K_pct = k_pct, BB_pct = bb_pct, ISO = as_num(ISO),
        `K%-`  = 100 * k_rel,
        `BB%+` = 100 * bb_rel,
        `HR+`  = 100 * hr_rel,
        wRC_plus = as_num(`wRC+`),
        prior_us_experience, fg_id
      ) |>
      # No wRC+ on the Standard leaderboard, so fall back to an OPS-based
      # ordering when it is missing.
      arrange(desc(coalesce(wRC_plus, 100 * (`HR+` + `BB%+`) / 2 - `K%-`)))
  } else {
    out <- pitcher_inputs(lb, ctx) |>
      filter(IP >= min_ip, Age >= age_range[1], Age <= age_range[2]) |>
      transmute(
        Name, team, Age, IP, ERA = as_num(ERA),
        K9 = k9, BB9 = bb9, HR9 = hr9, is_sp,
        `K/9+`  = 100 * k9_rel,
        `BB/9-` = 100 * bb9_rel,
        `ERA-`  = 100 * as_num(ERA) / ctx$lg_era,
        prior_us_experience, fg_id
      ) |>
      arrange(`ERA-`)
  }

  attr(out, "league_context") <- ctx
  write_csv(out, file.path(DIR_PROC, sprintf("screen_%s_current.csv", kind)))
  out
}

#' Screen both sides and print the top of each. Runs off FanGraphs alone.
screen_all_current <- function(n = 20, ...) {
  h <- screen_current("bat", ...)
  p <- screen_current("pit", ...)

  cat("\n=== NPB hitters, current season (league-relative screen) ===\n")
  print(as.data.frame(head(h, n)), digits = 3)
  cat("\n=== NPB pitchers, current season (league-relative screen) ===\n")
  print(as.data.frame(head(p, n)), digits = 3)
  cat("\nThis is a screen, not a projection. It says who is furthest above\n",
      "his own NPB league -- not what he would do in MLB. For that you need\n",
      "the fitted translation models (run_all.R), then score_all_current().\n", sep = "")

  invisible(list(hitters = h, pitchers = p))
}

# Sourcing this file only defines functions.
#   screen_all_current()  -- works now, FanGraphs only, no model needed
#   score_all_current()   -- needs run_all.R to have fit the models first
