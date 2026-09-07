# 03_league_context.R -----------------------------------------------------
# League-average run environment for NPB (Central + Pacific) and MLB.
#
# This matters more than it might seem. NPB's run environment is not stable:
# the 2011-12 "unified ball" years were a genuine dead-ball era (league ERA
# near 2.60), while the late 2000s and early 2020s are much livelier. A raw
# NPB ERA of 3.00 means something completely different in 2011 than in 2007.
# Comparing a player to his own league-season is the only way to keep the
# training sample coherent across three decades.

source(file.path("R", "00_config.R"))

BREF <- "https://www.baseball-reference.com"

# --- season index --------------------------------------------------------
#' Map each NPB season to its Baseball-Reference register league id.
#' BR uses opaque hashed ids, so we read them off the league index page.
npb_season_index <- function(refresh = FALSE) {
  purrr::map_dfr(c("JPCL", "JPPL"), function(code) {
    raw <- fetch_cached(
      sprintf("%s/register/league.cgi?code=%s&class=Fgn", BREF, code),
      paste0("league_index_", code), refresh = refresh
    )
    if (is.na(raw)) return(tibble())
    doc <- read_bref_html(raw)

    a <- html_elements(doc, "a")

    # The season links are bare years -- <a ...>2019</a> -- with nothing after
    # them. Anchoring on "^\\d{4}\\s" (year followed by whitespace) matched
    # nothing and silently produced an empty index, which then blew up two
    # steps later with a missing-column error. Match a leading year and pull
    # it out with str_extract instead of assuming anything follows it.
    out <- tibble(
      href = html_attr(a, "href"),
      txt  = stringr::str_squish(html_text2(a))
    ) |>
      filter(stringr::str_detect(coalesce(href, ""), "league\\.cgi\\?id="),
             stringr::str_detect(coalesce(txt, ""), "^\\d{4}\\b")) |>
      mutate(
        season = as.integer(stringr::str_extract(txt, "^\\d{4}")),
        lg_id  = stringr::str_match(href, "id=([a-z0-9]+)")[, 2],
        lg     = code
      ) |>
      filter(!is.na(season), !is.na(lg_id)) |>
      distinct(season, lg, .keep_all = TRUE) |>
      select(season, lg, lg_id)

    if (nrow(out) == 0) {
      warning("No season links found on the ", code, " index page. ",
              "Inspect data/raw/league_index_", code, ".html", call. = FALSE)
    }
    out
  })
}

# --- per-season league totals -------------------------------------------
#' Sum the team rows on a league-season page to get league totals.
npb_league_totals <- function(season, lg, lg_id, refresh = FALSE) {
  raw <- fetch_cached(sprintf("%s/register/league.cgi?id=%s", BREF, lg_id),
                      paste0("league_", lg, "_", season), refresh = refresh)
  if (is.na(raw)) return(NULL)
  doc <- read_bref_html(raw)

  tbls <- html_elements(doc, "table")
  ids  <- html_attr(tbls, "id")

  # Do not trust the table's id attribute. BR renames these between page
  # variants and the register pages do not always use "bat"/"pitch" at all.
  # Identify the table by the columns it actually has, scanning every table
  # on the page and keeping the widest match.
  grab <- function(need) {
    best <- NULL; best_n <- -1
    for (i in seq_along(tbls)) {
      t <- tryCatch(html_table(tbls[[i]], fill = TRUE), error = function(e) NULL)
      if (is.null(t) || nrow(t) < 2) next
      if (!all(need %in% names(t))) next
      if (ncol(t) > best_n) { best <- t; best_n <- ncol(t) }
    }
    best
  }

  # AB distinguishes a batting table from a pitching one (pitching tables
  # carry H/HR/BB/SO too, so those alone are ambiguous).
  bat <- grab(c("AB", "H", "HR", "BB", "SO"))
  pit <- grab(c("IP", "ER", "BB", "SO"))

  agg <- function(t, cols, is_pitching = FALSE) {
    if (is.null(t)) return(NULL)

    first_col <- names(t)[1]
    t <- t |>
      filter(!stringr::str_detect(coalesce(as.character(.data[[first_col]]), ""),
                                  "League|Total|Average|^$"))
    if (nrow(t) == 0) return(NULL)

    t <- ensure_cols_chr(t, cols) |> mutate(across(all_of(cols), as_num))
    # IP is printed in thirds; convert before summing.
    if (is_pitching && "IP" %in% names(t)) t$IP <- ip_to_num(t$IP)

    out <- t |> summarise(across(all_of(cols), ~ sum(.x, na.rm = TRUE)))

    # Register team tables do not always carry PA. Reconstruct it when the
    # pieces are there, rather than losing the whole season's context.
    if (!is_pitching && (is.na(out$PA) || out$PA == 0)) {
      out$PA <- out$AB + out$BB + coalesce(out$HBP, 0) +
                coalesce(out$SF, 0) + coalesce(out$SH, 0)
    }
    out
  }

  b <- agg(bat, c("PA", "AB", "H", "2B", "3B", "HR", "BB", "SO", "HBP", "SF", "SH"))
  p <- agg(pit, c("IP", "ER", "SO", "BB", "HR", "BF"), is_pitching = TRUE)

  tibble(season = season, lg = lg) |>
    bind_cols(if (is.null(b)) tibble() else rename_with(b, ~ paste0("bat_", .x))) |>
    bind_cols(if (is.null(p)) tibble() else rename_with(p, ~ paste0("pit_", .x)))
}

#' Print what is actually on a league-season page.
#'
#' If the context build comes back empty, run this on one season to see which
#' tables exist and what columns they carry, then adjust `grab()` above.
diagnose_league_page <- function(season = 2019, lg = "JPCL") {
  path <- file.path(DIR_RAW, sprintf("league_%s_%d.html", lg, season))
  if (!file.exists(path)) {
    stop("No cached page at ", path, ". Run build_npb_context() first.")
  }
  doc <- read_bref_html(read_file(path))
  tbls <- html_elements(doc, "table")
  cat("Found", length(tbls), "tables on", lg, season, "\n\n")
  for (i in seq_along(tbls)) {
    t <- tryCatch(html_table(tbls[[i]], fill = TRUE), error = function(e) NULL)
    cat(sprintf("[%d] id=%-28s rows=%-4s cols: %s\n",
                i,
                coalesce(html_attr(tbls[[i]], "id"), "<none>"),
                if (is.null(t)) "?" else nrow(t),
                if (is.null(t)) "<unparseable>" else paste(names(t), collapse = ", ")))
  }
  invisible(NULL)
}

build_npb_context <- function(refresh = FALSE) {
  idx <- npb_season_index(refresh = refresh) |>
    filter(season >= MIN_TRANSITION_YEAR - 1)

  # Fail loudly here rather than letting an empty index sail through and
  # surface as a missing-column error further downstream.
  if (nrow(idx) == 0) {
    warning("Could not read the NPB season index -- no league-seasons to ",
            "fetch. Skipping league context; the models will fall back to ",
            "raw rates over fixed baselines.", call. = FALSE)
    empty <- tibble(season = integer(), lg = character()) |>
      ensure_cols(c("npb_lg_k_pct", "npb_lg_bb_pct", "npb_lg_iso",
                    "npb_lg_hr_pa", "npb_lg_era", "npb_lg_k9",
                    "npb_lg_bb9", "npb_lg_hr9"))
    write_csv(empty, file.path(DIR_PROC, "npb_league_context.csv"))
    return(empty)
  }

  message("Pulling league context for ", nrow(idx), " NPB league-seasons...")

  ctx <- purrr::pmap_dfr(
    list(idx$season, idx$lg, idx$lg_id),
    function(s, l, i) {
      out <- tryCatch(npb_league_totals(s, l, i, refresh = refresh),
                      error = function(e) NULL)
      if (is.null(out)) tibble(season = s, lg = l) else out
    }
  )

  # Every season can fail to parse, in which case none of the bat_/pit_
  # columns exist at all. Create them as NA first so the arithmetic below is
  # always legal, then report honestly on how much actually came back.
  ctx <- ctx |>
    ensure_cols(c("bat_PA", "bat_AB", "bat_H", "bat_2B", "bat_3B", "bat_HR",
                  "bat_BB", "bat_SO", "pit_IP", "pit_ER", "pit_SO",
                  "pit_BB", "pit_HR")) |>
    mutate(
      npb_lg_k_pct  = bat_SO / bat_PA,
      npb_lg_bb_pct = bat_BB / bat_PA,
      npb_lg_iso    = (bat_2B + 2 * bat_3B + 3 * bat_HR) / bat_AB,
      npb_lg_hr_pa  = bat_HR / bat_PA,
      npb_lg_era    = 9 * pit_ER / pit_IP,
      npb_lg_k9     = 9 * pit_SO / pit_IP,
      npb_lg_bb9    = 9 * pit_BB / pit_IP,
      npb_lg_hr9    = 9 * pit_HR / pit_IP
    )

  got <- sum(is.finite(ctx$npb_lg_k_pct))
  if (got == 0) {
    warning("NPB league context came back empty for all ", nrow(ctx),
            " league-seasons.\n",
            "The pipeline will continue -- the models fall back to raw rates ",
            "over fixed baselines, they just lose the era adjustment.\n",
            "To debug: diagnose_league_page(2019, 'JPCL')",
            call. = FALSE)
  } else if (got < nrow(ctx)) {
    message("  league context parsed for ", got, "/", nrow(ctx), " league-seasons")
  } else {
    message("  league context parsed for all ", got, " league-seasons")
  }

  write_csv(ctx, file.path(DIR_PROC, "npb_league_context.csv"))
  ctx
}

# --- MLB context ---------------------------------------------------------
#' MLB league-wide rates by season, from BR's year-by-year league totals.
build_mlb_context <- function(refresh = FALSE) {
  raw <- fetch_cached(paste0(BREF, "/leagues/majors/bat.shtml"),
                      "mlb_bat_totals", refresh = refresh)
  raw_p <- fetch_cached(paste0(BREF, "/leagues/majors/pitch.shtml"),
                        "mlb_pitch_totals", refresh = refresh)

  pick <- function(r, need) {
    if (is.na(r)) return(NULL)
    doc <- read_bref_html(r)
    for (t in html_elements(doc, "table")) {
      tb <- tryCatch(html_table(t, fill = TRUE), error = function(e) NULL)
      if (!is.null(tb) && all(need %in% names(tb))) return(tb)
    }
    NULL
  }

  b <- pick(raw,   c("Year", "PA", "SO", "BB", "HR", "AB"))
  p <- pick(raw_p, c("Year", "IP", "ER", "SO", "BB", "HR"))

  clean <- function(t) {
    if (is.null(t)) return(tibble(season = integer()))
    t |>
      filter(stringr::str_detect(as.character(Year), "^\\d{4}$")) |>
      mutate(across(everything(), as_num)) |>
      rename(season = Year)
  }

  bb <- clean(b); pp <- clean(p)

  if (nrow(bb) == 0 && nrow(pp) == 0) {
    warning("MLB league context came back empty -- BR's league-totals pages ",
            "did not parse. The pipeline continues without an MLB era ",
            "adjustment.", call. = FALSE)
  }

  # Suffix the two sides explicitly rather than relying on join suffixes.
  # Those only fire on *shared* column names, so if one table fails to parse
  # the other side's columns silently come through unsuffixed and every
  # downstream reference misses.
  if (nrow(bb)) bb <- rename_with(bb, ~ paste0(.x, "_bat"), -season)
  if (nrow(pp)) pp <- rename_with(pp, ~ paste0(.x, "_pit"), -season)

  ctx <- full_join(bb, pp, by = "season") |>
    # Same guard as the NPB side: if either table failed to parse, these
    # columns will not exist and the arithmetic below would throw.
    ensure_cols(c("PA_bat", "AB_bat", "SO_bat", "BB_bat", "HR_bat",
                  "IP_pit", "ER_pit", "SO_pit", "BB_pit", "HR_pit")) |>
    mutate(
      mlb_lg_k_pct  = SO_bat / PA_bat,
      mlb_lg_bb_pct = BB_bat / PA_bat,
      mlb_lg_hr_pa  = HR_bat / PA_bat,
      mlb_lg_era    = 9 * ER_pit / IP_pit,
      mlb_lg_k9     = 9 * SO_pit / IP_pit,
      mlb_lg_bb9    = 9 * BB_pit / IP_pit,
      mlb_lg_hr9    = 9 * HR_pit / IP_pit
    ) |>
    filter(!is.na(season), season >= MIN_TRANSITION_YEAR - 1)

  write_csv(ctx, file.path(DIR_PROC, "mlb_league_context.csv"))
  ctx
}

# Sourcing this file only defines functions.
# run_all.R calls build_npb_context() and build_mlb_context().
