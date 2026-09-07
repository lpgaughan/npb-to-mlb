# 04_build_transitions.R --------------------------------------------------
# Turn the raw season logs into one row per NPB -> MLB transition:
# the player's final NPB season paired with his first MLB season after it.
#
# Two details that are easy to get wrong and that matter a lot:
#
# 1. "First MLB season" means the first MLB season *after* the NPB stint, not
#    the first of the player's career. Ryan Vogelsong pitched in MLB from
#    2000-06, spent 2007-09 in NPB, then came back in 2011. His transition
#    year is 2011.
#
# 2. Players are only included if the move was reasonably direct (<= 2 years
#    between the last NPB season and the first MLB one). A longer gap usually
#    means injury, independent ball, or a stint in the minors, and the final
#    NPB year stops being a fair read on what the player was at the time.

source(file.path("R", "00_config.R"))

# --- rate stats ----------------------------------------------------------
bat_rates <- function(df) {
  df |>
    mutate(
      HBP = coalesce(HBP, 0), SF = coalesce(SF, 0), IBB = coalesce(IBB, 0),
      `2B` = coalesce(`2B`, 0), `3B` = coalesce(`3B`, 0),
      k_pct   = SO / PA,
      bb_pct  = BB / PA,
      ubb_pct = (BB - IBB) / PA,
      hr_pa   = HR / PA,
      singles = H - `2B` - `3B` - HR,
      iso     = (`2B` + 2 * `3B` + 3 * HR) / AB,
      babip   = (H - HR) / pmax(AB - SO - HR + SF, 1),
      avg     = H / AB,
      obp     = (H + BB + HBP) / pmax(AB + BB + HBP + SF, 1),
      slg     = (singles + 2 * `2B` + 3 * `3B` + 4 * HR) / AB,
      ops     = obp + slg,
      # Fixed-weight wOBA. Using constant weights rather than season-specific
      # ones is fine here because everything gets expressed relative to the
      # league right afterwards, which absorbs the era drift.
      woba    = (0.69 * (BB - IBB) + 0.72 * HBP + 0.89 * singles +
                 1.27 * `2B` + 1.62 * `3B` + 2.10 * HR) /
                pmax(AB + BB - IBB + SF + HBP, 1)
    )
}

pitch_rates <- function(df) {
  df |>
    mutate(
      era   = 9 * ER / pmax(IP, 1),
      k9    = 9 * SO / pmax(IP, 1),
      bb9   = 9 * BB / pmax(IP, 1),
      hr9   = 9 * HR / pmax(IP, 1),
      h9    = 9 * H / pmax(IP, 1),
      whip  = (BB + H) / pmax(IP, 1),
      # FIP without the season constant; the constant is added back after
      # league-adjusting so the scale lands where people expect it.
      fip_raw = (13 * HR + 3 * BB - 2 * SO) / pmax(IP, 1)
    )
}

# --- transition extraction ----------------------------------------------
#' Locate the season pair that represents a player's NPB -> MLB crossing.
#'
#' The naive definition -- last NPB season, then first MLB season after it --
#' is wrong for anyone who went to MLB and later came *back* to Japan, which
#' is a large share of the sample: Shinjo, Johjima, Kazuo Matsui, Iguchi,
#' Nishioka, Fukudome, Akiyama, Tsutsugo, Nakamura, Kensuke Tanaka. For all of
#' them "last NPB season" is the end of their career, years after the crossing
#' we actually want, so they were silently dropped.
#'
#' Instead: for every qualifying MLB season, find the most recent qualifying
#' NPB season before it. Any pair within two years is a crossing. Take the
#' earliest such crossing, which is the move from NPB to MLB.
#'
#' @return list(npb_year, mlb_year, gap) or list(reason = "...") on failure.
find_transition_years <- function(npb, mlb, kind) {
  pt_col  <- if (kind == "bat") "PA" else "IP"
  min_npb <- if (kind == "bat") MIN_NPB_PA else MIN_NPB_IP
  min_mlb <- if (kind == "bat") MIN_MLB_PA else MIN_MLB_IP

  if (is.null(npb) || nrow(npb) == 0) return(list(reason = "no NPB rows"))
  if (is.null(mlb) || nrow(mlb) == 0) return(list(reason = "no MLB rows"))

  npb_ok <- npb |> filter(.data[[pt_col]] >= min_npb)
  if (nrow(npb_ok) == 0) {
    return(list(reason = sprintf("NPB best %s = %.0f (< %d)", pt_col,
                suppressWarnings(max(npb[[pt_col]], na.rm = TRUE)), min_npb)))
  }

  mlb_ok <- mlb |> filter(.data[[pt_col]] >= min_mlb)
  if (nrow(mlb_ok) == 0) {
    return(list(reason = sprintf("MLB best %s = %.0f (< %d)", pt_col,
                suppressWarnings(max(mlb[[pt_col]], na.rm = TRUE)), min_mlb)))
  }

  cands <- purrr::map_dfr(sort(unique(mlb_ok$Year)), function(ym) {
    prior <- npb_ok |> filter(Year < ym)
    if (nrow(prior) == 0) return(NULL)
    yn <- max(prior$Year)
    if (ym - yn > 2) return(NULL)
    tibble(mlb_year = ym, npb_year = yn, gap = ym - yn)
  })

  if (nrow(cands) == 0) {
    return(list(reason = sprintf(
      "no MLB season within 2 yrs of an NPB season (NPB thru %d, MLB from %d)",
      max(npb_ok$Year), min(mlb_ok$Year))))
  }

  c1 <- cands |> arrange(mlb_year) |> slice(1)

  if (c1$npb_year < MIN_TRANSITION_YEAR - 1) {
    return(list(reason = sprintf("crossing %d -> %d predates cutoff (%d)",
                                 c1$npb_year, c1$mlb_year, MIN_TRANSITION_YEAR)))
  }

  list(npb_year = c1$npb_year, mlb_year = c1$mlb_year, gap = c1$gap)
}

#' Given one player's NPB and MLB season logs, return the transition row.
make_transition <- function(npb, mlb, kind) {
  pt_col  <- if (kind == "bat") "PA" else "IP"
  min_npb <- if (kind == "bat") MIN_NPB_PA else MIN_NPB_IP
  min_mlb <- if (kind == "bat") MIN_MLB_PA else MIN_MLB_IP

  tr <- find_transition_years(npb, mlb, kind)
  if (!is.null(tr$reason)) return(NULL)

  final_npb <- npb |> filter(Year == tr$npb_year, .data[[pt_col]] >= min_npb) |> slice(1)
  first_mlb <- mlb |> filter(Year == tr$mlb_year, .data[[pt_col]] >= min_mlb) |> slice(1)
  if (nrow(final_npb) == 0 || nrow(first_mlb) == 0) return(NULL)
  gap <- tr$gap

  bind_cols(
    final_npb |> select(-any_of(c("name", "bref_mlb_id"))) |>
      rename_with(~ paste0("npb_", .x)),
    first_mlb |> select(-any_of(c("name", "bref_mlb_id"))) |>
      rename_with(~ paste0("mlb_", .x))
  ) |>
    mutate(
      bref_mlb_id = npb$bref_mlb_id[1],
      name        = npb$name[1],
      gap_years   = gap
    )
}

#' Report, per player, why he did or did not become a usable transition row.
#'
#' 68 players with confirmed NPB service yielding only 8 hitter transitions
#' means the filters are throwing away most of the sample. This prints the
#' reason for every one so the loss can be attributed rather than guessed at.
diagnose_transitions <- function(kind = c("bat", "pitch")) {
  kind <- match.arg(kind)
  stem <- if (kind == "bat") "batting" else "pitching"

  npb_raw <- read_csv(file.path(DIR_RAW, paste0("npb_", stem, "_raw.csv")),
                      show_col_types = FALSE)
  mlb_raw <- read_csv(file.path(DIR_RAW, paste0("mlb_", stem, "_raw.csv")),
                      show_col_types = FALSE)

  ids <- union(unique(npb_raw$bref_mlb_id), unique(mlb_raw$bref_mlb_id))

  # Share find_transition_years() with the builder so the diagnostic can never
  # disagree with what actually gets kept. The previous version reimplemented
  # the checks and quietly missed the era cutoff, reporting 9 kept when the
  # builder produced 8.
  out <- purrr::map_dfr(ids, function(id) {
    n <- filter(npb_raw, bref_mlb_id == id)
    m <- filter(mlb_raw, bref_mlb_id == id)
    nm <- c(n$name, m$name)[1]

    tr <- find_transition_years(n, m, kind)
    tibble(
      bref_mlb_id = id,
      name   = nm,
      reason = tr$reason %||% "KEPT",
      npb_year = tr$npb_year %||% NA_integer_,
      mlb_year = tr$mlb_year %||% NA_integer_
    )
  })

  cat("\n", kind, " transitions: ", sum(out$reason == "KEPT"), " kept of ",
      nrow(out), " players\n\n", sep = "")
  cat("--- dropped ---\n")
  print(as.data.frame(out |> filter(reason != "KEPT") |> select(name, reason) |>
                        arrange(reason)))
  cat("\n--- kept ---\n")
  print(as.data.frame(out |> filter(reason == "KEPT") |>
                        select(name, npb_year, mlb_year)))

  invisible(out)
}

build_transitions <- function(kind = c("bat", "pitch")) {
  kind <- match.arg(kind)

  stem <- if (kind == "bat") "batting" else "pitching"
  npb_raw <- read_csv(file.path(DIR_RAW, paste0("npb_", stem, "_raw.csv")),
                      show_col_types = FALSE)
  mlb_raw <- read_csv(file.path(DIR_RAW, paste0("mlb_", stem, "_raw.csv")),
                      show_col_types = FALSE)

  rate_fn <- if (kind == "bat") bat_rates else pitch_rates
  npb_raw <- rate_fn(npb_raw)
  mlb_raw <- rate_fn(mlb_raw)

  ids <- intersect(unique(npb_raw$bref_mlb_id), unique(mlb_raw$bref_mlb_id))

  out <- purrr::map_dfr(ids, function(id) {
    tryCatch(
      make_transition(filter(npb_raw, bref_mlb_id == id),
                      filter(mlb_raw, bref_mlb_id == id),
                      kind),
      error = function(e) NULL
    )
  })

  if (nrow(out) == 0) {
    warning("no ", kind, " transitions found -- check the scrape output")
    return(out)
  }

  # --- league adjustment -------------------------------------------------
  npb_ctx_path <- file.path(DIR_PROC, "npb_league_context.csv")
  mlb_ctx_path <- file.path(DIR_PROC, "mlb_league_context.csv")

  if (file.exists(npb_ctx_path) && file.exists(mlb_ctx_path)) {
    npb_ctx <- read_csv(npb_ctx_path, show_col_types = FALSE)
    mlb_ctx <- read_csv(mlb_ctx_path, show_col_types = FALSE)

    # NPB context is per league; average the two if we cannot match the
    # player's exact league (BR's Lg field is JPCL / JPPL).
    npb_ctx2 <- npb_ctx |>
      mutate(npb_Lg = lg) |>
      select(npb_Year = season, npb_Lg, starts_with("npb_lg_"))

    out <- out |>
      left_join(npb_ctx2, by = c("npb_Year", "npb_Lg")) |>
      left_join(select(mlb_ctx, mlb_Year = season, starts_with("mlb_lg_")),
                by = "mlb_Year")

    # Express every rate as a ratio to its own league-season. A value of
    # 1.20 means "20% above league", which is comparable across eras and
    # across the NPB/MLB boundary.
    rel <- function(x, lg) ifelse(is.na(lg) | lg == 0, NA_real_, x / lg)

    if (kind == "bat") {
      out <- out |>
        mutate(
          npb_k_rel   = rel(npb_k_pct,  npb_lg_k_pct),
          npb_bb_rel  = rel(npb_bb_pct, npb_lg_bb_pct),
          npb_iso_rel = rel(npb_iso,    npb_lg_iso),
          npb_hr_rel  = rel(npb_hr_pa,  npb_lg_hr_pa),
          mlb_k_rel   = rel(mlb_k_pct,  mlb_lg_k_pct),
          mlb_bb_rel  = rel(mlb_bb_pct, mlb_lg_bb_pct),
          mlb_hr_rel  = rel(mlb_hr_pa,  mlb_lg_hr_pa)
        )
    } else {
      out <- out |>
        mutate(
          npb_era_rel = rel(npb_lg_era, npb_era),   # ERA- style: higher = better
          npb_k9_rel  = rel(npb_k9,  npb_lg_k9),
          npb_bb9_rel = rel(npb_bb9, npb_lg_bb9),
          npb_hr9_rel = rel(npb_hr9, npb_lg_hr9),
          mlb_era_rel = rel(mlb_lg_era, mlb_era),
          mlb_k9_rel  = rel(mlb_k9,  mlb_lg_k9),
          mlb_bb9_rel = rel(mlb_bb9, mlb_lg_bb9),
          mlb_hr9_rel = rel(mlb_hr9, mlb_lg_hr9)
        )
    }
  } else {
    message("No league context files found; skipping league adjustment. ",
            "Run R/03_league_context.R to enable it.")
  }

  write_csv(out, file.path(DIR_PROC, sprintf("transitions_%s.csv", kind)))
  message("Built ", nrow(out), " ", kind, " transitions.")
  out
}

# Sourcing this file only defines functions. run_all.R calls build_transitions().
