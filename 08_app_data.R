# 08_app_data.R -----------------------------------------------------------
# Data access layer for the Shiny app.
#
# The app never scrapes on its own. Everything here reads the on-disk cache
# written by the pipeline, and reports honestly when a piece is missing so the
# UI can degrade instead of erroring. The one exception is refresh_leaderboards(),
# which the user triggers explicitly from a button.

source(file.path("R", "00_config.R"))
source(file.path("R", "05_fit_models.R"))
source(file.path("R", "07_score_current.R"))

# --- availability --------------------------------------------------------
#' What does this installation actually have on disk?
app_status <- function() {
  f <- function(...) file.path(...)
  list(
    hitter_model  = file.exists(f(DIR_MODEL, "hitter_models.rds")),
    pitcher_model = file.exists(f(DIR_MODEL, "pitcher_models.rds")),
    trans_bat     = file.exists(f(DIR_PROC, "transitions_bat.csv")),
    trans_pit     = file.exists(f(DIR_PROC, "transitions_pitch.csv")),
    validation    = file.exists(f(DIR_PROC, "validation.csv")),
    factors       = file.exists(f(DIR_PROC, "translation_factors.csv")),
    lb_bat        = length(list.files(DIR_RAW, "^fg_npb_lb_bat_")) > 0,
    lb_pit        = length(list.files(DIR_RAW, "^fg_npb_lb_pit_")) > 0
  )
}

have_models <- function() {
  s <- app_status()
  s$hitter_model && s$pitcher_model
}

# --- cached readers ------------------------------------------------------
read_if <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read_csv(path, show_col_types = FALSE, ...), error = function(e) NULL)
}

get_transitions <- function(kind = c("bat", "pitch")) {
  kind <- match.arg(kind)
  read_if(file.path(DIR_PROC, sprintf("transitions_%s.csv", kind)))
}

get_validation <- function() read_if(file.path(DIR_PROC, "validation.csv"))
get_factors    <- function() read_if(file.path(DIR_PROC, "translation_factors.csv"))

get_models <- function(kind = c("bat", "pit")) {
  kind <- match.arg(kind)
  p <- file.path(DIR_MODEL,
                 if (kind == "bat") "hitter_models.rds" else "pitcher_models.rds")
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

#' Force a re-pull of the FanGraphs leaderboards (the app's only network call).
refresh_leaderboards <- function() {
  fetch_npb_leaderboard("bat", refresh = TRUE)
  fetch_npb_leaderboard("pit", refresh = TRUE)
  invisible(TRUE)
}

# --- the current NPB pool, with model inputs attached --------------------
#' Load the current NPB pool and compute every predictor the models need.
#'
#' Returns the *unfiltered* pool with model inputs already attached, so the
#' app can filter and re-project reactively without refetching anything.
#' Uses the on-disk cache, so this is fast after the first call.
current_pool <- function(kind = c("bat", "pit")) {
  kind <- match.arg(kind)

  lb <- tryCatch(fetch_npb_leaderboard(kind), error = function(e) NULL)
  if (is.null(lb) || nrow(lb) == 0) return(NULL)

  ctx <- npb_current_context(lb, kind)
  out <- if (kind == "bat") hitter_inputs(lb, ctx) else pitcher_inputs(lb, ctx)

  attr(out, "league_context") <- ctx
  out
}

# --- projection ----------------------------------------------------------
#' Project a whole pool at a given playing-time assumption.
#'
#' Returns NULL when the models have not been fit, which the UI treats as
#' "show the screen instead" rather than as an error.
project_pool <- function(pool, kind = c("bat", "pit"), pt = NULL) {
  kind <- match.arg(kind)
  fits <- get_models(kind)
  if (is.null(fits) || is.null(pool) || nrow(pool) == 0) return(NULL)

  pt <- pt %||% (if (kind == "bat") 550 else 150)

  rows <- lapply(seq_len(nrow(pool)), function(i) {
    r <- as.list(pool[i, ])
    tryCatch(
      if (kind == "bat") predict_hitter(fits, r, pa = pt)
      else               predict_pitcher(fits, r, ip = pt),
      error = function(e) NULL
    )
  })

  ok <- !vapply(rows, is.null, logical(1))
  if (!any(ok)) return(NULL)

  proj <- bind_rows(rows[ok])
  bind_cols(pool[ok, ], proj |> rename_with(~ paste0("proj_", .x)))
}

# --- comparables ---------------------------------------------------------
#' Nearest neighbours in the historical transition set.
#'
#' Distance is computed on the same league-relative predictors the model uses,
#' standardised so no single component dominates. The point is not precision --
#' with ~60 players in the pool the neighbours are rough. It is that a name you
#' recognise ("this profile looks like Yoshida did") is a much better sanity
#' check on a projection than a number on its own.
find_comps <- function(player, kind = c("bat", "pit"), n = 5) {
  kind <- match.arg(kind)
  tr <- get_transitions(if (kind == "bat") "bat" else "pitch")
  if (is.null(tr) || nrow(tr) == 0) return(NULL)

  # League-relative fields can be absent (NULL) *or* present-but-NA, so fall
  # back on both. Using %||% alone would let an NA through and kill the
  # distance calculation.
  pk <- function(nm, fallback) {
    v <- player[[nm]]
    if (is.null(v) || length(v) == 0 || !is.finite(v)) fallback else v
  }

  if (kind == "bat") {
    tr <- tr |>
      ensure_cols(c("npb_k_rel", "npb_bb_rel", "npb_hr_rel")) |>
      mutate(
        f1 = coalesce(npb_k_rel,  npb_k_pct  / 0.170),
        f2 = coalesce(npb_bb_rel, npb_bb_pct / 0.085),
        f3 = coalesce(npb_hr_rel, npb_hr_pa  / 0.026),
        f4 = npb_Age
      )
    target <- c(
      pk("k_rel",  player$k_pct  / 0.170),
      pk("bb_rel", player$bb_pct / 0.085),
      pk("hr_rel", player$hr_pa  / 0.026),
      pk("Age", NA_real_)
    )
    show <- c("name", "npb_Year", "npb_Age", "mlb_Year", "mlb_PA",
              "mlb_woba", "mlb_HR", "mlb_k_pct", "mlb_bb_pct")
  } else {
    tr <- tr |>
      ensure_cols(c("npb_k9_rel", "npb_bb9_rel", "npb_hr9_rel")) |>
      mutate(
        f1 = coalesce(npb_k9_rel,  npb_k9  / 7.5),
        f2 = coalesce(npb_bb9_rel, npb_bb9 / 3.0),
        f3 = coalesce(npb_hr9_rel, npb_hr9 / 0.9),
        f4 = npb_Age
      )
    target <- c(
      pk("k9_rel",  player$k9  / 7.5),
      pk("bb9_rel", player$bb9 / 3.0),
      pk("hr9_rel", player$hr9 / 0.9),
      pk("Age", NA_real_)
    )
    show <- c("name", "npb_Year", "npb_Age", "mlb_Year", "mlb_IP",
              "mlb_era", "mlb_k9", "mlb_bb9")
  }

  F <- as.matrix(tr[, c("f1", "f2", "f3", "f4")])
  keep <- stats::complete.cases(F) & is.finite(rowSums(F))
  if (!any(keep)) return(NULL)
  F <- F[keep, , drop = FALSE]
  tr <- tr[keep, , drop = FALSE]

  ctr <- colMeans(F)
  sdv <- apply(F, 2, stats::sd)
  sdv[!is.finite(sdv) | sdv == 0] <- 1

  Z <- scale(F, center = ctr, scale = sdv)
  tz <- (as.numeric(target) - ctr) / sdv
  if (any(!is.finite(tz))) return(NULL)

  d <- sqrt(rowSums((Z - matrix(tz, nrow(Z), 4, byrow = TRUE))^2))

  tr |>
    mutate(similarity = round(1 / (1 + d), 3)) |>
    arrange(desc(similarity)) |>
    head(n) |>
    select(any_of(c(show, "similarity")))
}
