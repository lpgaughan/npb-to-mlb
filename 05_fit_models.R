# 05_fit_models.R ---------------------------------------------------------
# Component translation models: final NPB season -> first MLB season.
#
# Design notes
# ------------
# We model *components* (K%, BB%, HR rate, BABIP for hitters; K/9, BB/9, HR/9
# for pitchers) rather than modelling wRC+ or ERA directly. Two reasons:
#
#   * Components translate far more stably than outcome stats. Strikeout rate
#     is a skill that carries across leagues with a predictable haircut;
#     ERA is mostly noise over a single season.
#   * With a sample this small (~50-70 players per side), a model of four
#     well-behaved components beats one model of a noisy composite.
#
# Each component is a GLM with a count response and the relevant denominator
# as trials/offset. That means a player's first MLB season is automatically
# weighted by how much he actually played -- a 600 PA rookie year informs the
# fit much more than a 110 PA, without any manual weighting.
#
# The predictor is the log of the player's NPB rate *relative to his NPB
# league*, so a coefficient of 1 would mean "translates one-for-one" and
# anything below 1 means the skill compresses toward MLB average.

source(file.path("R", "00_config.R"))

logit <- function(p) log(p / (1 - p))

#' Build the shrink-toward-league-average function for a fitted model set.
#'
#' 06_validate.R attaches a per-component lambda chosen by leave-one-out:
#' how much of the fitted model to trust versus the league average. A
#' component with strong signal keeps most of its spread; one that only ranks
#' players correctly but is scaled too wide gets pulled in; one that is
#' anti-informative collapses to a constant. Returns identity when no
#' shrinkage has been fitted yet, so predictions still work mid-pipeline.
shrinker <- function(fits) {
  sh <- attr(fits, "shrink"); bs <- attr(fits, "baseline")
  if (is.null(sh) || is.null(bs)) return(function(p, key) p)
  function(p, key) {
    lam <- sh[[key]]; b <- bs[[key]]
    if (is.null(lam) || is.null(b) || !is.finite(lam) || !is.finite(b)) return(p)
    lam * p + (1 - lam) * b
  }
}

# --- hitters -------------------------------------------------------------
fit_hitter_models <- function(d = NULL) {
  if (is.null(d)) {
    d <- read_csv(file.path(DIR_PROC, "transitions_bat.csv"), show_col_types = FALSE)
  }

  # Fall back to raw-rate-over-a-fixed-baseline if league context was never
  # built. The models still work, they just lose the era adjustment.
  d <- d |>
    ensure_cols(c("npb_k_rel", "npb_bb_rel", "npb_hr_rel", "mlb_SF")) |>
    mutate(
      age_c   = npb_Age - 28,                    # centred so intercepts are readable
      bip_mlb = pmax(mlb_AB - mlb_SO - mlb_HR + coalesce(mlb_SF, 0), 1),
      p_k     = log(pmax(coalesce(npb_k_rel,  npb_k_pct  / 0.170), 1e-4)),
      p_bb    = log(pmax(coalesce(npb_bb_rel, npb_bb_pct / 0.085), 1e-4)),
      p_hr    = log(pmax(coalesce(npb_hr_rel, npb_hr_pa  / 0.026), 1e-4)),
      p_bab   = logit(pmin(pmax(npb_babip, 0.15), 0.45))
    ) |>
    filter(is.finite(p_k), is.finite(p_bb), is.finite(p_bab),
           mlb_PA >= MIN_MLB_PA)

  n <- nrow(d)

  # Predictor budget. Fitting four parameters on eight observations is not a
  # model, it is interpolation -- it produced a projection of 2.7 home runs in
  # 550 PA for a 40-homer NPB hitter. Below 25 transitions we drop the age
  # term; below 15 we drop the secondary predictor too and keep a single
  # slope. Better a crude model that behaves than a flexible one that does not.
  if (n < 15) {
    f_k  <- cbind(mlb_SO, mlb_PA - mlb_SO) ~ p_k
    f_bb <- cbind(mlb_BB, mlb_PA - mlb_BB) ~ p_bb
    f_hr <- cbind(mlb_HR, mlb_PA - mlb_HR) ~ p_hr
    f_ba <- cbind(mlb_H - mlb_HR, bip_mlb - (mlb_H - mlb_HR)) ~ p_bab
  } else if (n < 25) {
    f_k  <- cbind(mlb_SO, mlb_PA - mlb_SO) ~ p_k
    f_bb <- cbind(mlb_BB, mlb_PA - mlb_BB) ~ p_bb
    f_hr <- cbind(mlb_HR, mlb_PA - mlb_HR) ~ p_hr + p_k
    f_ba <- cbind(mlb_H - mlb_HR, bip_mlb - (mlb_H - mlb_HR)) ~ p_bab
  } else {
    f_k  <- cbind(mlb_SO, mlb_PA - mlb_SO) ~ p_k + age_c
    f_bb <- cbind(mlb_BB, mlb_PA - mlb_BB) ~ p_bb + age_c
    f_hr <- cbind(mlb_HR, mlb_PA - mlb_HR) ~ p_hr + p_k + age_c
    f_ba <- cbind(mlb_H - mlb_HR, bip_mlb - (mlb_H - mlb_HR)) ~ p_bab + age_c
  }

  if (n < 20) {
    warning("Hitter models fit on only ", n, " transitions. Treat the ",
            "projections as indicative at best, and check the leave-one-out ",
            "skill_pct before believing any of them.", call. = FALSE)
  }

  fits <- list(
    k   = glm(f_k,  family = binomial, data = d),
    bb  = glm(f_bb, family = binomial, data = d),
    hr  = glm(f_hr, family = binomial, data = d),
    bab = glm(f_ba, family = binomial, data = d)
  )

  attr(fits, "n") <- n
  attr(fits, "formulas") <- list(k = f_k, bb = f_bb, hr = f_hr, bab = f_ba)
  attr(fits, "data") <- d
  saveRDS(fits, file.path(DIR_MODEL, "hitter_models.rds"))
  message("Hitter models fit on ", nrow(d), " transitions.")
  fits
}

# --- pitchers ------------------------------------------------------------
fit_pitcher_models <- function(d = NULL) {
  if (is.null(d)) {
    d <- read_csv(file.path(DIR_PROC, "transitions_pitch.csv"), show_col_types = FALSE)
  }

  d <- d |>
    ensure_cols(c("npb_k9_rel", "npb_bb9_rel", "npb_hr9_rel", "npb_GS")) |>
    mutate(
      age_c  = npb_Age - 28,
      is_sp  = as.integer(coalesce(npb_GS, 0) / pmax(npb_G, 1) > 0.5),
      p_k    = log(pmax(coalesce(npb_k9_rel,  npb_k9  / 7.5), 1e-3)),
      p_bb   = log(pmax(coalesce(npb_bb9_rel, npb_bb9 / 3.0), 1e-3)),
      p_hr   = log(pmax(coalesce(npb_hr9_rel, npb_hr9 / 0.9), 1e-3)),
      log_ip = log(pmax(mlb_IP, 1))
    ) |>
    filter(is.finite(p_k), is.finite(p_bb), is.finite(p_hr),
           mlb_IP >= MIN_MLB_IP)

  f_k  <- mlb_SO ~ p_k  + age_c + is_sp + offset(log_ip)
  f_bb <- mlb_BB ~ p_bb + age_c + is_sp + offset(log_ip)
  f_hr <- mlb_HR ~ p_hr + p_k  + age_c + offset(log_ip)

  fits <- list(
    k  = glm(f_k,  family = quasipoisson, data = d),
    bb = glm(f_bb, family = quasipoisson, data = d),
    hr = glm(f_hr, family = quasipoisson, data = d)
  )

  attr(fits, "n") <- nrow(d)
  attr(fits, "formulas") <- list(k = f_k, bb = f_bb, hr = f_hr)

  # Batters faced per inning, so K% and BB% can be derived from the same
  # projected counts that produce K/9 and FIP. Deriving rather than fitting
  # them separately is deliberate: two independent models could report a K/9
  # and a K% that imply different pitchers.
  #
  # BF/IP is not constant across pitchers -- walks put extra men on (+0.11 per
  # BB/9) and strikeouts keep them off (-0.05 per K/9). A flat 4.26 costs
  # about 0.8 percentage points of K%; this costs about 0.5.
  attr(fits, "bf_model") <- tryCatch(
    lm(I(mlb_BF / mlb_IP) ~ mlb_bb9 + mlb_k9, data = d, weights = mlb_IP),
    error = function(e) NULL)
  attr(fits, "data") <- d
  saveRDS(fits, file.path(DIR_MODEL, "pitcher_models.rds"))
  message("Pitcher models fit on ", nrow(d), " transitions.")
  fits
}

# --- prediction ----------------------------------------------------------
#' Project an NPB hitter's first MLB season.
#'
#' @param npb one-row data frame of NPB rates (k_pct, bb_pct, hr_pa, babip,
#'   Age) plus optional league context columns.
#' @param pa projected MLB plate appearances (affects counting stats only).
predict_hitter <- function(fits, npb, pa = 550) {
  # npb may be a list or a one-row data frame, and league-relative fields may
  # be absent entirely, so pull each value defensively.
  g <- function(nm, fallback) {
    v <- npb[[nm]]
    if (is.null(v) || length(v) == 0 || all(is.na(v))) return(fallback)
    ifelse(is.na(v), fallback, v)
  }

  nd <- tibble(
    p_k   = log(pmax(g("k_rel",  npb$k_pct  / 0.170), 1e-4)),
    p_bb  = log(pmax(g("bb_rel", npb$bb_pct / 0.085), 1e-4)),
    p_hr  = log(pmax(g("hr_rel", npb$hr_pa  / 0.026), 1e-4)),
    p_bab = logit(pmin(pmax(npb$babip, 0.15), 0.45)),
    age_c = npb$Age - 28
  )

  bl  <- shrinker(fits)
  k   <- bl(predict(fits$k,   nd, type = "response"), "k")
  bb  <- bl(predict(fits$bb,  nd, type = "response"), "bb")
  hr  <- bl(predict(fits$hr,  nd, type = "response"), "hr")
  bab <- bl(predict(fits$bab, nd, type = "response"), "bab")

  # Rebuild a slash line from the projected components.
  # HBP and SF are carried at league-average rates rather than modelled. They
  # are small, but dropping HBP entirely understates OBP by about six points
  # across the board, which is a systematic bias rather than noise.
  HBP_RATE <- 0.009
  SF_SH_PA <- 5

  so <- k * pa; walks <- bb * pa; hrs <- hr * pa
  hbp <- HBP_RATE * pa
  ab  <- pa - walks - hbp - SF_SH_PA
  bip <- pmax(ab - so - hrs, 1)
  hits_bip <- bab * bip
  h  <- hits_bip + hrs

  # Split non-HR hits into 1B/2B/3B using a stable league-average mix.
  x2b <- 0.195 * hits_bip; x3b <- 0.018 * hits_bip
  x1b <- hits_bip - x2b - x3b

  obp <- (h + walks + hbp) / pa
  slg <- (x1b + 2 * x2b + 3 * x3b + 4 * hrs) / ab
  woba <- (0.69 * walks + 0.72 * hbp + 0.89 * x1b + 1.27 * x2b +
           1.62 * x3b + 2.10 * hrs) / pa

  # wRC+ from wOBA, using the standard identity
  #   wRC+ = 100 + ((wOBA - lgwOBA) / wOBAScale) / (lgR/PA) * 100
  # with modern-MLB constants. Park factors are deliberately left out --
  # we do not know where the player will sign.
  LG_WOBA <- 0.318; WOBA_SCALE <- 1.25; LG_R_PA <- 0.118
  wrc_plus <- 100 + ((woba - LG_WOBA) / WOBA_SCALE) / LG_R_PA * 100

  tibble(
    PA = pa, K_pct = k, BB_pct = bb, HR_per_PA = hr, BABIP = bab,
    HR = hrs, AVG = h / ab, OBP = obp, SLG = slg, OPS = obp + slg,
    wOBA = woba, wRC_plus = wrc_plus
  )
}

#' Project an NPB pitcher's first MLB season.
predict_pitcher <- function(fits, npb, ip = 150) {
  g <- function(nm, fallback) {
    v <- npb[[nm]]
    if (is.null(v) || length(v) == 0 || all(is.na(v))) return(fallback)
    ifelse(is.na(v), fallback, v)
  }

  nd <- tibble(
    p_k    = log(pmax(g("k9_rel",  npb$k9  / 7.5), 1e-3)),
    p_bb   = log(pmax(g("bb9_rel", npb$bb9 / 3.0), 1e-3)),
    p_hr   = log(pmax(g("hr9_rel", npb$hr9 / 0.9), 1e-3)),
    age_c  = npb$Age - 28,
    is_sp  = as.integer(g("is_sp", 1)),
    log_ip = log(ip)
  )

  # Shrink on the per-9 rate scale, which is what validation measured, then
  # convert back to counts for FIP.
  bl  <- shrinker(fits)
  k9  <- bl(9 * predict(fits$k,  nd, type = "response") / ip, "k")
  bb9 <- bl(9 * predict(fits$bb, nd, type = "response") / ip, "bb")
  hr9 <- bl(9 * predict(fits$hr, nd, type = "response") / ip, "hr")

  so <- k9 * ip / 9; bb <- bb9 * ip / 9; hr <- hr9 * ip / 9
  fip <- (13 * hr + 3 * bb - 2 * so) / ip + 3.15   # 3.15 ~ modern FIP constant

  # Batters faced, then the per-batter rates. Falls back to the league-average
  # 4.26 BF/IP if the helper model is missing (e.g. an older saved fit).
  bfm <- attr(fits, "bf_model")
  bf_per_ip <- if (is.null(bfm)) {
    4.26
  } else {
    p <- tryCatch(as.numeric(predict(bfm, tibble(mlb_bb9 = bb9, mlb_k9 = k9))),
                  error = function(e) NA_real_)
    ifelse(is.finite(p), pmin(pmax(p, 3.5), 5.5), 4.26)
  }
  bf <- bf_per_ip * ip

  tibble(IP = ip, BF = bf, SO = so, BB = bb, HR = hr,
         K9 = k9, BB9 = bb9, HR9 = hr9,
         K_pct = so / bf, BB_pct = bb / bf,
         FIP = fip)
}

# --- interpretable translation factors -----------------------------------
#' The classic MLE-style summary: how much of each skill survives the move.
#' Reported as the median observed MLB/NPB ratio, which is easier to sanity
#' check than a GLM coefficient and useful for a "rule of thumb" panel.
translation_factors <- function() {
  bat <- read_csv(file.path(DIR_PROC, "transitions_bat.csv"), show_col_types = FALSE)
  pit <- read_csv(file.path(DIR_PROC, "transitions_pitch.csv"), show_col_types = FALSE)

  ratio <- function(d, num, den) {
    x <- d[[num]] / d[[den]]
    x <- x[is.finite(x)]
    tibble(n = length(x), median = median(x), q25 = quantile(x, .25),
           q75 = quantile(x, .75))
  }

  bind_rows(
    ratio(bat, "mlb_k_pct",  "npb_k_pct")  |> mutate(side = "hitter", stat = "K%"),
    ratio(bat, "mlb_bb_pct", "npb_bb_pct") |> mutate(side = "hitter", stat = "BB%"),
    ratio(bat, "mlb_iso",    "npb_iso")    |> mutate(side = "hitter", stat = "ISO"),
    ratio(bat, "mlb_babip",  "npb_babip")  |> mutate(side = "hitter", stat = "BABIP"),
    ratio(bat, "mlb_woba",   "npb_woba")   |> mutate(side = "hitter", stat = "wOBA"),
    ratio(pit, "mlb_k9",     "npb_k9")     |> mutate(side = "pitcher", stat = "K/9"),
    ratio(pit, "mlb_bb9",    "npb_bb9")    |> mutate(side = "pitcher", stat = "BB/9"),
    ratio(pit, "mlb_hr9",    "npb_hr9")    |> mutate(side = "pitcher", stat = "HR/9"),
    ratio(pit, "mlb_era",    "npb_era")    |> mutate(side = "pitcher", stat = "ERA")
  ) |>
    select(side, stat, n, median, q25, q75) |>
    (\(x) { write_csv(x, file.path(DIR_PROC, "translation_factors.csv")); x })()
}

# Sourcing this file only defines functions. run_all.R does the fitting.
