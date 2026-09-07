# 06_validate.R -----------------------------------------------------------
# Honest accuracy check for the translation models.
#
# With a training set this small, in-sample fit is close to meaningless -- a
# four-parameter GLM on 55 players will look good on the players it was fit
# to no matter what. Everything here is leave-one-out: each player is
# predicted by a model that never saw him.
#
# The benchmark that matters is not R^2 against zero. It is whether the model
# beats the naive alternative of "just predict the MLB league average for
# everyone". If a model cannot beat that, it is not adding information.

source(file.path("R", "00_config.R"))
source(file.path("R", "05_fit_models.R"))

# --- leave-one-out -------------------------------------------------------
#' @param to_rate converts a model prediction to the same scale as
#'   `response_rate`. Binomial models already return a rate, so the default
#'   identity is right. Poisson models with an offset return a *count*, so
#'   pitcher components pass a function that divides back out to per-9.
loo_component <- function(d, formula, family, response_rate, denom,
                          to_rate = function(p, row) p) {
  n <- nrow(d)
  pred  <- rep(NA_real_, n)
  naive <- rep(NA_real_, n)

  actual <- d[[response_rate]]
  w      <- d[[denom]]

  for (i in seq_len(n)) {
    fit <- tryCatch(glm(formula, family = family, data = d[-i, ]),
                    error = function(e) NULL)
    if (!is.null(fit)) {
      raw <- tryCatch(predict(fit, d[i, ], type = "response"),
                      error = function(e) NA_real_)
      pred[i] <- to_rate(raw, d[i, ])
    }
    # The naive baseline is also computed leave-one-out. Using the full-sample
    # mean would quietly let the benchmark peek at the held-out player and
    # make the model look better than it is.
    naive[i] <- weighted.mean(actual[-i], w[-i], na.rm = TRUE)
  }

  ok <- is.finite(pred) & is.finite(actual) & is.finite(w) & is.finite(naive)

  wrmse <- function(p) sqrt(weighted.mean((actual[ok] - p)^2, w[ok]))

  # Optimal shrinkage toward the naive baseline.
  #
  # Rather than the all-or-nothing question "does the model beat the mean?",
  # ask how much of the model to use: prediction = lambda * model +
  # (1 - lambda) * league average. Minimising weighted squared error over
  # lambda has a closed form, and clamping to [0, 1] means the blend can never
  # do worse than the baseline. lambda = 0 reproduces the old binary gate,
  # lambda = 1 the raw model, and the useful cases sit in between -- a
  # component with weak-but-real signal contributes a little instead of
  # being thrown away entirely.
  a <- actual[ok]; p <- pred[ok]; nv <- naive[ok]; ww <- w[ok]
  den <- sum(ww * (p - nv)^2)
  lambda <- if (den > 0) max(0, min(1, sum(ww * (a - nv) * (p - nv)) / den)) else 0
  blended <- lambda * p + (1 - lambda) * nv

  tibble(
    n            = sum(ok),
    loo_rmse     = wrmse(p),
    naive_rmse   = wrmse(nv),
    skill_pct    = 100 * (1 - wrmse(p) / wrmse(nv)),
    lambda       = lambda,
    skill_shrunk = 100 * (1 - wrmse(blended) / wrmse(nv)),
    loo_cor      = cor(p, a),
    bias         = weighted.mean(p - a, ww)
  )
}

validate_hitters <- function() {
  fits <- readRDS(file.path(DIR_MODEL, "hitter_models.rds"))
  d <- attr(fits, "data")

  # Validate the formulas that were actually fitted. fit_hitter_models() drops
  # predictors when n is small, so hard-coding them here would validate a
  # different model than the one doing the projecting.
  f <- attr(fits, "formulas")

  res <- bind_rows(
    loo_component(d, f$k,   binomial, "mlb_k_pct",  "mlb_PA") |> mutate(stat = "K%"),
    loo_component(d, f$bb,  binomial, "mlb_bb_pct", "mlb_PA") |> mutate(stat = "BB%"),
    loo_component(d, f$hr,  binomial, "mlb_hr_pa",  "mlb_PA") |> mutate(stat = "HR/PA"),
    loo_component(d, f$bab, binomial, "mlb_babip",  "mlb_PA") |> mutate(stat = "BABIP")
  ) |>
    select(stat, everything()) |>
    mutate(side = "hitter")

  res
}

validate_pitchers <- function() {
  fits <- readRDS(file.path(DIR_MODEL, "pitcher_models.rds"))
  d <- attr(fits, "data")

  # These models predict a count with log(IP) as an offset, so divide the
  # prediction back out to a per-9 rate before scoring it.
  per9 <- function(p, row) 9 * p / pmax(row$mlb_IP, 1)

  f <- attr(fits, "formulas")

  res <- bind_rows(
    loo_component(d, f$k,  quasipoisson, "mlb_k9",  "mlb_IP", per9) |> mutate(stat = "K/9"),
    loo_component(d, f$bb, quasipoisson, "mlb_bb9", "mlb_IP", per9) |> mutate(stat = "BB/9"),
    loo_component(d, f$hr, quasipoisson, "mlb_hr9", "mlb_IP", per9) |> mutate(stat = "HR/9")
  ) |>
    select(stat, everything()) |>
    mutate(side = "pitcher")

  res
}

validate_all <- function() {
  h <- validate_hitters()
  p <- validate_pitchers()
  out <- bind_rows(h, p) |> select(side, stat, n, loo_rmse, naive_rmse,
                                   skill_pct, lambda, skill_shrunk,
                                   loo_cor, bias)
  write_csv(out, file.path(DIR_PROC, "validation.csv"))

  cat("\nLeave-one-out validation\n")
  cat("skill_pct    = % RMSE reduction vs league average, using the raw model.\n")
  cat("lambda       = how much of the model to trust (0 = none, 1 = all).\n")
  cat("skill_shrunk = % RMSE reduction after shrinking by lambda. This is what\n")
  cat("               the projections actually use, and it cannot go below 0.\n\n")
  print(as.data.frame(out), digits = 3)
  out
}

# --- skill gate ----------------------------------------------------------
#' Replace components that fail validation with an intercept-only model.
#'
#' If leave-one-out says a component cannot beat "predict the league average
#' for everyone", then the league average *is* the better estimator, and the
#' model should use it. Keeping a fitted slope that scores worse than the mean
#' is not conservatism, it is knowingly shipping a worse prediction.
#'
#' This is what happens to pitcher HR/9. Home-run rate is the least persistent
#' pitching skill, and NPB->MLB is close to a worst case for it: different ball,
#' smaller parks, hitters who punish mistakes harder. An NPB pitcher's own HR/9
#' carries no usable signal about his MLB HR/9 (LOO correlation was negative),
#' so every pitcher gets the league-average rate instead. The effect on FIP is
#' to make it behave like xFIP -- differences between pitchers come from
#' strikeouts and walks, which do translate, rather than from home runs, which
#' do not.
#'
#' @param min_skill keep the fitted model only if skill_pct exceeds this.
apply_shrinkage <- function() {
  spec <- list(
    hitter = list(
      file = "hitter_models.rds",
      map  = c("K%" = "k", "BB%" = "bb", "HR/PA" = "hr", "BABIP" = "bab"),
      rate = c(k = "mlb_k_pct", bb = "mlb_bb_pct", hr = "mlb_hr_pa", bab = "mlb_babip"),
      wt   = "mlb_PA"),
    pitcher = list(
      file = "pitcher_models.rds",
      map  = c("K/9" = "k", "BB/9" = "bb", "HR/9" = "hr"),
      rate = c(k = "mlb_k9", bb = "mlb_bb9", hr = "mlb_hr9"),
      wt   = "mlb_IP")
  )

  v <- get_validation_or_run()

  for (sd in names(spec)) {
    s    <- spec[[sd]]
    path <- file.path(DIR_MODEL, s$file)
    if (!file.exists(path)) next

    fits <- readRDS(path)
    d    <- attr(fits, "data")
    vs   <- v |> filter(side == sd)

    keys <- unname(s$map)
    lam  <- setNames(rep(1,  length(keys)), keys)
    base <- setNames(rep(NA_real_, length(keys)), keys)

    for (i in seq_len(nrow(vs))) {
      key <- s$map[[vs$stat[i]]]
      if (is.null(key) || is.na(key)) next
      lam[[key]]  <- if (is.finite(vs$lambda[i])) vs$lambda[i] else 0
      base[[key]] <- weighted.mean(d[[s$rate[[key]]]], d[[s$wt]], na.rm = TRUE)
    }

    attr(fits, "shrink")   <- lam
    attr(fits, "baseline") <- base
    saveRDS(fits, path)

    lines <- vapply(seq_along(keys), function(j) {
      k <- keys[j]
      nm <- names(s$map)[match(k, s$map)]
      sprintf("%s lambda=%.2f", nm, lam[[k]])
    }, character(1))
    message("  ", sd, ": ", paste(lines, collapse = " | "))
  }
  invisible(NULL)
}

# Kept so older calls still work.
gate_low_skill_components <- function(...) apply_shrinkage()

get_validation_or_run <- function() {
  p <- file.path(DIR_PROC, "validation.csv")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) else validate_all()
}

# --- known-case spot checks ---------------------------------------------
#' Print the model's retrodiction for players whose transitions are well
#' known, so the output can be eyeballed for obvious nonsense.
spot_check <- function(who = c("Seiya Suzuki", "Masataka Yoshida", "Shohei Ohtani",
                               "Yoshinobu Yamamoto", "Shota Imanaga",
                               "Kodai Senga", "Yu Darvish", "Masahiro Tanaka")) {
  bat <- read_csv(file.path(DIR_PROC, "transitions_bat.csv"), show_col_types = FALSE)
  pit <- read_csv(file.path(DIR_PROC, "transitions_pitch.csv"), show_col_types = FALSE)

  hb <- readRDS(file.path(DIR_MODEL, "hitter_models.rds"))
  pb <- readRDS(file.path(DIR_MODEL, "pitcher_models.rds"))

  cat("\nHitters -- projected vs actual first MLB season\n")
  for (nm in who) {
    r <- bat |> filter(name == nm)
    if (nrow(r) == 0) next
    pr <- predict_hitter(hb, list(k_pct = r$npb_k_pct, bb_pct = r$npb_bb_pct,
                                  hr_pa = r$npb_hr_pa, babip = r$npb_babip,
                                  Age = r$npb_Age,
                                  k_rel = r$npb_k_rel, bb_rel = r$npb_bb_rel,
                                  hr_rel = r$npb_hr_rel),
                        pa = r$mlb_PA)
    cat(sprintf("  %-20s proj wOBA %.3f  actual %.3f   (proj HR %4.1f vs %2.0f)\n",
                nm, pr$wOBA, r$mlb_woba, pr$HR, r$mlb_HR))
  }

  cat("\nPitchers -- projected vs actual first MLB season\n")
  for (nm in who) {
    r <- pit |> filter(name == nm)
    if (nrow(r) == 0) next
    pr <- predict_pitcher(pb, list(k9 = r$npb_k9, bb9 = r$npb_bb9, hr9 = r$npb_hr9,
                                   Age = r$npb_Age,
                                   is_sp = as.integer(coalesce(r$npb_GS, 0) /
                                                        pmax(r$npb_G, 1) > 0.5),
                                   k9_rel = r$npb_k9_rel, bb9_rel = r$npb_bb9_rel,
                                   hr9_rel = r$npb_hr9_rel),
                         ip = r$mlb_IP)
    cat(sprintf("  %-20s proj FIP %.2f  actual ERA %.2f   (proj K/9 %.1f vs %.1f)\n",
                nm, pr$FIP, r$mlb_era, pr$K9, r$mlb_k9))
  }
  invisible(NULL)
}

# Sourcing this file only defines functions.
# run_all.R calls validate_all() and spot_check().
