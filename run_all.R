# run_all.R ---------------------------------------------------------------
# Build the whole thing from scratch.
#
#   setwd("<this folder>")
#   source("run_all.R")
#
# The first run does a lot of polite, throttled scraping (roughly 250-300
# requests at 3.5s apart, so budget 20-25 minutes). Everything is cached to
# data/raw/, so every run after that is fast. Pass refresh = TRUE to a
# specific step if you want to re-pull.

if (!dir.exists("R")) {
  stop("Run this from the project root -- the folder containing R/ and run_all.R.\n",
       "In RStudio: Session > Set Working Directory > To Source File Location.")
}

message("=== NPB -> MLB translation pipeline ===\n")

source("R/00_config.R")

# 1. candidate roster
source("R/01_roster.R"); roster <- build_roster()

# 2. season logs for everyone on it
source("R/02_scrape_bref.R"); scrape_all(roster)

# 3. league run environments
source("R/03_league_context.R"); build_npb_context(); build_mlb_context()

# 4. one row per transition
source("R/04_build_transitions.R")
build_transitions("bat")
build_transitions("pitch")

# 5. fit
source("R/05_fit_models.R")
fit_hitter_models()
fit_pitcher_models()
cat("\nTranslation factors (median MLB/NPB ratio):\n")
print(as.data.frame(translation_factors()), digits = 3)

# 6. validate, then shrink each component toward the league average by however
#    much leave-one-out says it deserves
source("R/06_validate.R")
validate_all()
cat("\nShrinkage (lambda = how much of the fitted model the projections use):\n")
apply_shrinkage()
spot_check()

# 7. score the current NPB pool as MLB targets
source("R/07_score_current.R")
score_all_current()

message("\nDone. Models in models/, datasets in data/processed/.")
