# 00_config.R ------------------------------------------------------------
# Shared config, packages, and helpers for the NPB -> MLB translation model.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(rvest)
  library(httr)
  # jsonlite is deliberately NOT attached. It exports validate(), which masks
  # shiny::validate() and breaks every panel in the app with the cryptic
  # "is.character(txt) is not TRUE". The one place we need it calls
  # jsonlite::fromJSON() explicitly instead.
})

# --- paths ---------------------------------------------------------------
# Run everything from the project root (the folder containing R/ and app.R).
PROJ <- getwd()
if (!dir.exists(file.path(PROJ, "R"))) {
  stop("Set your working directory to the project root (the folder with R/ and app.R) ",
       "before sourcing. Currently: ", PROJ)
}

DIR_DATA  <- file.path(PROJ, "data")
DIR_RAW   <- file.path(DIR_DATA, "raw")     # cached HTML/JSON
DIR_PROC  <- file.path(DIR_DATA, "processed")
DIR_MODEL <- file.path(PROJ, "models")

for (d in c(DIR_DATA, DIR_RAW, DIR_PROC, DIR_MODEL)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- scraping etiquette --------------------------------------------------
# Baseball-Reference throttles hard. They publish a 20 req/min limit and will
# issue a temporary IP ban above it. 3.5s between requests keeps us under.
BREF_SLEEP <- 3.5
# Identifying yourself is the polite half of scraping etiquette -- an
# anonymous agent is the first thing a rate limiter blocks. But a real address
# does not belong in a public repo, where it gets harvested. So read it from
# the environment and fall back to the repo URL, which is a legitimate contact
# point and public by definition.
#
# Put your address in ~/.Renviron (which lives outside the repo):
#
#   NPB_SCRAPER_CONTACT=you@example.com
#
# then restart R. Sys.getenv() returns the fallback when it is unset, so a
# fresh clone scrapes politely without anyone configuring anything.
CONTACT <- Sys.getenv("NPB_SCRAPER_CONTACT",
                      unset = "https://github.com/lpgaughan/npb-to-mlb")
UA <- sprintf("npb-mlb-translation-research/0.1 (personal research; contact: %s)",
              CONTACT)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Guarantee that a set of columns exists, filling any missing one with NA.
#'
#' League-adjusted columns (*_rel) only exist if 03_league_context.R ran, and
#' every model has a raw-rate fallback for that case. Adding the columns up
#' front means downstream mutate() calls can reference them unconditionally
#' instead of branching on which files happen to be present.
ensure_cols <- function(d, cols) {
  missing <- setdiff(cols, names(d))
  if (length(missing)) d[missing] <- NA_real_
  d
}

#' Same, for character columns.
ensure_cols_chr <- function(d, cols) {
  missing <- setdiff(cols, names(d))
  if (length(missing)) d[missing] <- NA_character_
  d
}

#' Convert baseball's innings-pitched notation to real numbers.
#'
#' "150.1" does not mean 150.1 innings, it means 150 and one third. Summing
#' the printed values directly is a small but real error that compounds
#' across split seasons and quietly biases every per-9 rate.
ip_to_num <- function(x) {
  v <- suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(x))))
  whole <- trunc(v)
  frac  <- round((v - whole) * 10)
  whole + frac / 3
}

#' Fetch a URL, caching the raw body on disk.
#'
#' Every scrape goes through here so that re-running the pipeline costs
#' nothing and we never hammer a source twice for the same page.
fetch_cached <- function(url, cache_key, sleep = BREF_SLEEP, refresh = FALSE,
                         ext = "html") {
  path <- file.path(DIR_RAW, paste0(cache_key, ".", ext))

  if (file.exists(path) && !refresh) {
    return(read_file(path))
  }

  Sys.sleep(sleep)
  resp <- tryCatch(
    GET(url, user_agent(UA), timeout(30)),
    error = function(e) {
      warning("request failed for ", url, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NA_character_)

  if (status_code(resp) == 429) {
    message("  429 from server -- backing off 60s and retrying once")
    Sys.sleep(60)
    resp <- GET(url, user_agent(UA), timeout(30))
  }
  if (status_code(resp) != 200) {
    warning("HTTP ", status_code(resp), " for ", url)
    return(NA_character_)
  }

  body <- content(resp, as = "text", encoding = "UTF-8")
  write_file(body, path)
  body
}

#' Baseball-Reference hides many tables inside HTML comments so that they
#' render client-side. Un-comment everything before parsing.
read_bref_html <- function(raw) {
  if (is.na(raw)) return(NULL)
  raw <- gsub("<!--", "", raw, fixed = TRUE)
  raw <- gsub("-->",  "", raw, fixed = TRUE)
  read_html(raw)
}

#' Pull a table out of parsed BR html by its id attribute.
bref_table <- function(doc, id) {
  if (is.null(doc)) return(NULL)
  node <- html_element(doc, xpath = sprintf("//table[@id='%s']", id))
  if (inherits(node, "xml_missing") || is.na(node)) return(NULL)
  tbl <- tryCatch(html_table(node, fill = TRUE), error = function(e) NULL)
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  tbl
}

#' Coerce BR's character stat columns (which contain "", "*", ".300") to numeric.
as_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(x))))

# --- league / era constants ---------------------------------------------
# NPB league codes as they appear on Baseball-Reference register pages.
NPB_TOP_LEVEL <- c("JPCL", "JPPL")          # Central & Pacific (top flight)
NPB_FARM      <- c("JPEL", "JPWL")          # Eastern & Western (farm) - excluded

# Only look at transitions from this year forward. Nomo in '95 opened the
# door and nearly every move postdates him, but Cecil Fielder (Hanshin 1989 ->
# Detroit 1990) is a real transition and the sample is small enough that one
# extra observation is worth the slightly wider era. League-relative stats
# absorb the run-environment difference.
MIN_TRANSITION_YEAR <- 1989

# Minimum playing time for a season to count as a usable observation.
MIN_NPB_PA <- 200
MIN_MLB_PA <- 100
MIN_NPB_IP <- 50
MIN_MLB_IP <- 30
