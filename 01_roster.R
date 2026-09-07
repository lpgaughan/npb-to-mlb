# 01_roster.R -------------------------------------------------------------
# Build the candidate roster of players who might have made an NPB -> MLB move.
#
# Strategy: Baseball-Reference maintains an index of every MLB player born in
# Japan. That captures ~90% of NPB->MLB transitions with zero hand-curation.
# A short manual supplement covers the non-Japan-born NPB alumni (Mikolas,
# Colby Lewis, etc.). Whether a player *actually* has NPB service is decided
# later by the scraper, not here -- this is a candidate list, not a final one.

source(file.path("R", "00_config.R"))

BREF <- "https://www.baseball-reference.com"

# --- Japan-born MLB players ----------------------------------------------
scrape_japan_born <- function(refresh = FALSE) {
  raw <- fetch_cached(paste0(BREF, "/bio/Japan_born.shtml"), "bio_japan_born",
                      refresh = refresh)
  doc <- read_bref_html(raw)
  if (is.null(doc)) stop("could not fetch the Japan-born index")

  # The player links live in the page's main content block. Grab every
  # /players/x/xxxxxx01.shtml href and de-duplicate.
  hrefs <- doc |>
    html_elements("div#div_players a, div#content a") |>
    html_attr("href")

  ids <- hrefs |>
    stringr::str_subset("^/players/[a-z]/[a-z0-9]+\\.shtml$") |>
    unique()

  names_txt <- doc |>
    html_elements("div#div_players a, div#content a") |>
    (\(x) tibble(href = html_attr(x, "href"), name = html_text2(x)))() |>
    filter(stringr::str_detect(href, "^/players/[a-z]/[a-z0-9]+\\.shtml$")) |>
    distinct(href, .keep_all = TRUE)

  tibble(
    bref_mlb_id = stringr::str_match(ids, "/players/[a-z]/([a-z0-9]+)\\.shtml")[, 2],
    href        = ids
  ) |>
    left_join(names_txt, by = "href") |>
    mutate(name = stringr::str_squish(name),
           source = "japan_born") |>
    filter(!is.na(name), name != "") |>
    select(bref_mlb_id, name, source)
}

# --- non-Japan-born NPB alumni -------------------------------------------
# Foreign players who put in real NPB time and then went (back) to MLB.
# These are NOT on the Japan-born index, so they have to be named explicitly.
# The list lives in data/supplement_roster.csv so you can extend it without
# touching code. Every entry is verified downstream: the scraper checks for
# actual JPCL/JPPL service and silently drops anyone who has none, so a bad
# guess costs you one wasted request, not a corrupted sample.
read_supplement <- function() {
  path <- file.path(DIR_DATA, "supplement_roster.csv")
  if (!file.exists(path)) {
    warning("no supplement_roster.csv found -- using Japan-born index only")
    return(tibble(bref_mlb_id = character(), name = character()))
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(bref_mlb_id), bref_mlb_id != "") |>
    select(bref_mlb_id, name) |>
    mutate(source = "supplement")
}

build_roster <- function(refresh = FALSE) {
  jb  <- scrape_japan_born(refresh = refresh)
  sup <- read_supplement()

  roster <- bind_rows(jb, sup) |>
    distinct(bref_mlb_id, .keep_all = TRUE) |>
    arrange(name)

  message("Candidate roster: ", nrow(roster), " players (",
          sum(roster$source == "japan_born"), " Japan-born, ",
          sum(roster$source == "supplement"), " supplement).")

  write_csv(roster, file.path(DIR_PROC, "roster_candidates.csv"))
  roster
}

# Sourcing this file only defines functions. run_all.R calls build_roster().
