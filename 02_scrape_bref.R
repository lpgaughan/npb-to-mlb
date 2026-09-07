# 02_scrape_bref.R --------------------------------------------------------
# For every candidate on the roster, pull:
#   * their MLB season-by-season line (Baseball-Reference player page), and
#   * their NPB season-by-season line (Baseball-Reference *register* page).
#
# BR renames table ids from time to time, so nothing here hard-codes an id.
# We scan every table on the page and pick the one that structurally looks
# like a season-by-season batting or pitching log. That is slower to write
# but it does not silently break the next time BR ships a redesign.

source(file.path("R", "00_config.R"))

BREF <- "https://www.baseball-reference.com"

# --- generic table finder ------------------------------------------------
#' Return every table on the page as a list of tibbles, keyed by id.
all_tables <- function(doc) {
  if (is.null(doc)) return(list())
  nodes <- html_elements(doc, "table")
  if (length(nodes) == 0) return(list())
  ids <- html_attr(nodes, "id")
  out <- vector("list", length(nodes))
  for (i in seq_along(nodes)) {
    out[[i]] <- tryCatch(html_table(nodes[[i]], fill = TRUE),
                         error = function(e) NULL)
  }
  names(out) <- ifelse(is.na(ids), paste0("unnamed_", seq_along(ids)), ids)
  out[!vapply(out, is.null, logical(1))]
}

#' Does this table look like a season-by-season log of the given kind?
looks_like <- function(tbl, kind = c("bat", "pitch")) {
  kind <- match.arg(kind)
  if (is.null(tbl) || nrow(tbl) < 2) return(FALSE)
  cn <- names(tbl)
  has_year <- any(cn %in% c("Year", "Season"))
  if (!has_year) return(FALSE)
  if (kind == "bat")   return(all(c("PA", "AB", "H") %in% cn))
  all(c("IP") %in% cn) && any(c("ER", "ERA", "SO") %in% cn)
}

#' Pick the best season-log table of a given kind from a parsed page.
pick_stat_table <- function(doc, kind) {
  tbls <- all_tables(doc)
  if (length(tbls) == 0) return(NULL)
  ok <- tbls[vapply(tbls, looks_like, logical(1), kind = kind)]
  if (length(ok) == 0) return(NULL)
  # Prefer the table whose id mentions "standard"; else take the widest one.
  nm <- names(ok)
  std <- which(grepl("standard", nm, ignore.case = TRUE))
  if (length(std)) return(ok[[std[1]]])
  ok[[which.max(vapply(ok, ncol, integer(1)))]]
}

# --- MLB side ------------------------------------------------------------
#' Find the player's own Baseball-Reference register id.
#'
#' Two traps here, and the naive
#'   str_match(raw, "register/player\\.fcgi\\?id=([a-z0-9]+)")
#' fell into both.
#'
#' 1. Register ids are not alphanumeric. They contain hyphens as padding:
#'    "otani-000sho", "senga-000kod", "nomo--001hid". A [a-z0-9]+ class stops
#'    at the hyphen and yields "otani", which BR answers with its generic
#'    register index -- a real page, with no season tables, so the player was
#'    silently reported as having no NPB service.
#'
#' 2. A player page links to *other* people's register pages inside its
#'    transaction log ("Traded ... for Matt Drews (minors)"). Taking the first
#'    match in the raw HTML picked up whoever appeared earliest, which is how
#'    Cecil Fielder ended up pointed at Matt Drews' register page.
#'
#' The player's own link is the one whose anchor text advertises his
#' non-major-league stats, and it appears several times per page (nav, header,
#' footer) while an incidental trade mention appears once. Filter on the text,
#' then take the most frequent id.
find_register_id <- function(doc, raw) {
  a <- html_elements(doc, "a[href*='register/player.fcgi']")
  ids <- stringr::str_match(html_attr(a, "href"), "id=([A-Za-z0-9._-]+)")[, 2]
  txt <- stringr::str_squish(html_text2(a))

  keep <- !is.na(ids)
  ids <- ids[keep]; txt <- txt[keep]

  if (length(ids) == 0) {
    ids <- stringr::str_match_all(
      raw, "register/player\\.fcgi\\?id=([A-Za-z0-9._-]+)")[[1]][, 2]
    txt <- rep("", length(ids))
  }
  if (length(ids) == 0) return(NA_character_)

  own  <- grepl("japanese|minor|non.?major|independent|wbc|foreign",
                txt, ignore.case = TRUE)
  pool <- if (any(own)) ids[own] else ids

  names(sort(table(pool), decreasing = TRUE))[1]
}

scrape_mlb_player <- function(bref_mlb_id, refresh = FALSE) {
  letter <- substr(bref_mlb_id, 1, 1)
  url <- sprintf("%s/players/%s/%s.shtml", BREF, letter, bref_mlb_id)
  raw <- fetch_cached(url, paste0("mlb_", bref_mlb_id), refresh = refresh)
  if (is.na(raw)) return(NULL)

  doc <- read_bref_html(raw)

  list(
    bref_mlb_id = bref_mlb_id,
    bref_reg_id = find_register_id(doc, raw),
    bat   = pick_stat_table(doc, "bat"),
    pitch = pick_stat_table(doc, "pitch")
  )
}

# --- NPB side ------------------------------------------------------------
scrape_register_player <- function(bref_reg_id, refresh = FALSE) {
  if (is.na(bref_reg_id)) return(NULL)
  url <- sprintf("%s/register/player.fcgi?id=%s", BREF, bref_reg_id)
  raw <- fetch_cached(url, paste0("reg_", bref_reg_id), refresh = refresh)
  if (is.na(raw)) return(NULL)

  doc <- read_bref_html(raw)
  list(
    bref_reg_id = bref_reg_id,
    bat   = pick_stat_table(doc, "bat"),
    pitch = pick_stat_table(doc, "pitch")
  )
}

# --- tidying -------------------------------------------------------------
#' Normalise a raw BR season table into one tidy numeric row per season.
#'
#' @param keep_lg optional vector of league codes to restrict to *before*
#'   collapsing multi-row seasons.
#'
#' The ordering here matters and is easy to get wrong. On a register page a
#' player who split a year between the top flight and the farm shows up as
#' three rows: a combined "2 Lgs" row, a JPWL (farm) row, and a JPCL row.
#' Trusting BR's combined row would fold farm-league production into the
#' player's NPB line. So we filter to the leagues we actually want first, then
#' sum whatever is left -- which also handles a genuine mid-season trade
#' between two NPB clubs.
tidy_seasons <- function(tbl, kind, keep_lg = NULL) {
  if (is.null(tbl)) return(NULL)

  # Baseball-Reference uses two different column vocabularies depending on the
  # page generation: register pages say Year/Tm, current MLB player pages say
  # Season/Team. Normalise before anything else looks at them.
  df <- tbl |>
    rename_with(~ ifelse(.x == "Season", "Year", .x)) |>
    rename_with(~ ifelse(.x == "Team",   "Tm",   .x)) |>
    mutate(across(everything(), as.character))

  if (!"Year" %in% names(df)) return(NULL)

  # BR repeats its header mid-table and appends career totals, "162 Game Avg",
  # and award footers. Every real season row starts with four digits.
  df <- df |>
    filter(!is.na(Year), stringr::str_detect(Year, "^\\d{4}")) |>
    mutate(Year = as.integer(substr(Year, 1, 4)))

  if (nrow(df) == 0) return(NULL)

  # Tm/Lg are not guaranteed to be present on every BR table variant.
  df <- ensure_cols_chr(df, c("Tm", "Lg"))

  # Mark BR's combined multi-team rows. Register pages spell these "2 Teams" /
  # "2 Lgs"; current MLB player pages spell them "2TM" / "2LG". Matching only
  # the first spelling meant a traded player's combined row survived and got
  # summed alongside his per-team rows, doubling the season -- Foster Griffin's
  # 2026 came out as 276.2 IP instead of 138.1.
  df <- df |>
    mutate(.combined =
             stringr::str_detect(coalesce(Tm, ""), "(?i)^\\d+\\s*(TM|Teams?)$") |
             stringr::str_detect(coalesce(Lg, ""), "(?i)^\\d+\\s*(LG|Lgs?)$"))

  if (!is.null(keep_lg)) {
    # NPB: the combined row pools the top flight with the farm leagues, so it
    # is unusable. Drop it and rebuild the season from the league-filtered
    # rows instead.
    df <- df |>
      filter(!.combined) |>
      filter(stringr::str_detect(coalesce(Lg, ""), paste(keep_lg, collapse = "|")))
  } else {
    # MLB: the combined row *is* the season total across teams, which is
    # exactly what we want. Prefer it and discard the per-team splits for that
    # season; fall back to summing when no combined row exists.
    #
    # The combined row carries Lg = "2LG" and Tm = "2TM", which would then fail
    # filter_mlb()'s AL/NL test and silently delete the season we just went to
    # the trouble of selecting. So copy the real league and team labels off the
    # per-team rows before dropping them.
    df <- df |>
      group_by(Year) |>
      mutate(
        .lg_real = paste(unique(Lg[!.combined]), collapse = ","),
        .tm_real = paste(unique(Tm[!.combined]), collapse = ",")
      ) |>
      filter(if (any(.combined)) .combined else TRUE) |>
      mutate(
        Lg = ifelse(.combined & nzchar(.lg_real), .lg_real, Lg),
        Tm = ifelse(.combined & nzchar(.tm_real), .tm_real, Tm)
      ) |>
      ungroup() |>
      select(-.lg_real, -.tm_real)
  }

  df <- df |> select(-.combined)
  if (nrow(df) == 0) return(NULL)

  count_cols <- if (kind == "bat") {
    c("G", "PA", "AB", "R", "H", "2B", "3B", "HR", "RBI", "SB", "CS",
      "BB", "SO", "TB", "GDP", "HBP", "SH", "SF", "IBB")
  } else {
    c("W", "L", "G", "GS", "CG", "SHO", "SV", "IP", "H", "R", "ER",
      "HR", "BB", "IBB", "SO", "HBP", "BK", "WP", "BF")
  }
  present <- intersect(count_cols, names(df))

  df <- df |>
    ensure_cols_chr("Age") |>
    mutate(across(all_of(c(present, "Age")), as_num))

  # IP is printed in thirds ("150.1" = 150 1/3), so it needs its own parser
  # before anything sums it.
  if ("IP" %in% names(df)) df$IP <- ip_to_num(df$IP)

  # Sum counting stats within a season (handles mid-season trades). Rate
  # stats are recomputed downstream from these totals, so we deliberately
  # do not average BR's BA/ERA columns -- that would be wrong for split
  # seasons and is unnecessary everywhere else.
  df |>
    group_by(Year) |>
    summarise(
      Age = suppressWarnings(max(Age, na.rm = TRUE)),
      Lg  = paste(unique(Lg), collapse = ","),
      Tm  = paste(unique(Tm), collapse = ","),
      across(all_of(present), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(Age = ifelse(is.finite(Age), Age, NA_real_))
}

#' Keep only genuine MLB rows.
#'
#' BR marks these with Lg == AL / NL / MLB. "2LG" is also accepted as a
#' belt-and-braces fallback: tidy_seasons() normally rewrites a combined row's
#' league label from the per-team rows, but if those are ever missing we would
#' rather keep a season labelled 2LG than drop it.
filter_mlb <- function(df) {
  if (is.null(df) || !"Lg" %in% names(df)) return(NULL)
  out <- df |> filter(stringr::str_detect(coalesce(Lg, ""), "AL|NL|MLB|\\dLG"))
  if (nrow(out) == 0) NULL else out
}

# --- driver --------------------------------------------------------------
scrape_all <- function(roster = NULL, refresh = FALSE) {
  if (is.null(roster)) {
    roster <- read_csv(file.path(DIR_PROC, "roster_candidates.csv"),
                       show_col_types = FALSE)
  }

  npb_bat <- list(); npb_pit <- list()
  mlb_bat <- list(); mlb_pit <- list()
  link    <- list()

  for (i in seq_len(nrow(roster))) {
    id <- roster$bref_mlb_id[i]
    nm <- roster$name[i]
    message(sprintf("[%3d/%3d] %s", i, nrow(roster), nm))

    m <- scrape_mlb_player(id, refresh = refresh)
    if (is.null(m)) { message("   ! no MLB page"); next }

    r <- scrape_register_player(m$bref_reg_id, refresh = refresh)
    if (is.null(r)) { message("   ! no register page -- skipping"); next }

    # Distinguish "the register page had no stat tables at all" (almost always
    # a bad id, i.e. we fetched the wrong page) from "this player genuinely
    # never played top-flight NPB". Collapsing the two is what hid the
    # truncated-id bug: 22 players, Ohtani and Nomo among them, were reported
    # as having no NPB service.
    if (is.null(r$bat) && is.null(r$pitch)) {
      message("   ! register page has no season table (id='", m$bref_reg_id,
              "') -- likely the wrong page, skipping")
      next
    }

    # Restrict to NPB's two top-flight leagues before season totals are
    # rebuilt, so farm-league (JPEL/JPWL) production never leaks in.
    nb <- tidy_seasons(r$bat,   "bat",   keep_lg = NPB_TOP_LEVEL)
    np <- tidy_seasons(r$pitch, "pitch", keep_lg = NPB_TOP_LEVEL)
    if (is.null(nb) && is.null(np)) {
      message("   ~ no NPB top-flight service -- dropping")
      next
    }

    mb <- filter_mlb(tidy_seasons(m$bat,   "bat"))
    mp <- filter_mlb(tidy_seasons(m$pitch, "pitch"))

    tag <- function(d) if (is.null(d)) NULL else mutate(d, bref_mlb_id = id, name = nm)
    npb_bat[[id]] <- tag(nb); npb_pit[[id]] <- tag(np)
    mlb_bat[[id]] <- tag(mb); mlb_pit[[id]] <- tag(mp)
    link[[id]] <- tibble(bref_mlb_id = id, name = nm, bref_reg_id = m$bref_reg_id)
  }

  bind_write <- function(lst, fname) {
    df <- bind_rows(lst)
    write_csv(df, file.path(DIR_RAW, fname))
    df
  }

  out <- list(
    npb_bat = bind_write(npb_bat, "npb_batting_raw.csv"),
    npb_pit = bind_write(npb_pit, "npb_pitching_raw.csv"),
    mlb_bat = bind_write(mlb_bat, "mlb_batting_raw.csv"),
    mlb_pit = bind_write(mlb_pit, "mlb_pitching_raw.csv"),
    link    = bind_write(link,    "player_link.csv")
  )

  message("\nScraped ", nrow(out$link), " players with confirmed NPB service.")
  invisible(out)
}

# Sourcing this file only defines functions. run_all.R calls scrape_all().
