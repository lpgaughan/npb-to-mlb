# app.R -------------------------------------------------------------------
# NPB -> MLB target board.
#
#   setwd("<this folder>"); shiny::runApp()
#
# The app reads the on-disk cache only. It works in two modes:
#
#   * Screen mode  -- no fitted models present. Ranks current NPB players by
#                     how far above their own league they are. Needs nothing
#                     but the FanGraphs leaderboard.
#   * Projection   -- models fit (run_all.R has been run). Adds translated
#                     MLB lines, comparables, and the model diagnostics tabs.
#
# It degrades to screen mode on its own rather than erroring, so it is usable
# before the historical scrape has ever been run.

.needed <- c("shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr", "purrr",
             "readr", "stringr", "rvest", "httr", "jsonlite")
.missing <- .needed[!vapply(.needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  stop("Missing packages: ", paste(.missing, collapse = ", "),
       "\ninstall.packages(c(", paste0('"', .missing, '"', collapse = ", "), "))")
}

# Source the data layer FIRST, then attach shiny. 08_app_data.R pulls in
# 00_config.R, which attaches the tidyverse/scraping stack; loading shiny
# afterwards means shiny wins any name collision rather than losing one
# silently. This ordering is what keeps a masked validate() from taking the
# whole app down again.
source(file.path("R", "08_app_data.R"))

library(shiny)
library(bslib)
library(DT)
library(ggplot2)

# --- static bits ---------------------------------------------------------
STATUS <- app_status()
MODELS_READY <- have_models()

fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.1f%%", 100 * x))
fmt3    <- function(x) ifelse(is.na(x), NA, sprintf("%.3f", x))
fmt2    <- function(x) ifelse(is.na(x), NA, sprintf("%.2f", x))
fmt0    <- function(x) ifelse(is.na(x), NA, sprintf("%.0f", x))

#' Render innings pitched back into baseball notation.
#'
#' Internally IP is a true decimal so the arithmetic works (138 1/3 is stored
#' as 138.333, and summing split seasons is exact). Baseball writes that as
#' "138.1" -- one *out*, not one tenth. This is display only; nothing upstream
#' should ever consume the result.
#'
#' Returned as a character so the ".0" is never dropped. DataTables still sorts
#' it numerically, and the notation happens to preserve order anyway since the
#' fractional part only ever takes .0, .1 or .2.
fmt_ip <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  whole <- floor(x)
  outs  <- round((x - whole) * 3)
  # 2.9999 rounds to 3 outs, which is a whole inning.
  whole <- whole + (outs == 3)
  outs  <- ifelse(outs == 3, 0, outs)
  ifelse(is.na(x), NA_character_, paste0(whole, ".", outs))
}

# "l" in dom adds the page-length picker so long boards can be paged or shown
# all at once.
dt_opts <- list(pageLength = 25, scrollX = TRUE, dom = "lftip",
                lengthMenu = list(c(10, 25, 50, 100, -1),
                                  c("10", "25", "50", "100", "All")),
                order = list(), autoWidth = FALSE)

# Plot sizing. The wrapper carries a definite height so plotOutput(height="100%")
# always has something to resolve against, and the full-screen rule lets the
# chart actually use the space when a card is expanded -- Shiny re-renders the
# plot on container resize, which a fixed pixel height would prevent.
APP_CSS <- "
.plot-fill { height: 640px; }
.card.bslib-full-screen .plot-fill { height: calc(100vh - 150px); }
.card.bslib-full-screen .dataTables_wrapper { max-height: calc(100vh - 190px); overflow-y: auto; }
table.dataTable td, table.dataTable th { white-space: nowrap; }
"

# --- UI ------------------------------------------------------------------
ui <- page_sidebar(
  title = "NPB → MLB Target Board",
  # No font_google() here on purpose -- it needs network access at startup and
  # will hang the app if you are offline.
  theme = bs_theme(version = 5, preset = "flatly"),

  # fillable = FALSE is the important one. By default bslib tries to fit every
  # card into a single viewport height, which squashes a 25-row table and a
  # scatter plot into a few hundred pixels each. Turning it off lets the cards
  # size to their content and the page scroll like a normal document.
  fillable = FALSE,

  tags$head(tags$style(HTML(APP_CSS))),

  sidebar = sidebar(
    width = 300,

    radioButtons("kind", "Player type",
                 choices = c("Hitters" = "bat", "Pitchers" = "pit"),
                 selected = "bat", inline = TRUE),

    uiOutput("pt_filter_ui"),

    sliderInput("age", "Age range", min = 18, max = 42,
                value = c(21, 33), step = 1, sep = ""),

    checkboxInput("exclude_us", "Exclude players with prior MLB/MiLB record",
                  value = TRUE),

    hr(),

    uiOutput("proj_pt_ui"),

    hr(),

    actionButton("refresh", "Refresh from FanGraphs",
                 icon = icon("rotate"), class = "btn-sm btn-outline-secondary"),
    div(class = "small text-muted mt-2",
        "Re-pulls the NPB leaderboards. Everything else reads the local cache."),

    hr(),
    uiOutput("status_badge")
  ),

  navset_card_tab(
    id = "tabs",

    # ---------------------------------------------------------------- targets
    nav_panel(
      "Targets",
      uiOutput("mode_banner"),
      layout_columns(
        fill = FALSE,
        value_box("Players in pool", textOutput("n_pool"), theme = "primary"),
        value_box("League context", textOutput("lg_ctx"), theme = "secondary"),
        value_box("Season shown", textOutput("season_shown"), theme = "secondary")
      ),
      card(
        full_screen = TRUE,
        card_header("Ranked targets"),
        DTOutput("targets_tbl")
      ),
      card(
        full_screen = TRUE,
        card_header(textOutput("scatter_title")),
        # position:relative anchors the absolutely-positioned hover tooltip to
        # the plot rather than to the page. The .plot-fill class carries the
        # height so the full-screen CSS rule can grow it.
        div(
          class = "plot-fill",
          style = "position:relative;",
          plotOutput("targets_plot", height = "100%",
                     hover = hoverOpts("plot_hover", delay = 50,
                                       delayType = "debounce")),
          uiOutput("hover_tip")
        ),
        div(class = "small text-muted px-3 pb-2",
            "Hover a point for the player. Dot size is playing time. ",
            "Use the expand icon in the corner for a full-screen view.")
      )
    ),

    # ----------------------------------------------------------------- player
    nav_panel(
      "Player",
      layout_sidebar(
        sidebar = sidebar(
          width = 260, position = "left",
          selectizeInput("player", "Player", choices = NULL),
          div(class = "small text-muted",
              "Pool respects the filters in the main sidebar.")
        ),
        uiOutput("player_cards"),
        card(
          card_header("Closest historical transitions"),
          div(class = "small text-muted px-3 pt-2",
              "Nearest neighbours among past NPB→MLB players on the same ",
              "league-relative inputs the model uses. With ~60 players in the ",
              "training set these are rough — they are a sanity check on the ",
              "projection, not a second projection."),
          DTOutput("comps_tbl")
        )
      )
    ),

    # ------------------------------------------------------------------ model
    nav_panel(
      "Model",
      uiOutput("model_tab_body")
    ),

    # ------------------------------------------------------------ transitions
    nav_panel(
      "Training data",
      uiOutput("transitions_body")
    ),

    # ------------------------------------------------------------------ notes
    nav_panel(
      "Read this",
      card(
        card_body(
          htmltools::HTML(
            "<h4>What this does and does not say</h4>
             <p>The model pairs every NPB→MLB player's <b>final NPB season</b>
             with his <b>first MLB season</b>, learns how each component skill
             translates, and applies that to current NPB players.</p>

             <h5 class='mt-4'>The caveat that matters most</h5>
             <p><b>Survivorship bias is baked in and cannot be fixed with this
             data.</b> The training set only contains players who got an MLB
             contract <i>and</i> enough playing time to register. Nobody who was
             posted and flopped in the minors is in it, and neither is anyone
             MLB teams passed on.</p>
             <p>So a projection here answers: <i>&ldquo;conditional on this
             player reaching MLB and playing, what line should we
             expect?&rdquo;</i> It does <b>not</b> answer &ldquo;will he
             succeed?&rdquo; Treat the ranking as &ldquo;who most resembles past
             successful transitions&rdquo;, not as a probability of success.</p>

             <h5 class='mt-4'>Smaller caveats, still real</h5>
             <ul>
               <li><b>n is small</b> — roughly 50–70 players per side.
                   Coefficients move when you add a few. Don't read the third
                   decimal.</li>
               <li><b>No park factors.</b> We don't know where a player signs.</li>
               <li><b>First MLB season only</b>, by design — and it is the
                   noisiest season a player will have.</li>
               <li><b>Posting eligibility is not modelled.</b> Some players
                   ranked here simply cannot be signed yet.</li>
               <li><b>Mid-season NPB numbers are partial.</b> Rate stats from a
                   half season are noisier than the full seasons the model was
                   trained on.</li>
             </ul>

             <h5 class='mt-4'>How to read the validation tab</h5>
             <p><code>skill_pct</code> is the percent reduction in weighted RMSE
             versus predicting league average for everyone, computed
             leave-one-out on both the model and the baseline. Above 0 means the
             model beats the naive guess; at or below 0 means that component
             carries no usable signal and projections leaning on it should be
             discounted.</p>"
          )
        )
      )
    )
  )
)

# --- server --------------------------------------------------------------
server <- function(input, output, session) {

  refresh_tick <- reactiveVal(0)

  observeEvent(input$refresh, {
    showNotification("Re-pulling NPB leaderboards from FanGraphs…",
                     type = "message", duration = 4)
    ok <- tryCatch({ refresh_leaderboards(); TRUE },
                   error = function(e) { showNotification(
                     paste("Refresh failed:", conditionMessage(e)),
                     type = "error", duration = 8); FALSE })
    if (ok) {
      refresh_tick(refresh_tick() + 1)
      showNotification("Leaderboards updated.", type = "message")
    }
  })

  # ---- dynamic sidebar controls ----
  output$pt_filter_ui <- renderUI({
    if (input$kind == "bat") {
      sliderInput("min_pt", "Minimum NPB PA", min = 0, max = 700,
                  value = 300, step = 25)
    } else {
      sliderInput("min_pt", "Minimum NPB IP", min = 0, max = 220,
                  value = 80, step = 5)
    }
  })

  output$proj_pt_ui <- renderUI({
    if (input$kind == "bat") {
      sliderInput("proj_pt", "Project at MLB PA", min = 200, max = 700,
                  value = 550, step = 25)
    } else {
      sliderInput("proj_pt", "Project at MLB IP", min = 40, max = 220,
                  value = 150, step = 10)
    }
  })

  # ---- data ----
  pool_raw <- reactive({
    refresh_tick()
    withProgress(message = "Loading NPB pool…", value = 0.5, {
      current_pool(input$kind)
    })
  })

  pool_filtered <- reactive({
    p <- pool_raw()
    req(p, input$min_pt, input$age)

    pt_col <- if (input$kind == "bat") "PA" else "IP"
    p$.pt  <- as_num(p[[pt_col]])

    out <- p |>
      dplyr::filter(
        !is.na(.pt), .pt >= input$min_pt,
        !is.na(Age), Age >= input$age[1], Age <= input$age[2]
      )

    if (isTRUE(input$exclude_us) && "prior_us_experience" %in% names(out)) {
      out <- dplyr::filter(out, !prior_us_experience)
    }
    out
  })

  # The projection-PA/IP slider is built by renderUI, so it is NULL on the
  # first pass. Everything downstream goes through this so nothing has to
  # special-case that.
  proj_pt_val <- reactive({
    v <- input$proj_pt
    if (is.null(v) || !is.finite(v)) (if (input$kind == "bat") 550L else 150L) else as.integer(v)
  })

  projected <- reactive({
    p <- pool_filtered()
    req(p)
    if (!MODELS_READY || nrow(p) == 0) return(NULL)
    project_pool(p, input$kind, pt = proj_pt_val())
  })

  # ---- header widgets ----
  output$n_pool <- renderText({
    p <- pool_filtered()
    if (is.null(p)) "—" else format(nrow(p), big.mark = ",")
  })

  output$lg_ctx <- renderText({
    p <- pool_raw()
    if (is.null(p)) return("—")
    ctx <- attr(p, "league_context")
    if (is.null(ctx)) return("—")
    if (input$kind == "bat") {
      sprintf("K %.1f%% · BB %.1f%%", 100 * ctx$lg_k_pct, 100 * ctx$lg_bb_pct)
    } else {
      sprintf("K/9 %.2f · ERA %.2f", ctx$lg_k9, ctx$lg_era)
    }
  })

  output$season_shown <- renderText("Current NPB season")

  output$status_badge <- renderUI({
    if (MODELS_READY) {
      div(class = "small",
          span(class = "badge bg-success", "Projection mode"),
          div(class = "text-muted mt-1", "Translation models loaded."))
    } else {
      div(class = "small",
          span(class = "badge bg-warning text-dark", "Screen mode"),
          div(class = "text-muted mt-1",
              "No fitted models found. Run ", tags$code("run_all.R"),
              " to enable MLB projections."))
    }
  })

  output$mode_banner <- renderUI({
    if (MODELS_READY) return(NULL)
    div(class = "alert alert-warning py-2",
        tags$b("Screen mode. "),
        "These columns rank players by how far above their own NPB league they ",
        "are — the raw material the model translates, not an MLB forecast. ",
        "Run ", tags$code("run_all.R"), " to fit the translation models and turn ",
        "on projected MLB lines.")
  })

  # ---- targets table ----
  targets_data <- reactive({
    if (MODELS_READY) {
      d <- projected()
      req(d)
      if (input$kind == "bat") {
        d |>
          dplyr::transmute(
            Name, Team = team, Age,
            PA = .pt,
            `NPB wRC+`  = round(as_num(`wRC+`)),
            `NPB K%`    = fmt_pct(k_pct),
            `NPB BB%`   = fmt_pct(bb_pct),
            `Proj wRC+` = round(proj_wRC_plus),
            `Proj AVG`  = fmt3(proj_AVG),
            `Proj OBP`  = fmt3(proj_OBP),
            `Proj SLG`  = fmt3(proj_SLG),
            `Proj HR`   = round(proj_HR),
            `Proj K%`   = fmt_pct(proj_K_pct),
            `Proj BB%`  = fmt_pct(proj_BB_pct),
            .sort = proj_wRC_plus
          ) |>
          dplyr::arrange(dplyr::desc(.sort)) |>
          dplyr::select(-.sort)
      } else {
        d |>
          dplyr::transmute(
            Name, Team = team, Age,
            IP = fmt_ip(.pt),
            Role = ifelse(is_sp == 1, "SP", "RP"),
            `NPB ERA` = fmt2(as_num(ERA)),
            `NPB K/9` = fmt2(k9),
            `Proj FIP` = fmt2(proj_FIP),
            `Proj K/9` = fmt2(proj_K9),
            `Proj BB/9` = fmt2(proj_BB9),
            `Proj HR/9` = fmt2(proj_HR9),
            `Proj K%`  = fmt_pct(proj_K_pct),
            `Proj BB%` = fmt_pct(proj_BB_pct),
            .sort = proj_FIP
          ) |>
          dplyr::arrange(.sort) |>
          dplyr::select(-.sort)
      }
    } else {
      d <- pool_filtered()
      req(d)
      if (input$kind == "bat") {
        d |>
          dplyr::transmute(
            Name, Team = team, Age, PA = .pt,
            `NPB wRC+` = round(as_num(`wRC+`)),
            `K%`  = fmt_pct(k_pct),
            `BB%` = fmt_pct(bb_pct),
            ISO   = fmt3(as_num(ISO)),
            `K%-`  = round(100 * k_rel),
            `BB%+` = round(100 * bb_rel),
            `HR+`  = round(100 * hr_rel),
            .sort = dplyr::coalesce(as_num(`wRC+`),
                                    100 * (hr_rel + bb_rel) / 2 - 100 * k_rel)
          ) |>
          dplyr::arrange(dplyr::desc(.sort)) |>
          dplyr::select(-.sort)
      } else {
        d |>
          dplyr::transmute(
            Name, Team = team, Age, IP = fmt_ip(.pt),
            Role = ifelse(is_sp == 1, "SP", "RP"),
            ERA = fmt2(as_num(ERA)),
            `K/9` = fmt2(k9), `BB/9` = fmt2(bb9), `HR/9` = fmt2(hr9),
            `K/9+`  = round(100 * k9_rel),
            `BB/9-` = round(100 * bb9_rel),
            .sort = -k9_rel + bb9_rel
          ) |>
          dplyr::arrange(.sort) |>
          dplyr::select(-.sort)
      }
    }
  })

  # Number the board in model order. This is a real column rather than DT's
  # row index so the rank travels with the player when you re-sort by any
  # other column -- re-sorting then tells you where a guy sits on the model's
  # list, instead of silently renumbering 1..n under the new sort.
  targets_ranked <- reactive({
    d <- targets_data()
    req(d)
    d |>
      dplyr::mutate(`#` = dplyr::row_number()) |>
      dplyr::relocate(`#`)
  })

  output$targets_tbl <- renderDT({
    d <- targets_ranked()
    shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0,
                  "No players match these filters."))
    datatable(d, rownames = FALSE, options = dt_opts, selection = "single") |>
      formatStyle("#", fontWeight = "bold", color = "#6c757d")
  })

  # ---- scatter ----
  #' Plot-ready frame with the mapped aesthetics as literal .x/.y columns.
  #' Computing them here rather than inside aes() is what lets nearPoints()
  #' find the hovered player -- it matches on column names, so an expression
  #' like `100 * k_rel` in the aesthetic would be invisible to it.
  plot_data <- reactive({
    d <- pool_filtered()
    req(d)
    if (nrow(d) == 0) return(NULL)

    pj <- if (MODELS_READY) projected() else NULL
    same_rows <- !is.null(pj) && nrow(pj) == nrow(d)

    if (input$kind == "bat") {
      d$.x   <- 100 * d$k_rel
      d$.y   <- 100 * d$hr_rel
      d$.col <- if (same_rows) pj$proj_wRC_plus else as_num(d$`wRC+`)
      labs <- list(
        x   = "K% vs league (100 = average, lower is better)",
        y   = "HR rate vs league (100 = average)",
        col = if (same_rows) "Proj wRC+" else "NPB wRC+")
    } else {
      d$.x   <- 100 * d$k9_rel
      d$.y   <- 100 * d$bb9_rel
      d$.col <- if (same_rows) pj$proj_FIP else as_num(d$ERA)
      labs <- list(
        x   = "K/9 vs league (100 = average, higher is better)",
        y   = "BB/9 vs league (100 = average, lower is better)",
        col = if (same_rows) "Proj FIP" else "NPB ERA")
    }

    d <- d[is.finite(d$.x) & is.finite(d$.y), , drop = FALSE]
    attr(d, "labels") <- labs
    d
  })

  output$scatter_title <- renderText({
    if (input$kind == "bat") "Contact vs power, relative to NPB league"
    else "Strikeouts vs walks, relative to NPB league"
  })

  output$targets_plot <- renderPlot({
    d <- plot_data()
    shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0, "No players match these filters."))

    lab <- attr(d, "labels")

    p <- ggplot(d, aes(x = .x, y = .y, size = .pt, colour = .col)) +
      geom_vline(xintercept = 100, linetype = "dashed", colour = "grey60") +
      geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60") +
      geom_point(alpha = 0.8) +
      scale_size_continuous(range = c(2, 9), guide = "none") +
      labs(x = lab$x, y = lab$y, colour = lab$col) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "right",
            panel.grid.minor = element_blank())

    if (input$kind == "bat") {
      p + scale_colour_viridis_c(option = "C", na.value = "grey70")
    } else {
      p + scale_colour_viridis_c(option = "C", direction = -1, na.value = "grey70")
    }
  })

  # Floating tooltip. nearPoints() needs the plotted columns by name, which is
  # why plot_data() exposes plain .x/.y rather than computing them inside aes().
  output$hover_tip <- renderUI({
    h <- input$plot_hover
    if (is.null(h)) return(NULL)
    d <- plot_data()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    pt <- nearPoints(d, h, xvar = ".x", yvar = ".y",
                     threshold = 20, maxpoints = 1)
    if (nrow(pt) == 0) return(NULL)

    body <- if (input$kind == "bat") {
      tagList(
        tags$b(pt$Name[1]),
        tags$div(class = "small text-muted",
                 sprintf("%s · age %s · %s PA", pt$team[1], pt$Age[1], fmt0(pt$.pt[1]))),
        # These two lines are the plotted axes, in axis order. Keep them that
        # way -- the tooltip previously reported BB% while the y-axis showed
        # HR rate, so hovering told you about a stat that was not on screen.
        tags$div(class = "small",
                 sprintf("K%% %s  (%d vs lg)",
                         fmt_pct(pt$k_pct[1]), round(100 * pt$k_rel[1]))),
        tags$div(class = "small",
                 sprintf("HR/PA %s  (%d vs lg)",
                         fmt_pct(pt$hr_pa[1]), round(100 * pt$hr_rel[1]))),
        tags$div(class = "small text-muted",
                 sprintf("BB%% %s  (%d vs lg)",
                         fmt_pct(pt$bb_pct[1]), round(100 * pt$bb_rel[1]))),
        if (!is.na(pt$.col[1]))
          tags$div(class = "small fw-bold",
                   sprintf("%s %s", if (MODELS_READY) "Proj wRC+" else "NPB wRC+",
                           fmt0(pt$.col[1])))
      )
    } else {
      tagList(
        tags$b(pt$Name[1]),
        tags$div(class = "small text-muted",
                 sprintf("%s · age %s · %s IP", pt$team[1], pt$Age[1], fmt_ip(pt$.pt[1]))),
        tags$div(class = "small",
                 sprintf("K/9 %s (%d vs lg) · BB/9 %s (%d vs lg)",
                         fmt2(pt$k9[1]), round(100 * pt$k9_rel[1]),
                         fmt2(pt$bb9[1]), round(100 * pt$bb9_rel[1]))),
        if (!is.na(pt$.col[1]))
          tags$div(class = "small fw-bold",
                   sprintf("%s %s", if (MODELS_READY) "Proj FIP" else "NPB ERA",
                           fmt2(pt$.col[1])))
      )
    }

    # Flip the tooltip to the other side of the cursor near the right/bottom
    # edge so it never gets clipped by the card.
    left_px <- h$coords_css$x + 14
    top_px  <- h$coords_css$y + 14
    if (!is.null(h$range$right) && h$coords_css$x > 0.75 * h$range$right / 1) {
      left_px <- h$coords_css$x - 200
    }

    div(
      style = paste0(
        "position:absolute; z-index:200; pointer-events:none; ",
        "background: rgba(255,255,255,0.97); border:1px solid #ccc; ",
        "border-radius:6px; padding:6px 10px; box-shadow:0 2px 6px rgba(0,0,0,.15); ",
        "max-width:230px; left:", left_px, "px; top:", top_px, "px;"),
      body
    )
  })

  # ---- player tab ----
  observe({
    d <- pool_filtered()
    choices <- if (is.null(d) || nrow(d) == 0) character(0) else sort(unique(d$Name))
    updateSelectizeInput(session, "player", choices = choices,
                         selected = if (length(choices)) choices[1] else NULL,
                         server = TRUE)
  })

  player_row <- reactive({
    d <- pool_filtered()
    req(d, input$player)
    r <- d[d$Name == input$player, , drop = FALSE]
    if (nrow(r) == 0) return(NULL)
    r[1, , drop = FALSE]
  })

  output$player_cards <- renderUI({
    r <- player_row()
    if (is.null(r)) return(div(class = "text-muted", "Select a player."))

    npb_card <- if (input$kind == "bat") {
      card(card_header("NPB, current season"),
           card_body(
             tags$table(class = "table table-sm mb-0",
               tags$tbody(
                 tags$tr(tags$td("Team"), tags$td(r$team)),
                 tags$tr(tags$td("Age"),  tags$td(r$Age)),
                 tags$tr(tags$td("PA"),   tags$td(fmt0(r$.pt))),
                 tags$tr(tags$td("wRC+"), tags$td(fmt0(as_num(r$`wRC+`)))),
                 tags$tr(tags$td("K%"),   tags$td(fmt_pct(r$k_pct),
                                                  tags$span(class = "text-muted small",
                                                    sprintf("  (%d vs lg)", round(100 * r$k_rel))))),
                 tags$tr(tags$td("BB%"),  tags$td(fmt_pct(r$bb_pct),
                                                  tags$span(class = "text-muted small",
                                                    sprintf("  (%d vs lg)", round(100 * r$bb_rel))))),
                 tags$tr(tags$td("ISO"),  tags$td(fmt3(as_num(r$ISO)))),
                 tags$tr(tags$td("BABIP"), tags$td(fmt3(r$babip)))
               ))))
    } else {
      card(card_header("NPB, current season"),
           card_body(
             tags$table(class = "table table-sm mb-0",
               tags$tbody(
                 tags$tr(tags$td("Team"), tags$td(r$team)),
                 tags$tr(tags$td("Age"),  tags$td(r$Age)),
                 tags$tr(tags$td("IP"),   tags$td(fmt_ip(r$.pt))),
                 tags$tr(tags$td("Role"), tags$td(ifelse(r$is_sp == 1, "Starter", "Reliever"))),
                 tags$tr(tags$td("ERA"),  tags$td(fmt2(as_num(r$ERA)))),
                 tags$tr(tags$td("K/9"),  tags$td(fmt2(r$k9),
                                                  tags$span(class = "text-muted small",
                                                    sprintf("  (%d vs lg)", round(100 * r$k9_rel))))),
                 tags$tr(tags$td("BB/9"), tags$td(fmt2(r$bb9),
                                                  tags$span(class = "text-muted small",
                                                    sprintf("  (%d vs lg)", round(100 * r$bb9_rel))))),
                 tags$tr(tags$td("HR/9"), tags$td(fmt2(r$hr9)))
               ))))
    }

    proj_card <- if (!MODELS_READY) {
      card(card_header("Projected MLB line"),
           card_body(div(class = "text-muted",
                         "Not available in screen mode. Run ", tags$code("run_all.R"),
                         " to fit the translation models.")))
    } else {
      fits <- get_models(input$kind)
      pr <- tryCatch(
        if (input$kind == "bat") predict_hitter(fits, as.list(r), pa = proj_pt_val())
        else                     predict_pitcher(fits, as.list(r), ip = proj_pt_val()),
        error = function(e) NULL)

      if (is.null(pr)) {
        card(card_header("Projected MLB line"),
             card_body(div(class = "text-muted", "Could not project this player.")))
      } else if (input$kind == "bat") {
        card(card_header(sprintf("Projected MLB line — first season, %d PA", proj_pt_val())),
             card_body(
               tags$table(class = "table table-sm mb-0",
                 tags$tbody(
                   tags$tr(tags$td("wRC+"), tags$td(tags$b(fmt0(pr$wRC_plus)))),
                   tags$tr(tags$td("AVG / OBP / SLG"),
                           tags$td(sprintf("%s / %s / %s",
                                           fmt3(pr$AVG), fmt3(pr$OBP), fmt3(pr$SLG)))),
                   tags$tr(tags$td("wOBA"), tags$td(fmt3(pr$wOBA))),
                   tags$tr(tags$td("HR"),   tags$td(fmt0(pr$HR))),
                   tags$tr(tags$td("K%"),   tags$td(fmt_pct(pr$K_pct))),
                   tags$tr(tags$td("BB%"),  tags$td(fmt_pct(pr$BB_pct))),
                   tags$tr(tags$td("BABIP"), tags$td(fmt3(pr$BABIP)))
                 ))))
      } else {
        card(card_header(sprintf("Projected MLB line — first season, %d IP", proj_pt_val())),
             card_body(
               tags$table(class = "table table-sm mb-0",
                 tags$tbody(
                   tags$tr(tags$td("FIP"),  tags$td(tags$b(fmt2(pr$FIP)))),
                   tags$tr(tags$td("K/9"),  tags$td(fmt2(pr$K9))),
                   tags$tr(tags$td("BB/9"), tags$td(fmt2(pr$BB9))),
                   tags$tr(tags$td("HR/9"), tags$td(fmt2(pr$HR9))),
                   tags$tr(tags$td("K%"),   tags$td(fmt_pct(pr$K_pct))),
                   tags$tr(tags$td("BB%"),  tags$td(fmt_pct(pr$BB_pct))),
                   tags$tr(tags$td("SO"),   tags$td(fmt0(pr$SO))),
                   tags$tr(tags$td("BB"),   tags$td(fmt0(pr$BB))),
                   tags$tr(tags$td("BF"),   tags$td(fmt0(pr$BF)))
                 ))))
      }
    }

    layout_columns(col_widths = c(6, 6), npb_card, proj_card)
  })

  output$comps_tbl <- renderDT({
    r <- player_row()
    shiny::validate(shiny::need(!is.null(r), "Select a player."))
    cp <- find_comps(as.list(r), input$kind, n = 6)
    shiny::validate(shiny::need(!is.null(cp) && nrow(cp) > 0,
                  "No comparables — the historical transition set has not been built yet. Run run_all.R."))

    # find_comps() returns raw model columns, so format here rather than
    # showing 138.33333 IP and a 3.8472910 ERA. ensure_cols guards the
    # transmute in case find_comps ever drops one.
    cp <- if (input$kind == "bat") {
      cp |>
        ensure_cols(c("npb_Year", "npb_Age", "mlb_Year", "mlb_PA", "mlb_woba",
                      "mlb_HR", "mlb_k_pct", "mlb_bb_pct", "similarity")) |>
        ensure_cols_chr("name") |>
        dplyr::transmute(
          Player       = name,
          `NPB yr`     = npb_Year,
          Age          = npb_Age,
          `MLB yr`     = mlb_Year,
          `MLB PA`     = mlb_PA,
          `MLB wOBA`   = fmt3(mlb_woba),
          `MLB HR`     = round(mlb_HR),
          `MLB K%`     = fmt_pct(mlb_k_pct),
          `MLB BB%`    = fmt_pct(mlb_bb_pct),
          Similarity   = fmt2(similarity)
        )
    } else {
      cp |>
        ensure_cols(c("npb_Year", "npb_Age", "mlb_Year", "mlb_IP", "mlb_era",
                      "mlb_k9", "mlb_bb9", "similarity")) |>
        ensure_cols_chr("name") |>
        dplyr::transmute(
          Player       = name,
          `NPB yr`     = npb_Year,
          Age          = npb_Age,
          `MLB yr`     = mlb_Year,
          `MLB IP`     = fmt_ip(mlb_IP),
          `MLB ERA`    = fmt2(mlb_era),
          `MLB K/9`    = fmt2(mlb_k9),
          `MLB BB/9`   = fmt2(mlb_bb9),
          Similarity   = fmt2(similarity)
        )
    }

    datatable(cp, rownames = FALSE,
              options = list(dom = "t", pageLength = 6, scrollX = TRUE))
  })

  # ---- model tab ----
  output$model_tab_body <- renderUI({
    if (!MODELS_READY) {
      return(card(card_body(div(class = "alert alert-warning mb-0",
        "No fitted models yet. Run ", tags$code("run_all.R"),
        " to build the historical transition set and fit the translation models."))))
    }
    tagList(
      card(card_header("Leave-one-out validation"),
           div(class = "small text-muted px-3 pt-2",
               "skill_pct = % reduction in weighted RMSE versus predicting league ",
               "average, with the baseline also computed leave-one-out. At or below ",
               "zero means that component carries no usable signal."),
           DTOutput("val_tbl")),
      card(card_header("Translation factors (median MLB ÷ NPB ratio)"),
           div(class = "small text-muted px-3 pt-2",
               "Descriptive rules of thumb from the raw sample — not the model's ",
               "coefficients, which control for age and playing time."),
           DTOutput("fac_tbl")),
      card(card_header("Model coefficients"),
           div(class = "small text-muted px-3 pt-2",
               "Predictor is log(NPB rate ÷ NPB league rate). A slope of 1.0 means ",
               "the skill translates one-for-one; below 1.0 means it compresses ",
               "toward MLB average."),
           verbatimTextOutput("coef_txt"))
    )
  })

  output$val_tbl <- renderDT({
    v <- get_validation()
    shiny::validate(shiny::need(!is.null(v), "No validation output found."))
    datatable(v |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))),
              rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })

  output$fac_tbl <- renderDT({
    f <- get_factors()
    shiny::validate(shiny::need(!is.null(f), "No translation factors found."))
    datatable(f |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
              rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })

  output$coef_txt <- renderPrint({
    fits <- get_models(input$kind)
    if (is.null(fits)) return(cat("No model loaded.\n"))
    cat("n =", attr(fits, "n"), "transitions\n")
    sh <- attr(fits, "shrink"); bs <- attr(fits, "baseline")
    if (!is.null(sh)) {
      cat("\nShrinkage toward the league average, chosen by leave-one-out.\n")
      cat("lambda 1.00 = use the model as fitted; 0.00 = every player gets the\n")
      cat("league average for that component.\n\n")
      for (k in names(sh)) {
        cat(sprintf("  %-4s lambda %.2f   league baseline %.4f\n",
                    k, sh[[k]], if (is.null(bs[[k]])) NA_real_ else bs[[k]]))
      }
    }
    cat("\n")
    for (nm in names(fits)) {
      cat("---", nm, "---\n")
      print(round(summary(fits[[nm]])$coefficients, 4))
      cat("\n")
    }
  })

  # ---- training data tab ----
  output$transitions_body <- renderUI({
    tr <- get_transitions(if (input$kind == "bat") "bat" else "pitch")
    if (is.null(tr)) {
      return(card(card_body(div(class = "alert alert-warning mb-0",
        "No transition dataset yet. Run ", tags$code("run_all.R"), "."))))
    }
    tagList(
      card(full_screen = TRUE,
           card_header("Final NPB season vs first MLB season"),
           div(class = "small text-muted px-3 pt-2",
               "Each point is one player. The dashed line is a one-for-one ",
               "translation; points below it mean the skill compresses on the way over."),
           div(class = "plot-fill",
               style = "position:relative;",
               plotOutput("trans_plot", height = "100%",
                          hover = hoverOpts("trans_hover", delay = 50,
                                            delayType = "debounce")),
               uiOutput("trans_hover_tip")),
           div(class = "small text-muted px-3 pb-2",
               "Hover a point for the player and season.")),
      card(full_screen = TRUE,
           card_header("Every transition in the training set"),
           DTOutput("trans_tbl"))
    )
  })

  #' Long-format training data for the faceted NPB-vs-MLB plot.
  #'
  #' Held in a reactive rather than built inside renderPlot so the hover
  #' handler can look up the same rows the plot drew.
  trans_data <- reactive({
    kind <- input$kind
    tr <- get_transitions(if (kind == "bat") "bat" else "pitch")
    if (is.null(tr) || nrow(tr) == 0) return(NULL)

    if (kind == "bat") {
      stats  <- c("K%", "BB%", "ISO")
      npb    <- c(tr$npb_k_pct, tr$npb_bb_pct, tr$npb_iso)
      mlb    <- c(tr$mlb_k_pct, tr$mlb_bb_pct, tr$mlb_iso)
      pt_npb <- rep(tr$npb_PA, 3); pt_mlb <- rep(tr$mlb_PA, 3)
    } else {
      stats  <- c("K/9", "BB/9", "HR/9")
      npb    <- c(tr$npb_k9, tr$npb_bb9, tr$npb_hr9)
      mlb    <- c(tr$mlb_k9, tr$mlb_bb9, tr$mlb_hr9)
      pt_npb <- rep(tr$npb_IP, 3); pt_mlb <- rep(tr$mlb_IP, 3)
    }

    d <- data.frame(
      npb = npb, mlb = mlb,
      stat = factor(rep(stats, each = nrow(tr)), levels = stats),
      name = rep(tr$name, 3),
      npb_Year = rep(tr$npb_Year, 3),
      mlb_Year = rep(tr$mlb_Year, 3),
      pt_npb = pt_npb, pt_mlb = pt_mlb,
      stringsAsFactors = FALSE
    )
    d <- d[is.finite(d$npb) & is.finite(d$mlb), , drop = FALSE]
    attr(d, "kind") <- kind
    d
  })

  output$trans_plot <- renderPlot({
    d <- trans_data()
    shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0, "No usable training rows."))

    ggplot(d, aes(npb, mlb)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
      geom_point(alpha = 0.75, size = 2.6, colour = "#2c7fb8") +
      geom_smooth(method = "lm", se = FALSE, colour = "#d95f0e", linewidth = 0.8) +
      facet_wrap(~ stat, scales = "free") +
      labs(x = "Final NPB season", y = "First MLB season") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank())
  })

  # Hover for the faceted training-data plot.
  #
  # Faceting is the wrinkle here. Each panel has its own scale (scales="free"),
  # so a raw x/y match would find the wrong player. Shiny reports which facet
  # the cursor is over in `panelvar1`, and nearPoints() uses it to restrict the
  # search to that panel and to interpret the coordinates in that panel's own
  # data space.
  output$trans_hover_tip <- renderUI({
    h <- input$trans_hover
    if (is.null(h)) return(NULL)
    d <- trans_data()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    pt <- nearPoints(d, h, xvar = "npb", yvar = "mlb", panelvar1 = "stat",
                     threshold = 20, maxpoints = 1)
    if (nrow(pt) == 0) return(NULL)

    st <- as.character(pt$stat[1])
    fv <- function(x) {
      if (st %in% c("K%", "BB%")) fmt_pct(x)
      else if (st == "ISO")       fmt3(x)
      else                        fmt2(x)
    }
    pt_fmt <- if (attr(d, "kind") == "bat") fmt0 else fmt_ip
    unit   <- if (attr(d, "kind") == "bat") "PA" else "IP"

    left_px <- h$coords_css$x + 14
    top_px  <- h$coords_css$y + 14
    if (!is.null(h$range$right) && h$coords_css$x > 0.72 * h$range$right) {
      left_px <- h$coords_css$x - 210
    }

    div(
      style = paste0(
        "position:absolute; z-index:200; pointer-events:none; ",
        "background: rgba(255,255,255,0.97); border:1px solid #ccc; ",
        "border-radius:6px; padding:6px 10px; box-shadow:0 2px 6px rgba(0,0,0,.15); ",
        "max-width:250px; left:", left_px, "px; top:", top_px, "px;"),
      tags$b(pt$name[1]),
      tags$div(class = "small text-muted",
               sprintf("NPB %s → MLB %s", pt$npb_Year[1], pt$mlb_Year[1])),
      tags$div(class = "small fw-bold",
               sprintf("%s: %s → %s", st, fv(pt$npb[1]), fv(pt$mlb[1]))),
      tags$div(class = "small text-muted",
               sprintf("%s %s → %s %s",
                       pt_fmt(pt$pt_npb[1]), unit, pt_fmt(pt$pt_mlb[1]), unit))
    )
  })

  output$trans_tbl <- renderDT({
    kind <- input$kind
    tr <- get_transitions(if (kind == "bat") "bat" else "pitch")
    shiny::validate(shiny::need(!is.null(tr), "No training data."))

    d <- if (kind == "bat") {
      tr |> dplyr::transmute(
        Player = name, `NPB yr` = npb_Year, Age = npb_Age, `NPB PA` = npb_PA,
        `NPB K%` = fmt_pct(npb_k_pct), `NPB BB%` = fmt_pct(npb_bb_pct),
        `NPB ISO` = fmt3(npb_iso),
        `MLB yr` = mlb_Year, `MLB PA` = mlb_PA,
        `MLB K%` = fmt_pct(mlb_k_pct), `MLB BB%` = fmt_pct(mlb_bb_pct),
        `MLB ISO` = fmt3(mlb_iso), `MLB wOBA` = fmt3(mlb_woba))
    } else {
      tr |> dplyr::transmute(
        Player = name, `NPB yr` = npb_Year, Age = npb_Age,
        `NPB IP` = fmt_ip(npb_IP), `NPB ERA` = fmt2(npb_era),
        `NPB K/9` = fmt2(npb_k9), `NPB BB/9` = fmt2(npb_bb9),
        `MLB yr` = mlb_Year, `MLB IP` = fmt_ip(mlb_IP),
        `MLB ERA` = fmt2(mlb_era), `MLB K/9` = fmt2(mlb_k9),
        `MLB BB/9` = fmt2(mlb_bb9))
    }
    datatable(d, rownames = FALSE, options = dt_opts)
  })
}

shinyApp(ui, server)
