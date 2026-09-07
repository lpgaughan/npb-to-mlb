# NPB → MLB Translation Model

A model that learns how NPB performance translates to MLB by pairing every
NPB→MLB player's **final NPB season** with his **first MLB season**, then
applies those translations to current NPB players to flag MLB targets.

This stage builds the model. The Shiny app comes next.

---

## Run it

```r
setwd("<this folder>")   # RStudio: Session > Set Working Directory > To Source File Location
source("run_all.R")
```

First run scrapes ~250–300 pages at 3.5s apart, so budget **20–25 minutes**.
Everything is cached in `data/raw/`; subsequent runs take seconds.

Requires: `dplyr tidyr purrr readr stringr rvest httr jsonlite`

### The app

```r
setwd("<this folder>")
shiny::runApp()
```

Also needs: `shiny bslib DT ggplot2`

**The app runs before the pipeline does.** It has two modes and picks on its own:

- **Screen mode** — no fitted models on disk. Pulls the FanGraphs NPB
  leaderboards (two requests) and ranks current players by how far above their
  own league they are. A yellow banner says so, and the projection columns stay
  hidden rather than showing fabricated numbers.
- **Projection mode** — once `run_all.R` has fit the models, translated MLB
  lines, comparables, and the diagnostics tabs all switch on.

Tabs:

| Tab | What's there |
|---|---|
| Targets | Ranked board, filterable by age / playing time / prior MLB record. Scatter of contact vs power relative to league. |
| Player | One player's NPB line beside his projected MLB line, at a PA/IP assumption you set. Plus nearest-neighbour comps from the historical set. |
| Model | LOO validation, translation factors, raw GLM coefficients. |
| Training data | The transition set itself, with NPB-vs-MLB scatters per component. |
| Read this | The survivorship caveat and how to read `skill_pct`. |

The app never scrapes on its own — it reads `data/raw/` and `data/processed/`.
The one exception is the **Refresh from FanGraphs** button.

### Command line instead

```r
source("R/00_config.R"); source("R/05_fit_models.R"); source("R/07_score_current.R")
screen_all_current()   # no model needed
score_all_current()    # needs run_all.R first
```

---

## Where the data comes from

| Piece | Source | Why |
|---|---|---|
| Who transitioned | Baseball-Reference [Japan-born index](https://www.baseball-reference.com/bio/Japan_born.shtml) | 88 players, zero hand-curation |
| Non-Japan-born NPB alumni | `data/supplement_roster.csv` | Mikolas, Colby Lewis, Fielder… — edit freely |
| NPB seasons | BR **register** pages (`/register/player.fcgi`) | Full season logs back to the 1990s, in English |
| MLB seasons | BR player pages | Same player, same source, clean join |
| League run environments | BR register league pages + MLB league totals | Era adjustment (see below) |
| **Current** NPB players | FanGraphs NPB leaderboard + player API | wRC+/K%/BB%/ISO precomputed, 2019+ |

FanGraphs only carries NPB back to ~2019 — too shallow to train on, which is
why the historical side comes from Baseball-Reference. But it's the better
source for scoring *current* players.

Two things worth knowing about the FanGraphs side, both verified 2026-08-08:

- **The leaderboard page ignores the `season` URL parameter.** Passing
  `season=2025` returns current-season data. So `07_score_current.R` uses the
  leaderboard for the player universe and current-season league context, and
  the per-player JSON API (`/api/players/stats?playerid=…`) whenever a specific
  completed season is wanted. There is no public JSON API for the leaderboards
  themselves.
- **A numeric FanGraphs id means the player has an MLB/MiLB record; an `sa…`
  id means FanGraphs only knows him from NPB.** That's a free, reliable flag
  for foreign imports and MLB returnees (Franmil Reyes, Bobby Dalbec), who are
  a different evaluation problem than a Japanese player being posted. The
  output carries it as `prior_us_experience`.

League context is computed by summing the whole `qual=0` leaderboard (~680
batters), so it's a true league total rather than a qualified-players
approximation.

---

## How the model works

**It models components, not outcomes.** Four GLMs for hitters (K%, BB%, HR/PA,
BABIP) and three for pitchers (K/9, BB/9, HR/9), which are then reassembled
into a slash line, wOBA/wRC+, and FIP.

Two reasons this beats predicting wRC+ or ERA directly:

- Components translate stably across leagues; single-season ERA is mostly noise.
- With ~50–70 players per side, four well-behaved components fit better than
  one noisy composite.

**Playing time weights itself.** Each component is a count response with its
denominator as trials (binomial) or offset (Poisson). A 600-PA rookie season
informs the fit far more than a 110-PA cup of coffee — no manual weighting.

**Everything is league-relative.** The predictor is
`log(NPB rate ÷ NPB league rate that season)`. This is not cosmetic: NPB's
2011–12 "unified ball" years were a genuine dead-ball era with a league ERA
near 2.60, while the early 2020s are much livelier. A 3.00 NPB ERA means
completely different things in those two worlds. A coefficient of 1.0 means a
skill translates one-for-one; below 1.0 means it compresses toward MLB average.

---

## How to read the validation output

`R/06_validate.R` is leave-one-out — every player is predicted by a model that
never saw him. In-sample fit on 55 players would be meaningless.

The number that matters is **`skill_pct`**: percent reduction in weighted RMSE
versus the naive alternative of predicting league average for everyone. The
naive baseline is *also* computed leave-one-out, so it can't peek at the
held-out player.

- `skill_pct > 0` → the model beats the naive guess
- `skill_pct ≤ 0` → that component carries no usable signal, and you should
  not trust projections that lean on it

I verified this metric behaves correctly by simulation: it recovers a known
translation slope, and it goes **negative** when the predictor is pure noise.

`spot_check()` prints retrodictions for Suzuki, Yoshida, Ohtani, Yamamoto,
Imanaga, Senga, Darvish, and Tanaka so you can eyeball the output.

---

## The caveat that matters most

**Survivorship bias is baked in and cannot be fixed with this data.**

The training set only contains NPB players who (a) got an MLB contract and
(b) got enough MLB playing time to register. Nobody who was posted and flopped
in the minors is in here, and neither is anyone MLB teams passed on.

So the model answers: *"conditional on this player reaching MLB and playing,
what line should we expect?"*

It does **not** answer *"will this player succeed in MLB?"* — the honest version
of that question needs the players who never made it, and that data doesn't
exist in a usable form. Treat the current-player output as a ranking of who
looks most like past successful transitions, not as a probability of success.

Secondary caveats:

- **n is small.** ~50–70 per side. Coefficients move meaningfully when you add
  a few players. Don't over-read a third decimal place.
- **No park factors.** We don't know where a player will sign.
- **Age is linear.** Fine over the observed 24–34 range, not beyond it.
- **First MLB season only**, by design — but it's the noisiest season a player
  will have (new league, new ball, new travel, adjustment period).

---

## Files

```
app.R                        the Shiny app
run_all.R                    end-to-end build
R/00_config.R                paths, throttled+cached fetching, constants
R/01_roster.R                candidate NPB→MLB player list
R/02_scrape_bref.R           NPB + MLB season logs
R/03_league_context.R        league run environments by season
R/04_build_transitions.R     one row per transition, league-adjusted
R/05_fit_models.R            component GLMs + prediction functions
R/06_validate.R              leave-one-out validation + spot checks
R/07_score_current.R         score current NPB players as MLB targets
                             screen_all_current()  no model needed
                             score_all_current()   needs fitted models
R/08_app_data.R              app data layer: cache readers, comps, degraded-state
                             detection (the app never scrapes on its own)
data/supplement_roster.csv   editable: non-Japan-born NPB alumni
data/raw/                    scrape cache (safe to delete, slow to rebuild)
data/processed/              transition datasets, league context, validation
models/                      fitted model objects
```

### Scraping etiquette

`fetch_cached()` sleeps 3.5s between requests (BR's published limit is 20/min
and they will temp-ban above it), backs off 60s on a 429, sends a real
user-agent, and caches every response so nothing is ever fetched twice.
