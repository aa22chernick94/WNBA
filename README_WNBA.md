# ASPN — WNBA Dashboards (R pipeline, v4.3 + Predictions p2.0)

One self-contained `wnba_dashboards.html`, rebuilt from sportsdataverse's
public wehoop release files: team and player box scores, play-by-play,
rosters, and player impact. No server and no API key. Only team logos,
headshots and Google Fonts load from the internet at view time, and each has
a fallback.

v3 ports the full WBB dashboard to the WNBA and adds features that only the
WNBA's play-by-play makes possible.

## v4.3: Game Recaps and Matchup Previews

Game box scores are now full **Game Recap** pages, and scheduled games get a **Matchup
Preview** page. Both are built in the page from data it already carries (box scores,
quarter scores, play-by-play lineups and shots, adjusted ratings, and the prediction
model when it ran). Every season page has them.

**Game Recap** opens from any game link: team Game Log dates, Recent Form chips,
player game logs, and the season-series list on another recap.
- **Scoreboard:** final, line score, record after the game, and playoff series status.
- **How it was decided:** short auto-written points: the shape of the game (never
  trailed, comeback, back and forth), the decisive quarter, the biggest run, the
  biggest edges in the margin, the top performer against her average, and upsets by
  season-long ratings.
- **Momentum:** lead changes, ties, largest lead, minutes leading, biggest unanswered
  run for each team, and clutch-time points.
- **Game flow chart:** the margin through the game, with quarter lines, largest leads
  and each team's biggest run marked. Hover or drag for the score at any moment.
- **Where the margin came from:** the final margin split into 2-point shooting,
  3-point shooting, free throws, offensive rebounds, turnovers and possession count.
  The pieces add up to the margin exactly (checked on every 2026 game).
- **Four factors and team stats** for the game, each against that team's season average.
- **Standout performances:** each team's top three by Game Score against her season
  average, plus the game's biggest surprise and quietest night.
- **Shot chart** for the game (either team), **shot zones**, the **lineups** used
  (minutes and +/- per five), **assist connections**, and the full **box score**
  (starters, bench points, team totals, Game Score).

**Matchup Preview** opens from:
- the new **Up next** strip on each team page;
- **Preview** on each Predictions > Next Games card;
- **Open the full matchup preview** under the Matchup Predictor (any two teams, any venue);
- **Open full matchup preview** in Compare Teams, which also works in past seasons.

It shows:
- **Prediction:** win chance, projected score, spread, total and 80% margin range from
  the Predictions model, including your availability calls. Past seasons, or a build
  without predictions, use a season-ratings estimate and say so.
- **Keys to the matchup:** the biggest offense-vs-defense edges, tempo clashes,
  hot or cold form, and the season series.
- **Tale of the tape:** record, net, offense, defense, tempo, last 10, streak,
  home/road, close games, clutch net, schedule strength and days of rest, all with
  league ranks.
- **When each team has the ball:** offense vs the other side's defense on adjusted
  rating, eFG%, TOV%, OREB%, FT rate, 3P%, 2P% and 3PA rate, with an edge marker.
- **Shot profiles:** where each offense shoots against where the defense allows shots,
  with FG% and league markers.
- **Players:** season averages, TS%, usage, on-off, scoring against this opponent,
  injury or out status, and projected lines for the game.
- **Position matchups, scoring by quarter, recent form** (tap a game for its recap),
  **head to head**, and other scheduled games between the two.

**R change (predict_wnba.R):** the predictions payload adds `schedule`, every scheduled
game with both teams known, so previews reach beyond the 7-day Next Games window
(for example "if necessary" playoff games). Older builds without it fall back to the
7-day list. No change to build_dashboards.R; the template change makes the next run
rewrite the past-season pages, as before. The pages in this package are already rebuilt.

**Data notes:**
- In 2010 and later, the play-by-play running score matches the official final in all
  but 2–4 games a season. In 2003–2005 it often doesn't (80 of 214 games in 2004).
- When it doesn't match, the chart still shows with a note, the momentum numbers are
  labeled approximate, and the recap's written points skip runs and leads.
- Older seasons without stored quarter scores get them from the play-by-play, only
  when they add up to the final.
- Links: `#g=<game id>&t=<team id>` opens a recap and `#m=<team id>,<team id>` a
  preview. Switching seasons from a preview keeps the two teams.

## v4.2: Games and Opponent filters on every table

**Player Leaderboard**
- New **Games** filter: each player's **last 5 / 10 / 15 / 20 games played**. It counts her
  own games, not the team's, so a player who missed games isn't penalised.
- The **Conference** filter is gone (the Team filter is still there).
- **Opponent rank** now works in all four views (Basic, Advanced, Zone, On/Off), not just
  Shooting by Zone.
- **Opponent** picks a specific team: only games against it.
- Everything recomputes from the matching games (counting stats, shooting, usage, on/off).
  The **season-level value stats** (BPM, VORP, Win Shares, RAPM, WAR) can't be recomputed for
  a window, so they show "—" with a note while any of these filters is on.
- **Min. games** defaults to 1 (not 5) while Playoffs or any of these filters is on, unless you
  type your own.

**Opponent filter (a specific team) is now on:** Team Rankings, Player Leaderboard, Shot Zones,
Defense by Position, league Lineups, Conferences, Compare Teams, every team page (all five tabs),
and the player page's Shooting by Zone / shot chart / on-off / assist-network section.
- **Opponent rank** was added to league **Lineups** and **Conferences**, which lacked it.
- On **Conferences**, teams are re-rated on just those games, then averaged by conference.
- It combines with every other filter, including Game type: for example, a team's playoff games
  against New York, or the leaderboard for each player's last 5 games against Las Vegas.
- Rankings with an opponent selected list only the teams that played that opponent (never the
  opponent itself).
- League and team **Lineups** drop their default minutes bar for a single opponent (10%), since
  it is only a few games.
- Predictions don't use any of these.
- Not filtered by Opponent: the player page's season-line tables and game log (they stay
  season-long, as before) and the Career view (its Season and Games filters are unchanged).

## v4.1: Game type filter on everything

A **Game type** select in the top bar (next to the season picker) switches the whole
page between **All games**, **Regular season** and **Playoffs**. It is one setting,
so it applies at once to:

- **Team Rankings, Shot Zones, Conferences, Defense by Position, league Lineups,
  Compare Teams and the Stat Corner.** Net / AdjO / AdjD / Tempo and the power rank
  are re-rated from just that game type (opponents keep their full-season strength,
  as with the Games and Opponent-rank windows). Luck, WAB, Havoc, AST% and SOS are
  recomputed from those games too.
- **Player Leaderboard** (Basic, Advanced, Shooting by Zone, On/Off) and the
  **team page** (all five tabs; its own Game type filter mirrors the top bar).
- **Player pages**: season line, game log, zone table, shot chart, on/off and assist
  network. A player with no games of that type gets a short notice.
- **Shot charts**: the "vs league" comparison uses league shots from the same game type.
- **Career view**: its Games filter and the top bar are the same setting.
- **Season switching, search links and the player-page season chips** keep the choice.

Things to know:
- **Predictions** look forward, so they ignore the setting (the select dims there).
- **BPM, VORP, Win Shares, RAPM and WAR** come from the regular-season player-impact
  release, so they show "—" under Playoffs, with a note.
- **Small samples:** default minimums drop under Playoffs (Min. games 5 to 1; lineup
  minutes to 20% of the usual defaults). Playoff numbers can be very noisy.
- **Standings** ("top 8 by record") always use regular-season results.
- Under **All games**, playoff games are tagged with a small **P** in game logs.
- **Reset** on a team page clears Game type everywhere.
- Deep links accept `#gt=POST` or `#gt=REG`.
- If a season has no playoff games yet, the Playoffs option is disabled.
- No R changes: every game already carried its season type.

## v4.0: past seasons and careers

**Every season from 2003 on now has its own full dashboard.** A season
dropdown at the top of every page switches years. It keeps your place: the
team and tab, the player, or the landing view you were on. If that team or
player isn't in the other season, you get a short notice and the landing
page. Past seasons have everything except the Predictions tab.

**Player pages have a season row and a Career view.**
- **Season chips** list every season she played, with her team(s). Clicking
  a year opens that season's dashboard on her profile, with every
  single-season feature for that year.
- **Career** shows all seasons together. A Season filter narrows it to one
  year, and a Games filter picks regular season, playoffs, or both. It
  includes:
  - season-by-season tables (Per game, Totals, Advanced), with traded
    seasons split by team, and a career row;
  - shooting by zone, by season;
  - a career shot chart with every shot filter, compared with league shots
    from the same seasons and game type;
  - on/off by season;
  - best partners across her career;
  - an assist network: who she set up and who set her up;
  - the full career game log.

**New on the single-season player page:** an **Assist Network** panel,
using the same game filters as the rest of the page.

**Search** also finds players from other seasons, listed under "Other
seasons". Clicking one opens her latest season.

### Files

    wnba_dashboards.html          current season (rebuilt every run)
    wnba_seasons/
      wnba_2003.html ... wnba_2025.html   past seasons (written once)
      career_index.js             every player's seasons/teams (search, chips)
      league_shots.js             league shot baseline for career charts
      players/<espn id>.js        one file per player's career (loaded on demand)
    build_careers.R               assembles wnba_seasons/ career files (sourced)

Keep `wnba_seasons/` next to `wnba_dashboards.html`, since the pages link to it
by relative path. Everything works from `file://`; no server is needed. If
`COPY_TO` is set, the changed files in `wnba_seasons/` are copied over too.

### Pulled once, then only the current season

- **The first run builds each missing past season** in its own R process (the
  same script, run as `Rscript build_dashboards.R --season=YYYY`). Each
  season downloads about 4 MB of release files and takes about 45 seconds on
  one core. All 23 seasons took about 18 minutes here.
- **Each finished season is cached** in `.wnba_cache/season_<year>_*.rds`.
  Its raw files are kept, never re-downloaded (`max_age_hours = Inf`).
- **Every later run** downloads and rebuilds only the current season. It
  rewrites past pages only if the template, the season list, or
  `HISTORY_DATA_VERSION` changed. It rewrites career files only for players in
  the current season. Measured here: 78 seconds, versus about 60 seconds for
  v3.3.
- **When a season ends and the next one's files appear**, the finished
  season is built once from its final data and joins the dropdown.
- **To force every past season to reprocess** from the cached raw files,
  bump `HISTORY_DATA_VERSION`. `BUILD_HISTORY <- FALSE` builds only the
  current season, like v3.3.
- **A season that fails to build** is left out of the dropdown and retried
  on the next run.

### Historical data: what had to be handled

- **The `.rds` release files for 2003–2023 are empty** (every column is
  blank). Past seasons load from the `.parquet` releases, which are
  complete. The current season still uses `.rds`, as before.
- **2003–2005 were played in two 20-minute halves.** Lineup timing and the
  clutch window use halves for those years. Period filters show 1st half,
  2nd half and OT, and scoring by period shows halves.
- **The 3-point line was 20'6¼" before 2013.** Those seasons draw the old
  line, and use a matching corner-3 rule in the 5-zone table.
- **Older play-by-play doesn't mark threes.** Threes are rebuilt from the
  play text, then the points scored on makes, then (misses only) the shot
  location. Checked against 2024's official field, this agrees 99.96% of the
  time.
- **Old box scores list inactive players in every game**, with no minutes,
  no stats, and not flagged as DNP. That's 15–20% of rows in 2005–2012.
  These rows are now treated as DNP. Without this fix, Lisa Leslie shows 33
  games in 2007, a season she sat out.
- **Rosters only exist from 2024.** Earlier seasons get jersey and headshot
  from the box scores, and height from the newest roster that lists the
  player. Age and experience are blank.
- **Older box scores carry a placeholder +/-** (every row −1 or "--"). It's
  shown as blank rather than as a number.
- **Pre-2006 box-score minutes add up to about 190 per team-game, not
  200.** Lineup minutes come from the play-by-play and add up correctly.
  From 2010 on, the lineup rebuild needs at most a few dozen substitution
  repairs per season (2003–2006: about 60–125).
- **All-Star squads and exhibitions against national teams are dropped**
  (any team with under 10 games in the season).
- **Former franchises** (Houston, Sacramento, Detroit/Tulsa, San Antonio,
  Charlotte, Cleveland, Miami, Portland, Utah) are assigned to conferences.
  The historic ESPN abbreviations are mapped too. Team IDs are stable across
  relocations, so switching seasons from the Wings' page lands on the
  Shock's.
- **VORP uses each season's real schedule length** (34 games for most of
  2003–2019, 22 in 2020, and so on).

**Bug fix that also affects the current season:** the player-impact release
lists a player's regular season and playoffs as separate rows. Because of
that, every playoff player went unmatched, with blank BPM, RAPM and WAR. The
match now uses the regular-season row. For 2019, the match rate went from
100 to 154 of 165 player-team rows. In 2026 this would have appeared once
the playoffs entered the data.

### Verified (v4.0)

- **All 23 past seasons plus 2026 build cleanly.** The daily run re-downloads
  nothing from past seasons and rewrites 0 past pages and only this season's
  players' career files.
- **Lineup minutes match the box score.** For 2019, every player-team is
  within 1.5 minutes per game.
- **Checked in headless Chromium, desktop and 390 px phone width, with no
  script errors:**
  - season toggling from a team tab, a player page, the Career view and the
    landing tabs;
  - the banner link back to the current season;
  - search for retired players;
  - career views for a 20-season player (Taurasi), a retired one (Leslie)
    and a current one (Wilson);
  - Season and Games filters;
  - the pre-2013 court drawing;
  - the halves-era 2004 page;
  - the notice when a player didn't play that season.
- **Career totals track the official record.** Taurasi's regular-season
  line comes to 557 games and 10,525 points, against the official
  565 / 10,646; the gap is games missing from the ESPN release.

## v3.1 changes

**Shot chart comparisons match the filter.** Every "vs league" number (the
FG% / eFG% / pts-per-shot tiles, the shot-diet markers, and the hex and zone
colors) is now measured against league shots that pass the same **shot**
filters: type, period, clutch, assisted, and on the defense chart the
opponent's position. Olivia Miles' layups are compared with league layups;
her Q4 pull-ups with league Q4 pull-ups. A line above the tiles names the
comparison and shows its sample size. Filters that pick *whose* shots or
*which games* (player, team, lineup, game window) are deliberately left out
of the baseline.

**Adjusted ratings follow the filters.** In Team Rankings, a Games or
Opponent-rank filter now recomputes Net, AdjO, AdjD, Tempo and the power
rank from just those games. The same is true for every filter on a team
page's rating cards, including Player(s) in/out, location, rest and game
type. Opponents keep their full-season strength: a team's last 10 games are
judged against how good those opponents are over the whole season, not over
their own last 10. The calibration is checked: a window containing every
game reproduces the season ratings exactly. Havoc, AST%, Luck, WAB, SOS and
Radar remain season-long. Small windows (e.g. vs Top 4) are small samples.

## Carried over from the WBB dashboard

Everything, with the college concepts translated to a 15-team league.

**Every feature and custom filter, unchanged:**
- Team Rankings, Player Leaderboard, Shot Zones, Conferences, and Compare
  Teams (with the Four Factors radar).
- Stat-filter chips on every leaderboard.
- Games windows, opponent-rank presets plus custom range, and Min. games in
  window.
- Show-more paging, mobile filter drawers, touch tooltips, and percentile
  shading.
- Team page: Recent Form, the three KPI rows, identity blurb, Team Profile
  with live league ranks, and the Game Log with per-game four factors.
- Player Stats with Basic, Advanced/Value and Shooting by Zone, each Per
  Game or Season Total.
- The Player(s) in/out filter, player profile pages, and box scores.

**What changed for the WNBA:**
- **Adjusted ratings:** same model, re-tuned for the pros. The possession
  free-throw coefficient is 0.44 (college was 0.475), home court is worth 2.5
  pts/100 possessions, and the Pythagorean exponent is 14.
- **Opponent-rank presets:** Top 4, Top 8 (playoff line), 5–10, Outside top
  8, Bottom 4, and a custom range.
- **Conferences:** Eastern and Western, set in `WNBA_CONF` at the top of the
  R script. Add a line there when the league expands.
- **Quad Record** becomes **Record vs Tier**: record against opponents ranked
  1–4, 5–8, 9–12, and 13+.
- **WAB:** the "bubble" is now the playoff line, the average of teams ranked
  7–9.
- **Power conferences (P4)** becomes a **Standings** filter: top 8 by record
  vs. outside.

## New in the WNBA edition

**Team page tabs.** Overview, Players, Shooting, Lineups & On/Off, and
Defense. The filter bar applies to every tab and has two new filters: **Game
type** (regular season or playoffs) and **Rest** (back-to-back, 1 day, 2+
days).

**Shot charts.** The court is drawn to true WNBA dimensions: 22'1.75" arc,
21'7.75" corners, 16-ft lane, 4-ft restricted area. Three views:
- **Hex:** size shows how often shots come from a spot. Color shows FG% vs.
  the league average for shots from the same zone, with small samples shrunk
  toward league average.
- **Zones:** 12 NBA-stats-style regions, labeled with FG% and makes/attempts.
- **Dots:** every make and miss.

Filters:
- Shooter (on the defense chart, the opponent's position).
- Shot type family: layup, cutting layup, floater, pull-up, step-back, and
  so on.
- Period or half.
- Assisted or unassisted.
- Clutch only.
- **Lineup:** only shots taken while your Lineups-tab On/Off selection was on
  the floor. This works on offense and on the opponent chart.

The side panel shows FGA per game, FG%, eFG%, and points per shot, each
against league average. It also shows shot diet vs. league, the assisted
share of makes, and a shot-type table with FG%, points per shot, and
assisted %. Player pages get the same chart.

**Lineups & On/Off.**
- **On/Off Explorer:** tap roster players to cycle Any → On → Off, up to five
  On. Your selection's minutes are compared with every other minute in the
  same games, across 18 metrics:
  - Net, offensive and defensive rating.
  - Pace and +/- per 40.
  - All four factors on both ends.
  - 3PA rate and assisted %.
- **Period and clutch:** limit any of this to Q1–Q4, OT, a half, or clutch
  time (last 5:00 of Q4/OT, score within 5).
- **Lineup Combinations table:** Individuals (the roster on/off table), plus
  2-, 3-, 4- and 5-player groups. It's sortable, percentile-shaded, filtered
  by minimum minutes, and narrowed to your selection.
- **"Show these minutes on the shot chart"** opens the shot chart filtered to
  that selection.
- **League-wide Lineups tab** on the landing page: every 2–5 player group in
  the league, with the same filters. Click one to open it in that team's
  On/Off Explorer.
- **On/Off view** in the Player Leaderboard and the team page's Player Stats:
  - ON and OFF net rating, and the on–off swing.
  - ON and OFF offensive/defensive ratings.
  - +/- per game or total, and +/- per 40.
- **Player pages:** On/Off Impact cards and a **Best partners** table (her net
  rating with vs. without each teammate).
- "Off" minutes only count games the player played, so games she missed
  don't inflate the swing.

**Also new:**
- **Quarter scores:** now in every box score, plus a **Scoring by Quarter**
  panel (average scored and allowed, net, quarters won and lost).
- **Assist Network:** a passer × scorer matrix with the top connections.
- **Defense by Position:** a league tab showing PTS, REB, AST, 3PM, and FG%
  allowed to guards, forwards, and centers. Each team page gets a panel with
  league ranks and an opponent shot chart.
- **Search** finds players as well as teams.
- **Box scores and game logs:** the official +/- column, plus a starter tag
  in box scores.
- **Player Value:** adds **RAPM** (offense/defense/total) and **WAR** from the
  `wnba_player_impact` release. VORP and Win Shares are derived as in the WBB
  version, using a 44-game season.
- **Player pages:** headshots and age.
- **Stat Corner:** a Best Lineup (100+ min) card.

## How lineups are built, and how they were checked

The R script walks every game's play-by-play in order and tracks both teams'
five players on the floor:
- Q1 starters come from the box score.
- Later periods' starters are inferred from who acts before being subbed in.
- Substitutions are applied in order.

Two data-quirk rules:
- **Subs between free throws:** ESPN sometimes logs a substitution between a
  foul and its free throws, at the same clock time. Those subs are applied
  after the free throws, so the points go to the five who were on the floor
  for the foul (the standard +/- convention).
- **Malformed sub rows:** these are skipped or repaired automatically (3
  across the season).

Every unbroken stretch with the same ten players becomes one segment, with
both teams' stats. Segments also split when the game enters or leaves clutch
time. Points come from the running score, signed, so a basket overturned on
review comes off the lineup that was on the floor when the correction posted.

Checked against the real 2026 season:

| Check | Result |
|---|---|
| Minutes | Rebuilt on-floor minutes are within 1 minute of the official box score in 99.9% of 6,469 player-games (the box score rounds to whole minutes). |
| +/- | Matches ESPN's official +/- **exactly in 96%** of player-games, and within 2 points in 99.6%. |
| Points | Lineup points reconcile with the final score in 322 of 324 games. In the other 2, ESPN's own play-by-play final differs from its box score, and the segments match the play-by-play. |
| FGA | Lineup field-goal attempts match the box score in every game. |
| Lineup size | Every segment has exactly five players per side. |

## Shot data notes

- **Coordinates:** plotted in ESPN's raw half-court frame. Locations are
  recorded in whole feet, so:
  - dots get a tiny, stable, display-only jitter;
  - hexes are 2-ft bins so they don't alias against the grid;
  - all math uses the true location.
- **Left/right:** as drawn. ESPN doesn't document which sideline is which, so
  the charts make no "strong side" or "weak side" claims.
- **Zone tables:** the 5-zone tables use the same distance rules as the WBB
  version. The chart's 12 zones are a finer, separate scheme. Zone attempts
  match box-score FGA for 646 of 648 team-games.

## Setup and daily runs

1. Install R and add it to PATH.
2. Put `build_dashboards.R`, `build_careers.R`, `dashboard_template_wnba.html`,
   `predict_wnba.R` and optionally `run_daily_wnba.bat` in one folder. The script and template must come from
   the same version; the script stops with a clear message if a placeholder
   is missing.
3. Run `Rscript build_dashboards.R`, or double-click the .bat. The first run:
   - installs `dplyr`, `tidyr`, `jsonlite`, `purrr`, and `nanoparquet`;
   - downloads the season files into `.wnba_cache`;
   - writes `wnba_dashboards.html` (about 4 MB, in about a minute);
   - builds every past season once (about 45 s each, roughly 15–20 minutes
     the first time) into `wnba_seasons/`. Later runs skip them.
4. Schedule it with Task Scheduler, the same way as the WBB version. Set
   `COPY_TO` near the top of the script if you want a dated copy saved
   elsewhere, e.g. Google Drive. That copy step is non-fatal if the location
   is unreachable.

**Downloads are protected against failure:** a 30-minute timeout, 3
retries, and an integrity check before any cached file is replaced, so a
failed download never overwrites good data. **Bug fix worth knowing:** both previous scripts only downloaded a file if it
was missing, so nightly runs kept reusing the first day's data. Cached files
now refresh after 6 hours (`CACHE_MAX_AGE_HOURS`). If GitHub can't be
reached, the build falls back to the cached copy and logs a warning.

## What I verified

I installed R 4.3 here and ran this exact script end to end against the real
2026 files: regular season through 9/22, 15 teams, 648 team-games, 44,453
shots, and 10,701 lineup segments.

Then I drove the generated page in headless Chrome and checked:
- every landing tab;
- all 15 teams × all 5 team tabs;
- all three shot-chart views with their filters, on offense and defense;
- the On/Off Explorer, every lineup size, and the lineup → shot chart link;
- clicking a league lineup through to its team;
- every filter, including rest and opponent tier;
- box scores with quarter scores;
- player pages with charts and on/off;
- player search and Compare Teams;
- a 390-px phone viewport.

Zero JavaScript errors, and no horizontal overflow on the phone.

**Not yet exercised:**
- **Playoff games:** now present in the data (10 games in 2026 so far) and exercised
  by the v4.1 Game type filter; playoff games carry `season_type = 3`.
- **Impact name-matching:** the join to the impact release covers 241 of 261
  player-team rows. Unmatched players show BPR only.


## Predictions tab

A new landing tab, built by `predict_wnba.R` (keep it next to `build_dashboards.R`).
If it fails, the rest of the dashboard still builds and the tab shows the error.

- **Team model:** offensive and defensive ratings from weighted ridge regression on
  every score, with recent games weighted more and a preseason prior from last season.
- **Calibration:** walk-forward backtest over 2007–2025 (4,385 games); each game is
  predicted using only earlier games. Result: 65.7% picks right, log loss 0.618
  (home-team baseline 0.682), well calibrated. Chosen settings: 90-day half-life,
  prior worth 8 games, 45% carryover, home court +2.2 pts. This runs once (~2 min)
  and is cached in `.wnba_cache/predict_calibration_<season>.json`.
- **Simulation:** 10,000 runs of the remaining schedule, seeding and the full bracket
  (best-of-3 / 5 / 7). Team strength is redrawn every run from the model's uncertainty.
  Playoff games already played are respected.
- **Page:** Playoff Odds (playoffs %, seed 1–8, semis, finals, title), Next Games with
  win %, spread and total, a Matchup Predictor (any two teams, any venue, series odds),
  and a Model Report Card.
- **Prediction log:** every build saves its picks to `predictions_log.csv`; finished
  games are graded on the Report Card.

Settings at the top of `predict_wnba.R`: `N_SIMS`, `PLAYOFF_ROUNDS` (format),
`PLAYOFF_SEED_OVERRIDE`, `EXPANSION_NET`, `PREDICT_RECALIBRATE`.

**Tiebreakers** follow the WNBA's published order, including the restart rule for
3+ team ties (checked against wnba.com, Sept 2026). As a safety net, if the build
logs "real first-round matchups don't match the seeds", set `PLAYOFF_SEED_OVERRIDE`
to the official seeds.

### Players and availability (phase 2)

- **Player lines** for any matchup: minutes, points, rebounds, assists and threes,
  each with an 80% range. Points add up to the team's projected score.
  - *Per-minute rates:* recent games (20-game half-life), blended with her own last
    season (0.15x her minutes) and the position average (10 minutes).
  - *Calibrated minutes:* the team total includes average overtime (201), each player
    is pulled 6% toward the team average, and more in projected blowouts. The pull
    is fitted on regular-season games over everyone available before tip-off, a DNP
    counting as 0, which is the same group the page projects. (It used to be fitted
    only on players who actually got in. That doubled the pull and cost starters
    about a minute each.)
  - *No opponent adjustment:* the "what this defense allows" adjustment tested worse
    at every strength, so it's off.
  - *How the settings were chosen:* tuned on 2012-2025 (59,198 player-games) and
    graded on 2026, which the tuning never saw.
- **In/Out toggles** under the Matchup Predictor. Mark any player in or out; game
  projections, the matchup, and the playoff odds (re-simulated in the browser,
  5,000 runs) update. Your calls are saved in the browser until you reset them.
  Players who haven't played in their team's last 3 games start as out.
- **Lineup effect on team strength:** fitted on 2012-2025 (3,198 games) using only
  what was known before each game.
  - *Player value:* per-40 box offense, box defense and relative +/-, from that
    season's games so far. It's blended with last season (weight 1.5) and shrunk
    toward average by 150 minutes.
  - *Fit:* the team model's miss is regressed on the lineup shift. Weights
    (1 = full value): offense 0.90 ± 0.27, defense 0.60 ± 0.79, +/- ~0.
  - *Result:* held out one season at a time, log loss improved from 0.614 to 0.603,
    better in 13 of 14 seasons. On 2026 (not used in the fit) it's 0.584 vs 0.591,
    within noise.
  - *Traded players:* players traded away are removed from their old team's rating.
- **Playoff rotations:** in playoff games, rotations shorten. Measured on every
  playoff game since 2012 (518 team-games), from each available player's expected
  minutes before tip-off, ranked within her team, against what she actually played
  (a DNP counts as 0).
  - *Multipliers by rotation spot:* the top five play 6-9% more than their expected
    minutes; the 9th-12th play roughly half.
  - *Rotation cut:* after the multipliers, anyone projected under 4 minutes is left
    out and her minutes go to the rest. The cut is picked with each season held out.
    About 9 players get in per playoff game; the projection now plays 8.8 (it was
    projecting all 11-13 healthy players).
  - *The regular-season pull toward the team average isn't used in playoff games,*
    since it was what held starters down.
  - *Result:* held out one season at a time, playoff minutes miss 4.47 per player,
    vs 4.96 for the old method and 5.07 with no playoff adjustment. By spot, the old
    method had the top four 1-3 minutes low and the 9th-12th 2-3 minutes high.
  - Cached in `.wnba_cache/predict_rotation_<season>.json` (about 30 seconds to
    rebuild; set `PREDICT_RECALIBRATE <- TRUE` to force it).
- **Playoff mode:** a playoff-specific margin and home-court adjustment is tested
  on held-out seasons. It currently isn't used, because it didn't beat the plain model.
- **Test results** on the Model Report Card.
  - *Average miss on 2026:* points 4.024 (4.056 before tuning; season average
    4.145), minutes 4.73 (4.74 before). Minutes are now graded over everyone
    available before tip-off, so a player who sits counts as a 0-minute miss. The
    earlier 4.39 only counted players who got in.
  - *Early season:* in the first 15 game days of past seasons, points 3.95 (4.09
    before; season average 4.24).
  - *Right side:* when a projection differs from her season average, it lands on
    the right side 64-69% of the time.
  - *Ranges:* the 80% ranges catch 82-88% of games.

The historical lineup fit runs once per season (about 1 minute; it downloads the
2011-2025 player box scores, about 1.5 MB) and is cached in
`.wnba_cache/predict_avail_<season>.json`. The player test and this season's check
rerun weekly (about 1 minute), cached in `.wnba_cache/predict_players_<season>.json`.
Player-line tuning runs once per season (about 2-3 minutes), cached in
`.wnba_cache/predict_playertune_<season>.json`. A full recalibration (about 3 minutes)
happens once per season, or when `PREDICT_RECALIBRATE <- TRUE`.

### Context, return ramp, rookie prior and data checks

- **Rest, travel and motivation.** Fitted on 2007-2025 (4,385 games) as corrections
  to the team model, in points for the home team:
  - back-to-back -1.5; each day of rest +0.4 (capped at 3); -0.4 per 1,000 km
    traveled since the last game
  - seed already locked -5.1; already eliminated -2.7 (regular season only)

  Held-out log loss improved from 0.6177 to 0.6161. For upcoming games, a seed also
  counts as locked when every simulation agrees on it, which catches tiebreaker
  clinches. Next Games shows a "rest risk" tag and the size of the shift.
- **Tested and left out:** a separate home-court value per team (no better than one
  league-wide value) and capping blowout margins (no better).
- **Return from absence.** When you mark a player "in" after she missed 3+ games,
  she starts at the minutes players historically played on return: 86% of usual in
  game 1 after missing 3-5 games, 78% after 6-10, 49% after 11+ (measured on
  2012-2025). This also scales her effect on team strength.
- **Rookie prior.** In the lineup effect, players with no minutes last season now
  start from the historical average for such players instead of league average.
  It's a small held-out gain.
- **Data checks.** Each build checks this season's box scores and schedule, logs
  any problems, and lists them on the Predictions tab. The data is used as-is.
  Checks: player minutes and points that don't add up to the team's, games missing
  a box score, and missing plus-minus.

The context fit and the return ramp are cached once per season in
`.wnba_cache/predict_context_<season>.json` and `predict_ramp_<season>.json`.
