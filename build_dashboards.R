# =============================================================================
# build_dashboards.R  (WNBA edition, v3)
#
# Rebuilds the WNBA team/player dashboard as one self-contained HTML file,
# from real data pulled directly from sportsdataverse's public GitHub release
# assets -- the same files wehoop's own load_wnba_*() functions read from
# internally:
#   - team box scores, player box scores : espn_wnba_team_boxscores /
#                                           espn_wnba_player_boxscores
#   - play-by-play (shots, subs, clock)  : espn_wnba_pbp
#   - rosters (height, jersey, bio)      : espn_wnba_rosters
#   - player impact (RAPM/BPM/WAR)       : wnba_player_impact
#
# v3 = the WBB dashboard's full feature set, ported to the WNBA, plus three
# things only the WNBA's richer play-by-play makes possible:
#   1. LINEUP STINTS. Every game's play-by-play is walked event by event,
#      tracking who is on the floor for both teams (period starters inferred
#      from who acts before being subbed in; substitutions applied in order).
#      Each uninterrupted stretch with the same ten players becomes one
#      "segment" with both sides' counting stats. The page aggregates these
#      for the On/Off explorer, the 1-5 player lineup tables, the league
#      lineup leaderboard, and on/off-filtered shot charts. Segments also
#      split when the game enters/leaves "clutch" (last 5:00 of Q4/OT, score
#      within 5), so every lineup number can be filtered to clutch only.
#   2. SHOT-LEVEL DATA. Every field-goal attempt with its location, result,
#      shot type, assister, period/clock, and the lineup segment it happened
#      in -- this drives the redesigned shot charts (hex, zone and dot
#      modes), shot-type tables, and the assist network.
#   3. QUARTER SCORES. Taken from the play-by-play's running score at the
#      end of each period, which the WBB box-score releases don't carry.
#
# As in the WBB build, opponent-adjusted ratings (Adj O/D/EM/Tempo) are NOT
# computed here -- the page computes them client-side from the box scores
# embedded below, and that model is the single source of truth on the page.
#
# Output: wnba_dashboards.html next to this script (open directly in a
# browser -- no server, and nothing is fetched at view time except team
# logos/headshots from ESPN's CDN and the Google Fonts).
#
# v4 = PAST SEASONS + CAREERS.
#   * Every season from HISTORY_FIRST_SEASON on gets its own full dashboard
#     (wnba_seasons/wnba_<year>.html). A season toggle at the top of every page
#     jumps between them, keeping the team or player you were looking at.
#   * Player pages get a Career view: season-by-season and career stats, plus
#     career shot chart, shooting by zone, on/off, best partners, assist
#     network and game log (wnba_seasons/players/<id>.js, loaded on demand).
#   * Past seasons are downloaded and processed ONCE. Each finished season is
#     processed in its own R process (this same script, run with --season=YYYY)
#     and cached in .wnba_cache/season_<year>_payload.rds; later runs only
#     re-download and rebuild the current season, then re-assemble the career
#     files from the cached extracts (a few seconds).
# =============================================================================

needed_pkgs <- c("dplyr", "tidyr", "jsonlite", "purrr", "nanoparquet")
missing_pkgs <- needed_pkgs[!sapply(needed_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  library(purrr)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
na0 <- function(x) ifelse(is.na(x), 0, x)
safe_col <- function(df, name) if (name %in% names(df)) df[[name]] else rep(NA_real_, nrow(df))

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  getwd()
}

# ---- config -----------------------------------------------------------------
# WNBA season label = the calendar year the season is played in (regular
# season May-Sept, playoffs into Oct) -- no NCAA-style November rollover.
today <- Sys.Date()
SEASON_GUESS <- as.integer(format(today, "%Y"))
OUT_DIR <- get_script_dir()
SCRIPT_PATH <- file.path(OUT_DIR, "build_dashboards.R")

# ---- past seasons / careers (v4) ---------------------------------------------
# First season to build. sportsdataverse's WNBA box scores start in 2003
# (play-by-play, with shot locations, too).
HISTORY_FIRST_SEASON <- 2003
# FALSE = build only the current season (no season toggle, no career files).
BUILD_HISTORY <- TRUE
# Folder (next to this script) that holds the past-season pages and the
# career data. Keep it next to wnba_dashboards.html -- the pages link to it.
SEASONS_DIR <- file.path(OUT_DIR, "wnba_seasons")
# Bump to force every cached past season to be re-processed from the (still
# cached) raw files, e.g. after changing how shots or lineups are built.
HISTORY_DATA_VERSION <- "h2"

# --season=YYYY: build ONE finished season into the cache and exit. The main
# run starts these itself, one R process per missing season, so a season's
# memory is released before the next one starts.
CHILD_SEASON <- local({
  a <- grep("^--season=\\d{4}$", commandArgs(trailingOnly = TRUE), value = TRUE)
  if (length(a)) as.integer(sub("^--season=", "", a[1])) else NA_integer_
})
CHILD_MODE <- !is.na(CHILD_SEASON)
# Re-download a cached release file once it's older than this. The previous
# version only downloaded a file if it was missing entirely, which meant a
# nightly run during the season kept re-using the first day's box scores.
CACHE_MAX_AGE_HOURS <- 6
# Optional dated backup copy (e.g. a Google Drive for Desktop folder); non-
# fatal if unreachable. Set to NA to disable.
COPY_TO <- NA_character_  # e.g. "G:/My Drive/WNBA/2026"

# ---- conference map ---------------------------------------------------------
# ESPN's box score releases don't carry conference, and the WNBA has no
# crosswalk release like the college one -- so this is a small, explicit
# table by team abbreviation. Teams missing here show as "—" and simply
# never match a Conference filter; add a line when the league expands.
WNBA_CONF <- c(
  ATL = "Eastern", CHI = "Eastern", CON = "Eastern", IND = "Eastern",
  NY = "Eastern", TOR = "Eastern", WSH = "Eastern",
  DAL = "Western", GS = "Western", LV = "Western", LA = "Western",
  MIN = "Western", PHX = "Western", POR = "Western", SEA = "Western",
  # past seasons: ESPN's older abbreviations for current teams, and former
  # franchises (all verified against the season files, 2003-2025)
  CT = "Eastern", CONN = "Eastern", NYL = "Eastern", WAS = "Eastern", DET = "Eastern", CHA = "Eastern",
  CLE = "Eastern", MIA = "Eastern", ORL = "Eastern",
  LOS = "Western", PHO = "Western", SAC = "Western", HOU = "Western", SA = "Western",
  SAS = "Western", SAN = "Western", TUL = "Western", UTA = "Western", LVA = "Western"
)

# ---- direct data download (cached locally, refreshed when stale) ----------
GH_RELEASES <- "https://github.com/sportsdataverse/sportsdataverse-data/releases/download"

# R's default download timeout is 60 seconds; allow large release files time.
options(timeout = max(1800, getOption("timeout")))
# cheap integrity check on a downloaded release file, without loading it:
# .rds = gzip / bzip2 / xz / uncompressed-serialization header;
# .parquet = "PAR1" at both ends
release_file_ok <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sz <- file.info(path)$size
  if (is.na(sz) || sz < 100) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con))
  head <- readBin(con, "raw", 6)
  if (grepl("\\.parquet(\\.part)?$", path)) {   # a download in progress is "<name>.part"
    seek(con, sz - 4)
    tail <- readBin(con, "raw", 4)
    return(identical(rawToChar(head[1:4]), "PAR1") && identical(rawToChar(tail), "PAR1"))
  }
  (head[1] == as.raw(0x1f) && head[2] == as.raw(0x8b)) ||                 # gzip
    identical(rawToChar(head[1:3]), "BZh") ||                             # bzip2
    (head[1] == as.raw(0xfd) && identical(rawToChar(head[2:5]), "7zXZ")) || # xz
    head[1] %in% charToRaw("XAB")                                           # uncompressed
}

download_cached <- function(url, filename, required = TRUE, max_age_hours = CACHE_MAX_AGE_HOURS) {
  cache_dir <- file.path(OUT_DIR, ".wnba_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(cache_dir, filename)
  have_good <- release_file_ok(dest)
  # max_age_hours = Inf: a finished season never changes, so a good cached
  # copy is kept forever and never re-downloaded
  fresh <- have_good && (is.infinite(max_age_hours) ||
                         difftime(Sys.time(), file.info(dest)$mtime, units = "hours") < max_age_hours)
  if (fresh) return(dest)
  tmp <- paste0(dest, ".part")
  last_err <- NULL
  for (attempt in 1:3) {
    message("  downloading ", filename, if (attempt > 1) paste0(" (attempt ", attempt, ")") else "", " ...")
    if (file.exists(tmp)) file.remove(tmp)
    last_err <- tryCatch({
      # warnings (e.g. a length mismatch) don't decide success -- the
      # integrity check on the finished file does
      suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE))
      NULL
    }, error = function(e) conditionMessage(e))
    if (release_file_ok(tmp)) {
      # copy-then-delete rather than rename: renaming over an existing file
      # isn't reliable on every Windows setup
      if (file.copy(tmp, dest, overwrite = TRUE)) { file.remove(tmp); return(dest) }
      last_err <- "could not write into the .wbb_cache folder"
    } else if (is.null(last_err)) {
      last_err <- "the downloaded file was incomplete or not a valid release file"
    }
    Sys.sleep(3)
  }
  if (file.exists(tmp)) file.remove(tmp)
  if (have_good) {
    message("  WARNING: could not refresh ", filename, " (", last_err, ") -- using the cached copy from ",
            format(file.info(dest)$mtime, "%Y-%m-%d %H:%M"), ".")
    return(dest)
  }
  if (!required) return(NULL)
  stop("Could not download ", filename, " after 3 attempts: ", last_err)
}
read_rds_release <- function(tag, filename) {
  readRDS(download_cached(paste0(GH_RELEASES, "/", tag, "/", filename), filename))
}
read_parquet_release <- function(tag, filename) {
  nanoparquet::read_parquet(download_cached(paste0(GH_RELEASES, "/", tag, "/", filename), filename))
}

# One season's release file. Finished seasons come from the .parquet
# releases (the .rds files for 2003-2023 were published with every column
# empty) and are downloaded once, then kept; the current season uses the .rds
# release (as in v3) and refreshes every CACHE_MAX_AGE_HOURS. Either falls
# back to the other format.
read_season_release <- function(tag, stem, yr, past) {
  fmts <- if (past) c("parquet", "rds") else c("rds", "parquet")
  last <- NULL
  for (fmt in fmts) {
    fn <- paste0(stem, "_", yr, ".", fmt)
    res <- tryCatch({
      path <- download_cached(paste0(GH_RELEASES, "/", tag, "/", fn), fn,
                              max_age_hours = if (past) Inf else CACHE_MAX_AGE_HOURS)
      df <- if (fmt == "parquet") as.data.frame(nanoparquet::read_parquet(path)) else as.data.frame(readRDS(path))
      # a release file whose id column is entirely empty is unusable
      idc <- intersect(c("game_id", "athlete_id", "team_id"), names(df))
      if (nrow(df) == 0 || (length(idc) && all(is.na(df[[idc[1]]])))) stop("file has no usable rows")
      df
    }, error = function(e) { last <<- conditionMessage(e); NULL })
    if (!is.null(res)) return(res)
  }
  stop("Could not load ", stem, " for ", yr, ": ", last)
}

resolve_season <- function(guess) {
  for (yr in c(guess, guess - 1)) {
    ok <- tryCatch({
      read_rds_release("espn_wnba_team_boxscores", paste0("team_box_", yr, ".rds")); TRUE
    }, error = function(e) FALSE)
    if (ok) return(yr)
  }
  stop("Could not find espn_wnba_team_boxscores for ", guess, " or ", guess - 1,
       " -- the upstream release may not be published yet.")
}
if (CHILD_MODE) {
  SEASON <- CHILD_SEASON
  message("=== Building past season ", SEASON, " (one-time; cached afterwards) ===")
} else {
  SEASON <- resolve_season(SEASON_GUESS)
  message("Season: ", SEASON)
}
IS_PAST <- CHILD_MODE

message("Loading team box scores ...")
team_box <- read_season_release("espn_wnba_team_boxscores", "team_box", SEASON, IS_PAST)
message("Loading player box scores ...")
player_box <- read_season_release("espn_wnba_player_boxscores", "player_box", SEASON, IS_PAST)
message("Loading rosters ...")
rosters <- tryCatch(if (IS_PAST && SEASON < 2024) stop("no roster release before 2024") else read_season_release("espn_wnba_rosters", "rosters", SEASON, IS_PAST),
  error = function(e) {
    # ESPN rosters are only published from 2024 on; older seasons build a
    # minimal roster from the box scores (jersey, headshot) below
    if (!IS_PAST) stop(e)
    message("  (no roster release for ", SEASON, " -- using box-score bio fields)")
    NULL
  })
message("Loading play-by-play (shots, substitutions, quarter scores) ...")
pbp <- tryCatch(
  read_season_release("espn_wnba_pbp", "play_by_play", SEASON, IS_PAST),
  error = function(e) {
    message("  WARNING: play-by-play not available for ", SEASON, " -- shot charts, zones, lineups ",
            "and quarter scores will be empty this run. (", conditionMessage(e), ")")
    NULL
  }
)
message("Loading player impact (RAPM/BPM/WAR) ...")
player_impact <- tryCatch(
  nanoparquet::read_parquet(download_cached(
    paste0(GH_RELEASES, "/wnba_player_impact/wnba_player_impact_", SEASON, ".parquet"),
    paste0("wnba_player_impact_", SEASON, ".parquet"), max_age_hours = if (IS_PAST) Inf else CACHE_MAX_AGE_HOURS)),
  error = function(e) {
    message("  WARNING: wnba_player_impact not available for ", SEASON,
            " -- BPM/RAPM/WAR/VORP/WS will be blank this run (BPR still works). (", conditionMessage(e), ")")
    NULL
  }
)

# ---- era rules + older-release compatibility (v4) ---------------------------
# 2003-2005 were played in two 20-minute halves; quarters from 2006. The
# 3-point line moved from 20'6.25" to 22'1.75" in 2013.
REG_PERIODS <- if (SEASON <= 2005) 2L else 4L
REG_PERIOD_SECS <- if (REG_PERIODS == 2L) 1200 else 600
OLD_THREE_LINE <- SEASON < 2013
CORNER3_MIN_X <- if (OLD_THREE_LINE) 20 else 21      # 5-zone table rule (the page uses the same value)
# Page-facing period numbers: halves are stored as "periods" 2 and 4 so the
# page's 1st-half / 2nd-half / OT filters work unchanged (OT n -> 4 + n).
per_out <- function(p) if (REG_PERIODS == 2L) ifelse(p <= 2, 2L * p, p + 2L) else as.integer(p)

add_missing <- function(df, cols, value = NA_real_) {
  for (cc in setdiff(cols, names(df))) df[[cc]] <- value
  df
}
team_box <- add_missing(team_box, c("fast_break_points", "points_in_paint", "turnover_points", "largest_lead",
                                    "total_technical_fouls", "team_color", "team_alternate_color", "team_logo"))
player_box <- add_missing(player_box, c("plus_minus", "starter", "did_not_play", "offensive_rebounds",
                                        "defensive_rebounds", "turnovers", "fouls", "athlete_position_abbreviation",
                                        "athlete_jersey", "athlete_headshot_href"))
num_cols <- function(df, cols) { for (cc in intersect(cols, names(df))) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]])); df }
player_box <- num_cols(player_box, c("minutes", "points", "rebounds", "offensive_rebounds", "defensive_rebounds", "assists",
                                     "steals", "blocks", "turnovers", "fouls", "field_goals_made", "field_goals_attempted",
                                     "three_point_field_goals_made", "three_point_field_goals_attempted",
                                     "free_throws_made", "free_throws_attempted", "plus_minus"))
player_box$did_not_play <- player_box$did_not_play %in% TRUE
player_box$starter <- player_box$starter %in% TRUE
# Older releases list every rostered player in every box score -- injured or
# inactive ones included, with no minutes and no stats but NOT flagged as
# did-not-play (15-20% of rows in 2005-2012). Treat those as DNP so games
# played and per-game averages aren't padded with games she sat out.
local({
  stat_cols <- intersect(c("points", "rebounds", "assists", "steals", "blocks", "turnovers", "fouls",
                           "field_goals_attempted", "free_throws_attempted"), names(player_box))
  any_stat <- rowSums(abs(as.matrix(player_box[, stat_cols, drop = FALSE])), na.rm = TRUE) > 0
  phantom <- !player_box$did_not_play & (is.na(player_box$minutes) | player_box$minutes <= 0) & !any_stat
  player_box$did_not_play[phantom] <<- TRUE
})
# the older releases carry a placeholder +/- (every row -1 or "--"): blank it
# rather than show a fake number
if (length(unique(na.omit(player_box$plus_minus))) < 5) player_box$plus_minus <- NA_real_
team_box <- num_cols(team_box, c("team_score", "opponent_team_score", "field_goals_made", "field_goals_attempted",
                                 "three_point_field_goals_made", "three_point_field_goals_attempted", "free_throws_made",
                                 "free_throws_attempted", "turnovers", "offensive_rebounds", "defensive_rebounds", "assists",
                                 "steals", "blocks", "fouls", "fast_break_points", "points_in_paint", "largest_lead",
                                 "total_technical_fouls", "turnover_points", "season_type"))
team_box$team_winner <- team_box$team_winner %in% TRUE
if (all(is.na(team_box$season_type))) team_box$season_type <- 2

if (is.null(rosters)) {
  # heights from the newest roster release that lists the player (height
  # barely changes); jersey and headshot from this season's box scores
  later <- list.files(file.path(OUT_DIR, ".wnba_cache"), pattern = "^rosters_\\d{4}\\.(rds|parquet)$", full.names = TRUE)
  later <- later[order(sub(".*_(\\d{4}).*", "\\1", later), decreasing = TRUE)]
  ht_map <- character(0)
  for (f in later) {
    r <- tryCatch(if (grepl("parquet$", f)) as.data.frame(nanoparquet::read_parquet(f)) else as.data.frame(readRDS(f)), error = function(e) NULL)
    if (is.null(r) || !all(c("athlete_id", "height") %in% names(r))) next
    r <- r[!is.na(r$athlete_id) & !is.na(r$height), ]
    new_ids <- setdiff(as.character(r$athlete_id), names(ht_map))
    ht_map <- c(ht_map, setNames(as.character(r$height[match(new_ids, as.character(r$athlete_id))]), new_ids))
  }
  rosters <- player_box %>%
    filter(!is.na(athlete_id)) %>%
    arrange(desc(game_date)) %>%
    distinct(team_id, athlete_id, .keep_all = TRUE) %>%
    transmute(team_id, athlete_id, jersey = as.character(athlete_jersey),
              height = unname(ht_map[as.character(athlete_id)]),
              experience_years = NA_integer_, headshot_href = as.character(athlete_headshot_href),
              birth_place_city = NA_character_, birth_place_state = NA_character_,
              birth_place_country = NA_character_, age = NA_integer_)
}
rosters <- add_missing(as.data.frame(rosters), c("jersey", "height", "experience_years", "headshot_href", "birth_place_city",
                                                 "birth_place_state", "birth_place_country", "age"), NA)
# ESPN serves every player's headshot at a fixed address; older box scores
# don't carry it (the page hides a headshot that fails to load)
rosters$headshot_href <- ifelse(is.na(rosters$headshot_href) | !nzchar(rosters$headshot_href),
  paste0("https://a.espncdn.com/i/headshots/wnba/players/full/", rosters$athlete_id, ".png"), rosters$headshot_href)

if (!is.null(pbp)) {
  pbp <- as.data.frame(pbp)
  pbp <- num_cols(pbp, c("home_score", "away_score", "start_quarter_seconds_remaining", "period_number",
                         "coordinate_x", "coordinate_y", "coordinate_x_raw", "coordinate_y_raw", "score_value", "game_play_number"))
  pbp$shooting_play <- pbp$shooting_play %in% TRUE
  pbp$scoring_play <- pbp$scoring_play %in% TRUE
  # Older play-by-play has no points_attempted. Rebuild it: the play text
  # ("three point"), then the points scored on makes, then (misses only) the
  # location -- checked against 2024's official field: 99.96% agreement.
  if (!"points_attempted" %in% names(pbp) || all(is.na(pbp$points_attempted))) {
    ft <- grepl("free\\s*throw", pbp$type_text, ignore.case = TRUE)
    dx <- pbp$coordinate_x_raw - 25; dy <- pbp$coordinate_y_raw
    geom3 <- (abs(dx) >= (if (OLD_THREE_LINE) 20.5 else 21.5) & dy <= 9) |
      sqrt(dx^2 + dy^2) >= (if (OLD_THREE_LINE) 20.5 else 22.75)
    txt3 <- grepl("three point|three-point|3-pt|3pt", pbp$text, ignore.case = TRUE)
    three <- txt3 | (pbp$scoring_play & pbp$score_value %in% 3) |
      (!pbp$scoring_play & !txt3 & geom3 %in% TRUE)
    pbp$points_attempted <- ifelse(!pbp$shooting_play, NA_real_, ifelse(ft, 1, ifelse(three, 3, 2)))
  }
}

# ---- normalize key types (ALL id columns, including game_id) ----------------
team_box <- team_box %>%
  mutate(team_id = as.character(team_id), opponent_team_id = as.character(opponent_team_id),
         game_id = as.character(game_id))
player_box <- player_box %>%
  mutate(team_id = as.character(team_id), athlete_id = as.character(athlete_id),
         game_id = as.character(game_id), opponent_team_id = as.character(opponent_team_id))
rosters <- rosters %>% mutate(team_id = as.character(team_id), athlete_id = as.character(athlete_id))
if (!is.null(pbp)) {
  pbp <- pbp %>%
    mutate(team_id = as.character(team_id), game_id = as.character(game_id),
           home_team_id = as.character(home_team_id), away_team_id = as.character(away_team_id),
           athlete_id_1 = as.character(athlete_id_1), athlete_id_2 = as.character(athlete_id_2),
           athlete_id_3 = as.character(athlete_id_3))
}

# Drop the All-Star game's draft-style "teams" (e.g. "TEAM SPOON") -- they
# share ESPN's schema exactly like a franchise, so this has to be by name.
all_star_ids <- team_box %>% filter(grepl("^TEAM ", toupper(team_display_name))) %>% pull(team_id) %>% unique()
# Past seasons also carry All-Star squads (EAST/WEST, "WNBA Stars") and
# exhibitions against national teams (USA, Delaware Blue Coats, ...). Real
# franchises play a full schedule, so anything with under 10 games is dropped.
few_game_ids <- team_box %>% count(team_id) %>% filter(n < 10) %>% pull(team_id)
all_star_ids <- unique(c(all_star_ids, few_game_ids))
team_box <- team_box %>% filter(!team_id %in% all_star_ids, !opponent_team_id %in% all_star_ids)
player_box <- player_box %>% filter(!team_id %in% all_star_ids, !opponent_team_id %in% all_star_ids)
if (!is.null(pbp)) pbp <- pbp %>% filter(!home_team_id %in% all_star_ids, !away_team_id %in% all_star_ids)

# ---- colors / logos / conference ------------------------------------------
clean_hex <- function(x) {
  x <- trimws(tolower(as.character(x)))
  ifelse(grepl("^[0-9a-f]{6}$", x), x, NA_character_)
}
color_lookup <- team_box %>%
  mutate(team_color = clean_hex(team_color), team_alternate_color = clean_hex(team_alternate_color)) %>%
  group_by(team_id) %>%
  summarise(team_color = first(na.omit(team_color)), team_alternate_color = first(na.omit(team_alternate_color)),
            team_logo = first(na.omit(team_logo)), abbr = first(team_abbreviation), .groups = "drop")
lookup_chr <- function(map, id) { v <- unname(map[id]); if (length(v) == 0 || is.na(v)) NULL else v }
primary_by_id <- setNames(color_lookup$team_color, color_lookup$team_id)
secondary_by_id <- setNames(color_lookup$team_alternate_color, color_lookup$team_id)
logo_by_id <- setNames(color_lookup$team_logo, color_lookup$team_id)
abbr_by_id <- setNames(color_lookup$abbr, color_lookup$team_id)
conf_by_id <- setNames(unname(WNBA_CONF[color_lookup$abbr]), color_lookup$team_id)
lookup_conf <- function(id) { v <- unname(conf_by_id[id]); if (length(v) == 0 || is.na(v)) NA_character_ else v }
unmapped <- color_lookup$abbr[is.na(conf_by_id)]
if (length(unmapped)) message("  NOTE: no conference mapped for: ", paste(unmapped, collapse = ", "),
                              " -- add them to WNBA_CONF near the top of this script.")

# ---- opponent's raw box line, joined onto each team-game row --------------
opp_box <- team_box %>%
  select(game_id, team_id, field_goals_made, field_goals_attempted,
         three_point_field_goals_made, three_point_field_goals_attempted,
         free_throws_made, free_throws_attempted, turnovers,
         offensive_rebounds, defensive_rebounds, team_score,
         assists, steals, blocks, fouls, fast_break_points, points_in_paint,
         largest_lead, total_technical_fouls, turnover_points) %>%
  rename(
    opponent_team_id = team_id,
    o_fgm = field_goals_made, o_fga = field_goals_attempted,
    o_tpm = three_point_field_goals_made, o_tpa = three_point_field_goals_attempted,
    o_ftm = free_throws_made, o_fta = free_throws_attempted, o_tov = turnovers,
    o_oreb = offensive_rebounds, o_dreb = defensive_rebounds, o_pts = team_score,
    o_ast = assists, o_stl = steals, o_blk = blocks, o_pf = fouls,
    o_fbPts = fast_break_points, o_pip = points_in_paint,
    o_lead = largest_lead, o_techFouls = total_technical_fouls, o_tovPts = turnover_points
  )
team_box <- team_box %>% left_join(opp_box, by = c("game_id", "opponent_team_id"))

season_games <- team_box %>% arrange(team_id, desc(game_date), desc(game_id))
team_ids <- unique(season_games$team_id)
message("Building dashboards for ", length(team_ids), " WNBA teams, ",
        nrow(season_games), " team-games (", SEASON, " season) ...")

# =============================================================================
# ---- SHOTS: zone classification + shot-level payload -----------------------
# =============================================================================
# Coordinates: ESPN's raw half-court frame, as delivered in pbp's
# coordinate_x_raw / coordinate_y_raw -- x = 0..50 across the floor, y = feet
# from the hoop centre toward half court (the baseline is y = -5.25). This
# is what the page's shot chart plots directly (lateral = x_raw - 25), and it
# is the same geometry the 5-zone split has always used: pbp's transformed
# coordinate_x/coordinate_y are just this frame rotated onto the full court,
# so dist = hypot(x_raw - 25, y_raw) exactly.
#   - 2-point vs. 3-point is EXACT, from pbp's own points_attempted.
#   - Rim/Paint/Mid-Range/Corner 3/Above-the-Break is a distance + sideline
#     heuristic (not an official definition), verified against the real
#     season: every team's zone FGA sums to its box-score FGA.
#   - Rows with sentinel coordinates (|coordinate_x| > 60, a 32-bit overflow
#     value ESPN uses for "no location") are dropped rather than guessed.
ZONE_ORDER <- c("rim", "paint", "mid", "corner3", "break3")

shot_type_levels <- character(0)
if (!is.null(pbp)) {
  message("Classifying shots from play-by-play ...")
  pbp <- pbp %>% arrange(game_id, game_play_number)
  shots <- pbp %>%
    filter(shooting_play == TRUE, !grepl("free\\s*throw", type_text, ignore.case = TRUE),
           !is.na(coordinate_x), !is.na(coordinate_y),
           abs(coordinate_x) <= 60, abs(coordinate_y) <= 60,
           !is.na(coordinate_x_raw), !is.na(coordinate_y_raw)) %>%
    mutate(
      is_three = points_attempted == 3,
      sx = as.integer(round(coordinate_x_raw - 25)),
      sy = as.integer(round(coordinate_y_raw)),
      dist_ft = sqrt((coordinate_x_raw - 25)^2 + coordinate_y_raw^2),
      made = scoring_play == TRUE,
      zone = case_when(
        !is_three & dist_ft <= 4  ~ "rim",
        !is_three & dist_ft <= 14 ~ "paint",
        !is_three                 ~ "mid",
        is_three & abs(coordinate_x_raw - 25) >= CORNER3_MIN_X & dist_ft <= 24 ~ "corner3",
        is_three                  ~ "break3",
        TRUE ~ "other"
      ),
      def_team_id = ifelse(team_id == home_team_id, away_team_id, home_team_id)
    ) %>%
    filter(zone %in% ZONE_ORDER, team_id %in% c(home_team_id, away_team_id))
  shot_type_levels <- sort(unique(gsub("\\s+", " ", shots$type_text)))

  build_zone_lookup <- function(df, id_col) {
    agg <- df %>% group_by(.data[[id_col]], game_id, zone) %>%
      summarise(fga = n(), fgm = sum(made, na.rm = TRUE), .groups = "drop")
    agg$.key <- paste0(agg[[id_col]], "|", agg$game_id)
    split(agg, agg$.key)
  }
  off_zone_lookup <- build_zone_lookup(shots, "team_id")
  def_zone_lookup <- build_zone_lookup(shots, "def_team_id")
  player_zone_lookup <- build_zone_lookup(shots, "athlete_id_1")
} else {
  shots <- NULL
  off_zone_lookup <- list(); def_zone_lookup <- list(); player_zone_lookup <- list()
}

zone_dict <- function(lookup, id, gid) {
  sub <- lookup[[paste0(id, "|", gid)]]
  out <- list()
  for (z in ZONE_ORDER) {
    if (is.null(sub)) { out[[z]] <- list(0L, 0L); next }
    row <- sub[sub$zone == z, ]
    out[[z]] <- if (nrow(row) == 0) list(0L, 0L) else list(as.integer(row$fga[1]), as.integer(row$fgm[1]))
  }
  out
}
zone_vec <- function(lookup, id, gid) {
  zd <- zone_dict(lookup, id, gid)
  as.list(unlist(lapply(ZONE_ORDER, function(z) c(as.integer(zd[[z]][[1]]), as.integer(zd[[z]][[2]])))))
}

# =============================================================================
# ---- LINEUP STINTS (play-by-play walk) --------------------------------------
# =============================================================================
# Global compact player index: lineups and shots refer to players by their
# position in PLAYER_IDX (0-based on the page), resolved to names through
# the same box_info map the box scores use.
PLAYER_IDX <- new.env(hash = TRUE)
player_ids_ordered <- character(0)
pidx <- function(aid) {
  if (is.na(aid) || !nzchar(aid)) return(-1L)
  v <- PLAYER_IDX[[aid]]
  if (is.null(v)) {
    v <- length(player_ids_ordered)
    player_ids_ordered[v + 1] <<- aid
    assign(aid, v, envir = PLAYER_IDX)
  }
  v
}

# per-side stat vector order (documented for the page as LINEUP_STAT_COLS)
STAT_COLS <- c("pts", "fgm", "fga", "tpm", "tpa", "ftm", "fta", "oreb", "dreb", "tov", "ast", "stl", "blk", "pf")
S <- setNames(seq_along(STAT_COLS), STAT_COLS)
CLUTCH_SECS <- 300; CLUTCH_MARGIN <- 5

period_len <- function(p) if (p <= REG_PERIODS) REG_PERIOD_SECS else 300
period_offset <- function(p) if (p <= 1) 0 else sum(vapply(seq_len(p - 1), period_len, numeric(1)))

pbp_games <- list()
lineup_diag <- list()
career_shots <- list()
# ESPN's ~40 shot descriptions (and the older run-together spellings, e.g.
# "DrivingLayupShot", "Jump Stepback"), grouped into the page's shot families
SHOT_FAMILIES <- c("Layup", "Cutting layup", "Putback / tip", "Dunk / alley-oop", "Floater", "Hook",
                   "Jump shot", "Pull-up", "Step-back", "Turnaround / fadeaway")
shot_family <- function(t) {
  t <- tolower(gsub("\\s+", " ", as.character(t)))
  dplyr::case_when(
    grepl("dunk|alley ?oop", t) ~ "Dunk / alley-oop",
    grepl("tip|putback", t) ~ "Putback / tip",
    grepl("cutting", t) ~ "Cutting layup",
    grepl("lay ?up|finger roll", t) ~ "Layup",
    grepl("float", t) ~ "Floater",
    grepl("hook", t) ~ "Hook",
    grepl("step ?back", t) ~ "Step-back",
    grepl("turnaround|fade", t) ~ "Turnaround / fadeaway",
    grepl("pull ?-?up|driving", t) ~ "Pull-up",
    TRUE ~ "Jump shot")
}
if (!is.null(pbp)) {
  message("Reconstructing on-court lineups from play-by-play ...")
  pb_all <- player_box %>% select(game_id, team_id, athlete_id, starter, did_not_play, minutes)
  pb_split <- split(pb_all, pb_all$game_id)
  pbp_split <- split(pbp, pbp$game_id)
  shots_split <- if (!is.null(shots)) split(shots, shots$game_id) else list()
  is_turnover <- function(tt) grepl("turnover", tt, ignore.case = TRUE) & !grepl("^no turnover", tt, ignore.case = TRUE)
  is_foul <- function(tt) grepl("foul", tt, ignore.case = TRUE) & !grepl("technical|double|turnover", tt, ignore.case = TRUE)
  NON_PRESENCE <- "technical|ejection|timeout|end period|end game|challenge|review|delay"

  for (gid in names(pbp_split)) {
    ev <- pbp_split[[gid]]
    pbg <- pb_split[[gid]]
    if (is.null(pbg) || nrow(ev) == 0) next
    home <- ev$home_team_id[1]; away <- ev$away_team_id[1]
    if (!(home %in% team_ids) || !(away %in% team_ids)) next
    pteam <- setNames(pbg$team_id, pbg$athlete_id)
    mins_by <- setNames(na0(pbg$minutes), pbg$athlete_id)
    starters <- list(
      h = pbg$athlete_id[pbg$team_id == home & pbg$starter %in% TRUE],
      a = pbg$athlete_id[pbg$team_id == away & pbg$starter %in% TRUE]
    )
    side_of <- function(tid) if (identical(tid, home)) "h" else if (identical(tid, away)) "a" else NA_character_

    n <- nrow(ev)
    per <- ev$period_number
    # clamp the clock into its period (a few older rows carry a 20:00 clock in
    # a 10-minute quarter, which would otherwise make negative-length stints)
    clk <- pmin(pmax(na0(ev$start_quarter_seconds_remaining), 0), vapply(per, period_len, numeric(1)))
    tt <- ev$type_text; a1 <- ev$athlete_id_1; a2 <- ev$athlete_id_2; a3 <- ev$athlete_id_3
    etm <- ev$team_id
    locf <- function(v) { v <- as.numeric(v); if (is.na(v[1])) v[1] <- 0; for (j in seq_along(v)[-1]) if (is.na(v[j])) v[j] <- v[j - 1]; v }
    hs <- locf(ev$home_score); as_ <- locf(ev$away_score)
    shoot <- ev$shooting_play %in% TRUE; score <- ev$scoring_play %in% TRUE
    ptsatt <- na0(ev$points_attempted)
    is_ft <- grepl("free\\s*throw", tt, ignore.case = TRUE)
    is_sub <- tt == "Substitution"
    tov <- is_turnover(tt); foul <- is_foul(tt)
    presence_ok <- !grepl(NON_PRESENCE, tt, ignore.case = TRUE)
    periods <- sort(unique(per))

    segs <- list(); seg_of_play <- integer(n); issues <- 0L
    period_scores <- list()
    prev_end <- list(h = character(0), a = character(0))
    last_seen <- list()

    infer_starters <- function(idx, side, tid) {
      seen <- character(0); st <- character(0)
      for (k in idx) {
        if (is_sub[k]) {
          if (!identical(pteam[a1[k]] %||% etm[k], tid) && !identical(etm[k], tid)) next
          o <- a2[k]; i <- a1[k]
          if (!is.na(o) && !(o %in% seen)) { st <- c(st, o); seen <- c(seen, o) }
          if (!is.na(i) && !(i %in% seen)) seen <- c(seen, i)
        } else if (presence_ok[k]) {
          for (p in c(a1[k], a2[k], a3[k])) {
            if (is.na(p) || !(p %in% names(pteam)) || !identical(unname(pteam[p]), tid)) next
            if (!(p %in% seen)) { st <- c(st, p); seen <- c(seen, p) }
          }
        }
        if (length(st) >= 5) break
      }
      if (length(st) < 5) {
        # someone played the whole period without touching the box score:
        # carry over from the previous period's closing five, then by minutes
        cand <- setdiff(prev_end[[side]], seen)
        if (length(cand) < 5 - length(st)) {
          pool <- names(pteam)[pteam == tid]
          pool <- pool[order(-mins_by[pool])]
          cand <- c(cand, setdiff(pool, c(seen, cand)))
        }
        st <- c(st, head(cand, 5 - length(st)))
      }
      head(st, 5)
    }

    # Substitutions that ESPN logs between a foul and its free throws (same
    # clock) are applied AFTER the last of those free throws, so FT points
    # go to the five who were on the floor for the foul -- the standard
    # plus/minus convention, and what reconciles with ESPN's own box +/-.
    reorder_ft <- function(idx) {
      ck <- clk[idx]; out <- integer(0); i <- 1L; m <- length(idx)
      while (i <= m) {
        j <- i
        while (j < m && isTRUE(ck[j + 1] == ck[i])) j <- j + 1L
        grp <- idx[i:j]
        fts <- which(is_ft[grp] & shoot[grp])
        if (length(fts) && any(is_sub[grp])) {
          sb <- which(is_sub[grp]); sb <- sb[sb < max(fts)]
          if (length(sb)) {
            rest <- grp[-sb]; kk <- max(fts) - length(sb)
            grp <- c(rest[seq_len(kk)], grp[sb], rest[-seq_len(kk)])
          }
        }
        out <- c(out, grp); i <- j + 1L
      }
      out
    }

    for (p in periods) {
      idx <- reorder_ft(which(per == p))
      pidx_order <- idx
      plen <- period_len(p); off <- period_offset(p)
      cur <- list(
        h = if (p == 1 && length(starters$h) == 5) starters$h else infer_starters(idx, "h", home),
        a = if (p == 1 && length(starters$a) == 5) starters$a else infer_starters(idx, "a", away)
      )
      score_prev <- if (idx[1] > 1) c(hs[idx[1] - 1], as_[idx[1] - 1]) else c(0, 0)
      seg_start <- off; acc <- list(h = numeric(length(STAT_COLS)), a = numeric(length(STAT_COLS)))
      cl_state <- NA
      open_seg <- function() list(start = seg_start, h = cur$h, a = cur$a, cl = cl_state)
      segs_before <- length(segs)
      flush <- function(t_end) {
        dur <- t_end - seg_start
        if (dur > 0 || any(acc$h != 0) || any(acc$a != 0)) {
          segs[[length(segs) + 1]] <<- list(
            per_out(p), as.integer(seg_start), as.integer(round(dur)), as.integer(isTRUE(cl_state)),
            vapply(cur$h, pidx, integer(1)), vapply(cur$a, pidx, integer(1)),
            as.integer(acc$h), as.integer(acc$a))
        }
        seg_start <<- t_end; acc <<- list(h = numeric(length(STAT_COLS)), a = numeric(length(STAT_COLS)))
      }
      for (k in idx) {
        t <- off + (plen - na0(clk[k]))
        margin <- abs(score_prev[1] - score_prev[2])
        cl_now <- p >= REG_PERIODS && na0(clk[k]) <= CLUTCH_SECS && margin <= CLUTCH_MARGIN
        if (is.na(cl_state)) cl_state <- cl_now
        if (!identical(cl_now, cl_state)) { flush(t); cl_state <- cl_now }
        if (is_sub[k]) {
          sd <- side_of(unname(pteam[a1[k]]) %||% etm[k])
          if (is.na(sd)) sd <- side_of(etm[k])
          # malformed rows ("X enters the game for" with no one leaving)
          # are skipped -- ESPN re-logs the complete substitution after them
          if (!is.na(sd) && !is.na(a1[k]) && !is.na(a2[k])) {
            flush(t)
            i <- a1[k]; o <- a2[k]; l <- cur[[sd]]
            if (!is.na(o) && o %in% l) l <- setdiff(l, o) else if (!is.na(o)) issues <- issues + 1L
            if (!is.na(i) && !(i %in% l)) l <- c(l, i)
            if (length(l) > 5) {   # a missed sub-out: drop whoever acted least recently
              ls <- vapply(l, function(q) last_seen[[q]] %||% -1, numeric(1))
              ls[l == i] <- Inf
              l <- l[-which.min(ls)]
            }
            if (length(l) < 5) {
              # an earlier sub was mis-logged: pull in the next teammate who
              # acts (or is subbed out) later this period without entering first
              tid_sd <- if (sd == "h") home else away
              entered <- character(0)
              for (k2 in pidx_order[seq_len(length(pidx_order)) > match(k, pidx_order)]) {
                if (is_sub[k2]) {
                  if (!is.na(a2[k2]) && identical(unname(pteam[a2[k2]]), tid_sd) && !(a2[k2] %in% c(l, entered))) l <- c(l, a2[k2])
                  if (!is.na(a1[k2])) entered <- c(entered, a1[k2])
                } else if (presence_ok[k2]) {
                  for (q in c(a1[k2], a2[k2], a3[k2])) {
                    if (!is.na(q) && q %in% names(pteam) && identical(unname(pteam[q]), tid_sd) && !(q %in% c(l, entered))) l <- c(l, q)
                  }
                }
                if (length(l) >= 5) break
              }
              issues <- issues + 1L
            }
            cur[[sd]] <- l
          }
          seg_of_play[k] <- length(segs)
          next
        }
        # presence bookkeeping (for the >5 repair above)
        for (q in c(a1[k], a2[k], a3[k])) {
          if (is.na(q)) next
          last_seen[[q]] <- t
          # a missed sub-in leaves a side short; the first teammate to record
          # an action while not "on the floor" fills the gap
          qs <- if (q %in% names(pteam)) side_of(unname(pteam[q])) else NA_character_
          if (presence_ok[k] && !is.na(qs) && length(cur[[qs]]) < 5 && !(q %in% cur[[qs]])) {
            flush(t); cur[[qs]] <- c(cur[[qs]], q); issues <- issues + 1L
          }
        }
        sd <- side_of(etm[k]); od <- if (identical(sd, "h")) "a" else "h"
        if (!is.na(sd)) {
          if (shoot[k] && !is_ft[k]) {
            acc[[sd]][S["fga"]] <- acc[[sd]][S["fga"]] + 1
            if (ptsatt[k] == 3) acc[[sd]][S["tpa"]] <- acc[[sd]][S["tpa"]] + 1
            if (score[k]) {
              acc[[sd]][S["fgm"]] <- acc[[sd]][S["fgm"]] + 1
              if (ptsatt[k] == 3) acc[[sd]][S["tpm"]] <- acc[[sd]][S["tpm"]] + 1
              if (!is.na(a2[k])) acc[[sd]][S["ast"]] <- acc[[sd]][S["ast"]] + 1
            } else if (!is.na(a2[k])) acc[[od]][S["blk"]] <- acc[[od]][S["blk"]] + 1
          } else if (shoot[k] && is_ft[k]) {
            acc[[sd]][S["fta"]] <- acc[[sd]][S["fta"]] + 1
            if (score[k]) acc[[sd]][S["ftm"]] <- acc[[sd]][S["ftm"]] + 1
          } else if (tt[k] == "Offensive Rebound" && !is.na(a1[k])) {
            acc[[sd]][S["oreb"]] <- acc[[sd]][S["oreb"]] + 1
          } else if (tt[k] == "Defensive Rebound" && !is.na(a1[k])) {
            acc[[sd]][S["dreb"]] <- acc[[sd]][S["dreb"]] + 1
          }
          if (tov[k]) {
            acc[[sd]][S["tov"]] <- acc[[sd]][S["tov"]] + 1
            if (!is.na(a2[k])) acc[[od]][S["stl"]] <- acc[[od]][S["stl"]] + 1
          }
          if (foul[k]) acc[[sd]][S["pf"]] <- acc[[sd]][S["pf"]] + 1
        }
        # points from the running score (catches every scoring play, incl.
        # ones with a missing/odd team attribution)
        # signed: a later downward correction (e.g. a basket overturned on
        # review) comes off the lineup that's on the floor when it's posted,
        # so every game's segment points telescope exactly to the final score
        acc$h[S["pts"]] <- acc$h[S["pts"]] + (hs[k] - score_prev[1])
        acc$a[S["pts"]] <- acc$a[S["pts"]] + (as_[k] - score_prev[2])
        score_prev <- c(hs[k], as_[k])
        seg_of_play[k] <- length(segs)
      }
      flush(off + plen)
      prev_end <- cur
      last_k <- idx[length(idx)]
      period_scores[[length(period_scores) + 1]] <- c(p, hs[last_k], as_[last_k])
    }

    # quarter-by-quarter points from the running score at each period's end
    ps <- do.call(rbind, period_scores)
    ph <- diff(c(0, ps[, 2])); pa <- diff(c(0, ps[, 3]))

    # shot rows for this game: [shooter, assister, x, y, flags, type, period,
    # clock secs, segment]; flags: 1 made, 2 three, 4 home team shooting, 8 blocked
    g_shots <- list()
    sg <- shots_split[[gid]]
    if (!is.null(sg) && nrow(sg) > 0) {
      row_of <- match(sg$game_play_number, ev$game_play_number)
      g_shots <- purrr::pmap(list(
        a1 = sg$athlete_id_1, a2 = sg$athlete_id_2, x = sg$sx, y = sg$sy, m = sg$made, th = sg$is_three,
        tid = sg$team_id, ty = gsub("\\s+", " ", sg$type_text), pr = sg$period_number,
        ck = sg$start_quarter_seconds_remaining, r = row_of
      ), function(a1, a2, x, y, m, th, tid, ty, pr, ck, r) {
        made <- isTRUE(m)
        flags <- as.integer(made) + 2L * as.integer(isTRUE(th)) + 4L * as.integer(identical(tid, home)) +
          8L * as.integer(!made && !is.na(a2))
        list(pidx(a1), if (made && !is.na(a2)) pidx(a2) else -1L, x, y, flags,
             match(ty, shot_type_levels) - 1L, per_out(pr), as.integer(round(na0(ck))),
             if (!is.na(r)) as.integer(seg_of_play[r]) else -1L)
      })
    }

    if (!is.null(sg) && nrow(sg) > 0) {
      # career-file shot rows (v4): the same shots, keyed by athlete id, with
      # each shot's clutch flag taken from the lineup segment it happened in
      seg_i <- ifelse(is.na(row_of), -1L, seg_of_play[row_of])
      made_v <- sg$made %in% TRUE
      career_shots[[gid]] <- data.frame(
        gid = gid, tid = sg$team_id, shooter = sg$athlete_id_1,
        assister = ifelse(made_v & !is.na(sg$athlete_id_2), sg$athlete_id_2, NA_character_),
        x = sg$sx, y = sg$sy, made = made_v, three = sg$is_three %in% TRUE,
        blk = !made_v & !is.na(sg$athlete_id_2),
        per = per_out(sg$period_number),
        cl = vapply(seg_i, function(j) if (j >= 0 && j < length(segs)) as.integer(segs[[j + 1]][[4]]) else 0L, integer(1)),
        fam = shot_family(sg$type_text), stringsAsFactors = FALSE)
    }

    pbp_games[[gid]] <- list(h = home, a = away, segs = segs, shots = g_shots,
                             per = lapply(seq_len(nrow(ps)), function(i) c(as.integer(ps[i, 1]), as.integer(ph[i]), as.integer(pa[i]))))
    lineup_diag[[gid]] <- issues
  }
  message("  lineups rebuilt for ", length(pbp_games), " games (",
          sum(vapply(pbp_games, function(g) length(g$segs), integer(1))), " segments; ",
          sum(unlist(lineup_diag)), " substitution anomalies repaired)")
}

# =============================================================================
# ---- per-game box score rows (both teams), stored once ----------------------
# =============================================================================
BOX_COLS <- c("key", "min", "pts", "reb", "oreb", "ast", "stl", "blk", "tov", "pf", "fg", "tp", "ft", "pm", "gs", "zones")

height_lookup <- rosters %>% select(team_id, athlete_id, height) %>% distinct(team_id, athlete_id, .keep_all = TRUE)
player_box_by_game_team <- player_box %>%
  filter(did_not_play != TRUE) %>%
  left_join(height_lookup, by = c("team_id", "athlete_id")) %>%
  arrange(desc(starter), desc(minutes)) %>%
  { split(., paste0(.$game_id, ":", .$team_id)) }

box_store <- new.env(hash = TRUE)
box_info <- new.env(hash = TRUE)

box_rows_for <- function(game_id_, team_id_) {
  rows <- player_box_by_game_team[[paste0(game_id_, ":", team_id_)]]
  if (is.null(rows) || nrow(rows) == 0) return(list())
  purrr::pmap(list(
    aid = rows$athlete_id, n = rows$athlete_display_name, p = rows$athlete_position_abbreviation,
    h = rows$height, minv = rows$minutes, pts = rows$points, reb = rows$rebounds,
    oreb = safe_col(rows, "offensive_rebounds"), ast = rows$assists, stl = rows$steals, blk = rows$blocks,
    tov = safe_col(rows, "turnovers"), pf = safe_col(rows, "fouls"),
    fgm = rows$field_goals_made, fga = rows$field_goals_attempted,
    tpm = rows$three_point_field_goals_made, tpa = rows$three_point_field_goals_attempted,
    ftm = rows$free_throws_made, fta = rows$free_throws_attempted,
    pm = safe_col(rows, "plus_minus"), gs = safe_col(rows, "starter")
  ), function(aid, n, p, h, minv, pts, reb, oreb, ast, stl, blk, tov, pf, fgm, fga, tpm, tpa, ftm, fta, pm, gs) {
    info_key <- if (is.na(aid) || !nzchar(as.character(aid))) paste0("n:", n) else as.character(aid)
    if (is.null(box_info[[info_key]])) assign(info_key, list(n %||% "", p %||% "", h %||% ""), envir = box_info)
    if (!is.na(aid)) pidx(as.character(aid))
    list(
      info_key,
      as.integer(round(minv %||% 0)), as.integer(pts %||% 0),
      as.integer(reb %||% 0), as.integer(round(oreb %||% 0)), as.integer(ast %||% 0),
      as.integer(stl %||% 0), as.integer(blk %||% 0),
      as.integer(round(tov %||% 0)), as.integer(round(pf %||% 0)),
      paste0(fgm %||% 0, "-", fga %||% 0), paste0(tpm %||% 0, "-", tpa %||% 0), paste0(ftm %||% 0, "-", fta %||% 0),
      if (is.na(pm)) NULL else as.integer(pm), as.integer(isTRUE(as.logical(gs))),
      zone_vec(player_zone_lookup, as.character(aid), game_id_)
    )
  })
}
store_box <- function(game_id_, team_id_) {
  key <- paste0(game_id_, ":", team_id_)
  if (!exists(key, envir = box_store, inherits = FALSE)) assign(key, box_rows_for(game_id_, team_id_), envir = box_store)
  invisible(key)
}

# =============================================================================
# ---- per-team summary + detail ---------------------------------------------
# =============================================================================
message("Assembling per-team payloads ...")
teams_summary <- vector("list", length(team_ids))
teams_detail <- list()

for (i in seq_along(team_ids)) {
  tid <- team_ids[i]
  sub <- season_games %>% filter(team_id == tid)
  team_name <- sub$team_display_name[1]
  wins <- sum(sub$team_winner, na.rm = TRUE)

  detail_games <- purrr::pmap(list(
    game_id = sub$game_id, date = as.character(sub$game_date), opp = sub$opponent_team_display_name,
    opp_id = sub$opponent_team_id, loc = sub$team_home_away, res = sub$team_winner, st = sub$season_type,
    ts = sub$team_score, os = sub$opponent_team_score,
    fgm = sub$field_goals_made, fga = sub$field_goals_attempted,
    tpm = sub$three_point_field_goals_made, tpa = sub$three_point_field_goals_attempted,
    ftm = sub$free_throws_made, fta = sub$free_throws_attempted, tov = sub$turnovers,
    oreb = sub$offensive_rebounds, dreb = sub$defensive_rebounds,
    ast = sub$assists, stl = sub$steals, blk = sub$blocks, pf = sub$fouls,
    fbPts = sub$fast_break_points, pip = sub$points_in_paint,
    lead = sub$largest_lead, techFouls = sub$total_technical_fouls, tovPts = sub$turnover_points,
    o_fgm = sub$o_fgm, o_fga = sub$o_fga, o_tpm = sub$o_tpm, o_tpa = sub$o_tpa,
    o_ftm = sub$o_ftm, o_fta = sub$o_fta, o_tov = sub$o_tov,
    o_oreb = sub$o_oreb, o_dreb = sub$o_dreb,
    o_ast = sub$o_ast, o_stl = sub$o_stl, o_blk = sub$o_blk, o_pf = sub$o_pf,
    o_fbPts = sub$o_fbPts, o_pip = sub$o_pip, o_lead = sub$o_lead, o_techFouls = sub$o_techFouls, o_tovPts = sub$o_tovPts
  ), function(game_id, date, opp, opp_id, loc, res, st, ts, os,
              fgm, fga, tpm, tpa, ftm, fta, tov, oreb, dreb, ast, stl, blk, pf, fbPts, pip, lead, techFouls, tovPts,
              o_fgm, o_fga, o_tpm, o_tpa, o_ftm, o_fta, o_tov, o_oreb, o_dreb,
              o_ast, o_stl, o_blk, o_pf, o_fbPts, o_pip, o_lead, o_techFouls, o_tovPts) {
    store_box(game_id, tid)
    store_box(game_id, opp_id)
    # quarter scores from pbp, oriented to THIS team
    periods <- NULL
    pg <- pbp_games[[as.character(game_id)]]
    if (!is.null(pg) && length(pg$per)) {
      is_home <- identical(pg$h, tid)
      periods <- lapply(pg$per, function(v) list(
        label = if (v[1] <= REG_PERIODS) (if (REG_PERIODS == 2L) paste0("H", v[1]) else paste0("Q", v[1]))
                else if (v[1] == REG_PERIODS + 1) "OT" else paste0(v[1] - REG_PERIODS, "OT"),
        t = if (is_home) v[2] else v[3], o = if (is_home) v[3] else v[2]))
      # only trust it if it reconciles with the final score
      if (sum(vapply(periods, function(z) z$t, numeric(1))) != ts ||
          sum(vapply(periods, function(z) z$o, numeric(1))) != os) periods <- NULL
    }
    list(
      date = substr(date, 1, 10), opponent = opp, oppId = opp_id, gid = as.character(game_id),
      loc = if (loc == "home") "H" else if (loc == "away") "A" else "N",
      st = as.integer(st %||% 2L),
      result = ifelse(isTRUE(res), "W", "L"),
      score = paste0(as.integer(ts), "-", as.integer(os)),
      isConf = isTRUE(!is.na(lookup_conf(tid)) && lookup_conf(tid) == lookup_conf(opp_id)),
      t = list(fgm = na0(fgm), fga = na0(fga), tpm = na0(tpm), tpa = na0(tpa),
               ftm = na0(ftm), fta = na0(fta), tov = na0(tov),
               oreb = na0(oreb), dreb = na0(dreb), pts = as.integer(ts),
               ast = na0(ast), stl = na0(stl), blk = na0(blk), pf = na0(pf),
               fbPts = na0(fbPts), pip = na0(pip), lead = na0(lead), techFouls = na0(techFouls), tovPts = na0(tovPts)),
      o = list(fgm = na0(o_fgm), fga = na0(o_fga), tpm = na0(o_tpm), tpa = na0(o_tpa),
               ftm = na0(o_ftm), fta = na0(o_fta), tov = na0(o_tov),
               oreb = na0(o_oreb), dreb = na0(o_dreb), pts = as.integer(os),
               ast = na0(o_ast), stl = na0(o_stl), blk = na0(o_blk), pf = na0(o_pf),
               fbPts = na0(o_fbPts), pip = na0(o_pip), lead = na0(o_lead), techFouls = na0(o_techFouls), tovPts = na0(o_tovPts)),
      periods = periods,
      zonesOff = zone_dict(off_zone_lookup, tid, game_id),
      zonesDef = zone_dict(def_zone_lookup, tid, game_id)
    )
  })

  teams_summary[[i]] <- list(
    id = tid, team = team_name, abbr = unname(abbr_by_id[tid]), conf = lookup_conf(tid) %||% "",
    primary = lookup_chr(primary_by_id, tid), secondary = lookup_chr(secondary_by_id, tid),
    logo = lookup_chr(logo_by_id, tid),
    record = paste0(wins, "-", nrow(sub) - wins)
  )
  teams_detail[[tid]] <- list(team = team_name, games = detail_games)
}

summary_payload <- list(season = SEASON, league = "WNBA", n_teams = length(teams_summary),
                        generated = format(Sys.time(), "%Y-%m-%d %H:%M"), teams = teams_summary)

# =============================================================================
# ---- Player Value: BPR (box), BPM/RAPM/WAR (release), VORP/WS (derived) ---
# =============================================================================
# BPM/RAPM/WAR come from sportsdataverse's wnba_player_impact release. That
# release keys players by a WNBA Stats API id, not ESPN's athlete_id, and
# there's no published crosswalk -- so the join is by normalized name (plus
# team abbreviation when a name isn't unique). No confident match -> blank,
# never a guess. VORP and Win Shares are derived from BPM exactly as the
# WBB dashboard does (see its README); constants are tunable below.
message("Building player value stats ...")
REPL_OBPM <- -1.0; REPL_DBPM <- -1.0
FULL_SEASON_GAMES <- 44            # current WNBA regular-season length (VORP's analogue of the NBA's /82)
# past seasons were shorter (34 games for most of 2003-2019, 22 in 2020, ...):
# use that season's actual schedule length
if (IS_PAST) FULL_SEASON_GAMES <- max(table(season_games$team_id[season_games$season_type == 2]))
VORP_TO_WINS <- 2.7
WS_REPLACEMENT_RATE <- 0.03

norm_name <- function(x) toupper(trimws(gsub("[^A-Za-z0-9 ]", "", iconv(x %||% "", to = "ASCII//TRANSLIT"))))
impact_by_name <- list()
if (!is.null(player_impact)) {
  pi_df <- player_impact %>% as.data.frame() %>%
    mutate(name_key = norm_name(player_name), team_abbreviation = as.character(team_abbreviation))
  # the release lists a player's regular season and playoffs as separate rows;
  # use the regular-season line (otherwise every playoff player looks like
  # two different people and goes unmatched)
  if ("season_type" %in% names(pi_df) && any(grepl("^Regular", pi_df$season_type))) {
    pi_df <- pi_df[grepl("^Regular", pi_df$season_type), , drop = FALSE]
  }
  impact_by_name <- split(pi_df, pi_df$name_key)
}

player_agg <- player_box %>%
  filter(did_not_play != TRUE) %>%
  group_by(team_id, athlete_id) %>%
  summarise(
    name = first(athlete_display_name),
    gp = n(), min = sum(minutes, na.rm = TRUE), pts = sum(points, na.rm = TRUE),
    oreb = sum(offensive_rebounds, na.rm = TRUE), dreb = sum(defensive_rebounds, na.rm = TRUE),
    ast = sum(assists, na.rm = TRUE), stl = sum(steals, na.rm = TRUE), blk = sum(blocks, na.rm = TRUE),
    tov = sum(turnovers, na.rm = TRUE), pf = sum(fouls, na.rm = TRUE),
    fgm = sum(field_goals_made, na.rm = TRUE), fga = sum(field_goals_attempted, na.rm = TRUE),
    ftm = sum(free_throws_made, na.rm = TRUE), fta = sum(free_throws_attempted, na.rm = TRUE),
    .groups = "drop"
  )
team_games_by_id <- setNames(sapply(teams_detail, function(d) length(d$games)), names(teams_detail))
team_min_map <- player_agg %>% group_by(team_id) %>% summarise(m = sum(min), .groups = "drop") %>% { setNames(.$m, .$team_id) }

player_values <- list()
n_matched <- 0L
for (r in seq_len(nrow(player_agg))) {
  row <- player_agg[r, ]
  tid <- row$team_id; aid <- row$athlete_id
  if (is.na(row$min) || row$min <= 0) next
  off_raw <- row$pts + 0.4*row$fgm - 0.7*row$fga - 0.4*(row$fta - row$ftm) + 0.7*row$oreb + 0.7*row$ast - row$tov
  def_raw <- 0.3*row$dreb + row$stl + 0.7*row$blk - 0.4*row$pf
  entry <- list(
    gp = as.integer(row$gp), min = as.integer(row$min),
    bpr_o = round(off_raw * 40 / row$min, 1), bpr_d = round(def_raw * 40 / row$min, 1),
    bpr = round((off_raw + def_raw) * 40 / row$min, 1)
  )
  cand <- impact_by_name[[norm_name(row$name)]]
  if (!is.null(cand) && nrow(cand) > 1) {
    ab <- unname(abbr_by_id[tid])
    c2 <- cand[grepl(paste0("\\b", ab, "\\b"), paste(cand$team_abbreviation, cand$teams)), , drop = FALSE]
    cand <- if (nrow(c2) >= 1) c2 else NULL
  }
  if (!is.null(cand) && nrow(cand) >= 1) {
    pv <- cand[1, ]
    n_matched <- n_matched + 1L
    team_games <- unname(team_games_by_id[tid]); if (is.na(team_games)) team_games <- FULL_SEASON_GAMES
    team_min_total <- unname(team_min_map[tid]); if (is.na(team_min_total)) team_min_total <- team_games * 200
    min_pct <- row$min / team_min_total
    season_scale <- team_games / FULL_SEASON_GAMES
    vorp_o <- (pv$obpm - REPL_OBPM) * min_pct * season_scale
    vorp_d <- (pv$dbpm - REPL_DBPM) * min_pct * season_scale
    ws_base <- WS_REPLACEMENT_RATE * (row$min / 40)
    ws_o <- vorp_o / VORP_TO_WINS + ws_base / 2
    ws_d <- vorp_d / VORP_TO_WINS + ws_base / 2
    rnd <- function(v) if (is.null(v) || is.na(v)) NULL else round(v, 1)
    entry <- c(entry, list(
      obpm = rnd(pv$obpm), dbpm = rnd(pv$dbpm), bpm = rnd(pv$bpm),
      vorp_o = rnd(vorp_o), vorp_d = rnd(vorp_d), vorp = rnd(vorp_o + vorp_d),
      ws_o = rnd(ws_o), ws_d = rnd(ws_d), ws = rnd(ws_o + ws_d),
      war = rnd(pv$war),
      orapm = rnd(safe_col(pv, "o_rapm")), drapm = rnd(safe_col(pv, "d_rapm")), rapm = rnd(safe_col(pv, "rapm"))
    ))
  }
  player_values[[paste0(tid, ":", aid)]] <- entry
}
message("  impact release matched for ", n_matched, " of ", nrow(player_agg), " player-team rows")

# ---- Player bio ------------------------------------------------------------
message("Building player bio lookup ...")
bio_df <- rosters %>% distinct(team_id, athlete_id, .keep_all = TRUE)
exp_label <- function(y) ifelse(is.na(y), NA_character_, ifelse(as.integer(y) == 0, "Rookie", paste0(as.integer(y), " yrs")))
player_bio_list <- setNames(
  purrr::pmap(list(bio_df$jersey, bio_df$experience_years, bio_df$headshot_href, bio_df$birth_place_city,
                   bio_df$birth_place_state, bio_df$birth_place_country, bio_df$age, bio_df$height),
    function(jersey, expy, headshot, city, state, country, age, ht) {
      home <- paste(na.omit(c(city, if (!is.na(state)) state else if (!is.na(country) && country != "USA") country else NA)), collapse = ", ")
      list(jersey = jersey, exp = exp_label(expy), headshot = headshot,
           hometown = if (nzchar(home)) home else NULL, age = if (is.na(age)) NULL else as.integer(age))
    }),
  paste0(bio_df$team_id, ":", bio_df$athlete_id)
)

# =============================================================================
# ---- CAREER EXTRACT (v4): this season's slice of every player's career ------
# =============================================================================
# Everything the player page's Career view needs from this season, at the
# grain the page filters on (season, team, regular season vs playoffs). The
# main run stitches every season's extract into wnba_seasons/players/<id>.js.
message("Building career extract ...")
st_by_gid <- setNames(as.integer(season_games$season_type), season_games$game_id)
chart_zone <- function(x, y, three) {
  a <- atan2(y, x) * 180 / pi
  ifelse(!three,
    ifelse(sqrt(x^2 + y^2) <= 4, "ra",
      ifelse(abs(x) <= 8 & y <= 13.75, "paint",
        ifelse(y <= 8.75, ifelse(x < 0, "midLB", "midRB"), ifelse(a > 120, "midLW", ifelse(a < 60, "midRW", "midC"))))),
    ifelse(y <= 8.75, ifelse(x < 0, "c3L", "c3R"), ifelse(a > 120, "ab3L", ifelse(a < 60, "ab3R", "ab3C"))))
}
table_zone <- function(x, y, three) {
  d <- sqrt(x^2 + y^2)
  ifelse(!three, ifelse(d <= 4, "rim", ifelse(d <= 14, "paint", "mid")),
         ifelse(abs(x) >= CORNER3_MIN_X & d <= 24, "corner3", "break3"))
}

build_career_extract <- function() {
  pb <- player_box %>% filter(!did_not_play, game_id %in% season_games$game_id, team_id %in% team_ids)
  # per player-game (career game log)
  opp_info <- season_games %>%
    transmute(game_id, team_id, date = substr(as.character(game_date), 1, 10), st = as.integer(season_type),
              opp = unname(abbr_by_id[opponent_team_id]), opp_id = opponent_team_id,
              loc = ifelse(team_home_away == "home", "H", ifelse(team_home_away == "away", "A", "N")),
              res = ifelse(team_winner, "W", "L"), score = paste0(as.integer(team_score), "-", as.integer(opponent_team_score)))
  opp_info$opp <- ifelse(is.na(opp_info$opp), "", opp_info$opp)
  games <- pb %>% inner_join(opp_info, by = c("game_id", "team_id")) %>%
    transmute(aid = athlete_id, gid = game_id, tid = team_id, st, date, opp, loc, res, score,
              min = round(na0(minutes)), pts = na0(points), reb = na0(rebounds), oreb = na0(offensive_rebounds),
              dreb = na0(defensive_rebounds), ast = na0(assists), stl = na0(steals), blk = na0(blocks),
              tov = na0(turnovers), pf = na0(fouls), fgm = na0(field_goals_made), fga = na0(field_goals_attempted),
              tpm = na0(three_point_field_goals_made), tpa = na0(three_point_field_goals_attempted),
              ftm = na0(free_throws_made), fta = na0(free_throws_attempted), pm = plus_minus, gs = as.integer(starter),
              name = athlete_display_name, pos = athlete_position_abbreviation) %>%
    arrange(date, gid)
  # team totals per (team, game type), for Usage%
  team_tot <- games %>% group_by(tid, st) %>%
    summarise(fga = sum(fga), fta = sum(fta), tov = sum(tov), min = sum(min), gp = n_distinct(gid), .groups = "drop")
  teams <- lapply(team_ids, function(tid) list(abbr = unname(abbr_by_id[tid]), name = teams_detail[[tid]]$team,
                                               logo = lookup_chr(logo_by_id, tid), primary = lookup_chr(primary_by_id, tid),
                                               conf = lookup_conf(tid) %||% ""))
  names(teams) <- team_ids

  shots <- if (length(career_shots)) bind_rows(career_shots) else NULL
  if (!is.null(shots)) {
    shots$st <- unname(st_by_gid[shots$gid]); shots$st[is.na(shots$st)] <- 2L
    shots <- shots[!is.na(shots$shooter), ]
    shots$cz <- chart_zone(shots$x, shots$y, shots$three)
    shots$z5 <- table_zone(shots$x, shots$y, shots$three)
  }
  # league shot cube: the matched "vs league" baseline for career shot charts
  lg <- NULL
  if (!is.null(shots) && nrow(shots)) {
    lg <- shots %>% mutate(per = pmin(per, 5L), fam = match(fam, SHOT_FAMILIES) - 1L,
                           k = ifelse(!made, 0L, ifelse(is.na(assister), 2L, 1L))) %>%
      group_by(cz, z5, fam, per, cl, st) %>%
      summarise(miss = sum(k == 0L), madeA = sum(k == 1L), madeU = sum(k == 2L), .groups = "drop")
  }

  # on/off and teammate pairs, summed from the lineup segments
  onoff <- NULL; pairs <- NULL
  if (length(pbp_games)) {
    fix5 <- function(v) { v <- as.integer(v); if (length(v) >= 5) v[1:5] else c(v, rep(-1L, 5 - length(v))) }
    parts <- lapply(names(pbp_games), function(gid) {
      g <- pbp_games[[gid]]; sg <- g$segs; if (!length(sg)) return(NULL)
      dur <- vapply(sg, function(z) as.numeric(z[[3]]), numeric(1))
      h5 <- do.call(rbind, lapply(sg, function(z) fix5(z[[5]]))); a5 <- do.call(rbind, lapply(sg, function(z) fix5(z[[6]])))
      hs <- do.call(rbind, lapply(sg, function(z) as.numeric(z[[7]]))); as_ <- do.call(rbind, lapply(sg, function(z) as.numeric(z[[8]])))
      list(tid = c(rep(g$h, length(sg)), rep(g$a, length(sg))), gid = rep(gid, 2 * length(sg)),
           five = rbind(h5, a5), v = rbind(cbind(dur, hs, as_), cbind(dur, as_, hs)))
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    TID <- unlist(lapply(parts, `[[`, "tid")); GID <- unlist(lapply(parts, `[[`, "gid"))
    FIVE <- do.call(rbind, lapply(parts, `[[`, "five")); V <- do.call(rbind, lapply(parts, `[[`, "v"))
    ST <- unname(st_by_gid[GID]); ST[is.na(ST)] <- 2L
    n <- nrow(V)
    # ON per (player, team, game), team total per (team, game), OFF = total - ON
    rep5 <- rep(seq_len(n), 5)
    on_key <- paste(as.vector(FIVE), TID[rep5], GID[rep5], sep = "|")
    keep <- as.vector(FIVE) >= 0
    ON <- rowsum(V[rep5[keep], , drop = FALSE], on_key[keep])
    TOT <- rowsum(V, paste(TID, GID, sep = "|"))
    kp <- do.call(rbind, strsplit(rownames(ON), "|", fixed = TRUE))
    OFF <- TOT[paste(kp[, 2], kp[, 3], sep = "|"), , drop = FALSE] - ON
    st_k <- unname(st_by_gid[kp[, 3]]); st_k[is.na(st_k)] <- 2L
    agg_key <- paste(kp[, 1], kp[, 2], st_k, sep = "|")
    ONs <- rowsum(ON, agg_key); OFFs <- rowsum(OFF, agg_key)[rownames(ONs), , drop = FALSE]
    GPs <- tapply(rep(1L, length(agg_key)), agg_key, sum)[rownames(ONs)]
    ka <- do.call(rbind, strsplit(rownames(ONs), "|", fixed = TRUE))
    onoff <- list(aid = player_ids_ordered[as.integer(ka[, 1]) + 1], tid = ka[, 2], st = as.integer(ka[, 3]),
                  games = as.integer(GPs), on = unname(round(ONs)), off = unname(round(OFFs)))
    # unordered teammate pairs: minutes and team stats together
    pk <- character(0); pv <- list()
    prs <- combn(5, 2)
    PK <- unlist(lapply(seq_len(ncol(prs)), function(j) {
      a <- FIVE[, prs[1, j]]; b <- FIVE[, prs[2, j]]
      lo <- pmin(a, b); hi <- pmax(a, b)
      ifelse(lo < 0, NA_character_, paste(lo, hi, TID, ST, sep = "|"))
    }))
    RI <- rep(seq_len(n), ncol(prs))
    ok <- !is.na(PK)
    PS <- rowsum(V[RI[ok], , drop = FALSE], PK[ok])
    kk <- do.call(rbind, strsplit(rownames(PS), "|", fixed = TRUE))
    pairs <- list(a = player_ids_ordered[as.integer(kk[, 1]) + 1], b = player_ids_ordered[as.integer(kk[, 2]) + 1],
                  tid = kk[, 3], st = as.integer(kk[, 4]), v = unname(round(PS)))
  }
  bio <- games %>% group_by(aid) %>% arrange(desc(date)) %>%
    summarise(name = first(name), pos = first(na.omit(pos)), .groups = "drop")
  bio_extra <- rosters %>% arrange(desc(!is.na(height))) %>% distinct(athlete_id, .keep_all = TRUE)
  bio$height <- as.character(bio_extra$height[match(bio$aid, bio_extra$athlete_id)])
  bio$headshot <- as.character(bio_extra$headshot_href[match(bio$aid, bio_extra$athlete_id)])
  list(season = SEASON, reg_periods = REG_PERIODS, old_line = OLD_THREE_LINE, games = as.data.frame(games),
       team_tot = as.data.frame(team_tot), teams = teams, values = player_values, bio = as.data.frame(bio),
       shots = shots, lg = if (is.null(lg)) NULL else as.data.frame(lg), onoff = onoff, pairs = pairs)
}
career_extract <- tryCatch(build_career_extract(), error = function(e) {
  message("  WARNING: career extract failed for ", SEASON, " (", conditionMessage(e), ") -- careers will skip this season.")
  NULL
})

# ---- predictions (predict_wnba.R, sourced from this folder) -------------------
# Team strength model, season/playoff simulation and the model report card.
# Any failure there is contained: the rest of the dashboard still builds and
# the Predictions tab explains what went wrong.
predict_path <- file.path(OUT_DIR, "predict_wnba.R")
if (CHILD_MODE) {
  # past seasons: no Predictions tab
  predict_payload <- list(ok = FALSE, hidden = TRUE, error = "Predictions are only built for the current season.")
} else if (file.exists(predict_path)) {
  source(predict_path, local = TRUE)
} else {
  message("  NOTE: predict_wnba.R not found next to this script -- Predictions tab will be empty.")
  predict_payload <- list(ok = FALSE, error = "predict_wnba.R was not found next to build_dashboards.R.")
}

# ---- write -------------------------------------------------------------------
message("Serializing page data ...")
to_json <- function(x) {
  s <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  gsub("</", "<\\/", s, fixed = TRUE)   # can never close the <script> tag it lives in
}
page_json <- list(
  summary = to_json(summary_payload),
  detail = to_json(teams_detail),
  values = to_json(player_values),
  bio = to_json(player_bio_list),
  box = to_json(list(cols = BOX_COLS, zones = ZONE_ORDER, info = as.list(box_info), rows = as.list(box_store))),
  predict = to_json(predict_payload),
  pbp = to_json(list(
    players = as.list(player_ids_ordered),
    statCols = STAT_COLS,
    segCols = c("period", "start", "dur", "clutch", "home5", "away5", "homeStats", "awayStats"),
    shotCols = c("shooter", "assister", "x", "y", "flags", "type", "period", "clock", "seg"),
    shotTypes = as.list(shot_type_levels),
    games = pbp_games
  ))
)
message("  box scores stored: ", length(ls(box_store)), " team-games, ", length(ls(box_info)), " players; ",
        "shots: ", if (!is.null(shots)) nrow(shots) else 0)

cache_dir <- file.path(OUT_DIR, ".wnba_cache")
pages_path <- function(yr) file.path(cache_dir, paste0("season_", yr, "_pages.rds"))
career_path <- function(yr) file.path(cache_dir, paste0("season_", yr, "_career.rds"))
stamp_path <- function(yr) file.path(cache_dir, paste0("season_", yr, "_version.txt"))

# ---- child run: cache this finished season and stop ---------------------------
if (CHILD_MODE) {
  saveRDS(page_json, pages_path(SEASON))
  saveRDS(career_extract, career_path(SEASON))
  writeLines(HISTORY_DATA_VERSION, stamp_path(SEASON))   # written last: marks the season complete
  message("Cached season ", SEASON, " (", length(teams_summary), " teams, ", nrow(season_games) / 2, " games).")
  quit(save = "no", status = 0)
}

# ---- past seasons: process any that aren't cached yet (one-time) --------------
season_cached <- function(yr) {
  file.exists(stamp_path(yr)) && file.exists(pages_path(yr)) && file.exists(career_path(yr)) &&
    identical(readLines(stamp_path(yr), warn = FALSE)[1], HISTORY_DATA_VERSION)
}
past_seasons <- if (BUILD_HISTORY) seq.int(HISTORY_FIRST_SEASON, SEASON - 1L) else integer(0)
todo <- past_seasons[!vapply(past_seasons, season_cached, logical(1))]
if (length(todo) && !file.exists(SCRIPT_PATH)) {
  message("  NOTE: can't find build_dashboards.R at ", SCRIPT_PATH, " to build past seasons -- run it with Rscript, ",
          "or setwd() to its folder first. Skipping past seasons this run.")
  todo <- integer(0)
}
if (length(todo)) {
  message("Building ", length(todo), " past season(s) not cached yet: ", paste(todo, collapse = ", "),
          "\n  (one-time: each downloads ~4 MB and takes about a minute; later runs skip them)")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  for (yr in todo) {
    t0 <- Sys.time()
    status <- tryCatch(system2(rscript, c(shQuote(SCRIPT_PATH), paste0("--season=", yr))),
                       error = function(e) { message("  could not start R for ", yr, ": ", conditionMessage(e)); 1L })
    if (!identical(as.integer(status), 0L) || !season_cached(yr)) {
      message("  WARNING: season ", yr, " failed to build -- it's left out of the season toggle and will be retried next run.")
    } else {
      message("  season ", yr, " cached in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s")
    }
  }
}
past_ok <- past_seasons[vapply(past_seasons, season_cached, logical(1))]
all_seasons <- sort(c(past_ok, SEASON), decreasing = TRUE)

# ---- pages --------------------------------------------------------------------
template_path <- file.path(OUT_DIR, "dashboard_template_wnba.html")
if (!file.exists(template_path)) {
  stop("dashboard_template_wnba.html not found next to build_dashboards.R (expected: ", template_path, ").")
}
template_html <- paste(readLines(template_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
PLACEHOLDERS <- c(summary = "__SUMMARY_JSON__", detail = "__DETAIL_JSON__", values = "__PLAYER_VALUES_JSON__",
                  bio = "__PLAYER_BIO_JSON__", box = "__BOX_JSON__", pbp = "__PBP_JSON__", predict = "__PREDICT_JSON__",
                  site = "__SITE_JSON__")
for (ph in PLACEHOLDERS) {
  if (!grepl(ph, template_html, fixed = TRUE)) stop("Template is missing the ", ph, " placeholder -- build script and template must come from the same version.")
}
site_for <- function(yr) {
  cur <- yr == SEASON
  list(season = yr, current = SEASON, isCurrent = cur, seasons = as.list(all_seasons),
       base = if (cur) "wnba_seasons/" else "", root = if (cur) "" else "../",
       regPeriods = if (yr <= 2005) 2L else 4L, oldLine = yr < 2013, history = BUILD_HISTORY)
}
render_page <- function(pj, site) {
  out <- template_html
  pj$site <- to_json(site)
  # the site JSON goes in first, so a data payload can never contain a placeholder
  for (k in c("site", "summary", "detail", "values", "bio", "box", "pbp", "predict")) {
    out <- sub(PLACEHOLDERS[[k]], pj[[k]], out, fixed = TRUE)
  }
  out
}

message("Writing HTML ...")
out_path <- file.path(OUT_DIR, "wnba_dashboards.html")
writeLines(render_page(page_json, site_for(SEASON)), out_path, useBytes = TRUE)
message("Wrote ", out_path, " (", round(file.size(out_path) / 1e6, 1), " MB), ",
        length(teams_summary), " teams, season ", SEASON, ".")

if (length(past_ok)) {
  dir.create(SEASONS_DIR, showWarnings = FALSE, recursive = TRUE)
  # A past page is rewritten only when something it shows changed: the
  # template, the list of seasons in the toggle, or the cached data version.
  stamps_file <- file.path(cache_dir, "page_stamps.rds")
  stamps <- if (file.exists(stamps_file)) tryCatch(readRDS(stamps_file), error = function(e) list()) else list()
  tmpl_md5 <- unname(tools::md5sum(template_path))
  n_written <- 0L
  for (yr in past_ok) {
    site <- site_for(yr)
    stamp <- paste(tmpl_md5, HISTORY_DATA_VERSION, to_json(site), sep = "|")
    dest <- file.path(SEASONS_DIR, paste0("wnba_", yr, ".html"))
    if (file.exists(dest) && identical(stamps[[as.character(yr)]], stamp)) next
    writeLines(render_page(readRDS(pages_path(yr)), site), dest, useBytes = TRUE)
    stamps[[as.character(yr)]] <- stamp
    n_written <- n_written + 1L
  }
  saveRDS(stamps, stamps_file)
  message("Past-season pages: ", length(past_ok), " available, ", n_written, " (re)written.")
}

# ---- career files (players' season-by-season + career data) -------------------
if (BUILD_HISTORY) {
  source_career <- file.path(OUT_DIR, "build_careers.R")
  if (file.exists(source_career)) {
    tryCatch(source(source_career, local = TRUE),
             error = function(e) message("  WARNING: career files not updated (", conditionMessage(e), ")."))
  } else {
    message("  NOTE: build_careers.R not found next to this script -- player Career views will be empty.")
  }
}

if (!is.na(COPY_TO)) {
  tryCatch({
    dir.create(COPY_TO, showWarnings = FALSE, recursive = TRUE)
    file.copy(out_path, file.path(COPY_TO, paste0(format(today, "%m%d%Y"), "_WNBADashboard.html")), overwrite = TRUE)
    # the season pages and career files it links to: copy what's new or changed
    if (dir.exists(SEASONS_DIR)) {
      src <- list.files(SEASONS_DIR, recursive = TRUE, all.files = FALSE)
      dst_root <- file.path(COPY_TO, basename(SEASONS_DIR))
      n_cp <- 0L
      for (f in src) {
        a <- file.path(SEASONS_DIR, f); b <- file.path(dst_root, f)
        if (file.exists(b) && file.size(a) == file.size(b) && file.mtime(b) >= file.mtime(a)) next
        dir.create(dirname(b), showWarnings = FALSE, recursive = TRUE)
        if (file.copy(a, b, overwrite = TRUE)) n_cp <- n_cp + 1L
      }
      message("  copied ", n_cp, " changed season/career file(s)")
    }
    message("Copied to ", COPY_TO)
  }, error = function(e) message("WARNING: could not copy to ", COPY_TO, ": ", conditionMessage(e)))
}
message("Done.")
