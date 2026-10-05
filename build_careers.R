# =============================================================================
# build_careers.R  (WNBA edition, v4) -- sourced by build_dashboards.R
#
# Stitches every season's cached career extract (.wnba_cache/
# season_<year>_career.rds, built once per finished season) plus the current
# season's into the files the player page's Career view loads on demand:
#
#   wnba_seasons/career_index.js   every player: name + seasons/teams/minutes
#                                  (season chips, cross-season search, toggle)
#   wnba_seasons/league_shots.js   league shot counts by zone x type x period x
#                                  clutch x game type, per season -- the
#                                  "vs league" baseline for career shot charts
#   wnba_seasons/players/<id>.js   one player's career: game log, every shot,
#                                  assists, on/off and teammate pairings
#
# Daily runs rewrite the index, the league file and the files of players who
# appear this season. Everyone else's file is written once and left alone,
# unless a season is added or CAREER_FILE_VERSION changes.
# =============================================================================

local({
  CAREER_FILE_VERSION <- 1L
  t0 <- Sys.time()
  message("Assembling career files ...")
  players_dir <- file.path(SEASONS_DIR, "players")
  dir.create(players_dir, showWarnings = FALSE, recursive = TRUE)

  ex <- list()
  for (yr in past_ok) ex[[as.character(yr)]] <- tryCatch(readRDS(career_path(yr)), error = function(e) NULL)
  ex[[as.character(SEASON)]] <- career_extract
  ex <- ex[!vapply(ex, is.null, logical(1))]
  if (!length(ex)) { message("  no career data available"); return(invisible()) }
  yrs <- as.integer(names(ex))

  with_season <- function(key) bind_rows(lapply(names(ex), function(y) {
    d <- ex[[y]][[key]]
    if (is.null(d) || !NROW(d)) return(NULL)
    d <- as.data.frame(d, stringsAsFactors = FALSE); d$season <- as.integer(y); d
  }))
  games <- with_season("games") %>% arrange(date, gid)
  games$pm <- suppressWarnings(as.numeric(games$pm))
  shots <- with_season("shots")
  team_tot <- with_season("team_tot")
  bio <- with_season("bio")

  # on/off and pairs carry matrices: flatten per season
  mat_rows <- function(key, idcols) {
    out <- lapply(names(ex), function(y) {
      d <- ex[[y]][[key]]; if (is.null(d) || !length(d[[idcols[1]]])) return(NULL)
      df <- as.data.frame(d[idcols], stringsAsFactors = FALSE); df$season <- as.integer(y)
      list(df = df, m = d[setdiff(names(d), idcols)])
    })
    out[!vapply(out, is.null, logical(1))]
  }
  oo_parts <- mat_rows("onoff", c("aid", "tid", "st", "games"))
  onoff_df <- bind_rows(lapply(oo_parts, `[[`, "df"))
  onoff_on <- do.call(rbind, lapply(oo_parts, function(p) p$m$on))
  onoff_off <- do.call(rbind, lapply(oo_parts, function(p) p$m$off))
  pr_parts <- mat_rows("pairs", c("a", "b", "tid", "st"))
  pairs_df <- bind_rows(lapply(pr_parts, `[[`, "df"))
  pairs_v <- do.call(rbind, lapply(pr_parts, function(p) p$m$v))

  # latest name / position / height / headshot per player
  latest <- games %>% arrange(desc(date)) %>% distinct(aid, .keep_all = TRUE)
  name_of <- setNames(latest$name, latest$aid)
  pos_of <- setNames(latest$pos, latest$aid)
  bio <- bio %>% arrange(desc(season))
  ht_of <- with(bio[!is.na(bio$height) & nzchar(bio$height), ], setNames(height, aid))
  ht_of <- ht_of[!duplicated(names(ht_of))]
  hs_of <- with(bio[!is.na(bio$headshot), ], setNames(headshot, aid))
  hs_of <- hs_of[!duplicated(names(hs_of))]

  jsn <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, dataframe = "values"))
  write_js <- function(path, prefix, obj) {
    tmp <- paste0(path, ".part")
    writeLines(paste0(prefix, jsn(obj), ";"), tmp, useBytes = TRUE)
    file.copy(tmp, path, overwrite = TRUE); file.remove(tmp)
  }

  # ---- career_index.js ------------------------------------------------------
  seas <- games %>% group_by(aid, season, tid) %>%
    summarise(min = sum(min), gp = n_distinct(gid), .groups = "drop") %>% arrange(aid, season, desc(min))
  team_abbr <- function(y, tid) { t <- ex[[as.character(y)]]$teams[[tid]]; if (is.null(t)) "" else (t$abbr %||% "") }
  seas$abbr <- mapply(team_abbr, seas$season, seas$tid)
  idx_players <- lapply(split(seas, seas$aid), function(d)
    list(name_of[[d$aid[1]]] %||% "", unname(lapply(seq_len(nrow(d)), function(i) list(d$season[i], d$tid[i], d$abbr[i], d$min[i], d$gp[i])))))
  write_js(file.path(SEASONS_DIR, "career_index.js"), "window.WNBA_CAREER_INDEX=",
           list(v = CAREER_FILE_VERSION, built = format(Sys.time(), "%Y-%m-%d %H:%M"), seasons = sort(yrs, decreasing = TRUE),
                players = idx_players))

  # ---- league_shots.js --------------------------------------------------------
  CZ <- c("ra", "paint", "midLB", "midLW", "midC", "midRW", "midRB", "c3L", "ab3L", "ab3C", "ab3R", "c3R")
  Z5 <- c("rim", "paint", "mid", "corner3", "break3")
  lg <- lapply(names(ex), function(y) {
    d <- ex[[y]]$lg; if (is.null(d) || !nrow(d)) return(list())
    unname(as.matrix(data.frame(match(d$cz, CZ) - 1L, match(d$z5, Z5) - 1L, d$fam, d$per, d$cl, d$st, d$miss, d$madeA, d$madeU)))
  })
  names(lg) <- names(ex)
  write_js(file.path(SEASONS_DIR, "league_shots.js"), "window.WNBA_LEAGUE_SHOTS=",
           list(cz = CZ, z5 = Z5, fams = SHOT_FAMILIES, seasons = lg))

  # ---- players/<id>.js ------------------------------------------------------
  stamp_file <- file.path(cache_dir, "career_stamp.rds")
  # rewrite every player when the file format, the cached data version or the
  # set of seasons changes; otherwise only this season's players
  stamp <- list(v = CAREER_FILE_VERSION, data = HISTORY_DATA_VERSION, seasons = sort(yrs))
  full <- !file.exists(stamp_file) || !identical(tryCatch(readRDS(stamp_file), error = function(e) NULL), stamp)
  safe_id <- function(a) gsub("[^0-9A-Za-z_-]", "", a)
  all_ids <- unique(games$aid)
  cur_ids <- unique(games$aid[games$season == SEASON])
  missing_ids <- all_ids[!file.exists(file.path(players_dir, paste0(safe_id(all_ids), ".js")))]
  todo <- if (full) all_ids else unique(c(cur_ids, missing_ids))
  # drop files for ids no longer in the data
  stale <- setdiff(sub("\\.js$", "", list.files(players_dir, pattern = "\\.js$")), safe_id(all_ids))
  if (length(stale)) file.remove(file.path(players_dir, paste0(stale, ".js")))

  g_split <- split(games, games$aid)
  s_by_shooter <- if (!is.null(shots) && nrow(shots)) split(seq_len(nrow(shots)), shots$shooter) else list()
  s_by_assister <- if (!is.null(shots) && nrow(shots)) { k <- which(!is.na(shots$assister)); split(k, shots$assister[k]) } else list()
  oo_by <- if (nrow(onoff_df)) split(seq_len(nrow(onoff_df)), onoff_df$aid) else list()
  pr_a <- if (nrow(pairs_df)) split(seq_len(nrow(pairs_df)), pairs_df$a) else list()
  pr_b <- if (nrow(pairs_df)) split(seq_len(nrow(pairs_df)), pairs_df$b) else list()
  fam_idx <- function(f) match(f, SHOT_FAMILIES) - 1L

  for (aid in todo) {
    g <- g_split[[aid]]; if (is.null(g) || !nrow(g)) next
    gi_of <- setNames(seq_len(nrow(g)) - 1L, g$gid)
    others <- character(0)
    # her shots: [game index, x, y, flags, family, period, assister]
    si <- s_by_shooter[[aid]]
    sh <- NULL
    if (length(si)) {
      d <- shots[si, ]; d <- d[d$gid %in% g$gid, ]
      if (nrow(d)) {
        sh <- data.frame(gi = unname(gi_of[d$gid]), x = d$x, y = d$y,
                         f = as.integer(d$made) + 2L * as.integer(d$three) + 4L * as.integer(d$blk) + 8L * as.integer(d$cl),
                         fam = fam_idx(d$fam), per = d$per, a = d$assister, stringsAsFactors = FALSE)
        others <- c(others, na.omit(d$assister))
      }
    }
    # her assists: [game index, scorer, points]
    ai <- s_by_assister[[aid]]
    at <- NULL
    if (length(ai)) {
      d <- shots[ai, ]; d <- d[d$gid %in% g$gid, ]
      if (nrow(d)) { at <- data.frame(gi = unname(gi_of[d$gid]), p = d$shooter, pts = ifelse(d$three, 3L, 2L), stringsAsFactors = FALSE); others <- c(others, d$shooter) }
    }
    # on/off: [season, team, game type, games, on[29], off[29]]
    oi <- oo_by[[aid]]
    oo <- if (length(oi)) lapply(oi, function(i) list(onoff_df$season[i], onoff_df$tid[i], onoff_df$st[i], onoff_df$games[i],
                                                     as.numeric(onoff_on[i, ]), as.numeric(onoff_off[i, ]))) else list()
    # teammate pairs: [season, team, game type, partner, together[29]]
    pi_ <- c(pr_a[[aid]], pr_b[[aid]])
    prs <- if (length(pi_)) lapply(pi_, function(i) {
      partner <- if (pairs_df$a[i] == aid) pairs_df$b[i] else pairs_df$a[i]
      list(pairs_df$season[i], pairs_df$tid[i], pairs_df$st[i], partner, as.numeric(pairs_v[i, ]))
    }) else list()
    if (length(pi_)) others <- c(others, vapply(prs, function(z) z[[4]], character(1)))
    others <- unique(others)
    # teams, team totals and value stats for every season/team she played for
    st_keys <- unique(paste(g$season, g$tid, sep = "|"))
    teams <- list(); vals <- list()
    for (k in st_keys) {
      y <- sub("\\|.*", "", k); tid <- sub(".*\\|", "", k)
      t <- ex[[y]]$teams[[tid]]
      teams[[k]] <- if (is.null(t)) list("", "", NULL, NULL) else list(t$abbr, t$name, t$logo, t$primary)
      v <- ex[[y]]$values[[paste0(tid, ":", aid)]]
      if (!is.null(v)) vals[[k]] <- v
    }
    tt_rows <- team_tot[paste(team_tot$season, team_tot$tid, sep = "|") %in% st_keys, ]
    tt <- setNames(lapply(seq_len(nrow(tt_rows)), function(i) c(tt_rows$fga[i], tt_rows$fta[i], tt_rows$tov[i], tt_rows$min[i])),
                   paste(tt_rows$season, tt_rows$tid, tt_rows$st, sep = "|"))
    gl <- g[, c("season", "st", "date", "tid", "opp", "loc", "res", "score", "min", "pts", "reb", "oreb", "ast", "stl", "blk",
                "tov", "pf", "fgm", "fga", "tpm", "tpa", "ftm", "fta", "pm", "gs", "gid")]
    obj <- list(v = CAREER_FILE_VERSION, aid = aid, name = name_of[[aid]] %||% "", pos = pos_of[[aid]] %||% "",
                ht = if (aid %in% names(ht_of)) ht_of[[aid]] else NULL, headshot = if (aid %in% names(hs_of)) hs_of[[aid]] else NULL,
                gcols = names(gl), games = gl, teams = teams, tt = tt, vals = vals,
                names = as.list(setNames(unname(name_of[others]), others)),
                shots = if (is.null(sh)) list() else sh, astTo = if (is.null(at)) list() else at,
                onoff = oo, pairs = prs)
    write_js(file.path(players_dir, paste0(safe_id(aid), ".js")),
             paste0("(window.WNBA_PLAYER_DATA=window.WNBA_PLAYER_DATA||{})[", jsn(aid), "]="), obj)
  }
  saveRDS(stamp, stamp_file)
  message("  career files: ", length(all_ids), " players over ", length(yrs), " seasons; ", length(todo),
          " player file(s) written (", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s)")
})
