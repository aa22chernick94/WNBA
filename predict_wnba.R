# =============================================================================
# ASPN WNBA -- Prediction module (phase 1)
# =============================================================================
# Sourced by build_dashboards.R after the season's box scores are loaded.
# Produces `predict_payload`, written into the page as __PREDICT_JSON__.
#
# What it does
#   1. Team strength model. Every team gets an offensive rating (points it
#      adds to an average team's score) and a defensive rating (points it
#      lets an average opponent add). They are fit by weighted ridge
#      regression on every game's score:
#          points = league_avg + off[team] + def[opponent] +/- home_court/2
#      Recent games weigh more (exponential decay, half-life HL days), and
#      each rating is pulled toward a preseason prior: last season's final
#      rating times CARRY (regression to the mean). New franchises start at
#      EXPANSION_NET. The ridge penalty LAMBDA is how many "games' worth" of
#      evidence the prior is worth.
#   2. Calibration. HL, LAMBDA and CARRY are chosen by a walk-forward
#      backtest over every past season sportsdataverse publishes (2007 on):
#      before each game day, fit on only the games already played, predict
#      that day, move on. The combination with the lowest log loss wins, and
#      the spread of real margins around predictions sets SIGMA_M, which
#      turns a projected margin into a win probability. The result is cached
#      in .wnba_cache and only refit when a new season is added
#      (or set PREDICT_RECALIBRATE <- TRUE).
#   3. Simulation. The rest of the regular season, the seeding (with an
#      approximation of the official tiebreakers) and the full playoff
#      bracket are simulated N_SIMS times. Each simulation first redraws every
#      team's true strength from the model's uncertainty, so a team that is
#      only probably good isn't treated as certainly good. Playoff games
#      already played are respected, series by series.
#   4. Prediction log. Every build writes its game predictions to
#      predictions_log.csv next to the script; once those games are final
#      they are graded, so the Report Card shows live accuracy, not just the
#      backtest.
# =============================================================================

PREDICT_VERSION     <- "p2.0"
PREDICT_RECALIBRATE <- FALSE      # TRUE forces the backtest to rerun this build
N_SIMS              <- 10000
EXPANSION_NET       <- -4         # prior net rating (pts/game vs avg) for a first-year franchise
FIRST_HIST_SEASON   <- 2006       # 2006 is warm-up only (no prior), scoring starts 2007
BUBBLE_SEASONS      <- c(2020)    # every game treated as neutral-site
GRID <- list(hl = c(21, 45, 90, 180, 10000), lambda = c(2, 4, 8, 16), carry = c(0.25, 0.45, 0.65))
set.seed(20260924)

# Playoff format -- matches the 2026 schedule release (best-of-3 / 5 / 7).
# `home` lists, game by game, whether the HIGHER seed hosts. Edit if the
# league changes it; everything downstream reads from here.
PLAYOFF_SEEDS  <- 8
PLAYOFF_ROUNDS <- list(
  list(name = "First Round", best_of = 3, home = c(TRUE, FALSE, TRUE)),
  list(name = "Semifinals",  best_of = 5, home = c(TRUE, TRUE, FALSE, FALSE, TRUE)),
  list(name = "Finals",      best_of = 7, home = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))
)
# Fixed bracket (no reseeding): 1v8, 4v5, 3v6, 2v7; winners of the first two
# pairs meet, and of the last two.
R1_PAIRS <- list(c(1, 8), c(4, 5), c(3, 6), c(2, 7))
# If the official seeds ever differ from this script's tiebreaker approximation,
# list them here by abbreviation, seed 1 first, e.g. c("MIN","LV","NY",...).
# Used only once the regular season is complete. Leave NULL otherwise.
PLAYOFF_SEED_OVERRIDE <- NULL
# Playoff rotations: in playoff games, stars play more, benches less and the
# deep bench not at all (by rotation spot, measured from past playoffs, DNPs
# included; see pm_rotation_load). TRUE also lets that shift move
# the team's strength (a team whose top players are much better than its bench
# gains the most). Player minutes use it either way.
PLAYOFF_ROTATION_ADJ <- TRUE

# ---- data ------------------------------------------------------------------
pm_hist_file <- function(yr) {
  fn <- paste0("team_box_", yr, ".parquet")
  dest <- file.path(OUT_DIR, ".wnba_cache", fn)
  # finished seasons never change: download once, keep forever
  if (release_file_ok(dest) && yr < SEASON) return(dest)
  download_cached(paste0(GH_RELEASES, "/espn_wnba_team_boxscores/", fn), fn, required = FALSE)
}

# one row per game: gid, date, season, st, home, away, hs, as, neutral
pm_games_from_box <- function(tb, yr) {
  tb <- as.data.frame(tb)
  tb$team_id <- as.character(tb$team_id); tb$opponent_team_id <- as.character(tb$opponent_team_id)
  tb$game_id <- as.character(tb$game_id)
  tb <- tb[!is.na(tb$team_score) & !is.na(tb$opponent_team_score), ]
  # All-Star "teams" play once or twice; real franchises play 30+
  cnt <- table(tb$team_id); real <- names(cnt)[cnt >= 10]
  tb <- tb[tb$team_id %in% real & tb$opponent_team_id %in% real, ]
  h <- tb[tb$team_home_away == "home", ]
  h <- h[!duplicated(h$game_id), ]
  data.frame(gid = h$game_id, date = as.Date(h$game_date), season = yr,
             st = as.integer(h$season_type), home = h$team_id, away = h$opponent_team_id,
             hs = as.numeric(h$team_score), as = as.numeric(h$opponent_team_score),
             neutral = rep(yr %in% BUBBLE_SEASONS, nrow(h)), stringsAsFactors = FALSE)
}

# ---- the model ---------------------------------------------------------------
# theta = (mu, off[1..n], def[1..n]); prior list(mu, off, def) named by team.
pm_fit <- function(g, teams, prior, asof, par, H) {
  n <- length(teams); p <- 2 * n + 1
  A <- diag(c(1, rep(par$lambda, 2 * n)))        # prior precision (mu gets 1 game's worth)
  th0 <- c(prior$mu, prior$off[teams], prior$def[teams])
  rhs <- A %*% th0
  if (nrow(g)) {
    hi <- match(g$home, teams); ai <- match(g$away, teams)
    age <- as.numeric(asof - g$date)
    w <- 0.5^(age / par$hl)
    hc <- ifelse(g$neutral, 0, H / 2)
    m <- nrow(g)
    X <- matrix(0, 2 * m, p)
    X[, 1] <- 1
    r1 <- seq_len(m); r2 <- m + r1
    X[cbind(r1, 1 + hi)] <- 1; X[cbind(r1, 1 + n + ai)] <- 1   # home scoring: home off, away def
    X[cbind(r2, 1 + ai)] <- 1; X[cbind(r2, 1 + n + hi)] <- 1   # away scoring: away off, home def
    y <- c(g$hs - hc, g$as + hc)
    ww <- c(w, w)
    Xw <- X * ww
    A <- A + crossprod(Xw, X)
    rhs <- rhs + crossprod(Xw, y)
  }
  th <- solve(A, rhs)[, 1]
  list(teams = teams, mu = th[1], off = setNames(th[1 + seq_len(n)], teams),
       def = setNames(th[1 + n + seq_len(n)], teams), A = A)
}

pm_predict <- function(fit, home, away, neutral, H) {
  hc <- ifelse(neutral, 0, H)
  sh <- fit$mu + fit$off[home] + fit$def[away] + hc / 2
  sa <- fit$mu + fit$off[away] + fit$def[home] - hc / 2
  list(margin = unname(sh - sa), total = unname(sh + sa), hs = unname(sh), as = unname(sa))
}

pm_next_prior <- function(fit, next_teams, par) {
  off <- setNames(rep(EXPANSION_NET / 2, length(next_teams)), next_teams)
  def <- setNames(rep(-EXPANSION_NET / 2, length(next_teams)), next_teams)
  if (!is.null(fit)) {
    keep <- intersect(next_teams, fit$teams)
    off[keep] <- par$carry * fit$off[keep]; def[keep] <- par$carry * fit$def[keep]
    mu <- fit$mu
  } else mu <- 80
  list(mu = unname(mu), off = off, def = def)
}

# walk-forward backtest for one parameter set; returns per-game predictions
# and the last season's final fit (the prior source for the current season)
pm_backtest <- function(hist, par, H, score_from) {
  out <- vector("list", length(hist)); prev <- NULL
  for (k in seq_along(hist)) {
    g <- hist[[k]]; yr <- g$season[1]
    teams <- sort(unique(c(g$home, g$away)))
    prior <- pm_next_prior(prev, teams, par)
    rows <- NULL
    if (yr >= score_from) {
      days <- sort(unique(g$date))
      rows <- vector("list", length(days))
      for (d in seq_along(days)) {
        today_g <- g[g$date == days[d], ]
        fit <- pm_fit(g[g$date < days[d], ], teams, prior, days[d], par, H)
        pr <- pm_predict(fit, today_g$home, today_g$away, today_g$neutral, H)
        rows[[d]] <- data.frame(season = yr, st = today_g$st, date = today_g$date, gid = today_g$gid, home = today_g$home, away = today_g$away,
                                pm = pr$margin, pt = pr$total, pn = pr$margin - ifelse(today_g$neutral, 0, H),
                                hloc = as.numeric(!today_g$neutral),
                                am = today_g$hs - today_g$as, at = today_g$hs + today_g$as)
      }
      rows <- do.call(rbind, rows)
    }
    out[[k]] <- rows
    prev <- pm_fit(g, teams, prior, max(g$date) + 1, par, H)
  }
  list(pred = do.call(rbind, out), last_fit = prev)
}

pm_logloss <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
pm_fit_sigma <- function(pm, won) optimize(function(s) pm_logloss(pnorm(pm / s), won), c(4, 25))$minimum

# ---- playoff rotations -------------------------------------------------------
# Measured on every playoff game since ROT_FIRST_SEASON, from the same numbers
# the projection has before tip-off: each available player's expected minutes
# as of that date (pl_snapshot, once per minutes half-life in PL_TUNE$min_hl),
# ranked within her team. "Available" = not injured in that game and not out
# by default (or she played anyway, as when a player comes back and you mark
# her in). For rotation spot r the multiplier is
#   actual playoff minutes (a DNP counts as 0) / expected minutes,
# pooled over every team-game. Counting DNPs matters: playoff benches often
# don't get in at all, so averaging only the games they played overstates them.
# Then the rotation cut: after the multipliers, anyone projected under CUT
# minutes is out of the playoff rotation and her minutes go to the rest (about
# 9 players get in per playoff game, 7 for 10+ minutes). CUT is picked by
# minutes miss with each season held out in turn. The regular-season pull
# toward the team average is not used in playoff games; these multipliers
# replace it.
ROT_FIRST_SEASON <- 2012
ROT_CUTS <- c(0, 2, 3, 4, 5)
ROT_SPOTS <- 12
pl_norm <- function(m, tot) { for (it in 1:6) { s <- sum(m); if (s <= 0) break; m <- pmin(m * tot / s, 40) }; m }
# m: expected minutes already scaled to the team total (0 for players out)
pl_rot_apply <- function(m, rot, tot) {
  x <- as.numeric(unlist(rot$x)); if (!length(x)) return(m)
  r <- rank(-m, ties.method = "first")
  m <- pl_norm(m * x[pmin(r, length(x))], tot)
  cut <- as.numeric(rot$cut %||% 0)
  if (cut > 0) for (it in 1:3) m <- pl_norm(ifelse(m < cut, 0, m), tot)
  m
}
pm_rotation_rows <- function(team_hl) {
  out <- list()
  for (yr in ROT_FIRST_SEASON:(SEASON - 1)) {
    D <- tryCatch(pl_load_season(yr), error = function(e) NULL); if (is.null(D)) next
    po <- D$P[D$P$st == 3, ]; if (!nrow(po)) next
    for (d in as.list(sort(unique(po$date)))) {
      sn <- lapply(PL_TUNE$min_hl, function(h) pl_snapshot(D, d, list(rate_hl = 20, k = 60, min_hl = h), team_hl)$pl)
      if (is.null(sn[[1]])) next
      pd <- po[po$date == d, ]
      for (gt in unique(paste(pd$gid, pd$tid))) {
        A <- pd[paste(pd$gid, pd$tid) == gt, ]
        tp <- sn[[1]][sn[[1]]$tid == A$tid[1] & sn[[1]]$aid %in% A$aid[!A$inj], ]
        mA <- A$min[match(tp$aid, A$aid)]
        tp <- tp[!tp$defaultOut | mA > 0, ]; if (nrow(tp) < 5) next
        r <- data.frame(season = yr, gt = gt, aid = tp$aid, min_a = A$min[match(tp$aid, A$aid)], stringsAsFactors = FALSE)
        for (j in seq_along(PL_TUNE$min_hl)) r[[paste0("e", j)]] <- sn[[j]]$expMin[match(tp$aid, sn[[j]]$aid)] %|na|% 0
        out[[length(out) + 1]] <- r
      }
    }
  }
  if (!length(out)) stop("no past playoff games available")
  do.call(rbind, out)
}
pm_rotation_fit <- function(R) {
  G <- split(R, R$gt)
  seas <- vapply(G, function(g) g$season[1], numeric(1))
  one_hl <- function(col) {
    E <- lapply(G, function(g) pl_norm(g[[col]], 200))
    rk <- lapply(E, function(e) pmin(rank(-e, ties.method = "first"), ROT_SPOTS))
    fit_x <- function(keep) {
      num <- numeric(ROT_SPOTS); den <- numeric(ROT_SPOTS); cnt <- numeric(ROT_SPOTS)
      for (i in which(keep)) {
        num <- num + tabulate_w(rk[[i]], G[[i]]$min_a); den <- den + tabulate_w(rk[[i]], E[[i]]); cnt <- cnt + tabulate(rk[[i]], ROT_SPOTS)
      }
      x <- ifelse(cnt >= 20 & den > 0, num / den, 1)
      round(x, 3)
    }
    miss <- vapply(ROT_CUTS, function(cut) {
      err <- numeric(0)
      for (s in unique(seas)) {
        x <- fit_x(seas != s)
        for (i in which(seas == s)) err <- c(err, abs(pl_rot_apply(E[[i]], list(x = x, cut = cut), 200) - G[[i]]$min_a))
      }
      mean(err)
    }, numeric(1))
    cut <- ROT_CUTS[which.min(miss)]; x <- fit_x(rep(TRUE, length(G)))
    miss0 <- mean(abs(unlist(E) - unlist(lapply(G, function(g) g$min_a))))
    P <- lapply(seq_along(G), function(i) pl_rot_apply(E[[i]], list(x = x, cut = cut), 200))
    list(x = as.list(x), cut = cut, mae = min(miss), maeNoCut = miss[ROT_CUTS == 0], maePlain = miss0,
         played = mean(vapply(P, function(p) sum(p > 0), numeric(1))),
         playedAct = mean(vapply(G, function(g) sum(g$min_a > 0), numeric(1))))
  }
  by <- lapply(seq_along(PL_TUNE$min_hl), function(j) one_hl(paste0("e", j))); names(by) <- as.character(PL_TUNE$min_hl)
  list(byHl = by, n = length(G), seasons = paste0(min(seas), "-", max(seas)))
}
tabulate_w <- function(i, w) vapply(seq_len(ROT_SPOTS), function(k) sum(w[i == k]), numeric(1))
pm_rotation_load <- function(cal) {
  f <- file.path(OUT_DIR, ".wnba_cache", paste0("predict_rotation_", SEASON, ".json"))
  key <- paste(PREDICT_VERSION, "rot2", ROT_FIRST_SEASON, paste(PL_TUNE$min_hl, collapse = ","),
               paste(ROT_CUTS, collapse = ","), ROT_SPOTS, cal$par$hl, DEFAULT_OUT_GAMES)
  if (file.exists(f) && !isTRUE(PREDICT_RECALIBRATE)) {
    old <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(old) && identical(old$key, key)) { message("  using cached playoff rotations from ", old$made); return(old) }
  }
  message("  measuring playoff rotations on past playoff games (one-time; cached afterwards) ...")
  t0 <- Sys.time()
  r <- pm_rotation_fit(pm_rotation_rows(cal$par$hl))
  for (h in names(r$byHl)) {
    b <- r$byHl[[h]]
    message(sprintf("    minutes half-life %s: by rotation spot %s; rotation cut %g min; minutes miss %.2f (no cut %.2f, no playoff adjustment %.2f); players who get in %.1f (actual %.1f)",
                    h, paste(sprintf("%.2f", unlist(b$x)), collapse = " "), b$cut, b$mae, b$maeNoCut, b$maePlain, b$played, b$playedAct))
  }
  message(sprintf("    %d team-games, %.0fs", r$n, as.numeric(Sys.time() - t0, units = "secs")))
  r$key <- key; r$made <- format(Sys.time(), "%Y-%m-%d %H:%M")
  writeLines(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = NA), f)
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}
pm_rotation_for <- function(RT, h) {
  b <- if (is.null(RT)) NULL else RT$byHl[[as.character(h)]]
  if (is.null(b)) NULL else list(x = as.numeric(unlist(b$x)), cut = as.numeric(b$cut %||% 0))
}

# ---- calibration (cached) -------------------------------------------------
pm_calibrate <- function() {
  yrs <- FIRST_HIST_SEASON:(SEASON - 1)
  message("  loading ", length(yrs), " past seasons for calibration ...")
  hist <- list()
  for (yr in yrs) {
    f <- tryCatch(pm_hist_file(yr), error = function(e) NULL)
    if (is.null(f)) { message("    (", yr, " unavailable, skipped)"); next }
    g <- pm_games_from_box(nanoparquet::read_parquet(f), yr)
    if (nrow(g) > 50) hist[[as.character(yr)]] <- g[order(g$date, g$gid), ]
  }
  if (length(hist) < 5) stop("too few past seasons available to calibrate")
  allg <- do.call(rbind, hist)
  recent <- allg[!allg$neutral & allg$season >= SEASON - 10, ]
  H <- mean(recent$hs - recent$as)
  message(sprintf("  home court (last 10 seasons): %+.2f pts", H))

  grid <- expand.grid(hl = GRID$hl, lambda = GRID$lambda, carry = GRID$carry)
  message("  backtesting ", nrow(grid), " parameter sets (one-time; cached afterwards) ...")
  score_from <- as.integer(names(hist)[2])
  res <- vector("list", nrow(grid)); t0 <- Sys.time()
  for (i in seq_len(nrow(grid))) {
    par <- as.list(grid[i, ])
    bt <- pm_backtest(hist, par, H, score_from)
    won <- as.numeric(bt$pred$am > 0)
    s <- pm_fit_sigma(bt$pred$pm, won)
    res[[i]] <- list(par = par, sigma = s, ll = pm_logloss(pnorm(bt$pred$pm / s), won), bt = bt)
    if (i %% 10 == 0) message(sprintf("    %d/%d (%.0fs)", i, nrow(grid), as.numeric(Sys.time() - t0, units = "secs")))
  }
  best <- res[[which.min(vapply(res, `[[`, numeric(1), "ll"))]]
  pred <- best$bt$pred; won <- as.numeric(pred$am > 0); p <- pnorm(pred$pm / best$sigma)
  sigma_t <- sd(pred$at - pred$pt)

  # ---- playoff mode: does the regular-season model need adjusting for playoff games? ----
  # Two parameters, fit only on playoff games: K stretches the neutral-court
  # margin (K > 1 = the better team wins more often than in the regular
  # season) and HPO is the playoff home-court edge. TBIAS is the average miss
  # on playoff totals (playoff games tend to be slower). Checked by
  # leave-one-season-out: each season's playoffs are predicted with values fit
  # on the OTHER seasons. If that doesn't beat the plain model, playoff mode
  # falls back to the regular-season settings.
  po <- pred[pred$st == 3, ]
  fit_po <- function(d) {
    f <- function(v) pm_logloss(pnorm((v[1] * d$pn + v[2] * d$hloc) / best$sigma), as.numeric(d$am > 0))
    o <- optim(c(1, H), f, method = "L-BFGS-B", lower = c(0.5, -4), upper = c(2, 8))
    list(k = o$par[1], h = o$par[2], tb = mean(d$at - d$pt))
  }
  po_cv <- NULL; po_par <- list(k = 1, h = H, tb = 0, used = FALSE)
  if (nrow(po) >= 100) {
    cv <- do.call(rbind, lapply(unique(po$season), function(yr) {
      tr <- po[po$season != yr, ]; te <- po[po$season == yr, ]
      f <- fit_po(tr); y <- as.numeric(te$am > 0)
      data.frame(n = nrow(te),
                 ll_base = pm_logloss(pnorm(te$pm / best$sigma), y) * nrow(te),
                 ll_po = pm_logloss(pnorm((f$k * te$pn + f$h * te$hloc) / best$sigma), y) * nrow(te),
                 tot_base = sum(abs(te$at - te$pt)), tot_po = sum(abs(te$at - te$pt - f$tb)))
    }))
    all_f <- fit_po(po)
    po_cv <- list(n = sum(cv$n), ll_base = sum(cv$ll_base) / sum(cv$n), ll_po = sum(cv$ll_po) / sum(cv$n),
                  tot_base = sum(cv$tot_base) / sum(cv$n), tot_po = sum(cv$tot_po) / sum(cv$n),
                  k = all_f$k, h = all_f$h, tb = all_f$tb)
    use_margin <- po_cv$ll_po < po_cv$ll_base
    use_total <- po_cv$tot_po < po_cv$tot_base
    po_par <- list(k = if (use_margin) all_f$k else 1, h = if (use_margin) all_f$h else H,
                   tb = if (use_total) all_f$tb else 0, used = use_margin, usedTotal = use_total)
    message(sprintf("  playoff mode: K %.2f, home %+.2f, total %+.1f (held-out log loss %.4f vs %.4f plain; %s)",
                    all_f$k, all_f$h, all_f$tb, po_cv$ll_po, po_cv$ll_base, if (use_margin) "used" else "not used"))
  }
  sigma_r <- sd(pred$am - pred$pm)   # spread of real margins around the projection (for ranges, not win%)

  # ---- report card ----
  home_rate <- mean(won[pred$season %in% (SEASON - 10):(SEASON - 1)])
  fav_p <- ifelse(p >= 0.5, p, 1 - p); fav_won <- ifelse(p >= 0.5, won, 1 - won)
  bins <- cut(fav_p, breaks = seq(0.5, 1, by = 0.05), include.lowest = TRUE)
  calib <- lapply(split(seq_along(fav_p), bins), function(ix)
    if (length(ix)) list(p = mean(fav_p[ix]), actual = mean(fav_won[ix]), n = length(ix)) else NULL)
  calib <- Filter(Negate(is.null), unname(calib))
  by_season <- lapply(split(seq_along(p), pred$season), function(ix) list(
    season = pred$season[ix[1]], n = length(ix), ll = pm_logloss(p[ix], won[ix]),
    brier = mean((p[ix] - won[ix])^2), acc = mean((p[ix] >= 0.5) == won[ix]),
    mae = mean(abs(pred$am[ix] - pred$pm[ix]))))
  phase <- ifelse(pred$st == 3, "Playoffs",
                  ifelse(ave(as.numeric(pred$date), pred$season, FUN = function(d) rank(d, ties.method = "min") / length(d)) <= 0.2,
                         "First fifth of season", "Rest of regular season"))
  by_phase <- lapply(split(seq_along(p), phase), function(ix) list(
    phase = phase[ix[1]], n = length(ix), ll = pm_logloss(p[ix], won[ix]), acc = mean((p[ix] >= 0.5) == won[ix]),
    mae = mean(abs(pred$am[ix] - pred$pm[ix]))))
  grid_tbl <- lapply(res, function(r) c(r$par, list(ll = r$ll, sigma = r$sigma)))

  report <- list(
    seasons = paste0(min(pred$season), "\u2013", max(pred$season)), n = nrow(pred),
    ll = pm_logloss(p, won), brier = mean((p - won)^2), acc = mean((p >= 0.5) == won),
    mae_margin = mean(abs(pred$am - pred$pm)), mae_total = mean(abs(pred$at - pred$pt)),
    base_coin_ll = log(2), base_home_ll = pm_logloss(rep(home_rate, length(won)), won),
    base_home_acc = mean(won), calib = calib, by_season = unname(by_season), by_phase = unname(by_phase),
    grid = grid_tbl)

  last_fit <- best$bt$last_fit
  list(version = pm_cache_key(), season = SEASON, made = format(Sys.time(), "%Y-%m-%d %H:%M"),
       par = best$par, sigma_m = best$sigma, sigma_t = sigma_t, sigma_r = sigma_r, H = H,
       po = po_par, poCv = po_cv,
       prev = list(mu = unname(last_fit$mu), off = as.list(last_fit$off), def = as.list(last_fit$def)),
       report = report)
}

# the cache is reused only if the model version, grid and expansion prior are unchanged
pm_cache_key <- function() paste(PREDICT_VERSION, paste(unlist(GRID), collapse = ","), EXPANSION_NET,
                                 paste(BUBBLE_SEASONS, collapse = ","), sep = ";")

pm_load_calibration <- function() {
  path <- file.path(OUT_DIR, ".wnba_cache", paste0("predict_calibration_", SEASON, ".json"))
  if (!PREDICT_RECALIBRATE && file.exists(path)) {
    cal <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(cal) && identical(cal$version, pm_cache_key())) {
      message("  using cached calibration from ", cal$made)
      return(cal)
    }
  }
  cal <- pm_calibrate()
  writeLines(jsonlite::toJSON(cal, auto_unbox = TRUE, digits = NA), path)
  jsonlite::fromJSON(path, simplifyVector = FALSE)   # round-trip so both paths return the same shape
}

# ---- return from absence: minutes ramp ----------------------------------------
# From every regular season since ROT_FIRST_SEASON: a rotation player (12+ minutes
# over her last 5 games) who misses 3+ straight team games and comes back. For
# each of her first 5 games back, minutes played / her minutes before the absence,
# pooled by how long she was out. Applied on the page when you mark a player "in"
# who has been out.
RAMP_BUCKETS <- list(c(3, 5), c(6, 10), c(11, 999))
pm_return_ramp <- function() {
  ev <- list()
  for (yr in ROT_FIRST_SEASON:(SEASON - 1)) {
    f <- pm_player_box_file(yr); if (is.null(f)) next
    pb <- as.data.frame(nanoparquet::read_parquet(f)); pb <- pb[as.integer(pb$season_type) == 2, ]
    pb$min <- ifelse(isTRUE_vec(pb$did_not_play) | is.na(pb$minutes), 0, suppressWarnings(as.numeric(pb$minutes)))
    tg <- unique(data.frame(tid = as.character(pb$team_id), gid = as.character(pb$game_id), date = as.Date(pb$game_date), stringsAsFactors = FALSE))
    for (t in unique(tg$tid)) {
      games <- tg[tg$tid == t, ]; games <- games[order(games$date, games$gid), ]
      pt <- pb[as.character(pb$team_id) == t, ]
      for (a in unique(as.character(pt$athlete_id))) {
        x <- pt[as.character(pt$athlete_id) == a, ]
        m <- x$min[match(games$gid, as.character(x$game_id))]; m[is.na(m)] <- 0
        pl <- which(m > 0); if (length(pl) < 8) next
        m <- m[min(pl):max(pl)]; pl <- which(m > 0)
        gaps <- diff(pl) - 1
        for (j in which(gaps >= 3)) {
          back <- pl[j + 1]; before <- pl[pl <= pl[j]]
          if (length(before) < 5) next
          pre <- mean(m[tail(before, 5)]); if (pre < 12) next
          after <- pl[pl >= back][1:5]
          ev[[length(ev) + 1]] <- data.frame(L = gaps[j], k = 1:5, pre = pre, min = ifelse(is.na(after), NA, m[after]))
        }
      }
    }
  }
  E <- do.call(rbind, ev); E <- E[!is.na(E$min), ]
  out <- lapply(RAMP_BUCKETS, function(b) {
    x <- E[E$L >= b[1] & E$L <= b[2], ]
    list(lo = b[1], hi = b[2], n = sum(x$k == 1), r = vapply(1:5, function(k) { y <- x[x$k == k, ]; if (nrow(y) >= 15) sum(y$min) / sum(y$pre) else 1 }, numeric(1)))
  })
  message("  return from absence, minutes in games 1-5 back vs before: ",
          paste(vapply(out, function(o) sprintf("%d%s missed (n=%d): %s", o$lo, if (o$hi > 100) "+" else paste0("-", o$hi), o$n, paste(sprintf("%.2f", o$r), collapse = " ")), character(1)), collapse = "; "))
  out
}
pm_ramp_load <- function() {
  f <- file.path(OUT_DIR, ".wnba_cache", paste0("predict_ramp_", SEASON, ".json"))
  key <- paste(PREDICT_VERSION, ROT_FIRST_SEASON, paste(unlist(RAMP_BUCKETS), collapse = ","))
  if (file.exists(f) && !isTRUE(PREDICT_RECALIBRATE)) {
    old <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(old) && identical(old$key, key)) return(old$ramp)
  }
  r <- pm_return_ramp()
  writeLines(jsonlite::toJSON(list(key = key, ramp = r, made = format(Sys.time(), "%Y-%m-%d %H:%M")), auto_unbox = TRUE, digits = NA), f)
  jsonlite::fromJSON(f, simplifyVector = FALSE)$ramp
}

# ---- game context: rest, travel, motivation, team home court ------------------
# Fitted on 2007 onward as corrections to the team model's walk-forward misses:
#   b2b    back-to-back (home minus away)       rest   days of rest, capped at 3
#   trav   thousand km traveled since the last game (home minus away)
#   lock   seed can no longer change (regular season only)
#   elim   can no longer make the playoffs (regular season only)
#   plus a home-court adjustment per city, shrunk toward the league's (ridge).
# Every term is checked leave-one-season-out; the fit is cached once per season.
PM_CITY <- list(Atlanta = c(33.65, -84.45), Charlotte = c(35.23, -80.84), Chicago = c(41.88, -87.63),
  Connecticut = c(41.49, -72.09), Dallas = c(32.73, -97.11), Detroit = c(42.70, -83.25),
  `Golden State` = c(37.77, -122.39), Houston = c(29.75, -95.36), Indiana = c(39.76, -86.16),
  `Las Vegas` = c(36.10, -115.18), `Los Angeles` = c(34.04, -118.27), Minnesota = c(44.98, -93.28),
  `New York` = c(40.68, -73.97), Phoenix = c(33.45, -112.07), Sacramento = c(38.58, -121.50),
  `San Antonio` = c(29.43, -98.44), Seattle = c(47.62, -122.35), Tulsa = c(36.15, -95.99),
  Washington = c(38.87, -76.99), Toronto = c(43.64, -79.38), Portland = c(45.53, -122.67),
  Utah = c(40.77, -111.90), Cleveland = c(41.50, -81.69), Miami = c(25.78, -80.19), Orlando = c(28.54, -81.38),
  Bradenton = c(27.46, -82.57))
CTX_TERMS <- c("b2b", "rest", "trav", "lock", "elim")
CTX_LAMBDA <- c(Inf, 400, 100, 25)            # home-court shrinkage options (Inf = one league-wide value)

pm_team_locs <- function(tb) {
  tb <- as.data.frame(tb)
  if (!"team_location" %in% names(tb)) return(setNames(character(0), character(0)))
  x <- unique(data.frame(id = as.character(tb$team_id), loc = as.character(tb$team_location), stringsAsFactors = FALSE))
  setNames(x$loc, x$id)
}
pm_km <- function(a, b) {
  r <- pi / 180; dl <- (b[1] - a[1]) * r; dn <- (b[2] - a[2]) * r
  h <- sin(dl / 2)^2 + cos(a[1] * r) * cos(b[1] * r) * sin(dn / 2)^2
  6371 * 2 * asin(min(1, sqrt(h)))
}
# g: every game of a season in date order (future games with hs = NA are fine)
pm_ctx_features <- function(g, locs, yr) {
  g <- g[order(g$date, g$gid), ]; n <- nrow(g)
  city <- if (yr %in% BUBBLE_SEASONS) rep("Bradenton", n) else unname(locs[g$home])
  xy <- t(vapply(city, function(cc) { v <- if (is.na(cc)) NULL else PM_CITY[[cc]]; if (is.null(v)) c(NA_real_, NA_real_) else v }, numeric(2)))
  rest <- matrix(NA_real_, n, 2); trav <- matrix(0, n, 2); ld <- list(); lv <- list()
  for (i in seq_len(n)) {
    for (k in 1:2) {
      t <- if (k == 1) g$home[i] else g$away[i]
      if (!is.null(ld[[t]])) {
        rest[i, k] <- as.numeric(g$date[i] - ld[[t]])
        if (!anyNA(lv[[t]]) && !anyNA(xy[i, ])) trav[i, k] <- pm_km(lv[[t]], xy[i, ])
      }
    }
    for (t in c(g$home[i], g$away[i])) { ld[[t]] <- g$date[i]; lv[[t]] <- xy[i, ] }
  }
  r3 <- pmin(ifelse(is.na(rest), 3, rest), 3)
  lock <- matrix(0, n, 2); elim <- matrix(0, n, 2)
  teams <- sort(unique(c(g$home, g$away))); nt <- length(teams)
  rsd <- sort(unique(g$date[g$st == 2]))
  for (di in seq_along(rsd)) {
    d <- rsd[di]
    done <- g[g$st == 2 & g$date < d & !is.na(g$hs), ]; left <- g[g$st == 2 & g$date >= d, ]
    W <- setNames(numeric(nt), teams); R <- setNames(numeric(nt), teams)
    if (nrow(done)) { w <- table(ifelse(done$hs > done$as, done$home, done$away)); W[names(w)] <- w }
    if (nrow(left)) { r <- table(c(left$home, left$away)); R[names(r)] <- r }
    lk <- setNames(numeric(nt), teams); el <- lk
    for (t in teams) {
      o <- setdiff(teams, t)
      above <- sum(W[o] > W[t] + R[t]); below <- sum(W[o] + R[o] < W[t])
      el[t] <- as.numeric(above >= PLAYOFF_SEEDS)
      lk[t] <- as.numeric(above + below == nt - 1 && above < PLAYOFF_SEEDS)
    }
    i <- which(g$st == 2 & g$date == d)
    lock[i, 1] <- lk[g$home[i]]; lock[i, 2] <- lk[g$away[i]]; elim[i, 1] <- el[g$home[i]]; elim[i, 2] <- el[g$away[i]]
  }
  data.frame(gid = g$gid, b2b = (r3[, 1] == 1) - (r3[, 2] == 1), rest = r3[, 1] - r3[, 2],
             trav = (trav[, 1] - trav[, 2]) / 1000, lock = lock[, 1] - lock[, 2], elim = elim[, 1] - elim[, 2],
             hloc = ifelse(g$neutral, NA_character_, city),
             rest_h = r3[, 1], rest_a = r3[, 2], trav_h = round(trav[, 1]), trav_a = round(trav[, 2]),
             lock_h = lock[, 1], lock_a = lock[, 2], elim_h = elim[, 1], elim_a = elim[, 2], stringsAsFactors = FALSE)
}
pm_ctx_fit <- function(X, Z, y, lambda) {
  A <- if (is.infinite(lambda) || !ncol(Z)) X else cbind(X, Z)
  pen <- c(rep(1e-6, ncol(X)), if (is.infinite(lambda) || !ncol(Z)) NULL else rep(lambda, ncol(Z)))
  solve(crossprod(A) + diag(pen, ncol(A)), crossprod(A, y))[, 1]
}
pm_ctx_calibrate <- function(cal, H) {
  pred <- pm_hist_pred(cal, H, FIRST_HIST_SEASON + 1)
  message("  fitting game context (rest, travel, motivation, home court) on ", length(unique(pred$season)), " seasons ...")
  Fs <- list()
  for (yr in unique(pred$season)) {
    tb <- nanoparquet::read_parquet(pm_hist_file(yr))
    Fs[[length(Fs) + 1]] <- pm_ctx_features(pm_games_from_box(tb, yr), pm_team_locs(tb), yr)
  }
  G <- merge(pred[, c("gid", "season", "st", "pm", "am")], do.call(rbind, Fs), by = "gid")
  G <- G[order(G$season, G$gid), ]
  y <- G$am - G$pm; X <- as.matrix(G[, CTX_TERMS])
  cities <- sort(unique(na.omit(G$hloc)))
  Z <- vapply(cities, function(cc) as.numeric(!is.na(G$hloc) & G$hloc == cc), numeric(nrow(G)))
  loso <- function(lambda, use_x = TRUE) {
    yh <- numeric(length(y))
    for (s in unique(G$season)) {
      tr <- G$season != s
      Xs <- if (use_x) X else X[, 0, drop = FALSE]
      b <- pm_ctx_fit(Xs[tr, , drop = FALSE], Z[tr, , drop = FALSE], y[tr], lambda)
      A <- if (is.infinite(lambda)) Xs else cbind(Xs, Z)
      yh[!tr] <- A[!tr, , drop = FALSE] %*% b
    }
    yh
  }
  sc <- vapply(CTX_LAMBDA, function(l) mean((y - loso(l))^2), numeric(1))
  lam <- CTX_LAMBDA[which.min(sc)]; yh <- loso(lam)
  b <- pm_ctx_fit(X, Z, y, lam)
  bx <- b[seq_along(CTX_TERMS)]; names(bx) <- CTX_TERMS
  hdev <- if (is.infinite(lam)) list() else as.list(setNames(b[-seq_along(CTX_TERMS)], cities))
  e <- y - X %*% bx - (if (length(hdev)) Z %*% unlist(hdev) else 0)
  se <- sqrt(diag(solve(crossprod(X))) * sum(e^2) / (length(y) - ncol(X)))
  won <- as.numeric(G$am > 0); sg <- cal$sigma_m
  oos <- list(n = length(y), ll0 = pm_logloss(pnorm(G$pm / sg), won), ll1 = pm_logloss(pnorm((G$pm + yh) / sg), won),
              mae0 = mean(abs(y)), mae1 = mean(abs(y - yh)))
  by_season <- lapply(split(seq_along(y), G$season), function(ix) list(season = G$season[ix[1]], n = length(ix),
    ll0 = pm_logloss(pnorm(G$pm[ix] / sg), won[ix]), ll1 = pm_logloss(pnorm((G$pm[ix] + yh[ix]) / sg), won[ix])))
  message(sprintf("  context: b2b %+.2f (se %.2f), rest %+.2f (%.2f), travel/1000km %+.2f (%.2f), locked seed %+.2f (%.2f), eliminated %+.2f (%.2f); home court by city %s",
                  bx[["b2b"]], se[["b2b"]], bx[["rest"]], se[["rest"]], bx[["trav"]], se[["trav"]], bx[["lock"]], se[["lock"]], bx[["elim"]], se[["elim"]],
                  if (is.infinite(lam)) "not used" else paste0("shrunk by ", lam, " games")))
  message(sprintf("  held-out: log loss %.4f -> %.4f, margin miss %.2f -> %.2f", oos$ll0, oos$ll1, oos$mae0, oos$mae1))
  list(coef = as.list(bx), se = as.list(se), lambda = if (is.infinite(lam)) -1 else lam, hdev = hdev, oos = oos,
       bySeason = unname(by_season), nLock = sum(G$lock_h + G$lock_a > 0), nElim = sum(G$elim_h + G$elim_a > 0),
       seasons = paste0(min(G$season), "-", max(G$season)))
}
pm_ctx_load <- function(cal, H) {
  f <- file.path(OUT_DIR, ".wnba_cache", paste0("predict_context_", SEASON, ".json"))
  key <- paste(PREDICT_VERSION, paste(CTX_TERMS, collapse = ","), paste(CTX_LAMBDA, collapse = ","), cal$made)
  if (file.exists(f) && !isTRUE(PREDICT_RECALIBRATE)) {
    old <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(old) && identical(old$key, key)) { message("  using cached game-context fit from ", old$made); return(old) }
  }
  r <- pm_ctx_calibrate(cal, H); r$key <- key; r$made <- format(Sys.time(), "%Y-%m-%d %H:%M")
  writeLines(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = NA), f)
  r
}
pm_ctx_shift <- function(Fr, CX) {
  if (is.null(CX) || is.null(Fr) || !nrow(Fr)) return(rep(0, if (is.null(Fr)) 0 else nrow(Fr)))
  as.numeric(as.matrix(Fr[, CTX_TERMS]) %*% unlist(CX$coef)[CTX_TERMS])
}

# ---- standings with the official tiebreakers ------------------------------------
# WNBA order: (1) win% in games among the tied teams, (2) win% vs teams that
# finish .500 or better, (3) point differential among the tied teams,
# (4) overall point differential, (5) coin flip. For 3+ tied teams, each step
# separates as many teams as it can; any group still tied then restarts at
# step 1 among just those teams (the league's "process starts over" rule).
pm_rank <- function(W, G, h2hW, h2hPD, PD) {
  n <- length(W); pct <- ifelse(G > 0, W / G, 0)
  above <- which(pct >= 0.5)
  gm <- h2hW + t(h2hW)
  vsA <- rowSums(h2hW[, above, drop = FALSE]) / pmax(1, rowSums(gm[, above, drop = FALSE]))
  break_tie <- function(grp) {
    if (length(grp) < 2) return(grp)
    steps <- list(
      function(g) rowSums(h2hW[g, g, drop = FALSE]) / pmax(1, rowSums(gm[g, g, drop = FALSE])),
      function(g) vsA[g],
      function(g) rowSums(h2hPD[g, g, drop = FALSE]),
      function(g) PD[g])
    for (st in steps) {
      v <- round(st(grp), 9)
      if (length(unique(v)) > 1) {
        out <- integer(0)
        for (k in sort(unique(v), decreasing = TRUE)) out <- c(out, break_tie(grp[v == k]))  # restart for sub-ties
        return(out)
      }
    }
    grp[sample.int(length(grp))]                                                          # coin flip
  }
  key <- round(pct, 9); out <- integer(0)
  for (k in sort(unique(key), decreasing = TRUE)) out <- c(out, break_tie(which(key == k)))
  out
}

# ---- data checks ------------------------------------------------------------------
# Sanity checks on this season's box scores and schedule; problems are logged and
# listed on the page. Nothing is changed or dropped because of them.
pm_data_checks <- function(cur, sched, teams) {
  tb <- as.data.frame(team_box); pb <- as.data.frame(player_box); out <- list()
  add <- function(label, ids, n = length(ids)) if (n > 0) out[[length(out) + 1]] <<- list(label = label, n = n, examples = as.list(head(unique(as.character(ids)), 3)))
  pmn <- ifelse(isTRUE_vec(pb$did_not_play) | is.na(pb$minutes), 0, suppressWarnings(as.numeric(pb$minutes)))
  key <- paste(pb$game_id, pb$team_id)
  msum <- tapply(pmn, key, sum)
  psum <- tapply(suppressWarnings(as.numeric(pb$points)), key, sum, na.rm = TRUE)
  tk <- paste(tb$game_id, tb$team_id); tsc <- setNames(suppressWarnings(as.numeric(tb$team_score)), tk)
  bad <- names(msum)[msum < 195 | msum > 280]
  add("team box scores whose player minutes don't add up to a full game", sub(" .*", "", bad))
  cm <- intersect(names(psum), names(tsc)); bad <- cm[abs(psum[cm] - tsc[cm]) > 0.5]
  add("team box scores where player points don't add up to the team score", sub(" .*", "", bad))
  add("team games with no player box score", sub(" .*", "", setdiff(tk, names(msum))))
  one <- names(which(table(as.character(tb$game_id)) != 2))
  add("games with only one team's box score", one)
  if (!is.null(sched)) {
    done <- sched$game_id[isTRUE_vec(sched$status_type_completed) & sched$season_type %in% c(2, 3) &
                          sched$home_id %in% teams & sched$away_id %in% teams]
    add("finished games on the schedule that have no box score yet", setdiff(as.character(done), cur$gid))
  }
  pl <- pmn > 0
  if (any(pl)) { na <- mean(is.na(suppressWarnings(as.numeric(pb$plus_minus[pl])))); if (na > 0.02) add("of player games are missing plus-minus (%)", character(0), round(100 * na)) }
  for (o in out) message(sprintf("  DATA CHECK: %d %s (e.g. %s)", o$n, o$label, paste(unlist(o$examples), collapse = ", ")))
  out
}

# ---- main ----------------------------------------------------------------
pm_run <- function() {
  cal <- pm_load_calibration()
  par <- cal$par; H <- cal$H; SIG <- cal$sigma_m; SIGT <- cal$sigma_t
  message(sprintf("  model: half-life %s d, lambda %s, carry %s, sigma %.2f, HCA %+.2f",
                  par$hl, par$lambda, par$carry, SIG, H))

  # schedule: neutral sites, game types (to drop the Cup final / All-Star from
  # standings) and the games still to play
  sched <- tryCatch(as.data.frame(read_parquet_release("espn_wnba_schedules", paste0("wnba_schedule_", SEASON, ".parquet"))),
                    error = function(e) { message("  WARNING: schedule unavailable (", conditionMessage(e), ")"); NULL })
  if (!is.null(sched)) {
    sched$game_id <- as.character(sched$game_id)
    sched$home_id <- as.character(sched$home_id); sched$away_id <- as.character(sched$away_id)
  }

  cur <- pm_games_from_box(team_box, SEASON)
  if (!is.null(sched)) {
    cur$neutral <- cur$neutral | (cur$gid %in% sched$game_id[isTRUE_vec(sched$neutral_site)])
  }
  teams <- sort(unique(c(team_ids, cur$home, cur$away)))
  teams <- teams[teams %in% team_ids]
  cur <- cur[cur$home %in% teams & cur$away %in% teams, ]
  prev <- list(off = unlist(cal$prev$off), def = unlist(cal$prev$def))
  prev_fit <- list(teams = names(prev$off), mu = cal$prev$mu, off = prev$off, def = prev$def)
  prior <- pm_next_prior(prev_fit, teams, par)
  asof <- if (nrow(cur)) max(max(cur$date) + 1, today) else today
  data_checks <- tryCatch(pm_data_checks(cur, sched, teams), error = function(e) { message("  WARNING: data checks failed (", conditionMessage(e), ")"); list() })
  fit <- pm_fit(cur, teams, prior, asof, par, H)
  n <- length(teams); p <- 2 * n + 1
  v_score <- SIG^2 / 2                                  # variance of one team's score
  Sigma <- v_score * solve(fit$A)
  net <- fit$off - fit$def
  # sd of each team's net rating (off - def)
  net_sd <- vapply(seq_len(n), function(i) {
    a <- numeric(p); a[1 + i] <- 1; a[1 + n + i] <- -1; sqrt(sum(a * (Sigma %*% a)))
  }, numeric(1))

  # ---- player layer: availability-adjusted strength + projections ----
  D <- pl_prepare(teams)
  AV <- tryCatch(pl_av_load(cal, H), error = function(e) { message("  WARNING: availability fit skipped (", conditionMessage(e), ")"); NULL })
  Dp <- tryCatch(pl_load_season(SEASON - 1), error = function(e) NULL)
  prevIng <- if (is.null(Dp)) NULL else pl_ingredients(Dp$P)
  prevR <- if (is.null(Dp)) NULL else pl_prev_rates(Dp$P)
  PT <- tryCatch(pl_tune_load(cal, H), error = function(e) { message("  WARNING: player-line tuning skipped (", conditionMessage(e), ")"); NULL })
  plcal <- tryCatch(pl_load_calibration(D, cur, teams, prior, par, H, SIG, AV, prevIng, PT, prevR), error = function(e) {
    message("  WARNING: player test skipped (", conditionMessage(e), ")"); NULL })
  plpar <- if (!is.null(PT)) pl_par_in(PT$par) else PL_DEFAULT_PAR
  S <- pl_snapshot(D, asof, plpar, par$hl, prevR)
  # the lineup effect was fitted with the default minutes model, so team strength keeps using it
  S$pl$expMinAdj <- if (identical(plpar$min_hl, PL_DEFAULT_PAR$min_hl)) S$pl$expMin else {
    Sd <- pl_snapshot(D, asof, PL_DEFAULT_PAR, par$hl); Sd$pl$expMin[match(S$pl$aid, Sd$pl$aid)] %|na|% 0 }
  if (!is.null(AV)) S <- pl_apply_values(S, D$P[D$P$date < asof, ], prevIng, AV)
  PO <- cal$po
  # playoff rotations: one set for the minutes half-life team strength uses, one for player lines
  RT <- tryCatch(pm_rotation_load(cal), error = function(e) { message("  WARNING: playoff rotations skipped (", conditionMessage(e), ")"); NULL })
  ROT <- pm_rotation_for(RT, PL_DEFAULT_PAR$min_hl); ROT_L <- pm_rotation_for(RT, plpar$min_hl)
  # use the availability adjustment in the baseline only if it beat the plain
  # team model in this season's test; either way, what you toggle on the page
  # moves the numbers relative to the baseline
  ADJ_SCALE <- 1
  # in the baseline only if it beat the plain team model on held-out past seasons
  use_adj <- isTRUE(!is.null(AV) && AV$oos$ll1 < AV$oos$ll0)
  message(sprintf("  player layer: %d players; availability adjustment %s in the baseline",
                  nrow(S$pl), if (use_adj) "used" else "not used"))
  pace_of <- function(t) pl_pace(S, t)
  adj_team <- function(t, act, playoff, poss) {
    tp <- S$pl[S$pl$tid == t, ]
    if (is.null(act)) act <- !tp$defaultOut
    m <- pl_minutes(tp$expMinAdj, act, playoff && PLAYOFF_ROTATION_ADJ, ROT)
    pl_team_adj(tp, m, poss, S$dep[[t]] %||% c(o = 0, d = 0), ADJ_SCALE)
  }
  adj_ref <- function(t, poss) if (use_adj) c(o = 0, d = 0) else adj_team(t, NULL, FALSE, poss)
  game_pred <- function(h, a, neutral, playoff) {
    poss <- pace_of(h) + pace_of(a) - S$lgPoss
    aH <- adj_team(h, NULL, playoff, poss) - adj_ref(h, poss); aA <- adj_team(a, NULL, playoff, poss) - adj_ref(a, poss)
    b <- pm_predict(fit, h, a, neutral, H)
    sH <- b$hs + aH[["o"]] - aA[["d"]]; sA <- b$as + aA[["o"]] - aH[["d"]]
    if (playoff) {
      hc <- if (neutral) 0 else H
      mg <- PO$k * (sH - sA - hc) + (if (neutral) 0 else PO$h); tot <- sH + sA + PO$tb
      sH <- (tot + mg) / 2; sA <- (tot - mg) / 2
    }
    c(margin = sH - sA, total = sH + sA, hs = sH, as = sA)
  }
  A_rs <- vapply(teams, function(t) sum(adj_team(t, NULL, FALSE, S$lgPoss) - adj_ref(t, S$lgPoss)), numeric(1))
  A_po <- vapply(teams, function(t) sum(adj_team(t, NULL, TRUE, S$lgPoss) - adj_ref(t, S$lgPoss)), numeric(1))

  # ---- remaining regular-season games + game-type exclusions ----
  excl_types <- c("CC", "ALLSTAR")
  excluded <- if (!is.null(sched)) sched$game_id[sched$type_abbreviation %in% excl_types] else character(0)
  standings_g <- cur[cur$st == 2 & !cur$gid %in% excluded, ]
  playoff_g <- cur[cur$st == 3, ]
  remaining <- data.frame()
  upcoming <- data.frame()
  schedule <- data.frame()
  if (!is.null(sched)) {
    live <- sched[!isTRUE_vec(sched$status_type_completed) &
                    !grepl("POSTPONED|CANCELED|CANCELLED", toupper(sched$status_type_name %||% "")) &
                    !sched$game_id %in% cur$gid &
                    sched$home_id %in% teams & sched$away_id %in% teams, ]
    live$date <- as.Date(live$game_date)
    rs <- live[live$season_type == 2 & !live$type_abbreviation %in% excl_types, ]
    remaining <- data.frame(gid = rs$game_id, date = rs$date, home = rs$home_id, away = rs$away_id,
                            neutral = isTRUE_vec(rs$neutral_site), stringsAsFactors = FALSE)
    up <- live[live$date >= today & live$date <= today + 7, ]
    up <- up[order(up$date, up$game_id), ]
    if (nrow(up)) {
      prm <- t(vapply(seq_len(nrow(up)), function(k) game_pred(up$home_id[k], up$away_id[k], isTRUE_vec(up$neutral_site[k]), up$season_type[k] == 3), numeric(4)))
      pr <- list(margin = prm[, "margin"], total = prm[, "total"], hs = prm[, "hs"], as = prm[, "as"])
      upcoming <- data.frame(gid = up$game_id, date = format(up$date), home = up$home_id, away = up$away_id,
                             neutral = isTRUE_vec(up$neutral_site), st = up$season_type,
                             note = ifelse(is.na(up$notes_headline), "", up$notes_headline),
                             pHome = pnorm(pr$margin / SIG), margin = pr$margin, total = pr$total,
                             hs = pr$hs, as = pr$as, stringsAsFactors = FALSE)
    }
    # every scheduled game with both teams known (any date), for the dashboard's
    # Matchup Preview and the team pages' "Up next" strip; the page projects these
    # itself with the same model, so only the fixture details are sent
    if (nrow(live)) {
      sl <- live[order(live$date, live$game_id), ]
      schedule <- data.frame(gid = as.character(sl$game_id), date = format(sl$date), home = as.character(sl$home_id),
                             away = as.character(sl$away_id), neutral = isTRUE_vec(sl$neutral_site), st = as.integer(sl$season_type),
                             note = ifelse(is.na(sl$notes_headline), "", sl$notes_headline), stringsAsFactors = FALSE)
    }
  }
  season_complete <- nrow(remaining) == 0

  # ---- game context (rest, travel, motivation) for scheduled games ----
  CX <- tryCatch(pm_ctx_load(cal, H), error = function(e) { message("  WARNING: game-context fit skipped (", conditionMessage(e), ")"); NULL })
  ctx_rem <- rep(0, nrow(remaining))
  if (!is.null(CX) && !is.null(sched)) {
    ga <- rbind(cur[, c("gid", "date", "st", "home", "away", "hs", "as", "neutral")],
                data.frame(gid = as.character(live$game_id), date = as.Date(live$date), st = as.integer(live$season_type),
                           home = as.character(live$home_id), away = as.character(live$away_id),
                           hs = rep(NA_real_, nrow(live)), as = rep(NA_real_, nrow(live)), neutral = isTRUE_vec(live$neutral_site), stringsAsFactors = FALSE))
    FX <- tryCatch(pm_ctx_features(ga, pm_team_locs(team_box), SEASON), error = function(e) { message("  WARNING: game context unavailable (", conditionMessage(e), ")"); NULL })
    if (!is.null(FX)) {
      FX$shift <- pm_ctx_shift(FX, CX)
      ctx_rem <- FX$shift[match(remaining$gid, FX$gid)] %|na|% 0
      if (nrow(upcoming)) {
        fu <- FX[match(upcoming$gid, FX$gid), ]
        sh <- fu$shift %|na|% 0
        upcoming$margin <- upcoming$margin + sh; upcoming$hs <- upcoming$hs + sh / 2; upcoming$as <- upcoming$as - sh / 2
        upcoming$pHome <- pnorm(upcoming$margin / SIG); upcoming$ctx <- sh
        ab <- function(id) unname(abbr_by_id[id])
        upcoming$ctxNote <- vapply(seq_len(nrow(upcoming)), function(k) {
          f <- fu[k, ]; if (is.na(f$gid)) return("")
          side <- function(id, lock, elim, rest, trav) c(
            if (isTRUE(lock == 1)) paste0(ab(id), "'s seed is locked"), if (isTRUE(elim == 1)) paste0(ab(id), " is eliminated"),
            if (isTRUE(rest == 1)) paste0(ab(id), " on a back-to-back"), if (isTRUE(trav >= 2500)) paste0(ab(id), " traveled ", format(trav, big.mark = ","), " km"))
          paste(c(side(upcoming$away[k], f$lock_a, f$elim_a, f$rest_a, f$trav_a), side(upcoming$home[k], f$lock_h, f$elim_h, f$rest_h, f$trav_h)), collapse = "; ")
        }, character(1))
      }
    }
  }

  # ---- current standings inputs ----
  ix <- function(id) match(id, teams)
  W0 <- numeric(n); G0 <- numeric(n); PD0 <- numeric(n)
  h2hW0 <- matrix(0, n, n); h2hPD0 <- matrix(0, n, n)
  if (nrow(standings_g)) {
    hi <- ix(standings_g$home); ai <- ix(standings_g$away); mg <- standings_g$hs - standings_g$as
    win <- ifelse(mg > 0, hi, ai); los <- ifelse(mg > 0, ai, hi)
    W0 <- tabulate(win, n); G0 <- tabulate(c(hi, ai), n)
    for (k in seq_along(win)) {
      PD0[hi[k]] <- PD0[hi[k]] + mg[k]; PD0[ai[k]] <- PD0[ai[k]] - mg[k]
      h2hW0[win[k], los[k]] <- h2hW0[win[k], los[k]] + 1
      h2hPD0[hi[k], ai[k]] <- h2hPD0[hi[k], ai[k]] + mg[k]
      h2hPD0[ai[k], hi[k]] <- h2hPD0[ai[k], hi[k]] - mg[k]
    }
  }

  # ---- official seeds: override, and a check against real playoff pairings ----
  seed_fixed <- NULL; seed_warning <- NULL
  if (season_complete) {
    if (length(PLAYOFF_SEED_OVERRIDE)) {
      ids <- vapply(PLAYOFF_SEED_OVERRIDE, function(a) { k <- names(abbr_by_id)[abbr_by_id == a]; if (length(k)) k[1] else NA_character_ }, character(1))
      if (anyNA(ids) || length(ids) != PLAYOFF_SEEDS) {
        seed_warning <- "PLAYOFF_SEED_OVERRIDE is set but doesn't list exactly the playoff teams by abbreviation; it was ignored."
      } else seed_fixed <- match(ids, teams)
      if (anyNA(seed_fixed)) { seed_fixed <- NULL; seed_warning <- "PLAYOFF_SEED_OVERRIDE names a team with no games this season; it was ignored." }
    }
    if (nrow(playoff_g)) {
      seeds_now <- seed_fixed %||% pm_rank(W0, G0, h2hW0, h2hPD0, PD0)[seq_len(PLAYOFF_SEEDS)]
      r1_real <- vapply(R1_PAIRS, function(pr) paste(sort(teams[seeds_now[pr]]), collapse = "|"), character(1))
      first_day <- min(playoff_g$date)
      opened <- playoff_g[playoff_g$date <= first_day + 1, ]
      played <- unique(vapply(seq_len(nrow(opened)), function(k) paste(sort(c(opened$home[k], opened$away[k])), collapse = "|"), character(1)))
      if (length(setdiff(played, r1_real))) {
        seed_warning <- paste0("The real first-round matchups don't match the seeds this script computed from the standings ",
                               "(most likely a coin-flip tiebreak or a rule change). Set PLAYOFF_SEED_OVERRIDE in predict_wnba.R ",
                               "to the official seeds; until then, playoff odds may be wrong for the affected teams.")
      }
    }
    if (!is.null(seed_warning)) message("  WARNING: ", seed_warning)
  }

  # ---- simulate ----
  L <- tryCatch(chol(Sigma), error = function(e) chol(Sigma + diag(1e-8, p)))
  th_hat <- c(fit$mu, fit$off, fit$def)
  rem_h <- ix(remaining$home); rem_a <- ix(remaining$away)
  rem_hc <- ifelse(remaining$neutral, 0, H) + ctx_rem
  rem_games <- tabulate(c(rem_h, rem_a), n)
  tabulate_w <- function(idx, w, len) { out <- numeric(len); v <- rowsum(w, idx); out[as.integer(rownames(v))] <- v[, 1]; out }
  # game-level noise: calibrated SIG already includes rating uncertainty, so
  # remove the average rating-uncertainty variance before redrawing it
  mvar <- if (nrow(remaining)) mean(vapply(seq_along(rem_h), function(k) {
    a <- numeric(p); a[1 + rem_h[k]] <- 1; a[1 + n + rem_a[k]] <- 1; a[1 + rem_a[k]] <- a[1 + rem_a[k]] - 1; a[1 + n + rem_h[k]] <- a[1 + n + rem_h[k]] - 1
    sum(a * (Sigma %*% a)) }, numeric(1))) else mean(net_sd^2) * 2
  sig_game <- sqrt(max(SIG^2 - mvar, (0.85 * SIG)^2))

  seed_ct <- matrix(0, n, PLAYOFF_SEEDS); wins_sum <- numeric(n)
  adv <- matrix(0, n, length(PLAYOFF_ROUNDS) + 1)   # col r = reached round r (1 = made playoffs), last = champion
  series_obs <- list()
  # actual playoff results so far, between each pair
  po_w <- matrix(0, n, n)
  if (nrow(playoff_g)) for (k in seq_len(nrow(playoff_g))) {
    a <- ix(playoff_g$home[k]); b <- ix(playoff_g$away[k])
    if (playoff_g$hs[k] > playoff_g$as[k]) po_w[a, b] <- po_w[a, b] + 1 else po_w[b, a] <- po_w[b, a] + 1
  }
  play_series <- function(hi, lo, rnd, net_s) {
    fmt <- PLAYOFF_ROUNDS[[rnd]]; need <- (fmt$best_of + 1) / 2
    wh <- po_w[hi, lo]; wl <- po_w[lo, hi]; gno <- wh + wl
    while (wh < need && wl < need) {
      gno <- gno + 1
      hc <- if (fmt$home[gno]) PO$h else -PO$h
      if (rnorm(1, PO$k * ((net_s[hi] + A_po[hi]) - (net_s[lo] + A_po[lo])) + hc, sig_game) > 0) wh <- wh + 1 else wl <- wl + 1
    }
    if (wh >= need) hi else lo
  }
  r1_key <- function(a, b) paste(sort(teams[c(a, b)]), collapse = "|")
  r1_tally <- list()

  z <- matrix(rnorm(N_SIMS * p), N_SIMS, p)
  for (s in seq_len(N_SIMS)) {
    th <- th_hat + drop(z[s, ] %*% L)
    off_s <- th[1 + seq_len(n)]; def_s <- th[1 + n + seq_len(n)]; net_s <- off_s - def_s
    W <- W0; G <- G0; PD <- PD0; h2hW <- h2hW0; h2hPD <- h2hPD0
    if (length(rem_h)) {
      mg <- round(rnorm(length(rem_h), (net_s[rem_h] + A_rs[rem_h]) - (net_s[rem_a] + A_rs[rem_a]) + rem_hc, sig_game))
      mg[mg == 0] <- sample(c(-1, 1), sum(mg == 0), replace = TRUE)
      win <- ifelse(mg > 0, rem_h, rem_a); los <- ifelse(mg > 0, rem_a, rem_h)
      W <- W + tabulate(win, n); G <- G + rem_games
      PD <- PD + tabulate_w(c(rem_h, rem_a), c(mg, -mg), n)
      h2hW <- h2hW + tabulate(win + (los - 1L) * n, n * n)
      h2hPD <- h2hPD + tabulate_w(c(rem_h + (rem_a - 1L) * n, rem_a + (rem_h - 1L) * n), c(mg, -mg), n * n)
    }
    wins_sum <- wins_sum + W
    seeds <- if (!is.null(seed_fixed)) seed_fixed else pm_rank(W, G, h2hW, h2hPD, PD)[seq_len(min(PLAYOFF_SEEDS, n))]
    seed_ct[cbind(seeds, seq_along(seeds))] <- seed_ct[cbind(seeds, seq_along(seeds))] + 1
    adv[seeds, 1] <- adv[seeds, 1] + 1
    # bracket
    alive <- lapply(R1_PAIRS, function(pr) seeds[pr])
    for (pr in alive) { k <- r1_key(pr[1], pr[2]); r1_tally[[k]] <- (r1_tally[[k]] %||% 0) + 1 }
    rnd <- 1
    while (length(alive) >= 1) {
      winners <- vapply(alive, function(pr) play_series(pr[1], pr[2], rnd, net_s), integer(1))
      adv[winners, rnd + 1] <- adv[winners, rnd + 1] + 1
      if (length(winners) == 1) break
      alive <- lapply(seq(1, length(winners), by = 2), function(k) {
        a <- winners[k]; b <- winners[k + 1]
        if (match(a, seeds) < match(b, seeds)) c(a, b) else c(b, a)
      })
      rnd <- rnd + 1
    }
  }

  # ---- assemble ----
  rank_net <- rank(-net, ties.method = "first")
  cur_rank <- if (nrow(standings_g)) pm_rank(W0, G0, h2hW0, h2hPD0, PD0) else seq_len(n)
  if (!is.null(seed_fixed)) cur_rank <- c(seed_fixed, setdiff(cur_rank, seed_fixed))
  team_rows <- lapply(seq_len(n), function(i) list(
    id = teams[i], off = unname(fit$off[i]), def = unname(fit$def[i]), net = unname(net[i]),
    netSd = net_sd[i], rank = rank_net[i],
    w = W0[i], l = G0[i] - W0[i], pd = PD0[i], standing = match(i, cur_rank),
    projW = wins_sum[i] / N_SIMS, projG = G0[i] + sum(c(rem_h, rem_a) == i),
    pPlayoffs = adv[i, 1] / N_SIMS, seed = as.list(seed_ct[i, ] / N_SIMS),
    pSemis = adv[i, 2] / N_SIMS, pFinals = adv[i, 3] / N_SIMS, pChamp = adv[i, 4] / N_SIMS,
    playoffW = sum(po_w[i, ]), playoffL = sum(po_w[, i])))

  # most likely first-round matchups
  r1 <- if (length(r1_tally)) {
    kk <- names(r1_tally); vv <- unlist(r1_tally) / N_SIMS
    o <- order(-vv); kk <- kk[o]; vv <- vv[o]
    lapply(seq_len(min(8, length(kk))), function(j) {
      ab <- strsplit(kk[j], "|", fixed = TRUE)[[1]]
      list(a = ab[1], b = ab[2], p = vv[j])
    })
  } else list()

  # ---- prediction log: record today's predictions, grade finished ones ----
  log_path <- file.path(OUT_DIR, "predictions_log.csv")
  log <- if (file.exists(log_path)) tryCatch(read.csv(log_path, colClasses = "character"), error = function(e) NULL) else NULL
  if (nrow(upcoming)) {
    new <- data.frame(gid = upcoming$gid, date = upcoming$date, home = upcoming$home, away = upcoming$away,
                      p_home = sprintf("%.4f", upcoming$pHome), margin = sprintf("%.2f", upcoming$margin),
                      total = sprintf("%.2f", upcoming$total), made = format(Sys.time(), "%Y-%m-%d %H:%M"),
                      stringsAsFactors = FALSE)
    # keep the latest pre-game prediction for each game
    log <- if (is.null(log)) new else rbind(log[!log$gid %in% new$gid, names(new)], new)
    tryCatch(write.csv(log, log_path, row.names = FALSE), error = function(e) message("  WARNING: could not write ", log_path))
  }
  live <- NULL
  if (!is.null(log) && nrow(log)) {
    gl <- merge(log, cur[, c("gid", "hs", "as")], by = "gid")
    if (nrow(gl)) {
      ph <- as.numeric(gl$p_home); won <- as.numeric(gl$hs > gl$as)
      live <- list(n = nrow(gl), ll = pm_logloss(ph, won), brier = mean((ph - won)^2),
                   acc = mean((ph >= 0.5) == won), mae = mean(abs((gl$hs - gl$as) - as.numeric(gl$margin))),
                   since = min(gl$date))
    }
  }

  # A seed can also be locked by a tiebreaker the simple lock rule can't see; if every
  # simulation puts a team in the same playoff seed, treat it as locked for its
  # upcoming regular-season games (display and win chance only; the sims already ran).
  if (!is.null(CX) && nrow(upcoming) && exists("fu")) {
    sure <- teams[apply(seed_ct, 1, max) >= N_SIMS]
    add_h <- upcoming$st == 2 & upcoming$home %in% sure & !(fu$lock_h %in% 1)
    add_a <- upcoming$st == 2 & upcoming$away %in% sure & !(fu$lock_a %in% 1)
    if (any(add_h | add_a)) {
      d <- CX$coef$lock * (as.numeric(add_h) - as.numeric(add_a))
      upcoming$margin <- upcoming$margin + d; upcoming$hs <- upcoming$hs + d / 2; upcoming$as <- upcoming$as - d / 2
      upcoming$ctx <- upcoming$ctx + d; upcoming$pHome <- pnorm(upcoming$margin / SIG)
      nt <- ifelse(add_a, paste0(abbr_by_id[upcoming$away], "'s seed is locked"), "")
      nt <- ifelse(add_h, paste0(ifelse(nzchar(nt), paste0(nt, "; "), ""), abbr_by_id[upcoming$home], "'s seed is locked"), nt)
      upcoming$ctxNote <- ifelse(nzchar(nt), ifelse(nzchar(upcoming$ctxNote), paste0(nt, "; ", upcoming$ctxNote), nt), upcoming$ctxNote)
    }
  }
  list(ok = TRUE, version = PREDICT_VERSION, season = SEASON, asOf = if (nrow(cur)) format(max(cur$date)) else NA,
       generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
       nSims = N_SIMS, seasonComplete = season_complete, remainingGames = nrow(remaining),
       playoffsStarted = nrow(playoff_g) > 0,
       model = list(hl = par$hl, lambda = par$lambda, carry = par$carry, hca = H, sigma = SIG,
                    sigmaTotal = SIGT, sigmaMargin = cal$sigma_r, sigmaGame = sig_game, mu = unname(fit$mu),
                    calibrated = cal$made, expansionNet = EXPANSION_NET),
       rounds = lapply(PLAYOFF_ROUNDS, function(r) list(name = r$name, bestOf = r$best_of, home = as.list(r$home))),
       teams = team_rows, upcoming = if (nrow(upcoming)) upcoming else list(),
       schedule = if (nrow(schedule)) schedule else list(), r1 = r1,
       report = cal$report, live = live, seedWarning = seed_warning, dataChecks = data_checks,
       context = if (is.null(CX)) NULL else CX[c("coef", "se", "oos", "bySeason", "nLock", "nElim", "seasons", "made")],
       playoffModel = list(k = PO$k, h = PO$h, tb = PO$tb, used = isTRUE(PO$used), cv = cal$poCv,
                           rot = list(team = if (is.null(ROT)) NULL else list(x = as.list(ROT$x), cut = ROT$cut),
                                      lines = if (is.null(ROT_L)) NULL else list(x = as.list(ROT_L$x), cut = ROT_L$cut)),
                           rotFit = if (is.null(RT)) NULL else RT[c("byHl", "n", "seasons")],
                           rotAdj = PLAYOFF_ROTATION_ADJ),
       ramp = tryCatch(pm_ramp_load(), error = function(e) { message("  WARNING: return ramp skipped (", conditionMessage(e), ")"); NULL }),
       players = c(pl_payload(S, teams, plcal, use_adj, ADJ_SCALE, if (is.null(PT)) NULL else PT$mc), list(avail = AV[c("K", "a", "coef", "se", "oos", "bySeason", "seasons", "made")])),
       sim = list(teams = as.list(teams), W = W0, G = G0, PD = PD0, h2hW = h2hW0, h2hPD = h2hPD0, poW = po_w,
                  seedFixed = if (is.null(seed_fixed)) NULL else as.list(seed_fixed),
                  remaining = if (nrow(remaining)) lapply(seq_len(nrow(remaining)), function(k)
                    list(h = remaining$home[k], a = remaining$away[k], n = remaining$neutral[k], c = round(ctx_rem[k], 3))) else list(),
                  sigGame = sig_game, r1Pairs = R1_PAIRS))
}

isTRUE_vec <- function(x) !is.na(x) & as.logical(x)

# =============================================================================
# ---- PLAYER LAYER: availability (#2) and player projections (#3) -----------
# =============================================================================
# Every player gets a small set of ingredients; the page combines them for
# any matchup and any availability you set, with the same equations as below.
#
#   Expected minutes   minutes when she plays (recent games weigh more,
#                      half-life MIN_HL games) x how often she plays when
#                      healthy. Active players' minutes are scaled so the
#                      team totals 200; a player marked out gives her minutes
#                      to the rest in proportion.
#   Rates              points/rebounds/assists/threes per minute, recent
#                      games weighted (RATE_HL games), shrunk toward her
#                      position's league rate by K minutes.
#   Matchup            x expected pace / her team's pace, x what the opponent
#                      allows to her position (per possession, shrunk toward
#                      league average). Points are then scaled so the team's
#                      players add up to the team model's projected score.
#   Team adjustment    the team model's rating already contains whoever has
#                      been playing. Each player adds (her BPM - replacement)
#                      x (minutes this game - minutes baked into the rating)
#                      / 40, per 100 possessions. So marking a starter out
#                      costs about her value; bringing back someone who missed
#                      half the season adds about half of it.
# Settings are picked by a walk-forward test on this season: each game day is
# projected from earlier games only, with the real lineups as availability.
REPL_O <- 0; REPL_D <- 0         # replacement level is fitted into each player's value (see pl_apply_values)
DEFAULT_OUT_GAMES <- 3           # hasn't played in the team's last N games -> out by default
DEF_SHRINK_GAMES  <- 10          # opponent-by-position factors: games' worth of league average
PL_MIN_DATES      <- 10          # game days before the player test starts scoring
PL_STATS <- c("pts", "reb", "ast", "tpm")

pl_prepare_from <- function(pb, tb, team_ids_keep = NULL) {
  pb <- as.data.frame(pb); tb <- as.data.frame(tb)
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), 0, x) }
  if (is.null(team_ids_keep)) { cnt <- table(as.character(tb$team_id)); team_ids_keep <- names(cnt)[cnt >= 10] }
  dnp <- isTRUE_vec(safe_col(pb, "did_not_play"))
  rs0 <- as.character(safe_col(pb, "reason"))
  rsn <- ifelse(is.na(rs0), "", toupper(trimws(rs0)))
  P <- data.frame(gid = as.character(pb$game_id), date = as.Date(pb$game_date),
                  tid = as.character(pb$team_id), oid = as.character(pb$opponent_team_id),
                  aid = as.character(pb$athlete_id), name = as.character(pb$athlete_display_name),
                  pos = ifelse(pb$athlete_position_abbreviation %in% c("G", "F", "C"), pb$athlete_position_abbreviation, "F"),
                  st = as.integer(pb$season_type),
                  min = ifelse(dnp | is.na(pb$minutes), 0, num(pb$minutes)),
                  inj = dnp & rsn != "" & rsn != "COACH'S DECISION",
                  reason = ifelse(dnp & rsn != "" & rsn != "COACH'S DECISION", rs0, NA_character_),
                  pts = num(pb$points), reb = num(pb$rebounds), ast = num(pb$assists),
                  tpm = num(safe_col(pb, "three_point_field_goals_made")), stringsAsFactors = FALSE)
  # box-score value ingredients (same weights as the Player Value tab's box rating)
  P$offr <- P$pts + 0.4 * num(safe_col(pb, "field_goals_made")) - 0.7 * num(safe_col(pb, "field_goals_attempted")) -
            0.4 * (num(safe_col(pb, "free_throws_attempted")) - num(safe_col(pb, "free_throws_made"))) + 0.7 * num(safe_col(pb, "offensive_rebounds")) +
            0.7 * P$ast - num(safe_col(pb, "turnovers"))
  P$defr <- 0.3 * num(safe_col(pb, "defensive_rebounds")) + num(safe_col(pb, "steals")) + 0.7 * num(safe_col(pb, "blocks")) - 0.4 * num(safe_col(pb, "fouls"))
  P$pmv <- suppressWarnings(as.numeric(safe_col(pb, "plus_minus")))
  P <- P[P$tid %in% team_ids_keep & P$oid %in% team_ids_keep, ]
  TG <- data.frame(gid = as.character(tb$game_id), date = as.Date(tb$game_date), tid = as.character(tb$team_id),
                   oid = as.character(tb$opponent_team_id), st = as.integer(tb$season_type),
                   p1 = num(tb$field_goals_attempted) - num(tb$offensive_rebounds) + num(tb$turnovers) + 0.44 * num(tb$free_throws_attempted),
                   mg = num(tb$team_score) - num(tb$opponent_team_score), stringsAsFactors = FALSE)
  TG <- TG[TG$tid %in% team_ids_keep & TG$oid %in% team_ids_keep & !is.na(TG$date), ]
  TG$poss <- ave(TG$p1, TG$gid, FUN = mean)
  TG <- TG[!duplicated(paste(TG$gid, TG$tid)), ]
  # plus-minus relative to her team's margin in that game (a rough on/off)
  tmg <- TG$mg[match(paste(P$gid, P$tid), paste(TG$gid, TG$tid))]
  P$pmrel <- ifelse(is.na(P$pmv) | is.na(tmg), 0, P$pmv - tmg * P$min / 40)   # missing +/- counts as neutral
  list(P = P, TG = TG)
}
pl_prepare <- function(team_ids_keep) pl_prepare_from(player_box, team_box, team_ids_keep)

pl_snapshot <- function(D, asof, par, team_hl, prevR = NULL) {
  P <- D$P[D$P$date < asof, ]; TG <- D$TG[D$TG$date < asof, ]
  if (!nrow(P) || !nrow(TG)) return(NULL)
  TG <- TG[order(TG$tid, -as.numeric(TG$date)), ]
  TG$gi <- ave(seq_len(nrow(TG)), TG$tid, FUN = seq_along) - 1
  TG$tw <- 0.5^(as.numeric(asof - TG$date) / team_hl)
  pace <- tapply(TG$poss * TG$tw, TG$tid, sum) / tapply(TG$tw, TG$tid, sum)
  tw_tot <- tapply(TG$tw, TG$tid, sum)
  m <- match(paste(P$gid, P$tid), paste(TG$gid, TG$tid))
  P$gi <- TG$gi[m]; P$tw <- TG$tw[m]; P$poss <- TG$poss[m]
  P <- P[!is.na(P$gi), ]
  P <- P[order(P$aid, -as.numeric(P$date)), ]
  last <- P[!duplicated(P$aid), ]                                  # latest row per player -> current team
  pl <- data.frame(aid = last$aid, tid = last$tid, name = last$name, pos = last$pos, stringsAsFactors = FALSE)
  # --- rates (all teams), recency by her own played games ---
  PP <- P[P$min > 0, ]
  PP$pgi <- ave(seq_len(nrow(PP)), PP$aid, FUN = seq_along) - 1
  wr <- 0.5^(PP$pgi / par$rate_hl); wm <- 0.5^(PP$pgi / par$min_hl)
  pos_rate <- sapply(PL_STATS, function(s) tapply(PP[[s]], PP$pos, sum) / tapply(PP$min, PP$pos, sum))
  sw_min <- rowsum(wr * PP$min, PP$aid)[, 1]
  pv <- if (is.null(prevR)) NULL else prevR[match(pl$aid, prevR$aid), , drop = FALSE]
  Mp <- if (is.null(pv)) rep(0, nrow(pl)) else (pv$M %|na|% 0)
  for (s in PL_STATS) {
    sw <- rowsum(wr * PP[[s]], PP$aid)[, 1]
    rp <- if (is.null(pv)) rep(0, nrow(pl)) else (pv[[s]] %|na|% 0)
    pl[[paste0("r_", s)]] <- pl_rate_mix(sw[pl$aid] %|na|% 0, sw_min[pl$aid] %|na|% 0, Mp, rp, pos_rate[pl$pos, s], par$a %||% 0, par$k)
  }
  mwp <- rowsum(wm * PP$min, PP$aid)[, 1] / rowsum(wm, PP$aid)[, 1]
  pl$mwp <- mwp[pl$aid] %|na|% 0
  pl$gp <- as.numeric(table(PP$aid)[pl$aid]) %|na|% 0
  pl$mpg <- (rowsum(PP$min, PP$aid)[, 1] / pmax(1, as.numeric(table(PP$aid))))[pl$aid] %|na|% 0
  for (s in PL_STATS) pl[[paste0("avg_", s)]] <- (rowsum(PP[[s]], PP$aid)[, 1] / pmax(1, as.numeric(table(PP$aid))))[pl$aid] %|na|% 0
  # --- how often she plays for her current team when healthy ---
  CT <- P[P$tid == pl$tid[match(P$aid, pl$aid)], ]
  hw <- 0.5^(CT$gi / par$min_hl) * !CT$inj
  pp <- (rowsum(hw * (CT$min > 0), CT$aid)[, 1] + 0.4) / (rowsum(hw, CT$aid)[, 1] + 0.5)
  pl$pplay <- pmin(1, pp[pl$aid] %|na|% 0.8)
  pl$expMin <- pl$mwp * pl$pplay
  rec <- CT[CT$gi < DEFAULT_OUT_GAMES, ]
  played_recent <- unique(rec$aid[rec$min > 0])
  pl$defaultOut <- !(pl$aid %in% played_recent)
  lp <- tapply(as.character(CT$date[CT$min > 0]), CT$aid[CT$min > 0], max)
  mg <- tapply(CT$gi[CT$min > 0], CT$aid[CT$min > 0], min)             # team games since she last played
  pl$missed <- as.numeric(mg[pl$aid])
  pl$lastPlayed <- unname(lp[pl$aid])
  rr <- CT[!is.na(CT$reason) & CT$gi < DEFAULT_OUT_GAMES + 2, ]
  rr <- rr[order(rr$aid, rr$gi), ]; rr <- rr[!duplicated(rr$aid), ]
  pl$reason <- rr$reason[match(pl$aid, rr$aid)]
  # --- minutes baked into each team's rating (team-model weights, all its games) ---
  bw <- rowsum(P$tw * P$min, paste(P$tid, P$aid))[, 1]
  bk <- data.frame(key = names(bw), tid = sub(" .*", "", names(bw)), aid = sub("^[^ ]* ", "", names(bw)),
                   base = as.numeric(bw) / as.numeric(tw_tot[sub(" .*", "", names(bw))]), stringsAsFactors = FALSE)
  pl$base <- bk$base[match(paste(pl$tid, pl$aid), bk$key)] %|na|% 0
  pl$vo <- 0; pl$vd <- 0; pl$vsrc <- TRUE                      # set by pl_apply_values()
  dep <- lapply(names(tw_tot), function(t) c(o = 0, d = 0)); names(dep) <- names(tw_tot)   # set by pl_apply_values()
  # --- what each defense allows, by position, per possession ---
  poss_team <- tapply(TG$poss, TG$tid, sum); n_team <- table(TG$tid)
  fac <- list(); facRaw <- list(); facN <- list()
  lg <- sapply(PL_STATS, function(s) tapply(PP[[s]], PP$pos, sum)) / sum(TG$poss)
  for (t in names(poss_team)) {
    A <- PP[PP$oid == t, ]
    raw <- sapply(PL_STATS, function(s) { v <- tapply(A[[s]], factor(A$pos, levels = c("C", "F", "G")), sum); v[is.na(v)] <- 0; v / poss_team[[t]] })
    raw <- raw / lg[c("C", "F", "G"), , drop = FALSE]
    n <- as.numeric(n_team[t])
    facRaw[[t]] <- raw; facN[[t]] <- n
    fac[[t]] <- pl_fac_mix(raw, n, par$S %||% DEF_SHRINK_GAMES)
  }
  list(pl = pl, pace = pace, lgPoss = mean(TG$poss), fac = fac, facRaw = facRaw, facN = facN, dep = dep, bk = bk[, c("tid", "aid", "base")])
}
`%|na|%` <- function(a, b) ifelse(is.na(a), b, a)

# per-minute rate: recent games (weighted), last season (a x her minutes), position average (k minutes)
pl_rate_mix <- function(Wstat, Wmin, Mp, rp, pos_r, a, k) (Wstat + a * Mp * rp + k * pos_r) / (Wmin + a * Mp + k)
# what a defense allows by position, shrunk toward average by S games (Inf = no adjustment)
pl_fac_mix <- function(raw, n, S) {
  raw[!is.finite(raw)] <- 1                                  # e.g. 0/0 for a position that never shoots threes
  if (is.infinite(S)) { raw[] <- 1; raw } else (n * raw + S) / (n + S)
}
# last season's minutes and per-minute rates by player
pl_prev_rates <- function(P) {
  PP <- P[P$min > 0, ]
  if (!nrow(PP)) return(NULL)
  M <- rowsum(PP$min, PP$aid)[, 1]
  out <- data.frame(aid = names(M), M = as.numeric(M), stringsAsFactors = FALSE)
  for (s in PL_STATS) out[[s]] <- rowsum(PP[[s]], PP$aid)[, 1] / M
  out
}

# One team in one game. act: logical vector over S$pl rows of that team.
# mc (optional, player lines only): calibrated minutes -- the team total
# (includes overtime on average), and a pull toward the team average that
# grows with the projected margin B = |margin| / 10 (blowouts rest starters).
# Playoff games (rot = list(x, cut) from pm_rotation_load) use the playoff
# rotation instead of the pull: multipliers by rotation spot, then the cut.
pl_minutes <- function(expMin, act, playoff, rot, mc = NULL, B = 0) {
  m <- ifelse(act, expMin, 0)
  tot <- if (is.null(mc)) 200 else mc$tot
  m <- pl_norm(m, tot)
  if (playoff && sum(act) && !is.null(rot)) return(pl_rot_apply(m, rot, tot))
  if (!is.null(mc) && sum(act)) {
    mu <- tot / sum(act)
    m <- ifelse(act, pmax(0, mu + (mc$b - mc$c * B) * (m - mu)), 0)
  }
  pl_norm(m, tot)
}
pl_team_adj <- function(tp, m, poss, dep, scale = 1) {
  scale * c(o = (sum((tp$vo - REPL_O) * (m - tp$base)) / 40 + dep[["o"]]) * poss / 100,
            d = (sum((tp$vd - REPL_D) * (m - tp$base)) / 40 + dep[["d"]]) * poss / 100)
}
pl_stats <- function(tp, m, poss, team_pace, fac_opp, T_pts) {
  pf <- poss / team_pace
  out <- list()
  for (s in PL_STATS) out[[s]] <- tp[[paste0("r_", s)]] * m * pf * fac_opp[cbind(tp$pos, s)]
  if (!is.null(T_pts) && sum(out$pts) > 0) out$pts <- out$pts * T_pts / sum(out$pts)
  out
}

# =============================================================================
# ---- player lines: tuned on past seasons, graded on this one ------------------
# =============================================================================
# A scoring table holds, for every player in every past game, the ingredients
# known before tip-off (weighted sums for several recency settings, expected
# minutes for two settings, last season's rates, position averages, what the
# opponent allowed). Any combination of settings can then be scored in a
# fraction of a second, so the whole grid is tried on 2012 onward.
PL_TUNE <- list(rate_hl = c(20, 50, 120, 1000), k = c(10, 30, 60), a = c(0, 0.15, 0.4), S = c(40, Inf), min_hl = c(2, 4, 8))
PL_OLD  <- list(rate_hl = 20, k = 60, a = 0, S = 10, min_hl = 4)          # the settings before tuning
PL_SIDE <- c(pts = 1.5, reb = 0.75, ast = 0.75, tpm = 0.4)               # "disagrees with her average" thresholds

pl_tune_rows <- function(D, g, team_hl, prevR, from_day = 1) {
  days <- sort(unique(g$date)); days <- days[seq_along(days) >= from_day]
  out <- list()
  for (d in as.list(days)) {
    S1 <- pl_snapshot(D, d, list(rate_hl = 20, k = 60, min_hl = PL_TUNE$min_hl[1]), team_hl)
    if (is.null(S1)) next
    pl <- S1$pl; pl$em1 <- pl$expMin
    for (j in seq_along(PL_TUNE$min_hl)[-1]) {
      Sj <- pl_snapshot(D, d, list(rate_hl = 20, k = 60, min_hl = PL_TUNE$min_hl[j]), team_hl)
      pl[[paste0("em", j)]] <- Sj$pl$expMin[match(pl$aid, Sj$pl$aid)] %|na|% 0
    }
    P <- D$P[D$P$date < d & D$P$min > 0, ]
    P <- P[order(P$aid, -as.numeric(P$date)), ]
    pgi <- ave(seq_len(nrow(P)), P$aid, FUN = seq_along) - 1
    for (h in seq_along(PL_TUNE$rate_hl)) {
      w <- 0.5^(pgi / PL_TUNE$rate_hl[h])
      pl[[paste0("W", h, "_min")]] <- (rowsum(w * P$min, P$aid)[, 1])[pl$aid] %|na|% 0
      for (st in PL_STATS) pl[[paste0("W", h, "_", st)]] <- (rowsum(w * P[[st]], P$aid)[, 1])[pl$aid] %|na|% 0
    }
    lgr <- sapply(PL_STATS, function(st) sum(P[[st]]) / sum(P$min))
    for (st in PL_STATS) { pr <- tapply(P[[st]], P$pos, sum) / tapply(P$min, P$pos, sum); pl[[paste0("pr_", st)]] <- unname(pr[pl$pos]) %|na|% lgr[[st]] }
    gd <- g[g$date == d, ]
    for (q in seq_len(nrow(gd))) {
      A <- D$P[D$P$gid == gd$gid[q], ]
      if (!any(A$min > 0)) next
      for (k in 1:2) {
        t <- if (k == 1) gd$home[q] else gd$away[q]; o <- if (k == 1) gd$away[q] else gd$home[q]
        tp <- pl[pl$tid == t, ]; if (!nrow(tp)) next
        At <- A[A$tid == t, ]; ia <- match(tp$aid, At$aid)
        poss <- pl_pace(S1, t) + pl_pace(S1, o) - S1$lgPoss
        fr <- S1$facRaw[[o]]; fn <- S1$facN[[o]] %||% 0
        r <- data.frame(gt = paste(gd$gid[q], t), gid = gd$gid[q], date = d, aid = tp$aid, pos = tp$pos,
                        act = tp$aid %in% At$aid[At$min > 0], gp = tp$gp,
                        # known before tip-off, as on the page: healthy, and not out by default
                        # (unless she played -- a return you'd mark "in")
                        avail = tp$aid %in% At$aid[!At$inj] & (!tp$defaultOut | tp$aid %in% At$aid[At$min > 0]),
                        po = any(At$st == 3),
                        pf = poss / pl_pace(S1, t), fn = fn, B = abs(gd$pm[q]) / 10,
                        T = if (k == 1) (gd$pt[q] + gd$pm[q]) / 2 else (gd$pt[q] - gd$pm[q]) / 2,
                        min_a = At$min[ia] %|na|% 0, stringsAsFactors = FALSE)
        for (j in seq_along(PL_TUNE$min_hl)) r[[paste0("em", j)]] <- tp[[paste0("em", j)]]
        for (st in PL_STATS) {
          r[[paste0(st, "_a")]] <- At[[st]][ia] %|na|% 0
          r[[paste0("avg_", st)]] <- tp[[paste0("avg_", st)]]
          r[[paste0("pr_", st)]] <- tp[[paste0("pr_", st)]]
          r[[paste0("fr_", st)]] <- if (is.null(fr)) 1 else fr[cbind(tp$pos, st)]
        }
        for (h in seq_along(PL_TUNE$rate_hl)) for (st in c("min", PL_STATS)) r[[paste0("W", h, "_", st)]] <- tp[[paste0("W", h, "_", st)]]
        out[[length(out) + 1]] <- r
      }
    }
  }
  if (!length(out)) return(NULL)
  R <- do.call(rbind, out)
  pv <- if (is.null(prevR)) NULL else prevR[match(R$aid, prevR$aid), , drop = FALSE]
  R$Mp <- if (is.null(pv)) 0 else (pv$M %|na|% 0)
  for (st in PL_STATS) R[[paste0("rp_", st)]] <- if (is.null(pv)) 0 else (pv[[st]] %|na|% 0)
  R
}

# the same equations as pl_minutes / pl_snapshot / pl_stats, vectorized over the table.
# The active set is who was available before tip-off (R$avail), as on the page --
# not who actually got in, which the page can't know. rot (optional): playoff
# rotations by minutes half-life (pm_rotation_load); playoff rows then use them.
pl_grp_sum <- function(x, gi) rowsum(x, gi, reorder = FALSE)[as.character(gi), 1]
pl_set <- function(R) if (is.null(R$avail)) R$act else R$avail
pl_eval_minutes <- function(R, min_hl, mc, rot = NULL) {
  gi <- match(R$gt, unique(R$gt)); em <- R[[paste0("em", match(min_hl, PL_TUNE$min_hl))]]
  set <- pl_set(R)
  tot <- if (is.null(mc)) 200 else mc$tot
  renorm <- function(m) { for (it in 1:6) { sm <- pl_grp_sum(m, gi); m <- ifelse(sm > 0, pmin(m * tot / sm, 40), 0) }; m }
  m <- renorm(ifelse(set, em, 0))
  m0 <- m
  if (!is.null(mc)) {
    mu <- tot / pmax(1, pl_grp_sum(as.numeric(set), gi))
    m <- renorm(ifelse(set, pmax(0, mu + (mc$b - mc$c * R$B) * (m - mu)), 0))
  }
  rt <- pm_rotation_for(rot, min_hl)
  if (!is.null(rt) && !is.null(R$po) && any(R$po)) {
    for (g in unique(gi[R$po])) { k <- which(gi == g); m[k] <- pl_rot_apply(m0[k], rt, tot) }
  }
  m
}
pl_eval_stats <- function(R, m, p) {
  gi <- match(R$gt, unique(R$gt)); h <- match(p$rate_hl, PL_TUNE$rate_hl)
  out <- list(min = m)
  for (st in PL_STATS) {
    r <- pl_rate_mix(R[[paste0("W", h, "_", st)]], R[[paste0("W", h, "_min")]], R$Mp, R[[paste0("rp_", st)]], R[[paste0("pr_", st)]], p$a, p$k)
    out[[st]] <- r * m * R$pf * pl_fac_mix(R[[paste0("fr_", st)]], R$fn, p$S)
  }
  sp <- pl_grp_sum(out$pts, gi); out$pts <- ifelse(sp > 0, out$pts * R$T / sp, out$pts)
  out
}
# minutes calibration: team total, pull toward the team average, blowout term.
# Fitted on regular-season games over everyone available before tip-off
# (a DNP counts as 0); playoff games use the playoff rotation instead.
pl_fit_mc <- function(R, min_hl) {
  gi <- match(R$gt, unique(R$gt)); set <- pl_set(R)
  rs <- if (is.null(R$po)) rep(TRUE, nrow(R)) else !R$po
  tot <- mean(rowsum(R$min_a[set & rs], gi[set & rs])[, 1])
  m <- pl_eval_minutes(R, min_hl, list(tot = tot, b = 1, c = 0))
  mu <- tot / pmax(1, pl_grp_sum(as.numeric(set), gi))
  a <- set & rs; x1 <- (m - mu)[a]; x2 <- x1 * R$B[a]; y <- (R$min_a - mu)[a]
  cf <- solve(crossprod(cbind(x1, x2)), crossprod(cbind(x1, x2), y))[, 1]
  list(tot = tot, b = unname(cf[1]), c = unname(-cf[2]))
}
pl_objective <- function(pr, R, ok) sum(vapply(c("pts", "reb", "ast"), function(st) mean(abs(pr[[st]][ok] - R[[paste0(st, "_a")]][ok])), numeric(1)))
pl_metrics <- function(pr, R, ok, phi) {
  lapply(PL_STATS, function(st) {
    y <- R[[paste0(st, "_a")]][ok]; mu <- pmax(pr[[st]][ok], 0.05); av <- R[[paste0("avg_", st)]][ok]
    ph <- suppressWarnings(as.numeric(phi[[st]])); if (!length(ph) || !is.finite(ph)) ph <- 1.5
    ph <- max(ph, 1.01); sd <- sqrt(ph * mu)
    lsc <- function(mm) { mm <- pmax(mm, 0.05); -mean(dnbinom(round(y), mu = mm, size = mm / (ph - 1), log = TRUE)) }
    dis <- abs(mu - av) >= PL_SIDE[[st]] & y != av
    list(stat = st, mae = mean(abs(mu - y)), base = mean(abs(av - y)),
         cover = mean(y >= pmax(0, mu - 1.2816 * sd) & y <= mu + 1.2816 * sd),
         ll = lsc(mu), llBase = lsc(av), side = if (any(dis)) mean(sign(mu[dis] - av[dis]) == sign(y[dis] - av[dis])) else NA, sideN = sum(dis))
  })
}
pm_hist_pred <- function(cal, H, from) {
  par <- list(hl = cal$par$hl, lambda = cal$par$lambda, carry = cal$par$carry); hist <- list()
  for (yr in FIRST_HIST_SEASON:(SEASON - 1)) {
    f <- tryCatch(pm_hist_file(yr), error = function(e) NULL); if (is.null(f)) next
    g <- pm_games_from_box(nanoparquet::read_parquet(f), yr)
    if (nrow(g) > 50) hist[[as.character(yr)]] <- g[order(g$date, g$gid), ]
  }
  pm_backtest(hist, par, H, from)$pred
}

pl_tune <- function(cal, H) {
  yrs <- max(AV_FIRST_SEASON, FIRST_HIST_SEASON + 1):(SEASON - 1)
  pred <- pm_hist_pred(cal, H, min(yrs))
  message("  tuning player lines on ", length(yrs), " past seasons ...")
  t0 <- Sys.time(); Rs <- list()
  prevD <- pl_load_season(min(yrs) - 1); prevR <- if (is.null(prevD)) NULL else pl_prev_rates(prevD$P)
  for (yr in yrs) {
    D <- pl_load_season(yr); if (is.null(D)) { prevR <- NULL; next }
    g <- pred[pred$season == yr & pred$gid %in% D$P$gid, ]
    R <- pl_tune_rows(D, g, cal$par$hl, prevR, from_day = 2)
    if (!is.null(R)) { R$season <- yr; Rs[[length(Rs) + 1]] <- R }
    prevR <- pl_prev_rates(D$P)
  }
  R <- do.call(rbind, Rs); ok <- R$act & R$gp >= 1
  okm <- pl_set(R)                                   # minutes miss: everyone available before tip-off
  RT <- tryCatch(pm_rotation_load(cal), error = function(e) NULL)
  message(sprintf("    %d player-games (%.0fs)", sum(ok), as.numeric(Sys.time() - t0, units = "secs")))
  grid <- expand.grid(rate_hl = PL_TUNE$rate_hl, k = PL_TUNE$k, a = PL_TUNE$a, S = PL_TUNE$S, min_hl = PL_TUNE$min_hl)
  mcs <- lapply(PL_TUNE$min_hl, function(h) pl_fit_mc(R, h)); names(mcs) <- PL_TUNE$min_hl
  ms <- lapply(PL_TUNE$min_hl, function(h) pl_eval_minutes(R, h, mcs[[as.character(h)]], RT)); names(ms) <- PL_TUNE$min_hl
  score <- vapply(seq_len(nrow(grid)), function(i) {
    p <- as.list(grid[i, ]); pl_objective(pl_eval_stats(R, ms[[as.character(p$min_hl)]], p), R, ok)
  }, numeric(1))
  best <- as.list(grid[which.min(score), ]); mc <- mcs[[as.character(best$min_hl)]]
  pr <- pl_eval_stats(R, ms[[as.character(best$min_hl)]], best)
  phi <- sapply(PL_STATS, function(st) { mu <- pmax(pr[[st]][ok], 0.05); sum((R[[paste0(st, "_a")]][ok] - mu)^2) / sum(mu) })
  # the old settings on the same games, for comparison
  pr0 <- pl_eval_stats(R, pl_eval_minutes(R, PL_OLD$min_hl, NULL), PL_OLD)
  early <- ok & ave(as.numeric(R$date), R$season, FUN = function(x) as.numeric(x <= sort(unique(x))[min(length(unique(x)), 15)])) == 1
  message(sprintf("  player lines: rate half-life %g, k %g, last season x%g, opponent shrink %s, minutes half-life %g; minutes total %.1f, pull %.2f, blowout %.3f",
                  best$rate_hl, best$k, best$a, if (is.infinite(best$S)) "off" else best$S, best$min_hl, mc$tot, mc$b, mc$c))
  message(sprintf("    points miss %.3f -> %.3f (season average %.3f); minutes miss %.2f -> %.2f",
                  mean(abs(pr0$pts[ok] - R$pts_a[ok])), mean(abs(pr$pts[ok] - R$pts_a[ok])), mean(abs(R$avg_pts[ok] - R$pts_a[ok])),
                  mean(abs(pr0$min[okm] - R$min_a[okm])), mean(abs(pr$min[okm] - R$min_a[okm]))))
  best_out <- best; best_out$S <- if (is.infinite(best$S)) -1 else best$S          # JSON has no Inf
  list(par = best_out, mc = mc, phi = as.list(phi),
       hist = list(seasons = paste0(min(yrs), "-", max(yrs)), n = sum(ok),
                   stats = pl_metrics(pr, R, ok, phi), old = pl_metrics(pr0, R, ok, phi),
                   minMae = mean(abs(pr$min[okm] - R$min_a[okm])), minMaeOld = mean(abs(pr0$min[okm] - R$min_a[okm])),
                   early = list(n = sum(early), pts = mean(abs(pr$pts[early] - R$pts_a[early])), ptsOld = mean(abs(pr0$pts[early] - R$pts_a[early])),
                                base = mean(abs(R$avg_pts[early] - R$pts_a[early])))))
}
pl_par_in <- function(p) {
  p <- lapply(p, function(x) suppressWarnings(as.numeric(x)))
  if (is.null(p$S) || is.na(p$S) || p$S < 0) p$S <- Inf
  p
}
pl_tune_load <- function(cal, H) {
  f <- file.path(OUT_DIR, ".wnba_cache", paste0("predict_playertune_", SEASON, ".json"))
  key <- paste(PREDICT_VERSION, paste(unlist(PL_TUNE), collapse = ","), cal$made, "avail-rot2")
  if (file.exists(f) && !isTRUE(PREDICT_RECALIBRATE)) {
    old <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(old) && identical(old$key, key)) { message("  using cached player-line tuning from ", old$made); return(old) }
  }
  r <- pl_tune(cal, H); r$key <- key; r$made <- format(Sys.time(), "%Y-%m-%d %H:%M")
  writeLines(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = NA), f)
  r
}

# =============================================================================
# ---- availability: historical, point-in-time calibration ---------------------
# =============================================================================
# How much a lineup change moves a team is fitted on past seasons, using only
# what was known before each game:
#   value   each player's per-40 box offense, box defense and relative +/-
#           (her on-court margin minus her team's), from this season's games
#           so far, blended with last season (weight A) and shrunk toward
#           league average by K minutes.
#   shift   sum over players of value x (her expected minutes given who
#           actually played - her minutes baked into the team rating).
# The margin the team model missed by is regressed on four shifts (offense,
# defense, +/-, and total minutes for the replacement level). K and A are
# chosen by leave-one-season-out error; the reported gains are out of sample.
AV_FIRST_SEASON <- 2012
AV_GRID <- list(K = c(30, 75, 150, 300, 600), a = c(0.5, 1, 1.5, 2.5))
PL_DEFAULT_PAR <- list(rate_hl = 20, k = 60, min_hl = 4)

pl_pace <- function(S, t) { v <- unname(S$pace[t]); if (length(v) != 1 || is.na(v)) S$lgPoss else v }

pm_player_box_file <- function(yr) {
  fn <- paste0("player_box_", yr, ".parquet"); dest <- file.path(OUT_DIR, ".wnba_cache", fn)
  if (release_file_ok(dest) && yr < SEASON) return(dest)
  tryCatch(download_cached(paste0(GH_RELEASES, "/espn_wnba_player_boxscores/", fn), fn, required = FALSE), error = function(e) NULL)
}
pl_load_season <- function(yr) {
  fp <- pm_player_box_file(yr); ft <- tryCatch(pm_hist_file(yr), error = function(e) NULL)
  if (is.null(fp) || is.null(ft)) return(NULL)
  pl_prepare_from(nanoparquet::read_parquet(fp), nanoparquet::read_parquet(ft))
}
# minutes and per-40 rates vs league average, per player
pl_ingredients <- function(P) {
  P <- P[P$min > 0, ]
  if (!nrow(P)) return(data.frame(aid = character(0), M = numeric(0), ro = numeric(0), rd = numeric(0), rp = numeric(0)))
  lo <- sum(P$offr) / sum(P$min) * 40; ld <- sum(P$defr) / sum(P$min) * 40
  M <- rowsum(P$min, P$aid)[, 1]
  data.frame(aid = names(M), M = as.numeric(M),
             ro = rowsum(P$offr, P$aid)[, 1] / M * 40 - lo,
             rd = rowsum(P$defr, P$aid)[, 1] / M * 40 - ld,
             rp = rowsum(P$pmrel, P$aid)[, 1] / M * 40, stringsAsFactors = FALSE)
}
pl_attach <- function(R, ing, prefix) {
  i <- if (is.null(ing)) rep(NA_integer_, nrow(R)) else match(R$aid, ing$aid)
  g <- function(col) if (is.null(ing)) rep(0, nrow(R)) else (ing[[col]][i] %|na|% 0)
  R[[paste0(prefix, "M")]] <- g("M"); R[[paste0(prefix, "o")]] <- g("ro")
  R[[paste0(prefix, "d")]] <- g("rd"); R[[paste0(prefix, "p")]] <- g("rp")
  R
}
# one row per (game, player whose minutes differ from the rating's mix)
pl_av_rows <- function(D, g, plpar, team_hl, prevIng) {
  out <- list(); days <- sort(unique(g$date))
  for (di in seq_along(days)) {
    d <- days[di]
    S <- pl_snapshot(D, d, plpar, team_hl); if (is.null(S)) next
    gd <- g[g$date == d, ]; dayrows <- list()
    for (q in seq_len(nrow(gd))) {
      played <- D$P[D$P$gid == gd$gid[q] & D$P$min > 0, ]
      if (!nrow(played)) next
      poss <- pl_pace(S, gd$home[q]) + pl_pace(S, gd$away[q]) - S$lgPoss
      for (k in 1:2) {
        t <- if (k == 1) gd$home[q] else gd$away[q]; sg <- if (k == 1) 1 else -1
        tp <- S$pl[S$pl$tid == t, ]
        act <- tp$aid %in% played$aid[played$tid == t]
        m <- pl_minutes(tp$expMin, act, FALSE, NULL)
        gone <- S$bk[S$bk$tid == t & !(S$bk$aid %in% tp$aid), ]
        aid <- c(tp$aid, gone$aid); dl <- c(m - tp$base, -gone$base)
        keep <- abs(dl) > 1e-6
        if (any(keep)) dayrows[[length(dayrows) + 1]] <- data.frame(gid = gd$gid[q], aid = aid[keep],
                                                                     dmp = sg * dl[keep] / 40 * poss / 100, stringsAsFactors = FALSE)
      }
    }
    if (length(dayrows)) out[[length(out) + 1]] <- pl_attach(do.call(rbind, dayrows), pl_ingredients(D$P[D$P$date < d, ]), "c")
  }
  if (!length(out)) return(NULL)
  pl_attach(do.call(rbind, out), prevIng, "p")
}
# per-game shift features for given K, A (rows follow gids)
# tgt: optional prior for players with no minutes last season (mostly rookies)
pl_av_design <- function(R, gids, K, a, tgt = NULL) {
  X <- matrix(0, length(gids), 4, dimnames = list(NULL, c("o", "d", "p", "m")))
  if (is.null(R) || !nrow(R)) return(X)
  den <- R$cM + a * R$pM + K
  nw <- as.numeric(R$pM == 0)
  v <- function(x) (R$cM * R[[paste0("c", x)]] + a * R$pM * R[[paste0("p", x)]] + K * nw * (if (is.null(tgt)) 0 else tgt[[x]])) / den
  Z <- R$dmp * cbind(o = v("o"), d = v("d"), p = v("p"), m = 1)
  S <- rowsum(Z, R$gid)
  i <- match(rownames(S), gids); X[i[!is.na(i)], ] <- S[!is.na(i), ]
  X
}
pl_ols <- function(X, y) solve(crossprod(X) + diag(1e-6, ncol(X)), crossprod(X, y))[, 1]

pl_av_calibrate <- function(cal, H) {
  par <- list(hl = cal$par$hl, lambda = cal$par$lambda, carry = cal$par$carry)
  yrs <- max(AV_FIRST_SEASON, FIRST_HIST_SEASON + 1):(SEASON - 1)
  hist <- list()
  for (yr in FIRST_HIST_SEASON:(SEASON - 1)) {
    f <- tryCatch(pm_hist_file(yr), error = function(e) NULL); if (is.null(f)) next
    g <- pm_games_from_box(nanoparquet::read_parquet(f), yr)
    if (nrow(g) > 50) hist[[as.character(yr)]] <- g[order(g$date, g$gid), ]
  }
  pred <- pm_backtest(hist, par, H, min(yrs))$pred
  message("  fitting availability on ", length(yrs), " past seasons (point-in-time player values) ...")
  t0 <- Sys.time(); Rs <- list(); Gs <- list(); NW <- NULL
  prevD <- pl_load_season(min(yrs) - 1); prevIng <- if (is.null(prevD)) NULL else pl_ingredients(prevD$P)
  for (yr in yrs) {
    D <- pl_load_season(yr)
    if (is.null(D)) { prevIng <- NULL; next }
    g <- pred[pred$season == yr & pred$gid %in% D$P$gid, ]
    R <- pl_av_rows(D, g, PL_DEFAULT_PAR, par$hl, prevIng)
    Rs[[length(Rs) + 1]] <- R; Gs[[length(Gs) + 1]] <- g
    ing <- pl_ingredients(D$P); nw <- !(ing$aid %in% (if (is.null(prevIng)) character(0) else prevIng$aid))
    NW <- rbind(NW, data.frame(M = ing$M[nw], o = ing$ro[nw], d = ing$rd[nw], p = ing$rp[nw]))
    prevIng <- ing
  }
  R <- do.call(rbind, Rs); G <- do.call(rbind, Gs)
  message(sprintf("    %d games, %d player rows (%.0fs)", nrow(G), nrow(R), as.numeric(Sys.time() - t0, units = "secs")))
  y <- G$am - G$pm
  tgt <- list(o = sum(NW$M * NW$o) / sum(NW$M), d = sum(NW$M * NW$d) / sum(NW$M), p = sum(NW$M * NW$p) / sum(NW$M))
  grid <- expand.grid(K = AV_GRID$K, a = AV_GRID$a, rookie = c(FALSE, TRUE))
  loso <- function(X) {
    yh <- numeric(length(y))
    for (s in unique(G$season)) { tr <- G$season != s; yh[!tr] <- X[!tr, , drop = FALSE] %*% pl_ols(X[tr, , drop = FALSE], y[tr]) }
    yh
  }
  tg <- function(i) if (grid$rookie[i]) tgt else NULL
  scores <- vapply(seq_len(nrow(grid)), function(i) mean((y - loso(pl_av_design(R, G$gid, grid$K[i], grid$a[i], tg(i))))^2), numeric(1))
  bi <- which.min(scores); K <- grid$K[bi]; a <- grid$a[bi]; rookie <- grid$rookie[bi]
  best_plain <- min(scores[!grid$rookie]); best_rookie <- min(scores[grid$rookie])
  message(sprintf("  no-minutes-last-season prior (off %.2f, def %.2f, +/- %+.2f); held-out RMSE plain %.4f vs rookie prior %.4f -> %s",
                  tgt$o, tgt$d, tgt$p, sqrt(best_plain), sqrt(best_rookie), if (rookie) "used" else "not used"))
  X <- pl_av_design(R, G$gid, K, a, if (rookie) tgt else NULL); yh <- loso(X); b <- pl_ols(X, y)
  se <- sqrt(diag(solve(crossprod(X))) * sum((y - X %*% b)^2) / (length(y) - 4))
  won <- as.numeric(G$am > 0); sg <- cal$sigma_m
  oos <- list(n = length(y), ll0 = pm_logloss(pnorm(G$pm / sg), won), ll1 = pm_logloss(pnorm((G$pm + yh) / sg), won),
              mae0 = mean(abs(y)), mae1 = mean(abs(y - yh)), rmse0 = sqrt(mean(y^2)), rmse1 = sqrt(mean((y - yh)^2)))
  by_season <- lapply(split(seq_along(y), G$season), function(ix) list(season = G$season[ix[1]], n = length(ix),
    ll0 = pm_logloss(pnorm(G$pm[ix] / sg), won[ix]), ll1 = pm_logloss(pnorm((G$pm[ix] + yh[ix]) / sg), won[ix]),
    mae0 = mean(abs(y[ix])), mae1 = mean(abs(y[ix] - yh[ix]))))
  message(sprintf("  availability: K %d min, last season x%.2f; weights off %.2f (se %.2f), def %.2f (%.2f), +/- %.2f (%.2f), minutes %.2f (%.2f)",
                  K, a, b[["o"]], se[["o"]], b[["d"]], se[["d"]], b[["p"]], se[["p"]], b[["m"]], se[["m"]]))
  message(sprintf("  held-out: log loss %.4f -> %.4f, margin miss %.2f -> %.2f", oos$ll0, oos$ll1, oos$mae0, oos$mae1))
  list(K = K, a = a, rookie = rookie, tgt = tgt, rmsePlain = sqrt(best_plain), rmseRookie = sqrt(best_rookie),
       coef = as.list(b), se = as.list(se), oos = oos, bySeason = unname(by_season),
       grid = lapply(seq_len(nrow(grid)), function(i) list(K = grid$K[i], a = grid$a[i], rookie = grid$rookie[i], rmse = sqrt(scores[i]))),
       seasons = paste0(min(yrs), "-", max(yrs)))
}
pl_av_load <- function(cal, H) {
  f <- file.path(OUT_DIR, ".wnba_cache", paste0("predict_avail_", SEASON, ".json"))
  key <- paste(PREDICT_VERSION, AV_FIRST_SEASON, paste(unlist(AV_GRID), collapse = ","), "rookie-prior", cal$version, cal$made)
  if (file.exists(f) && !isTRUE(PREDICT_RECALIBRATE)) {
    old <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(old) && identical(old$key, key)) { message("  using cached availability fit from ", old$made); return(old) }
  }
  r <- pl_av_calibrate(cal, H)
  r$key <- key; r$made <- format(Sys.time(), "%Y-%m-%d %H:%M")
  writeLines(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = NA), f)
  r
}
# put each player's effective value on the snapshot (o and d parts; the +/- and
# replacement terms are split evenly, which leaves the margin effect exact)
pl_apply_values <- function(S, P_before, prevIng, AV) {
  cur <- pl_ingredients(P_before); b <- AV$coef
  val <- function(aid) {
    R <- pl_attach(pl_attach(data.frame(aid = aid, stringsAsFactors = FALSE), cur, "c"), prevIng, "p")
    den <- R$cM + AV$a * R$pM + AV$K
    tg <- if (isTRUE(AV$rookie)) AV$tgt else NULL; nw <- as.numeric(R$pM == 0)
    v <- function(x) (R$cM * R[[paste0("c", x)]] + AV$a * R$pM * R[[paste0("p", x)]] + AV$K * nw * (if (is.null(tg)) 0 else as.numeric(tg[[x]]))) / den
    sh <- (b$p * v("p") + b$m) / 2
    list(o = b$o * v("o") + sh, d = b$d * v("d") + sh)
  }
  v <- val(S$pl$aid); S$pl$vo <- v$o; S$pl$vd <- v$d
  gone <- S$bk[!paste(S$bk$tid, S$bk$aid) %in% paste(S$pl$tid, S$pl$aid), ]
  gone$o <- 0; gone$d <- 0
  if (nrow(gone)) { gv <- val(gone$aid); gone$o <- -gv$o * gone$base / 40; gone$d <- -gv$d * gone$base / 40 }
  S$dep <- setNames(lapply(names(S$dep), function(t) { g <- gone[gone$tid == t, ]; c(o = sum(g$o), d = sum(g$d)) }), names(S$dep))
  S
}
pm_walk <- function(g, teams, prior, par, H) {
  days <- sort(unique(g$date))
  do.call(rbind, lapply(seq_along(days), function(i) {
    td <- g[g$date == days[i], ]
    fit <- pm_fit(g[g$date < days[i], ], teams, prior, days[i], par, H)
    pr <- pm_predict(fit, td$home, td$away, td$neutral, H)
    data.frame(gid = td$gid, date = td$date, home = td$home, away = td$away, pm = pr$margin, pt = pr$total, am = td$hs - td$as, stringsAsFactors = FALSE)
  }))
}

# This season's check (player lines with the tuned settings, and the lineup
# effect) reruns weekly; nothing in it was used for tuning.
PL_RECAL_DAYS <- 7
pl_load_calibration <- function(D, cur, teams, prior, par_t, H, SIG, AV, prevIng, PT, prevR) {
  f <- file.path(OUT_DIR, ".wnba_cache", paste0("predict_players_", SEASON, ".json"))
  key <- paste(PREDICT_VERSION, PT$made %||% "none", DEFAULT_OUT_GAMES, AV$made %||% "none")
  if (file.exists(f) && !isTRUE(PREDICT_RECALIBRATE)) {
    old <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(old) && identical(old$key, key) && !is.null(old$made) &&
        difftime(Sys.time(), as.POSIXct(old$made), units = "days") < PL_RECAL_DAYS) {
      message("  using cached player check from ", old$made)
      return(old)
    }
  }
  g <- pm_walk(cur, teams, prior, par_t, H)
  days <- sort(unique(g$date))
  if (length(days) <= PL_MIN_DATES + 5) stop("not enough games yet this season to check player lines")
  g <- g[g$date >= days[PL_MIN_DATES + 1], ]
  r <- list(par = PT$par, mc = PT$mc, phi = PT$phi, hist = PT$hist,
            days = length(unique(g$date)), from = format(min(g$date)), to = format(max(g$date)))
  R <- pl_tune_rows(D, g, par_t$hl, prevR)
  ok <- R$act & R$gp >= 1; okm <- pl_set(R)
  p <- pl_par_in(PT$par)
  RT <- tryCatch(pm_rotation_load(list(par = list(hl = par_t$hl))), error = function(e) NULL)
  pr <- pl_eval_stats(R, pl_eval_minutes(R, p$min_hl, PT$mc, RT), p)
  pr0 <- pl_eval_stats(R, pl_eval_minutes(R, PL_OLD$min_hl, NULL), PL_OLD)
  r$n <- sum(ok); r$stats <- pl_metrics(pr, R, ok, PT$phi); r$old <- pl_metrics(pr0, R, ok, PT$phi)
  r$minMae <- mean(abs(pr$min[okm] - R$min_a[okm])); r$minMaeOld <- mean(abs(pr0$min[okm] - R$min_a[okm]))
  message(sprintf("  this season (held out): points miss %.3f (before tuning %.3f, season average %.3f); minutes %.2f (before %.2f)",
                  r$stats[[1]]$mae, r$old[[1]]$mae, r$stats[[1]]$base, r$minMae, r$minMaeOld))
  if (!is.null(AV)) {
    Ra <- pl_av_rows(D, g, PL_DEFAULT_PAR, par_t$hl, prevIng)
    X <- pl_av_design(Ra, g$gid, AV$K, AV$a, if (isTRUE(AV$rookie)) lapply(AV$tgt, as.numeric) else NULL)
    yh <- as.numeric(X %*% unlist(AV$coef)[c("o", "d", "p", "m")]); y <- g$am - g$pm; won <- as.numeric(g$am > 0)
    r$team <- list(n = nrow(g), ll0 = pm_logloss(pnorm(g$pm / SIG), won), ll1 = pm_logloss(pnorm((g$pm + yh) / SIG), won),
                   mae0 = mean(abs(y)), mae1 = mean(abs(y - yh)),
                   acc0 = mean((g$pm > 0) == (won == 1)), acc1 = mean(((g$pm + yh) > 0) == (won == 1)))
  }
  r$key <- key; r$made <- format(Sys.time(), "%Y-%m-%d %H:%M")
  writeLines(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = NA), f)
  r
}

pl_payload <- function(S, teams, plcal, use_adj, adj_scale = 1, mc = NULL) {
  r1 <- function(x) round(x, 4)
  by_team <- lapply(teams, function(t) {
    tp <- S$pl[S$pl$tid == t, ]
    tp <- tp[order(-tp$expMin), ]
    lapply(seq_len(nrow(tp)), function(i) list(
      id = tp$aid[i], name = tp$name[i], pos = tp$pos[i], expMin = r1(tp$expMin[i]), expMinAdj = r1(tp$expMinAdj[i]), base = r1(tp$base[i]),
      r = list(pts = r1(tp$r_pts[i]), reb = r1(tp$r_reb[i]), ast = r1(tp$r_ast[i]), tpm = r1(tp$r_tpm[i])),
      vo = tp$vo[i], vd = tp$vd[i], hasValue = tp$vsrc[i], out = tp$defaultOut[i],
      last = tp$lastPlayed[i], reason = if (is.na(tp$reason[i])) NULL else tp$reason[i],
      gp = tp$gp[i], mpg = round(tp$mpg[i], 1), ppg = round(tp$avg_pts[i], 1),
      missed = if (is.na(tp$missed[i])) NULL else tp$missed[i]))
  })
  names(by_team) <- teams
  list(byTeam = by_team, pace = as.list(S$pace[teams]), lgPoss = S$lgPoss,
       fac = lapply(S$fac[teams], function(f) lapply(c(G = "G", F = "F", C = "C"), function(p) as.list(f[p, ]))),
       dep = lapply(S$dep[teams], as.list), repl = list(o = REPL_O, d = REPL_D), useAdj = use_adj, adjScale = adj_scale,
       defaultOutGames = DEFAULT_OUT_GAMES,
       mc = mc,
       test = if (is.null(plcal)) NULL else plcal[c("par", "phi", "n", "days", "from", "to", "stats", "old", "hist", "minMae", "minMaeOld", "team", "made")])
}

message("Building predictions ...")
t_pred <- Sys.time()
predict_payload <- tryCatch(pm_run(), error = function(e) {
  message("  WARNING: prediction module failed -- the Predictions tab will show this message. (",
          conditionMessage(e), ")")
  list(ok = FALSE, error = conditionMessage(e))
})
message(sprintf("  predictions done in %.0fs", as.numeric(Sys.time() - t_pred, units = "secs")))
