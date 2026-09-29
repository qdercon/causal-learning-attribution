# Figure 1 -- symptom change =====================================================
# pre-registered ANCOVAs of questionnaire change by arm (H1), the sessions-completed
# analyses (H2), a cLDA sensitivity analysis over all randomised participants, and
# exploratory DAQ subscales.
library(patchwork)
source("analyses/model_fns.R")

## 0. data ------------------------------------------------------------------------
# sessions_completed == 6 is the per-protocol flag (verified against the raw training csvs)
self_report_df <- readr::read_csv("./data/self-report-combined-data_scrambled.csv", col_types = readr::cols())

sessions_completed_df <- self_report_df |>
  dplyr::distinct(prolificSubID, sessions_completed)

qqnrs_t2 <- readr::read_csv("./data/qqnrs_t2_scrambled.csv", col_types = readr::cols()) |>
  dplyr::mutate(group = factor(group, levels = c(ARM_REF, ARM_INT))) |>
  dplyr::left_join(sessions_completed_df, by = c("subID" = "prolificSubID")) |>
  dplyr::mutate(pp = sessions_completed == 6)
stopifnot(sum(is.na(qqnrs_t2$sessions_completed)) == 0) # every subID must join

## 0b. DAQ subscales (EXPLORATORY) ---------------------------------------------------
# post hoc: only the "I" subscale measures what H1b predicts, so all four subscales are
# scored and reported together (the pre-registration names the total).
#   I = internal attribution of negative / external attribution of positive events
#   H = perceived helplessness   S = stable cause of negative   G = global attribution of negative
daq_subscales <- list(I = c(1, 4, 5, 8), H = c(2, 3, 7, 10), S = c(6, 11, 12, 15), G = c(9, 13, 14, 16))
stopifnot(setequal(unlist(daq_subscales), 1:16), !anyDuplicated(unlist(daq_subscales)))

daq_items <- self_report_df |>
  dplyr::filter(catch_1_das_corr == 1, catch_2_erqcr_corr == 1, sessionNo %in% c(0, 1)) |>
  dplyr::select(subID = prolificSubID, sessionNo, tidyselect::all_of(paste0("DAQ_", 1:16)))

# items must reconstruct DAQ_total exactly
daq_check <- self_report_df |>
  dplyr::filter(!is.na(DAQ_total)) |>
  dplyr::mutate(item_sum = rowSums(dplyr::across(tidyselect::all_of(paste0("DAQ_", 1:16)))))
stopifnot(
  all(daq_check$item_sum == daq_check$DAQ_total, na.rm = TRUE),
  all(unlist(daq_items[paste0("DAQ_", 1:16)]) %in% 0:4, na.rm = TRUE)
)

for (nm in names(daq_subscales)) {
  daq_items[[nm]] <- rowSums(daq_items[, paste0("DAQ_", daq_subscales[[nm]])])
}
daq_prefixes <- stats::setNames(paste0("DAQ", names(daq_subscales)), names(daq_subscales))

qqnrs_t2 <- Reduce(function(acc, nm) {
  wide <- daq_items |>
    dplyr::select(subID, sessionNo, tidyselect::all_of(nm)) |>
    tidyr::pivot_wider(names_from = sessionNo, values_from = tidyselect::all_of(nm),
                       names_prefix = "s")
  # sessionNo 0 = prescreen (t1), 1 = postscreen (t2); renamed explicitly, not by pattern
  stopifnot(setequal(setdiff(names(wide), "subID"), c("s0", "s1")))
  names(wide)[names(wide) == "s0"] <- paste0(daq_prefixes[[nm]], "_total_t1")
  names(wide)[names(wide) == "s1"] <- paste0(daq_prefixes[[nm]], "_total_t2")
  dplyr::left_join(acc, wide, by = "subID")
}, names(daq_subscales), init = qqnrs_t2)

# subscales must cover exactly the same participants as the total they came from
stopifnot(
  nrow(qqnrs_t2) == 361,
  vapply(daq_prefixes, function(p) {
    sum(!is.na(qqnrs_t2[[paste0(p, "_total_t1")]])) == sum(!is.na(qqnrs_t2$DAQ_total_t1)) &&
      sum(!is.na(qqnrs_t2[[paste0(p, "_total_t2")]])) == sum(!is.na(qqnrs_t2$DAQ_total_t2))
  }, logical(1)),
  # and must sum back to the total at both timepoints
  all(abs(rowSums(qqnrs_t2[paste0(daq_prefixes, "_total_t1")]) - qqnrs_t2$DAQ_total_t1) < 1e-8, na.rm = TRUE),
  all(abs(rowSums(qqnrs_t2[paste0(daq_prefixes, "_total_t2")]) - qqnrs_t2$DAQ_total_t2) < 1e-8, na.rm = TRUE)
)

qqnrs_t2 |>
  dplyr::group_by(group) |>
  # summarise sessions_completed
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(sessions_completed),
    median = stats::median(sessions_completed),
    iqr = stats::IQR(sessions_completed),
    sd   = stats::sd(sessions_completed),
    range = paste0(min(sessions_completed), "-", max(sessions_completed))
  )

## 1. measure metadata --------------------------------------------------------------
# pre-registered primaries first (PHQ-9, H1a; DAQ, H1b); `pretty_nms` in section 6 must match.
# PHQ-2 is omitted: it was the screening criterion and is 2 of the PHQ-9's items
measure_meta <- tibble::tribble(
  ~measure,    ~prefix,    ~title,               ~subtitle,                                                  ~ylab,                            ~tier,        #nolint
  "PHQ-9",     "PHQ9",     "PHQ-9",              "patient health questionnaire",                             "PHQ-9 total (/27)",              "primary",    #nolint
  "DAQ",       "DAQ",      "DAQ",                "depressive attributions questionnaire",                    "DAQ total (/64)",                "primary",    #nolint
  "DAS",       "DAS",      "DAS",                "dysfunctional attitudes scale",                            "DAS total (/36)",                "secondary",  #nolint
  "ERQ-CR",    "ERQCR",    "ERQ-CR<sup>†</sup>", "emotion regulation questionnaire\n(cognitive reappraisal)", "ERQ-CR<sup>†</sup> total (/36)", "secondary",  #nolint
  "miniSPIN",  "miniSPIN", "miniSPIN",           "mini social phobia inventory",                             "miniSPIN total (/12)",           "secondary"   #nolint
)
main_measures <- measure_meta$measure
measure_tier  <- stats::setNames(measure_meta$tier, measure_meta$measure)
stopifnot(measure_meta$tier[1:2] == "primary", !any(measure_meta$tier[-(1:2)] == "primary"))

prefix_to_measure <- stats::setNames(measure_meta$measure, measure_meta$prefix)
total_cols <- as.vector(outer(paste0(measure_meta$prefix, "_total_"), c("t1", "t2"), paste0))

## 2. long/summary frames for the per-measure slope panels --------------------------
slope_long <- qqnrs_t2 |>
  dplyr::select(subID, group, tidyselect::all_of(total_cols)) |>
  tidyr::pivot_longer(
    cols = -c(subID, group), names_to = c("measure", "timepoint"), names_pattern = "(.*)_total_(t[12])"
  ) |>
  dplyr::mutate(
    measure   = factor(prefix_to_measure[measure], levels = main_measures),
    timepoint = factor(timepoint, levels = c("t1", "t2"), labels = c("baseline", "post-intervention")),
    subID     = factor(subID)
  ) |>
  tidyr::drop_na(value)

slope_summ <- slope_long |>
  dplyr::group_by(measure, group, timepoint) |>
  dplyr::summarise(
    mean_score = mean(value),
    n          = dplyr::n(),
    se         = stats::sd(value) / sqrt(dplyr::n()),
    ci         = stats::qt(0.975, dplyr::n() - 1) * (stats::sd(value) / sqrt(dplyr::n())),
    .groups    = "drop"
  )

# per-measure overall (both arms pooled) pre->post effect size
compute_panel_stats <- function(meas, col_prefix) {
  t1 <- qqnrs_t2[[paste0(col_prefix, "_total_t1")]]
  t2 <- qqnrs_t2[[paste0(col_prefix, "_total_t2")]]
  keep <- !is.na(t1) & !is.na(t2)
  t1 <- t1[keep]
  t2 <- t2[keep]

  d_av <- effectsize::repeated_measures_d(t2, t1, method = "av", adjust = FALSE)[["d_av"]]
  ttp <- stats::t.test(t2, t1, paired = TRUE)

  tibble::tibble(
    measure = meas, d_av = d_av, raw_change = mean(t2 - t1), t_stat_timepoint = unname(ttp$statistic),
    p_star_timepoint = as.character(stats::symnum(
      ttp$p.value, corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", "•", "ns")
    ))
  )
}

panel_stats <- dplyr::bind_rows(
  lapply(seq_len(nrow(measure_meta)), function(i) compute_panel_stats(measure_meta$measure[i], measure_meta$prefix[i]))
) |>
  dplyr::mutate(measure = factor(measure, levels = main_measures))

## 3. shared aesthetics --------------------------------------------------------------
grp_cols    <- ARM_COLS[c(ARM_REF, ARM_INT)]
names(grp_cols) <- c(ARM_REF, ARM_INT)
grp_shapes  <- ARM_SHP[c(ARM_REF, ARM_INT)]
grp_lty     <- c(control = "solid", causal = "32")
grp_labs    <- ARM_LABS[c(ARM_REF, ARM_INT)]
grp_labs_br <- ARM_BRK[c(ARM_REF, ARM_INT)]
dodge_w     <- 0.45

# blends toward white (rather than alpha) so tints are predictable over any background
lighten_col <- function(col, amt = 0.4) {
  rgb <- grDevices::col2rgb(col)
  grDevices::rgb(t(rgb + (255 - rgb) * amt), maxColorValue = 255)
}
grp_cols_pp <- c(
  control_itt = unname(ARM_COLS[["control"]]), causal_itt = unname(ARM_COLS[["causal"]]),
  control_pp  = "#6e241e", causal_pp = "#c9922f"
)
grp_shapes_pp <- c(control_itt = 16, causal_itt = 17, control_pp = 1, causal_pp = 2)
grp_labs_pp <- c(
  control_itt = "control learning (ITT)", causal_itt = "causal learning (ITT)",
  control_pp  = "control learning (PP)", causal_pp = "causal learning (PP)"
)
grp_labs_br_pp <- c(
  control_itt = "control\n(ITT)", causal_itt = "causal\n(ITT)",
  control_pp  = "control\n(PP)", causal_pp = "causal\n(PP)"
)

## 4. supplementary S2 panels A-E: per-measure pre->post slope ------------------------
# NB d_av and the bracket stars are the pooled (both-arm) pre->post paired test, not the arm contrast
tag_std <- ggplot2::element_text(family = "Open Sans", face = "bold", size = 28, colour = "black")

panel_list <- lapply(seq_len(nrow(measure_meta)), function(i) {
  mm <- measure_meta[i, ]
  plot_qnr_measure(
    mm$measure, mm$title, mm$subtitle, mm$ylab, tag = LETTERS[i], dodge_w = dodge_w,
    tag_theme = tag_std
  )
})

## 5a. pre-registered ANCOVA: final score ~ arm + baseline ---------------------------
ancova_cache_dir <- "analyses/stan-fits/qnr"
# fixed seed on every brm(), so the lm/gls cross-checks below are deterministic
ANCOVA_SEED <- 20260811
dir.create(ancova_cache_dir, showWarnings = FALSE, recursive = TRUE)

# the power calculation's target (d = 0.3), used only for a one-sided rope on the primaries
ROPE_THRESH <- 0.3

# NB "ITT" is a label only: the ANCOVA is complete-case (everyone with outcome data, n = 292
# of 361) -- report it as "with outcome data". "PP" = the 6/6-sessions subset (n = 212)
ancova_frames <- list(ITT = qqnrs_t2, PP = qqnrs_t2[qqnrs_t2$pp, ])

stopifnot(
  sum(!is.na(qqnrs_t2$PHQ9_total_t2) & qqnrs_t2$sessions_completed == 0) == 0, # mITT == ITT
  all(ancova_frames$PP$subID %in% ancova_frames$ITT$subID)                     # PP subset of ITT
)

# centring / standardising constants from the ITT frame only, reused for PP
ancova_const <- stats::setNames(
  lapply(measure_meta$prefix, function(p) ancova_constants(ancova_frames$ITT, p)),
  measure_meta$measure
)

ancova_fits <- list()
ancova_draws <- list()
ancova_summ <- list()

for (i in seq_len(nrow(measure_meta))) {
  meas <- measure_meta$measure[i]
  pfx  <- measure_meta$prefix[i]
  cst  <- ancova_const[[meas]]

  for (fr in names(ancova_frames)) {
    df  <- make_ancova_df(ancova_frames[[fr]], pfx, cst$t1_mean)
    fit <- brms::brm(
      formula = stats::as.formula(t2 ~ group + t1_c), data = df,
      family = stats::gaussian(),
      chains = 4, cores = 4, iter = 6000, warmup = 2000,
      backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
      file = file.path(ancova_cache_dir, paste0("ancova_", pfx, "_", fr)),
      file_refit = "on_change"
    )
    cd  <- ancova_contrast_draws(fit)

    # cross-check against lm() (flat priors, so they agree to mc error): absolute 0.02 sd tolerance
    b_lm <- stats::coef(stats::lm(t2 ~ group + t1_c, data = df))[[paste0("group", ARM_INT)]]
    b_bayes <- mean(cd$contrast$.value)
    stopifnot(abs(b_bayes - b_lm) <= 0.02 * cst$sd_t2)

    key <- paste0(meas, "|", fr)
    ancova_fits[[key]]  <- fit
    ancova_draws[[key]] <- cd
    # rope for the primary tier only (what the power calculation targeted)
    rope_thresh_i <- if (measure_tier[[meas]] == "primary") ROPE_THRESH else NULL
    ancova_summ[[key]]  <- summarise_ancova_draws(
      fit, cd$contrast, cst, meas, fr, nrow(df), rope_thresh = rope_thresh_i
    ) |>
      dplyr::mutate(tier = measure_tier[[meas]], b_lm = b_lm, .after = frame)
  }
}

ancova_tbl <- dplyr::bind_rows(ancova_summ) |>
  dplyr::mutate(measure = factor(measure, levels = main_measures)) |>
  dplyr::arrange(frame, measure)

stopifnot(
  ancova_tbl$n[ancova_tbl$frame == "ITT"] == 292,
  ancova_tbl$n[ancova_tbl$frame == "PP"] == 212
)

for (fr in names(ancova_frames)) {
  readr::write_csv(
    ancova_tbl |> dplyr::filter(frame == fr),
    file = paste0("analyses/outputs/ancova_", tolower(fr), ".csv")
  )
}
readr::write_csv(
  ancova_tbl |>
    dplyr::select(measure, frame, n, rhat_max, ess_bulk_min, ess_tail_min, divergent),
  file = "analyses/outputs/ancova_diagnostics.csv"
)

# long draws for the forest panels, standardised by the (constant) ITT sd(t2)
ancova_draws_long <- dplyr::bind_rows(lapply(names(ancova_draws), function(k) {
  parts <- strsplit(k, "|", fixed = TRUE)[[1]]
  tibble::tibble(
    measure = parts[1], frame = parts[2], tier = measure_tier[[parts[1]]],
    .draw = ancova_draws[[k]]$contrast$.draw,
    .value = ancova_draws[[k]]$contrast$.value / ancova_const[[parts[1]]]$sd_t2
  )
})) |>
  dplyr::mutate(
    measure = factor(measure, levels = rev(main_measures)),
    frame   = factor(frame, levels = c("ITT", "PP"))
  )

## 5b. hypothesis 2: does the effect scale with amount of training? -------------------
# two forms: the pre-registered continuous sessions x arm interaction (barely identifiable --
# only 31 control participants completed < 6 sessions) and a binary completed-all-6 interaction
completion_lvls <- c("partial", "complete")

h2_fits <- list()
h2_draws <- list()

for (i in seq_len(nrow(measure_meta))) {
  meas <- measure_meta$measure[i]
  pfx  <- measure_meta$prefix[i]
  cst  <- ancova_const[[meas]]

  df <- make_ancova_df(ancova_frames$ITT, pfx, cst$t1_mean, extra_cols = "sessions_completed")
  df$sessions_c <- df$sessions_completed - mean(df$sessions_completed)
  df$completion <- factor(
    ifelse(df$sessions_completed == 6, "complete", "partial"), levels = completion_lvls
  )
  stopifnot(levels(df$completion)[1] == "partial", all(df$sessions_completed %in% 1:6))

  pth_add <- file.path(ancova_cache_dir, paste0("ancova_", pfx, "_ITT_sessadd"))
  pth_int <- file.path(ancova_cache_dir, paste0("ancova_", pfx, "_ITT_sessint"))
  pth_cmp <- file.path(ancova_cache_dir, paste0("ancova_", pfx, "_ITT_complint"))

  fit_add <- brms::brm(
    formula = stats::as.formula(t2 ~ group + t1_c + sessions_c), data = df,
    family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = pth_add,
    file_refit = "on_change"
  )
  fit_int <- brms::brm(
    formula = stats::as.formula(t2 ~ group * sessions_c + t1_c), data = df,
    family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = pth_int,
    file_refit = "on_change"
  )
  fit_cmp <- brms::brm(
    formula = stats::as.formula(t2 ~ group * completion + t1_c), data = df,
    family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = pth_cmp,
    file_refit = "on_change"
  )

  b_sess <- paste0("b_group", ARM_INT, ":sessions_c")
  b_cmpl <- paste0("b_group", ARM_INT, ":completioncomplete")

  h2_fits[[meas]]  <- list(add = fit_add, int = fit_int, cmp = fit_cmp)
  h2_draws[[meas]] <- list(
    arm_adjusted  = ancova_contrast_draws(fit_add)$contrast$.value,
    # continuous: per-arm slope of t2 on sessions; the arm difference IS the interaction
    slope_control = coef_draws(fit_int, "b_sessions_c"),
    slope_causal  = coef_draws(fit_int, "b_sessions_c", b_sess),
    slope_diff    = coef_draws(fit_int, b_sess),
    # binary: the arm effect within each completion stratum ("partial" is the reference
    # level, so its arm effect is the plain group coefficient); difference IS the interaction
    arm_partial   = coef_draws(fit_cmp, paste0("b_group", ARM_INT)),
    arm_complete  = coef_draws(fit_cmp, paste0("b_group", ARM_INT), b_cmpl),
    arm_cmp_diff  = coef_draws(fit_cmp, b_cmpl),
    sd_t2         = cst$sd_t2
  )

  # cross-check both interactions against lm(): absolute 0.02 sd tolerance (not a ratio,
  # which is meaningless on near-null coefficients)
  check_matches_lm <- function(bayes_v, lm_b, sd_t2) {
    b <- mean(bayes_v)
    stopifnot(abs(b - lm_b) <= 0.02 * sd_t2)
    # sign agreement only where the effect is big enough for a sign to be meaningful
    if (abs(lm_b) > 0.02 * sd_t2) stopifnot(sign(b) == sign(lm_b))
  }
  check_matches_lm(
    h2_draws[[meas]]$slope_diff,
    stats::coef(stats::lm(t2 ~ group * sessions_c + t1_c, data = df))[[
      paste0("group", ARM_INT, ":sessions_c")
    ]],
    cst$sd_t2
  )
  check_matches_lm(
    h2_draws[[meas]]$arm_cmp_diff,
    stats::coef(stats::lm(t2 ~ group * completion + t1_c, data = df))[[
      paste0("group", ARM_INT, ":completioncomplete")
    ]],
    cst$sd_t2
  )
}

# long, standardised draws for panels C (per-arm slopes) and E (arm contrast per completion stratum)
make_h2_draws_long <- function(term_keys) {
  dplyr::bind_rows(lapply(names(h2_draws), function(m) {
    dplyr::bind_rows(lapply(names(term_keys), function(tm) {
      tibble::tibble(
        measure = m, term = tm, tier = measure_tier[[m]],
        .value = h2_draws[[m]][[term_keys[[tm]]]] / h2_draws[[m]]$sd_t2
      )
    }))
  })) |>
    dplyr::mutate(
      measure = factor(measure, levels = rev(main_measures)),
      term    = factor(term, levels = names(term_keys))
    )
}

sessions_draws_long <- make_h2_draws_long(stats::setNames(
  c("slope_control", "slope_causal", "slope_diff"), c(ARM_REF, ARM_INT, "difference")
))
completion_draws_long <- make_h2_draws_long(stats::setNames(
  c("arm_partial", "arm_complete", "arm_cmp_diff"), c("partial", "complete", "difference")
))

sessions_term_cols <- ARM_COLS[c(ARM_REF, ARM_INT, "difference")]
completion_term_cols <- c(
  partial = "#b9b9b8", complete = "#8b8b99", difference = unname(ARM_COLS[["difference"]])
)

## 5c. cLDA sensitivity analysis: all 361 randomised ------------------------------------
# constrained longitudinal data analysis over all 361 (shared baseline mean; post:group is the
# arm difference in change) -- tests whether the complete-case restriction matters
clda_fits <- list()
clda_draws <- list()

for (i in seq_len(nrow(measure_meta))) {
  meas <- measure_meta$measure[i]
  pfx  <- measure_meta$prefix[i]
  cst  <- ancova_const[[meas]]
  dl   <- make_clda_df(ancova_frames$ITT, pfx)

  # assert the constraint (a wrong formula still fits and returns a plausible number)
  mm_c <- stats::model.matrix(~ post + post:group, data = dl)
  b_grp <- paste0("post:group", ARM_INT)
  stopifnot(
    !(paste0("group", ARM_INT) %in% colnames(mm_c)), # no free baseline arm difference
    b_grp %in% colnames(mm_c),
    all(mm_c[dl$post == 0L, b_grp] == 0),            # group column is 0 at baseline
    nrow(dl) == sum(!is.na(ancova_frames$ITT[[paste0(pfx, "_total_t1")]])) +
      sum(!is.na(ancova_frames$ITT[[paste0(pfx, "_total_t2")]])),
    dplyr::n_distinct(dl$subID) == nrow(qqnrs_t2),   # every randomised participant present
    sum(dl$post == 0L) == 361, sum(dl$post == 1L) == 292
  )

  # unstructured covariance + per-timepoint sd, not a random intercept: compound symmetry is
  # badly violated for PHQ-9 (screened at baseline), and halves its arm effect
  fit <- brms::brm(
    formula = brms::bf(
      score ~ post + post:group + unstr(time = tf, gr = subID),
      sigma ~ 0 + tf
    ),
    data = dl, family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = file.path(ancova_cache_dir, paste0("clda_", pfx)),
    file_refit = "on_change"
  )
  v <- coef_draws(fit, paste0("b_post:group", ARM_INT))

  # cross-check against gls (corSymm + varIdent matches this covariance); absolute 0.05 sd
  # tolerance (researcher choice)
  b_gls <- stats::coef(nlme::gls(
    score ~ post + post:group, data = dl,
    correlation = nlme::corSymm(form = ~ post + 1 | subID),
    weights = nlme::varIdent(form = ~ 1 | tf), method = "ML"
  ))[[b_grp]]
  stopifnot(abs(mean(v) - b_gls) <= 0.05 * cst$sd_t2)
  # sign agreement only where there is a sign to agree on
  if (abs(b_gls) > 0.05 * cst$sd_t2) stopifnot(sign(mean(v)) == sign(b_gls))

  clda_fits[[meas]]  <- fit
  clda_draws[[meas]] <- v
}

# side by side with the pre-registered ANCOVA, on the same sd(t2) scale
summarise_effect_draws <- function(v, sd_t2) {
  h95 <- bayestestR::hdi(v, ci = 0.95)
  tibble::tibble(
    est = mean(v), lo95 = h95$CI_low, hi95 = h95$CI_high,
    d_sd_t2 = mean(v) / sd_t2, sd_t2 = sd_t2,
    pd = as.numeric(bayestestR::p_direction(v, method = "direct"))
  )
}

clda_diag <- function(fit) {
  np <- brms::nuts_params(fit)
  dg <- posterior::summarise_draws(
    posterior::subset_draws(
      posterior::as_draws_df(fit$fit), variable = c("b_", "sigma", "sd_"), regex = TRUE
    ),
    "rhat", "ess_bulk", "ess_tail"
  )
  tibble::tibble(
    rhat_max = max(dg$rhat), ess_bulk_min = min(dg$ess_bulk), ess_tail_min = min(dg$ess_tail),
    divergent = sum(np$Value[np$Parameter == "divergent__"])
  )
}

clda_tbl <- dplyr::bind_rows(lapply(measure_meta$measure, function(meas) {
  cst <- ancova_const[[meas]]
  a <- ancova_summ[[paste0(meas, "|ITT")]]
  dplyr::bind_rows(
    summarise_effect_draws(ancova_draws[[paste0(meas, "|ITT")]]$contrast$.value, cst$sd_t2) |>
      dplyr::mutate(
        model = "ANCOVA (with outcome data)", n_ppts = 292L,
        rhat_max = a$rhat_max, ess_bulk_min = a$ess_bulk_min,
        ess_tail_min = a$ess_tail_min, divergent = a$divergent
      ),
    dplyr::bind_cols(
      summarise_effect_draws(clda_draws[[meas]], cst$sd_t2) |>
        dplyr::mutate(model = "cLDA (all randomised)", n_ppts = 361L),
      clda_diag(clda_fits[[meas]])
    )
  ) |>
    dplyr::mutate(measure = meas, tier = measure_tier[[meas]], .before = 1)
}))

readr::write_csv(clda_tbl, "analyses/outputs/ancova_clda_sensitivity.csv")

## 5d. DAQ subscales (EXPLORATORY): where the total's null comes from -------------------
# same model and participants as the primary ANCOVA; each subscale standardised by its own sd(t2)
daq_sub_const <- stats::setNames(
  lapply(daq_prefixes, function(p) ancova_constants(ancova_frames$ITT, p)), names(daq_prefixes)
)
daq_sub_draws <- list()
daq_sub_alpha <- stats::setNames(numeric(length(daq_prefixes)), names(daq_prefixes))

for (nm in names(daq_prefixes)) {
  pfx <- daq_prefixes[[nm]]
  cst <- daq_sub_const[[nm]]
  df  <- make_ancova_df(ancova_frames$ITT, pfx, cst$t1_mean)
  fit <- brms::brm(
    formula = stats::as.formula(t2 ~ group + t1_c), data = df,
    family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = file.path(ancova_cache_dir, paste0("ancova_", pfx, "_ITT")),
    file_refit = "on_change"
  )
  b_lm <- stats::coef(stats::lm(t2 ~ group + t1_c, data = df))[[paste0("group", ARM_INT)]]
  stopifnot(nrow(df) == 292, abs(mean(ancova_contrast_draws(fit)$contrast$.value) - b_lm) <= 0.02 * cst$sd_t2)

  daq_sub_draws[[nm]] <- ancova_contrast_draws(fit)$contrast$.value
  # internal consistency, since a 4-item subscale is noisier than the total
  daq_sub_alpha[[nm]] <- suppressWarnings(psych::alpha(
    self_report_df |>
      dplyr::filter(sessionNo == 0, catch_1_das_corr == 1, catch_2_erqcr_corr == 1) |>
      dplyr::select(tidyselect::all_of(paste0("DAQ_", daq_subscales[[nm]]))),
    warnings = FALSE
  )$total$raw_alpha)
}

## 5e. sensitivity: gender-adjusted ANCOVA -------------------------------------------
# pre-registered (gender is imbalanced across arms). woman = 1, man = 0; the 5 participants in
# other categories are NA and dropped by make_ancova_df() (too few to model)
gender_lookup <- self_report_df |>
  dplyr::filter(sessionNo == 0) |>
  dplyr::transmute(
    subID = prolificSubID,
    gender_woman = dplyr::case_when(
      demogs_gender == "woman" ~ 1L, demogs_gender == "man" ~ 0L, TRUE ~ NA_integer_
    )
  )
stopifnot(dplyr::n_distinct(gender_lookup$subID) == nrow(qqnrs_t2))
gender_frame <- qqnrs_t2 |> dplyr::left_join(gender_lookup, by = "subID")

gender_fits <- list()
gender_draws <- list()

for (i in seq_len(nrow(measure_meta))) {
  meas <- measure_meta$measure[i]
  pfx  <- measure_meta$prefix[i]
  cst  <- ancova_const[[meas]]

  df  <- make_ancova_df(gender_frame, pfx, cst$t1_mean, extra_cols = "gender_woman")
  fit <- brms::brm(
    formula = stats::as.formula(t2 ~ group + t1_c + gender_woman), data = df,
    family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = file.path(ancova_cache_dir, paste0("ancova_", pfx, "_ITT_gender")),
    file_refit = "on_change"
  )
  cd <- ancova_contrast_draws(fit)
  b_lm <- stats::coef(stats::lm(t2 ~ group + t1_c + gender_woman, data = df))[[paste0("group", ARM_INT)]]
  stopifnot(nrow(df) == 287, abs(mean(cd$contrast$.value) - b_lm) <= 0.02 * cst$sd_t2)

  gender_fits[[meas]]  <- fit
  gender_draws[[meas]] <- cd$contrast$.value
}

## 5f. sensitivity: ethnicity-adjusted ANCOVA ----------------------------------------
# pre-registered (ethnicity is imbalanced across arms); folded to white / non-white as most
# categories are too sparse to model
ethnicity_lookup <- self_report_df |>
  dplyr::filter(sessionNo == 0) |>
  dplyr::transmute(subID = prolificSubID, nonwhite = as.integer(demogs_ethnicity != "white_british"))
stopifnot(dplyr::n_distinct(ethnicity_lookup$subID) == nrow(qqnrs_t2), !anyNA(ethnicity_lookup$nonwhite))
ethnicity_frame <- qqnrs_t2 |> dplyr::left_join(ethnicity_lookup, by = "subID")

ethnicity_fits <- list()
ethnicity_draws <- list()

for (i in seq_len(nrow(measure_meta))) {
  meas <- measure_meta$measure[i]
  pfx  <- measure_meta$prefix[i]
  cst  <- ancova_const[[meas]]

  df  <- make_ancova_df(ethnicity_frame, pfx, cst$t1_mean, extra_cols = "nonwhite")
  fit <- brms::brm(
    formula = stats::as.formula(t2 ~ group + t1_c + nonwhite), data = df,
    family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = file.path(ancova_cache_dir, paste0("ancova_", pfx, "_ITT_ethnicity")),
    file_refit = "on_change"
  )
  cd <- ancova_contrast_draws(fit)
  b_lm <- stats::coef(stats::lm(t2 ~ group + t1_c + nonwhite, data = df))[[paste0("group", ARM_INT)]]
  stopifnot(nrow(df) == 292, abs(mean(cd$contrast$.value) - b_lm) <= 0.02 * cst$sd_t2)

  ethnicity_fits[[meas]]  <- fit
  ethnicity_draws[[meas]] <- cd$contrast$.value
}

## 5g. sensitivity: relaxing the (upstream) t2 catch-question exclusion --------------
# the primary frame already excludes the 3 t2 catch-question failures, so this adds them back
# (with their real t2 totals) to test whether that exclusion matters
catch_t2 <- self_report_df |>
  dplyr::filter(sessionNo == 1) |>
  dplyr::transmute(
    subID = prolificSubID,
    n_catch_wrong = (catch_1_das_corr == 0L) + (catch_2_erqcr_corr == 0L)
  )
stopifnot(all(catch_t2$n_catch_wrong %in% 0:1)) # nobody fails both catch questions

catch_fail_ids <- catch_t2$subID[catch_t2$n_catch_wrong >= 1L]
stopifnot(
  length(catch_fail_ids) == 3,
  all(is.na(qqnrs_t2[qqnrs_t2$subID %in% catch_fail_ids, paste0(measure_meta$prefix, "_total_t2")]))
)

raw_t2_wide <- self_report_df |>
  dplyr::filter(sessionNo == 1, prolificSubID %in% catch_fail_ids) |>
  dplyr::select(subID = prolificSubID, tidyselect::all_of(paste0(measure_meta$prefix, "_total"))) |>
  stats::setNames(c("subID", paste0(measure_meta$prefix, "_total_t2")))
stopifnot(nrow(raw_t2_wide) == 3, !anyNA(raw_t2_wide))

catch_frame <- qqnrs_t2
m <- match(raw_t2_wide$subID, catch_frame$subID)
for (col in paste0(measure_meta$prefix, "_total_t2")) catch_frame[[col]][m] <- raw_t2_wide[[col]]

stopifnot(
  sum(!is.na(catch_frame$PHQ9_total_t2)) == 295,
  all(catch_fail_ids %in% catch_frame$subID[!is.na(catch_frame$PHQ9_total_t2)])
)

catch_fits <- list()
catch_draws <- list()

for (i in seq_len(nrow(measure_meta))) {
  meas <- measure_meta$measure[i]
  pfx  <- measure_meta$prefix[i]
  cst  <- ancova_const[[meas]]

  df  <- make_ancova_df(catch_frame, pfx, cst$t1_mean)
  fit <- brms::brm(
    formula = stats::as.formula(t2 ~ group + t1_c), data = df,
    family = stats::gaussian(),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ANCOVA_SEED,
    file = file.path(ancova_cache_dir, paste0("ancova_", pfx, "_ITT_catchrelaxed")),
    file_refit = "on_change"
  )
  cd <- ancova_contrast_draws(fit)
  b_lm <- stats::coef(stats::lm(t2 ~ group + t1_c, data = df))[[paste0("group", ARM_INT)]]
  stopifnot(nrow(df) == 295, abs(mean(cd$contrast$.value) - b_lm) <= 0.02 * cst$sd_t2)

  catch_fits[[meas]]  <- fit
  catch_draws[[meas]] <- cd$contrast$.value
}

## 6. Figure 1 panels --------------------------------------------------------------
# shared by every panel that puts measures on an axis
ancova_labels <- stats::setNames(measure_meta$title, measure_meta$measure)
measure_cols  <- stats::setNames(MetBrewer::met.brewer("Cassatt2", length(main_measures)), main_measures)

### panel A: per-participant standardised change, by measure and arm
# units are d_av, (t2 - t1) / ((sd_t1 + sd_t2) / 2), with one denominator per measure across
# both arms -- not the sd(t2) the model panels use (PHQ-9 differs most)
change_draws_long <- dplyr::bind_rows(lapply(seq_len(nrow(measure_meta)), function(i) {
  mm  <- measure_meta[i, ]
  cst <- ancova_const[[mm$measure]]
  d   <- make_ancova_df(ancova_frames$ITT, mm$prefix, cst$t1_mean)
  tibble::tibble(
    measure = mm$measure, tier = mm$tier, group = d$group,
    .value = (d$t2 - d$t1) / cst$sd_av
  )
})) |>
  dplyr::mutate(
    measure = factor(measure, levels = main_measures),
    group   = factor(as.character(group), levels = c(ARM_REF, ARM_INT))
  )

# sd_av must lie strictly between the two sds it averages
stopifnot(vapply(ancova_const, function(c1) {
  c1$sd_av > min(c1$sd_t1, c1$sd_t2) && c1$sd_av < max(c1$sd_t1, c1$sd_t2)
}, logical(1)))

# pooled (both-arm) raw change and d_av per measure, from panel_stats (as in figure S2)
panel_raw_change <- panel_stats |>
  dplyr::transmute(
    measure, label = sprintf("raw &Delta; = %+.1f<br>*d*<sub>av</sub> = %+.2f", raw_change, d_av)
  )

change_plt <- plot_change_distributions(
  change_draws_long, labels = ancova_labels, tag = "A",
  ylab = "pre → post change (*d*<sub>av</sub>)",
  note = "<sup>†</sup> higher score = better", note_pos = "br",
  show_values = TRUE, value_nudge = 2, dodge_w = 0.8,
  raw_change = panel_raw_change, raw_change_nudge = 1.1
) +
  ggplot2::theme(plot.tag = tag_std)

### panel B: pre-registered arm effect (forest), ITT and per-protocol
# *b*<sub>std</sub> = the ANCOVA coefficient divided by that measure's ITT sd(t2)

# ancova_xlab <- paste0(
#   "difference in baseline-adjusted final score, *b*<sub>std</sub>",
#   "<span style='font-family: \"Open Sans SemiBold\"; color:",
#   grp_cols[[ARM_INT]], "'><br>(causal learning</span>",
#   "<span> − </span>",
#   "<span style='font-family: \"Open Sans SemiBold\"; color:",
#   grp_cols[[ARM_REF]], "'>control learning)</span>"
# )

# ITT frame only; per-protocol is shown in supplementary S1 (and panel D covers completers)
forest_plt <- plot_ancova_forest(
  ancova_draws_long |> dplyr::filter(frame == "ITT"),
  labels = ancova_labels, measure_cols = measure_cols,
  tag = "B", xlab = "group difference in baseline-adjusted final score (*b*<sub>std</sub>)"
) +
  ggplot2::theme(plot.tag = tag_std)

### panels C & D: hypothesis 2 -------------------------------------------------------
sessions_plt <- plot_ancova_forest(
  sessions_draws_long, labels = ancova_labels, measure_cols = measure_cols,
  dodge_by = "term", colour_by = "term", colour_values = sessions_term_cols,
  tag = "C", dodge_shapes = c(16, 17, 15), dodge_alphas = c(1, 1, 0.85),
  dodge_labels = c(ARM_LABS[[ARM_REF]], ARM_LABS[[ARM_INT]], "difference"),
  xlab = "slope on sessions completed (*b*<sub>std</sub> per session)"
) +
  ggplot2::theme(plot.tag = tag_std, legend.position = "bottom")

completion_plt <- plot_ancova_forest(
  completion_draws_long, labels = ancova_labels, measure_cols = measure_cols,
  dodge_by = "term", colour_by = "term", colour_values = completion_term_cols,
  tag = "D", dodge_shapes = c(16, 17, 15), dodge_alphas = c(1, 1, 0.85),
  dodge_labels = c("< 6 sessions", "all 6 sessions", "difference"),
  # the ANCOVA arm effect within each completion stratum (not a continuous relationship)
  xlab = paste0(
    "group difference in baseline-adjusted final score,<br>by session completion (*b*<sub>std</sub>)"
  )
) +
  ggplot2::theme(
    plot.tag = tag_std,
    legend.position = "bottom",
    axis.title.x = ggtext::element_markdown(lineheight = 1.1)
  )

### panel E: sessions completed -- why the binary contrast (D) is better powered than C
sessions_hist_plt <- plot_sessions_histogram(
  ancova_frames$ITT[!is.na(ancova_frames$ITT$PHQ9_total_t2), ], tag = "E"
) +
  ggplot2::theme(plot.tag = tag_std, legend.position = "none")

### panel F: DAQ subscales (EXPLORATORY) ----------------------------------------------
# pre-registered total above the rule, exploratory subscales below, on panel B's axis
daq_row_lvls <- c("I", "G", "S", "H", "total")
stopifnot(vapply(daq_subscales, length, integer(1)) == 4L)
daq_row_labs <- c(
  I = paste0(
    "internal<br><span style='font-size:12pt;color:grey40;'>",
    "(negative events) or<br>external (positive)</span>"
  ),
  G = "global<br><span style='font-size:12pt;color:grey40;'>(negative events)</span>",
  S = "stable<br><span style='font-size:12pt;color:grey40;'>(negative events)</span>",
  H = "helplessness<br><span style='font-size:12pt;color:grey40;'>(perceived)</span>",
  total = "DAQ total"
)

daq_sub_long <- dplyr::bind_rows(
  dplyr::bind_rows(lapply(names(daq_sub_draws), function(nm) {
    tibble::tibble(
      measure = nm, tier = "primary",
      .value = daq_sub_draws[[nm]] / daq_sub_const[[nm]]$sd_t2
    )
  })),
  tibble::tibble(
    measure = "total", tier = "secondary",
    .value = ancova_draws[["DAQ|ITT"]]$contrast$.value / ancova_const[["DAQ"]]$sd_t2
  )
) |>
  dplyr::mutate(measure = factor(measure, levels = rev(daq_row_lvls)))

daq_row_cols <- stats::setNames(
  ifelse(daq_row_lvls == "total", "#b695bc", "#d3bfd7"), daq_row_lvls
)

daq_sub_plt <- plot_ancova_forest(
  daq_sub_long, labels = daq_row_labs, measure_cols = daq_row_cols,
  tag = "F", tier_labels = c("sub-scales", "overall"),
  xlab = "group difference in baseline-adjusted final score (*b*<sub>std</sub>)"
) +
  ggplot2::theme(plot.tag = tag_std)


## 6b. supplementary S2 panel F: correlations between Δ scores (heatmap) -------------
# order must match measure_meta (section 1)
pretty_nms <- list(
  "PHQ9_delta"     = "ΔPHQ-9",
  "DAQ_delta"      = "ΔDAQ",
  "DAS_delta"      = "ΔDAS",
  "ERQCR_delta"    = "ΔERQ-CR<sup>†</sup>",
  "miniSPIN_delta" = "ΔminiSPIN"
)
stopifnot(identical(sub("_delta$", "", names(pretty_nms)), measure_meta$prefix))

make_delta_heatmap <- function(dat, tag, base_size = 18) {
  cor_input <- dat |>
    dplyr::select(tidyselect::all_of(paste0(measure_meta$prefix, "_total_delta"))) |>
    stats::setNames(names(pretty_nms)) |>
    dplyr::mutate(ERQCR_delta = -ERQCR_delta)

  cor_results <- psych::corr.test(cor_input, method = "pearson", adjust = "none", ci = TRUE, minlength = 20)

  cor_df <- cor_results$ci |>
    tibble::as_tibble(rownames = "pair") |>
    tidyr::separate(col = "pair", into = c("qqnr1", "qqnr2"), sep = "-") |>
    dplyr::mutate(
      cor_coeff = cor_results$r[cbind(qqnr1, qqnr2)],
      p_value   = cor_results$p[cbind(qqnr1, qqnr2)],
      p_stars   = stats::symnum(
        p_value, corr = FALSE, na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", "˙", "")
      ),
      label = sprintf("%.2f%s", cor_coeff, p_stars)
    ) |>
    dplyr::mutate(
      qqnr1 = factor(qqnr1, levels = names(pretty_nms)),
      qqnr2 = factor(qqnr2, levels = rev(names(pretty_nms)))
    )

  # in the empty upper-right of the lower-triangular grid
  explainer_txt <- data.frame(
    x = length(main_measures) - 1.5, y = length(main_measures) - 1,
    label = "<sup>†</sup> higher score = better;<br>sign flipped for correlation<br>analysis only"
  )

  cor_df |>
    ggplot2::ggplot(ggplot2::aes(x = qqnr1, y = qqnr2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = cor_coeff)) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 5.5, colour = "white", family = "Open Sans") +
    ggtext::geom_richtext(
      data = explainer_txt, ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE, fill = NA,
      label.color = "slateblue4", label.padding = grid::unit(rep(0.4, 4), "lines"),
      label.r = grid::unit(0.5, "lines"), colour = "grey20", lineheight = 1.25, size = 5, family = "Open Sans"
    ) +
    ggplot2::scale_x_discrete(labels = unlist(pretty_nms)) +
    ggplot2::scale_y_discrete(labels = unlist(pretty_nms)) +
    ggplot2::scale_fill_gradientn(name = "*r*", colours = MetBrewer::met.brewer("Hokusai2", type = "continuous")) +
    ggplot2::labs(x = "", y = "", tag = tag) +
    cowplot::theme_half_open(font_size = base_size, font_family = "Open Sans") +
    ggplot2::theme(
      axis.text.y   = ggtext::element_markdown(angle = 0, hjust = 1),
      axis.text.x   = ggtext::element_markdown(),
      legend.title  = ggtext::element_markdown(size = 14),
      plot.tag      = tag_std
    )
}

htmp <- make_delta_heatmap(qqnrs_t2, tag = "F")

## 7. Figure 1 assembly ---------------------------------------------------------------
fig_1 <-
  (wrap_elements(change_plt) + wrap_elements(forest_plt) + plot_layout(widths = c(0.55, 0.45))) /
  (
    wrap_elements(sessions_plt) + wrap_elements(completion_plt) +
    plot_layout(widths = c(0.5, 0.5))
  ) /
  (
    wrap_elements(sessions_hist_plt) + wrap_elements(daq_sub_plt) +
    plot_layout(widths = c(0.3, 0.7))
  ) +
  plot_layout(heights = c(0.3, 0.4, 0.3))
fig_1

## 8. Supplementary S1: per-measure arm-effect posteriors, both frames ------------------
# figure 1B's posteriors on the raw point scale, with the pre-registered per-protocol frame
# (in a lighter tint)
make_posterior_panel <- function(mm, tag, include_ylab = TRUE) {
  d <- dplyr::bind_rows(lapply(names(ancova_frames), function(fr) {
    tibble::tibble(frame = fr, .value = ancova_draws[[paste0(mm$measure, "|", fr)]]$contrast$.value)
  })) |>
    dplyr::mutate(frame = factor(frame, levels = c("ITT", "PP")))

  col <- measure_cols[[mm$measure]]

  p <-
    ggplot2::ggplot(d, ggplot2::aes(x = .value, y = frame, fill = frame, colour = frame)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "32", colour = "grey50") +
    # slab outline in the measure's full colour so the pale fills still have a defined edge
    ggdist::stat_slabinterval(
      .width = ANCOVA_WIDTHS, point_interval = "mean_hdci", slab_alpha = 0.6, scale = 0.7,
      point_size = 3, interval_size_range = c(1, 3), fatten_point = 2,
      slab_colour = col, slab_linewidth = 0.5
    ) +
    ggplot2::scale_fill_manual(values = c(ITT = col, PP = lighten_col(col)), guide = "none") +
    ggplot2::scale_colour_manual(values = c(ITT = col, PP = lighten_col(col)), guide = "none") +
    # "ITT" would be wrong here (complete-case)
    ggplot2::scale_y_discrete(
      limits = rev, # primary frame on top, matching Figure 1B
      labels = c(ITT = "with outcome data", PP = "per-protocol")
    ) +
    ggplot2::labs(
      x = paste0("difference (causal − control) in ", mm$ylab), y = NULL,
      title = mm$title, subtitle = mm$subtitle, tag = tag
    ) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 18) +
    cowplot::background_grid(major = "x", minor = "none") +
    ggplot2::theme(
      plot.title    = ggtext::element_markdown(size = 21, hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 13, hjust = 0.5, colour = "grey30"),
      plot.tag      = tag_std,
      axis.title.x  = ggtext::element_markdown(size = 14)
    )
  if (!include_ylab) {
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
  p
}

posterior_panels <- lapply(seq_len(nrow(measure_meta)), function(i) {
  make_posterior_panel(measure_meta[i, ], tag = LETTERS[i], include_ylab = i %in% c(1, 3))
})

suppl_fig_s1 <- wrap_plots(posterior_panels, design = "AAABBB\nCCDDEE")
suppl_fig_s1

## 8a. Supplementary S2: per-measure pre->post slopes + Δ-score correlations -----------
suppl_fig_s2 <- wrap_plots(panel_list, design = "AAABBB\nCCDDEE", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")
suppl_fig_s2 <- suppl_fig_s2 / wrap_elements(
  htmp + ggplot2::theme(plot.margin = ggplot2::margin(0, 0.15, 0, 0.15, unit = "npc"))
) +
  patchwork::plot_layout(heights = c(0.6, 0.4))
suppl_fig_s2

## 8b. Supplementary S3: missing-data sensitivity, ANCOVA vs cLDA ----------------------
# complete-case ANCOVA (n = 292) vs. cLDA (n = 361), on one axis
clda_draws_long <- dplyr::bind_rows(lapply(measure_meta$measure, function(meas) {
  cst <- ancova_const[[meas]]
  dplyr::bind_rows(
    tibble::tibble(model = "ancova", .value = ancova_draws[[paste0(meas, "|ITT")]]$contrast$.value),
    tibble::tibble(model = "clda", .value = clda_draws[[meas]])
  ) |>
    dplyr::mutate(measure = meas, tier = measure_tier[[meas]], .value = .value / cst$sd_t2)
})) |>
  dplyr::mutate(
    measure = factor(measure, levels = rev(main_measures)),
    model   = factor(model, levels = c("ancova", "clda"))
  )

suppl_fig_s3 <- plot_ancova_forest(
  clda_draws_long, labels = ancova_labels, measure_cols = measure_cols,
  dodge_by = "model", tag = NULL, # single-panel figure -- nothing to letter
  xlab = "group difference (causal - control) in final score (*b*<sub>std</sub>)",
  dodge_labels = c("ANCOVA (n = 292)", "cLDA (n = 361)")
) +
  ggplot2::theme(
    legend.position = "inside", legend.position.inside = c(0.02, 0.07)
  )
suppl_fig_s3

## 8c. Supplementary S4: gender-adjusted sensitivity -----------------------------------
# primary vs. gender-adjusted ANCOVA (n = 287; see section 5e)
gender_draws_long <- dplyr::bind_rows(lapply(measure_meta$measure, function(meas) {
  cst <- ancova_const[[meas]]
  dplyr::bind_rows(
    tibble::tibble(model = "primary", .value = ancova_draws[[paste0(meas, "|ITT")]]$contrast$.value),
    tibble::tibble(model = "gender", .value = gender_draws[[meas]])
  ) |>
    dplyr::mutate(measure = meas, tier = measure_tier[[meas]], .value = .value / cst$sd_t2)
})) |>
  dplyr::mutate(
    measure = factor(measure, levels = rev(main_measures)),
    model   = factor(model, levels = c("primary", "gender"))
  )

suppl_fig_s4 <- plot_ancova_forest(
  gender_draws_long, labels = ancova_labels, measure_cols = measure_cols,
  dodge_by = "model", tag = NULL, base_size = 16,
  xlab = "group difference (causal - control) in final score (*b*<sub>std</sub>)",
  dodge_labels = c("primary (n = 292)", "+ gender, man/woman only (n = 287)")
) +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 13),
    legend.text = ggplot2::element_text(size = 11),
    legend.background = ggplot2::element_rect(
      fill = "white", colour = "#cacaca", linewidth = 0.5
    ),
    legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
    legend.justification = "center",
    legend.position = "bottom"
  )

## 8d. Supplementary S5: ethnicity-adjusted sensitivity --------------------------------
ethnicity_draws_long <- dplyr::bind_rows(lapply(measure_meta$measure, function(meas) {
  cst <- ancova_const[[meas]]
  dplyr::bind_rows(
    tibble::tibble(model = "primary", .value = ancova_draws[[paste0(meas, "|ITT")]]$contrast$.value),
    tibble::tibble(model = "ethnicity", .value = ethnicity_draws[[meas]])
  ) |>
    dplyr::mutate(measure = meas, tier = measure_tier[[meas]], .value = .value / cst$sd_t2)
})) |>
  dplyr::mutate(
    measure = factor(measure, levels = rev(main_measures)),
    model   = factor(model, levels = c("primary", "ethnicity"))
  )

suppl_fig_s5 <- plot_ancova_forest(
  ethnicity_draws_long, labels = ancova_labels, measure_cols = measure_cols,
  dodge_by = "model", tag = NULL, base_size = 16,
  xlab = "group difference (causal - control) in final score (*b*<sub>std</sub>)",
  dodge_labels = c("primary (n = 292)", "+ ethnicity, white/non-white (n = 292)")
) +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 13),
    legend.text = ggplot2::element_text(size = 11),
    legend.background = ggplot2::element_rect(
      fill = "white", colour = "#cacaca", linewidth = 0.5
    ),
    legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
    legend.justification = "center",
    legend.position = "bottom"
  )

## 8e. Supplementary S6: t2 catch-question sensitivity (relaxed exclusion) -------------
# primary vs. adding the 3 t2 catch-failures back in (section 5g)
catch_draws_long <- dplyr::bind_rows(lapply(measure_meta$measure, function(meas) {
  cst <- ancova_const[[meas]]
  dplyr::bind_rows(
    tibble::tibble(model = "primary", .value = ancova_draws[[paste0(meas, "|ITT")]]$contrast$.value),
    tibble::tibble(model = "catch_relaxed", .value = catch_draws[[meas]])
  ) |>
    dplyr::mutate(measure = meas, tier = measure_tier[[meas]], .value = .value / cst$sd_t2)
})) |>
  dplyr::mutate(
    measure = factor(measure, levels = rev(main_measures)),
    model   = factor(model, levels = c("primary", "catch_relaxed"))
  )

suppl_fig_s6 <- plot_ancova_forest(
  catch_draws_long, labels = ancova_labels, measure_cols = measure_cols,
  dodge_by = "model", tag = NULL, base_size = 16,
  xlab = "group difference (causal - control) in final score (*b*<sub>std</sub>)",
  dodge_labels = c("primary, excl. t2 catch-fails (n = 292)", "+ 3 t2 catch-fails (n = 295)")
) +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 13),
    legend.text = ggplot2::element_text(size = 11),
    legend.background = ggplot2::element_rect(
      fill = "white", colour = "#cacaca", linewidth = 0.5
    ),
    legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
    legend.justification = "center",
    legend.position = "bottom"
  )

suppl_fig_s4 + suppl_fig_s5 + suppl_fig_s6 +
  plot_layout(ncol = 3) &
  plot_annotation(tag_levels = "A") &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(face = "bold", size = 28),
    legend.position = "bottom"
  )

## 9. write-up statistics (SENSITIVITY): change scores, effect sizes, t/p -------------
# descriptive change-score analyses; the reported arm effect is the ANCOVA (section 9a), which
# takes precedence where they disagree (DAS)
p_stars <- function(p) {
  as.character(stats::symnum(
    p, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", "•", "ns")
  ))
}

# pooled (both-arm) pre->post change: raw points (95% ci), d_av, t/df/p. as compute_panel_stats(),
# parameterised on `data`
compute_overall_stats <- function(meas, col_prefix, data) {
  t1 <- data[[paste0(col_prefix, "_total_t1")]]
  t2 <- data[[paste0(col_prefix, "_total_t2")]]
  keep <- !is.na(t1) & !is.na(t2)
  t1 <- t1[keep]
  t2 <- t2[keep]

  ttp  <- stats::t.test(t2, t1, paired = TRUE)
  d_av <- effectsize::repeated_measures_d(t2, t1, method = "av", adjust = FALSE)[["d_av"]]

  tibble::tibble(
    measure = meas, n = length(t1), mean_t1 = mean(t1), mean_t2 = mean(t2),
    raw_change = mean(t2 - t1), ci_lo = ttp$conf.int[[1]], ci_hi = ttp$conf.int[[2]],
    d_av = d_av, t_stat = unname(ttp$statistic), df = unname(ttp$parameter),
    p_value = ttp$p.value, stars = p_stars(ttp$p.value)
  )
}

# one arm's own pre->post change for one measure: raw points (95% CI), one-sample d_s
# (mean delta / sd delta), t/df/p.
compute_group_change_stats <- function(meas, col_prefix, grp, data) {
  sub <- data[data$group == grp, ]
  t1  <- sub[[paste0(col_prefix, "_total_t1")]]
  t2  <- sub[[paste0(col_prefix, "_total_t2")]]
  keep <- !is.na(t1) & !is.na(t2)
  t1 <- t1[keep]
  t2 <- t2[keep]

  ttp   <- stats::t.test(t2, t1, paired = TRUE)
  delta <- t2 - t1
  d_s   <- mean(delta) / stats::sd(delta)

  tibble::tibble(
    measure = meas, group = grp, n = length(t1), mean_t1 = mean(t1), mean_t2 = mean(t2),
    raw_change = mean(delta), ci_lo = ttp$conf.int[[1]], ci_hi = ttp$conf.int[[2]],
    d_s = d_s, t_stat = unname(ttp$statistic), df = unname(ttp$parameter),
    p_value = ttp$p.value, stars = p_stars(ttp$p.value)
  )
}

# between-group difference in change: raw diff (comp - ref, 95% ci), cohen's d (95% ci),
# t/df/p. printed as a sensitivity only
compute_group_diff_stats <- function(meas, col_prefix, data, ref = ARM_REF, comp = ARM_INT) {
  t1 <- data[[paste0(col_prefix, "_total_t1")]]
  t2 <- data[[paste0(col_prefix, "_total_t2")]]
  keep <- !is.na(t1) & !is.na(t2)
  d <- data.frame(group = as.character(data$group)[keep], delta = (t2 - t1)[keep])
  stopifnot(setequal(unique(d$group), c(ref, comp)))
  n_ref  <- sum(d$group == ref)
  n_comp <- sum(d$group == comp)

  d$group <- factor(d$group, levels = c(ref, comp))
  m       <- stats::lm(delta ~ group, data = d)
  coef_nm <- paste0("group", comp)
  coefs   <- summary(m)$coefficients
  ci      <- stats::confint(m)[coef_nm, ]

  d$group <- factor(d$group, levels = c(comp, ref))
  cd <- effectsize::cohens_d(delta ~ group, data = d, pooled_sd = TRUE)

  tibble::tibble(
    measure = meas, comparison = paste0(comp, " − ", ref), n_ref = n_ref, n_comp = n_comp,
    raw_diff = coefs[coef_nm, "Estimate"], ci_lo = ci[[1]], ci_hi = ci[[2]],
    d = cd$Cohens_d, d_ci_lo = cd$CI_low, d_ci_hi = cd$CI_high,
    t_stat = coefs[coef_nm, "t value"], df = m$df.residual,
    p_value = coefs[coef_nm, "Pr(>|t|)"], stars = p_stars(coefs[coef_nm, "Pr(>|t|)"])
  )
}

# `type`: "ITT" (everyone with t1/t2 data) or "PP" (6/6 sessions). `by_group`: "overall"
# (arms pooled) or "group" (per-arm change + between-group difference)
print_qnr_stats <- function(type = c("ITT", "PP"), by_group = c("overall", "group")) {
  type     <- match.arg(type)
  by_group <- match.arg(by_group)
  dat <- if (type == "PP") qqnrs_t2[qqnrs_t2$pp, ] else qqnrs_t2

  round_num <- function(df) {
    df |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric) & !tidyselect::any_of("p_value"), ~round(.x, 3))) |>
      dplyr::mutate(p_value = round(p_value, 4))
  }

  if (by_group == "overall") {
    out <- dplyr::bind_rows(lapply(seq_len(nrow(measure_meta)), function(i) {
      compute_overall_stats(measure_meta$measure[i], measure_meta$prefix[i], dat)
    }))
    cat("\n==== ", type, " -- overall (both arms) pre -> post change ====\n\n", sep = "")
    print(round_num(out), n = Inf)
  } else {
    within <- dplyr::bind_rows(lapply(seq_len(nrow(measure_meta)), function(i) {
      dplyr::bind_rows(
        compute_group_change_stats(measure_meta$measure[i], measure_meta$prefix[i], ARM_REF, dat),
        compute_group_change_stats(measure_meta$measure[i], measure_meta$prefix[i], ARM_INT, dat)
      )
    }))
    between <- dplyr::bind_rows(lapply(seq_len(nrow(measure_meta)), function(i) {
      compute_group_diff_stats(measure_meta$measure[i], measure_meta$prefix[i], dat)
    }))
    cat("\n==== ", type, " -- within-group pre -> post change ====\n\n", sep = "")
    print(round_num(within), n = Inf)
    cat("\n==== ", type, " -- between-group difference in change (", ARM_INT, " − ", ARM_REF, ") ====\n\n", sep = "")
    print(round_num(between), n = Inf)
    out <- list(within_group_change = within, between_group_difference = between)
  }
  invisible(out)
}

# full write-up console output: ITT/PP x overall/by-group, all six measures
for (.type in c("ITT", "PP")) {
  for (.by_group in c("overall", "group")) {
    print_qnr_stats(.type, .by_group)
  }
}

## 9a. write-up statistics (PRIMARY): pre-registered ANCOVA ---------------------------
# raw points are the reported effect, b/SD_t2 the cross-scale version; b/sigma is shown only
# to make the denominator choice visible (not reported)
cat(sprintf(paste0(
  "\nP(d>-%.1f) below = one-sided posterior probability that the standardised arm effect\n",
  "(b/SD_t2) fell short of the study's powered, pre-registered directional target\n",
  "(causal < control, Cohen's d < -%.1f) -- i.e. the null plus any control-favouring effect,\n",
  "not the symmetric |d|<%.1f. reported for the two pre-registered primary outcomes only.\n"
), ROPE_THRESH, ROPE_THRESH, ROPE_THRESH))

for (.fr in names(ancova_frames)) {
  cat("\n==== ", .fr, " -- pre-registered ANCOVA: t2 ~ arm + t1 (", ARM_INT, " − ", ARM_REF, ") ====\n\n", sep = "")
  sub <- ancova_tbl |> dplyr::filter(frame == .fr)
  for (j in seq_len(nrow(sub))) {
    r <- sub[j, ]
    cat(sprintf(
      "%-9s [%-9s] n=%3d  b=%+.3f  95%%HDI=[%+.3f, %+.3f]  b/SD=%+.3f  b/sigma=%+.3f  pd=%.1f%%%s\n",
      r$measure, r$tier, r$n, r$est, r$lo95, r$hi95, r$d_sd_t2, r$d_sigma, 100 * r$pd,
      if (is.na(r$rope_pct)) "" else sprintf("  P(d>-%.1f)=%.1f%%", r$rope_thresh, 100 * r$rope_pct)
    ))
  }
}

cat("\n==== ANCOVA diagnostics (worst across b_* and sigma per fit) ====\n\n")
print(
  ancova_tbl |>
    dplyr::select(measure, frame, rhat_max, ess_bulk_min, ess_tail_min, divergent) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(.x, 3))),
  n = Inf
)
if (any(ancova_tbl$rhat_max > 1.01) || any(ancova_tbl$divergent > 0)) {
  warning("ANCOVA fits: R-hat > 1.01 or divergent transitions present -- inspect before reporting")
} else {
  cat("\nall fits: R-hat <= 1.01, zero divergent transitions\n")
}

## 9b. write-up statistics: hypothesis 2 ---------------------------------------------
# models fitted in section 5b; printed with the cell counts they rest on
h2_terms <- list(
  arm_adjusted = "arm effect (sessions-adjusted)",
  slope_control = paste0("sessions slope (", ARM_REF, ")"),
  slope_causal  = paste0("sessions slope (", ARM_INT, ")"),
  slope_diff    = "arm x sessions interaction",
  arm_partial   = "arm effect | < 6 sessions",
  arm_complete  = "arm effect | all 6 sessions",
  arm_cmp_diff  = "arm x completion interaction"
)

h2_tbl <- dplyr::bind_rows(lapply(names(h2_draws), function(m) {
  dplyr::bind_rows(lapply(names(h2_terms), function(k) {
    v <- h2_draws[[m]][[k]]
    h <- bayestestR::hdi(v, ci = 0.95)
    tibble::tibble(
      measure = m, tier = measure_tier[[m]], term = h2_terms[[k]],
      est = mean(v), lo95 = h$CI_low, hi95 = h$CI_high,
      est_sd_t2 = mean(v) / h2_draws[[m]]$sd_t2,
      pd = as.numeric(bayestestR::p_direction(v, method = "direct"))
    )
  }))
})) |>
  dplyr::mutate(measure = factor(measure, levels = main_measures)) |>
  dplyr::arrange(measure)

readr::write_csv(h2_tbl, file = "analyses/outputs/ancova_h2_sessions.csv")

sess_tab <- table(
  ancova_frames$ITT$group[!is.na(ancova_frames$ITT$PHQ9_total_t2)],
  ancova_frames$ITT$sessions_completed[!is.na(ancova_frames$ITT$PHQ9_total_t2)]
)
cat("\n==== ITT -- hypothesis 2: does the effect scale with amount of training? ====\n\n")
cat("sessions completed, by arm (the identifiability constraint on the continuous model):\n")
print(sess_tab)
cat(sprintf(
  "\ncontinuous sessions slope for %s rests on %d participants below 6/6; %s on %d.\n",
  ARM_REF, sum(sess_tab[ARM_REF, colnames(sess_tab) != "6"]),
  ARM_INT, sum(sess_tab[ARM_INT, colnames(sess_tab) != "6"])
))
cat("the binary completion contrast uses all 292 (80 partial vs 212 complete).\n\n")
print(h2_tbl |> dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(.x, 3))), n = Inf)

## 9c. write-up statistics: missing-data sensitivity (cLDA) ----------------------------
cat("\n\n========== MISSING DATA: complete-case ANCOVA vs cLDA on all randomised ==========\n")
cat(paste0(
  "the pre-registered ANCOVA is complete-case by construction (t2 outcome + no imputation),\n",
  "so 69 of 361 randomised contribute nothing to it. the cLDA below constrains the arms to a\n",
  "shared baseline and admits every randomised participant. NB complete-case ANCOVA is\n",
  "already MAR-valid given baseline, so agreement is expected -- and neither addresses MNAR.\n\n"
))

# retention by arm (differential attrition is the threat to a complete-case analysis)
retention <- table(qqnrs_t2$group, !is.na(qqnrs_t2$PHQ9_total_t2))
ret_test <- stats::chisq.test(retention)
cat(sprintf(
  "retention: %s %d/%d (%.1f%%), %s %d/%d (%.1f%%); chi2(%d) = %.2f, p = %.3f\n\n",
  ARM_REF, retention[ARM_REF, "TRUE"], sum(retention[ARM_REF, ]),
  100 * retention[ARM_REF, "TRUE"] / sum(retention[ARM_REF, ]),
  ARM_INT, retention[ARM_INT, "TRUE"], sum(retention[ARM_INT, ]),
  100 * retention[ARM_INT, "TRUE"] / sum(retention[ARM_INT, ]),
  ret_test$parameter, ret_test$statistic, ret_test$p.value
))

for (meas in measure_meta$measure) {
  rows <- clda_tbl |> dplyr::filter(measure == meas)
  for (j in seq_len(nrow(rows))) {
    r <- rows[j, ]
    cat(sprintf(
      "%-9s %-28s n=%3d  b=%+.3f  95%%HDI=[%+.3f, %+.3f]  b/SD=%+.3f  pd=%.1f%%  rhat=%.3f\n",
      if (j == 1) meas else "", r$model, r$n_ppts, r$est, r$lo95, r$hi95, r$d_sd_t2,
      100 * r$pd, r$rhat_max
    ))
  }
  d <- rows$d_sd_t2
  cat(sprintf("%-9s %-28s %+.3f SD\n\n", "", "-> cLDA - ANCOVA:", d[2] - d[1]))
}

## 9d. write-up statistics: DAQ subscales (EXPLORATORY) ---------------------------------
cat("\n\n========== DAQ SUBSCALES (EXPLORATORY, post hoc) ==========\n")
cat(paste0(
  "the pre-registration named the DAQ TOTAL. H1b's wording ('reduces internal attributions\n",
  "of negative events, and enhances internal attributions of positive events') describes\n",
  "only the 4-item I subscale; the other 12 items measure helplessness, stability and\n",
  "globality, which H1b does not predict. all four are reported together -- reporting only\n",
  "I after seeing the total was null would be selection on the outcome. alpha is given\n",
  "because a 4-item null could be attenuation rather than absence.\n\n"
))
cat(sprintf("%-26s %5s %8s %9s %9s %8s %7s %6s\n",
            "scale", "n", "est", "lo95", "hi95", "b/SD", "pd", "alpha"))
daq_rows <- c(total = "DAQ total (pre-registered)", I = "I: internal-neg/external-pos",
              G = "G: global-neg", S = "S: stable-neg", H = "H: helplessness")
for (nm in names(daq_rows)) {
  if (nm == "total") {
    v <- ancova_draws[["DAQ|ITT"]]$contrast$.value
    sdv <- ancova_const[["DAQ"]]$sd_t2
    al <- NA_real_
  } else {
    v <- daq_sub_draws[[nm]]
    sdv <- daq_sub_const[[nm]]$sd_t2
    al <- daq_sub_alpha[[nm]]
  }
  h <- bayestestR::hdi(v, ci = 0.95)
  cat(sprintf("%-26s %5d %+8.3f %+9.3f %+9.3f %+8.3f %6.1f%% %6s\n",
              daq_rows[[nm]], 292L, mean(v), h$CI_low, h$CI_high, mean(v) / sdv,
              100 * as.numeric(bayestestR::p_direction(v, method = "direct")),
              if (is.na(al)) "-" else formatC(al, format = "f", digits = 2)))
}
cat(paste0(
  "\nNB the I subscale is scored so that HIGH = internal attribution of negative events AND\n",
  "external attribution of positive events, so BOTH halves of H1b predict a decrease.\n",
  "convergence with the task-based measure is what this rests on -- it is not independent\n",
  "evidence (same participants, same sessions), and the 95% HDI includes zero.\n"
))

## 9e. write-up statistics: gender-, ethnicity- and catch-question sensitivity ----------
# shared printer for the primary vs. adjusted sensitivity tables
print_sensitivity_tbl <- function(header, note, draws_list, model_label, n_adj, csv_path) {
  cat("\n\n========== SENSITIVITY: ", header, " ==========\n", sep = "")
  cat(note)
  tbl <- dplyr::bind_rows(lapply(measure_meta$measure, function(meas) {
    cst <- ancova_const[[meas]]
    dplyr::bind_rows(
      summarise_effect_draws(ancova_draws[[paste0(meas, "|ITT")]]$contrast$.value, cst$sd_t2) |>
        dplyr::mutate(model = "primary", n_ppts = 292L),
      summarise_effect_draws(draws_list[[meas]], cst$sd_t2) |>
        dplyr::mutate(model = model_label, n_ppts = n_adj)
    ) |>
      dplyr::mutate(measure = meas, tier = measure_tier[[meas]], .before = 1)
  }))
  readr::write_csv(tbl, csv_path)
  for (meas in measure_meta$measure) {
    rows <- tbl |> dplyr::filter(measure == meas)
    for (j in seq_len(nrow(rows))) {
      r <- rows[j, ]
      cat(sprintf(
        "%-9s %-28s n=%3d  b=%+.3f  95%%HDI=[%+.3f, %+.3f]  b/SD=%+.3f  pd=%.1f%%\n",
        if (j == 1) meas else "", r$model, r$n_ppts, r$est, r$lo95, r$hi95, r$d_sd_t2, 100 * r$pd
      ))
    }
  }
  invisible(tbl)
}

print_sensitivity_tbl(
  header = "gender-adjusted ANCOVA (t1 + gender[woman/man])",
  note = "excludes 5 of 292 (non-binary/other/prefer-not-to-say): too few to model separately.\n\n",
  draws_list = gender_draws, model_label = "+ gender (n=287)", n_adj = 287L,
  csv_path = "analyses/outputs/ancova_gender_sensitivity.csv"
)
print_sensitivity_tbl(
  header = "ethnicity-adjusted ANCOVA (t1 + ethnicity[white/non-white])",
  note = "folded to white/non-white: 5 categories are too sparse to model separately (e.g. n=2).\n\n",
  draws_list = ethnicity_draws, model_label = "+ ethnicity (n=292)", n_adj = 292L,
  csv_path = "analyses/outputs/ancova_ethnicity_sensitivity.csv"
)
print_sensitivity_tbl(
  header = "t2 catch-question sensitivity (relaxed exclusion)",
  note = paste0(
    "the primary frame is ALREADY catch-filtered at t2 (see section 5g) -- this relaxes\n",
    "that upstream exclusion, adding the 3 t2 catch-failures back in with their real totals.\n\n"
  ),
  draws_list = catch_draws, model_label = "+ 3 catch-fails (n=295)", n_adj = 295L,
  csv_path = "analyses/outputs/ancova_catch_sensitivity.csv"
)

## 10. baseline characteristics: attrition check + demographic table by group -------------
# demographics are collected at prescreen only (sessionNo == 0): one row per participant
baseline_demogs <- self_report_df |>
  dplyr::filter(sessionNo == 0) |>
  dplyr::select(subID = prolificSubID, tidyselect::starts_with("demogs_"))
stopifnot(nrow(baseline_demogs) == nrow(qqnrs_t2))
stopifnot(setequal(baseline_demogs$subID, qqnrs_t2$subID))

# "completed" = has a postscreen row (3 of these have no PHQ9_total_t2)
postscreen_ids <- self_report_df$prolificSubID[self_report_df$sessionNo == 1]
baseline_df <- qqnrs_t2 |>
  dplyr::left_join(baseline_demogs, by = "subID") |>
  dplyr::mutate(completed_postscreen = subID %in% postscreen_ids)

format_digits <- function(x, digit = 2, small = 2) format(round(x, digits = digit), nsmall = small, scientific = FALSE)

mean_sd_range <- function(x, d = 2, s = 2) {
  x <- x[!is.na(x)]
  paste0(
    format_digits(mean(x), d, s), " (", format_digits(stats::sd(x), d, s), "; ",
    format_digits(min(x), d, s), "-", format_digits(max(x), d, s), ")"
  )
}

# multi-select fields are python-list strings, so `multi = TRUE` matches the quoted token
# (which also avoids prefix collisions, e.g. "medication" vs "medication_prev")
count_pct <- function(x, value, multi = FALSE) {
  n_tot <- sum(!is.na(x))
  n_val <- if (multi) sum(grepl(paste0("'", value, "'"), x, fixed = TRUE)) else sum(x == value, na.rm = TRUE)
  paste0(n_val, " (", format_digits(100 * n_val / n_tot, 1, 1), ")")
}

# 10a. baseline scores: postscreen-completers vs. dropouts, pooled across arms
compare_completion_stats <- function(meas, col_prefix) {
  compl <- baseline_df[[paste0(col_prefix, "_total_t1")]][baseline_df$completed_postscreen]
  drop  <- baseline_df[[paste0(col_prefix, "_total_t1")]][!baseline_df$completed_postscreen]
  tt <- stats::t.test(compl, drop, var.equal = TRUE)
  tibble::tibble(
    measure = meas, completed = mean_sd_range(compl, 1, 1), dropped_out = mean_sd_range(drop, 1, 1),
    p_value = format_digits(tt$p.value)
  )
}

bsl_compare <- dplyr::bind_rows(lapply(seq_len(nrow(measure_meta)), function(i) {
  compare_completion_stats(measure_meta$measure[i], measure_meta$prefix[i])
}))
cat("\n==== baseline questionnaire scores: postscreen-completers vs. dropouts ====\n\n")
print(bsl_compare, n = Inf)
readr::write_csv(
  bsl_compare, file = "analyses/outputs/baseline_completers_vs_dropouts.csv"
)

# 10b. sample characteristics by group: "itt" = all randomised (n = 361), "completers" = those
# with postscreen data (n = 295)
make_demog_table <- function(population = c("itt", "completers")) {
  population <- match.arg(population)
  dat <- if (population == "completers") baseline_df[baseline_df$completed_postscreen, ] else baseline_df

  build_col <- function(grp) {
    g <- dat[dat$group == grp, ]
    c(
      nrow(g),
      mean_sd_range(as.numeric(g$demogs_age), 1, 1),
      NA,
      count_pct(g$demogs_gender, "man"),
      count_pct(g$demogs_gender, "woman"),
      count_pct(g$demogs_gender, "non-binary"),
      count_pct(g$demogs_gender, "other"),
      count_pct(g$demogs_gender, "prefer_not_to_say"),
      NA,
      count_pct(g$demogs_ethnicity, "white_british"),
      count_pct(g$demogs_ethnicity, "mixed"),
      count_pct(g$demogs_ethnicity, "asian"),
      count_pct(g$demogs_ethnicity, "black"),
      count_pct(g$demogs_ethnicity, "other_ethnicity"),
      NA,
      count_pct(g$demogs_employment, "employed"),
      count_pct(g$demogs_employment, "unemployed"),
      count_pct(g$demogs_employment, "not_seeking"),
      NA,
      count_pct(g$demogs_financial, "doing_okay"),
      count_pct(g$demogs_financial, "getting_by"),
      count_pct(g$demogs_financial, "struggling"),
      NA,
      count_pct(g$demogs_housing, "homeowner"),
      count_pct(g$demogs_housing, "tenant"),
      count_pct(g$demogs_housing, "other_housing"),
      NA,
      count_pct(g$demogs_neurodiv, "yes"),
      count_pct(g$demogs_neurodiv, "no"),
      count_pct(g$demogs_neurodiv, "prefer_not_to_say_neurodiv"),
      NA,
      count_pct(g$demogs_disability, "concentration", multi = TRUE),
      count_pct(g$demogs_disability, "physical_effort", multi = TRUE),
      count_pct(g$demogs_disability, "reading_writing_maths", multi = TRUE),
      count_pct(g$demogs_disability, "social_interaction", multi = TRUE),
      count_pct(g$demogs_disability, "other_impact", multi = TRUE),
      count_pct(g$demogs_disability, "none", multi = TRUE),
      count_pct(g$demogs_disability, "prefer_not_to_say_disability", multi = TRUE),
      NA,
      count_pct(g$demogs_tx_current, "talk_therapy", multi = TRUE),
      count_pct(g$demogs_tx_current, "medication", multi = TRUE),
      count_pct(g$demogs_tx_current, "self_guided", multi = TRUE),
      count_pct(g$demogs_tx_current, "other_tx", multi = TRUE),
      count_pct(g$demogs_tx_current, "none", multi = TRUE),
      count_pct(g$demogs_tx_current, "prefer_not_to_say_tx_current", multi = TRUE),
      NA,
      count_pct(g$demogs_tx_previous, "talk_therapy_prev", multi = TRUE),
      count_pct(g$demogs_tx_previous, "medication_prev", multi = TRUE),
      count_pct(g$demogs_tx_previous, "self_guided_prev", multi = TRUE),
      count_pct(g$demogs_tx_previous, "other_tx_prev", multi = TRUE),
      count_pct(g$demogs_tx_previous, "none", multi = TRUE),
      count_pct(g$demogs_tx_previous, "prefer_not_to_say_tx_prev", multi = TRUE),
      mean_sd_range(g$PHQ9_total_t1, 1, 1)
    )
  }

  demog_rows <- c(
    "Cohort size",
    "Age, mean (SD; range)",
    "Gender, number (%)", "  Man", "  Woman", "  Non-binary", "  Other", "  Prefer not to say",
    "Ethnicity, number (%)", "  White", "  Mixed or multiple ethnic groups", "  Asian or Asian British",
    "  Black, African, Caribbean or Black British", "  Other ethnic group",
    "Employment status, number (%)", "  Employed", "  Unemployed", "  Not seeking employment",
    "Financial status, number (%)", "  Doing okay", "  Getting by", "  Struggling",
    "Housing status, number (%)", "  Homeowner", "  Renting", "  Other",
    "Neurodivergent?, number (%)", "  Yes", "  No", "  Prefer not to say",
    "Disability that affects any of the below, number (%)",
    "  Concentrate for extended periods", "  Perform physically effortful activities",
    "  Read, write, or do maths", "  Deal with people you do not know",
    "  Other form of impact not listed", "  None of the above", "  Prefer not to say",
    "Currently receiving treatment for mental ill-health, number (%)",
    "  Talking therapy", "  Medication", "  Self-guided", "  Other", "  None", "  Prefer not to say",
    "Previously received treatment for mental ill-health, number (%)",
    "  Talking therapy", "  Medication", "  Self-guided", "  Other", "  None", "  Prefer not to say",
    "Baseline PHQ-9 score, mean (SD; range)"
  )

  tibble::tibble(
    Characteristic    = demog_rows,
    `Control learning` = build_col(ARM_REF),
    `Causal learning`  = build_col(ARM_INT)
  )
}

for (.population in c("itt", "completers")) {
  dem <- make_demog_table(.population)
  readr::write_csv(
    dem, file = paste0("analyses/outputs/demographics_", .population, ".csv")
  )
}
