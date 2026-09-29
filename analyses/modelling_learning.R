# Figure 5 -- learning ============================================================
# both groups learn across the 6 training sessions but hit ceiling, so the modelling
# focuses on session 1, where the model comparison selects free group q0 + valence-
# specific alpha (M2). run learning_attr_assoc.R (panel E) and learning_recovery.R
# (figure S3) first.

library(patchwork)
source("analyses/study_data.R")

# base font for every panel, so relative sizes stay consistent
FIG5_FNT <- 16
SUPPL_FNT <- 14
fig5_tag <- function(tag, hjust = 0) {
  list(
    ggplot2::labs(tag = tag),
    ggplot2::theme(plot.tag = ggplot2::element_text(
      family = "Open Sans", face = "bold", size = FIG5_FNT * 1.5, hjust = hjust
    ))
  )
}

# the raw data codes both arms with the causal task's labels; the control arm's images map as
# int_glob = natural-smaller, int_spec = natural-bigger, ext_glob = man-made-smaller,
# ext_spec = man-made-bigger (public/js/trialsControl.js). labels name the alternative to the
# correct (internal-global / natural-smaller) option
BLOCK_ALT_LABS <- c(
  internal_specific = "control: natural, bigger | causal: internal-specific",
  external_global   = "control: man-made, smaller | causal: external-global",
  external_specific = "control: man-made, bigger | causal: external-specific"
)
# two-line version (one line per arm) for panel C's strips; ggtext needs <br>, not "\n"
BLOCK_ALT_LABS_2L <- c(
  internal_specific = "<em>control</em>: natural, bigger<br><em>causal</em>: internal-specific",
  external_global   = "<em>control</em>: man-made, smaller<br><em>causal</em>: external-global",
  external_specific = "<em>control</em>: man-made, bigger<br><em>causal</em>: external-specific"
)
# per-arm vocabularies for panel A's separate legends
CONTROL_BLOCK_LABS <- c(
  internal_specific = "natural, bigger", external_global = "man-made, smaller", external_specific = "man-made, bigger"
)
CAUSAL_BLOCK_LABS <- c(
  internal_specific = "internal-specific", external_global = "external-global", external_specific = "external-specific"
)
# block colours (panel A, figure S3D): chosen to be distinct from VAL_COLS and ARM_COLS
BLOCK_PAL_NM <- "Cassatt2"

## 1. panel A: model-free learning curves across all 6 sessions, by block type ------
# coloured by block type (the foil), not block number: the foil-to-block mapping only holds
# in session 1 and rotates in sessions 2-6
learn_df_all6 <- prep_data(
  learning = list(control = control, causal = causal),
  learning_session = 1:6, learning_blocks = 1:3,
  self_report = self_report_df, filter_by_catch = TRUE, ret = "df"
)

# one plot per arm, stacked, so each legend uses its own arm's vocabulary
learn_df_all6 <- learn_df_all6 |>
  dplyr::mutate(arm = factor(
    ifelse(condition01 == 1, "causal", "control"),
    levels = c("control", "causal")
  ))

# gridlines at chance and the bounds only (the contingency is deterministic, so there is no
# natural learning criterion). legends on the left keep the six session facets from flattening
learn_panel_a <- function(df, block_labels, legend_title, show_top_strip, show_x_axis) {
  p <- make_learning_plot(
    df, title = NULL, by = "block", row_var = "arm", palette = BLOCK_PAL_NM,
    block_labels = block_labels, legend_title = legend_title,
    x_breaks = seq(0, 15, by = 5), y_breaks = c(0, 0.5, 1),
    font_size = FIG5_FNT, legend_pos = "left"
  ) +
    # cumulative rather than per-trial: this panel pools valence, which is interleaved on a fixed
    # schedule, so a per-trial curve would sawtooth on valence (panel C splits it instead)
    ggplot2::labs(y = "cumulative proportion correct", x = "trial number (within block)") +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = FIG5_FNT * 0.8, family = "Open Sans SemiBold"),
      legend.text = ggplot2::element_text(size = FIG5_FNT * 0.8, family = "Open Sans")
    )
  if (!show_top_strip) p <- p + ggplot2::theme(strip.text.x = ggplot2::element_blank())
  if (!show_x_axis) {
    p <- p + ggplot2::theme(
      axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  }
  p
}

figure_5a <- wrap_elements(
  learn_panel_a(
    dplyr::filter(learn_df_all6, arm == "control"), CONTROL_BLOCK_LABS,
    "block:\nalternative to natural, smaller", show_top_strip = TRUE, show_x_axis = FALSE
  ) /
    learn_panel_a(
      dplyr::filter(learn_df_all6, arm == "causal"), CAUSAL_BLOCK_LABS,
      "block:\nalternative to internal-global", show_top_strip = FALSE, show_x_axis = TRUE
    ) +
    plot_layout(heights = c(0.48, 0.52), axis_titles = "collect")
) + fig5_tag("A")

## 2. panel B: session-1 all-blocks learning model comparison (LOO) -----------------
# M0-M3: (q0 free vs anchored) x (alpha single vs valence-specific); M4 / M5 add counterfactual
# updating to M2 / M3
learn_model_fits <- c(
  m0 = "./analyses/stan-fits/learning/fit_learn-s1-single.rds",
  m1 = "./analyses/stan-fits/learning/fit_learn-s1-anchored.rds",
  m2 = "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha.rds",
  m3 = "./analyses/stan-fits/learning/fit_learn-s1-anchored-valalpha.rds",
  m4 = "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha-cf.rds",
  m5 = "./analyses/stan-fits/learning/fit_learn-s1-cf.rds"
)
learn_model_labels <- c(
  m0 = "free <em>q</em><sub>0</sub>, single <em>α</em>,<br>chosen-option (M0)",
  m1 = "anchored <em>q</em><sub>0</sub>, single <em>α</em>,<br>chosen-option (M1)",
  m2 = "free <em>q</em><sub>0</sub>, valence <em>α</em>,<br>chosen-option (M2)",
  m3 = "anchored <em>q</em><sub>0</sub>, valence <em>α</em>,<br>chosen-option (M3)",
  m4 = "free <em>q</em><sub>0</sub>, valence <em>α</em>,<br>counterfactual (M4)",
  m5 = "anchored <em>q</em><sub>0</sub>, valence <em>α</em>,<br>counterfactual (M5)"
)

loo_cache_path <- "./analyses/stan-fits/learning/loo_compare_s1_allblocks.rds"
# recompute if the cached comparison covers a different model set
loo_cache_ok <- file.exists(loo_cache_path) &&
  setequal(rownames(readr::read_rds(loo_cache_path)), names(learn_model_fits))
if (loo_cache_ok) {
  loo_compare_learn <- readr::read_rds(loo_cache_path)
} else {
  if (file.exists(loo_cache_path)) {
    message("loo cache covers a different model set -- recomputing over all ",
            length(learn_model_fits), " models")
  }
  # one fit in memory at a time (~1.5GB each)
  loo_list <- lapply(learn_model_fits, function(path) {
    fit <- readr::read_rds(path)
    ll_dims <- dim(fit$draws("log_lik", format = "matrix"))
    lo <- fit$loo(variables = "log_lik", cores = 4)
    rm(fit)
    gc(verbose = FALSE)
    list(loo = lo, nPpts = ll_dims[2])
  })
  # all fits must cover the same participants for pointwise elpd differences to be valid
  stopifnot(length(unique(sapply(loo_list, `[[`, "nPpts"))) == 1)
  loo_compare_learn <- loo::loo_compare(lapply(loo_list, `[[`, "loo"))
  saveRDS(loo_compare_learn, loo_cache_path)
}
print(loo_compare_learn, simplify = FALSE)

figure_5b <- wrap_elements(
  plot_loo_compare(
    loo_compare_learn, model_labels = learn_model_labels, font_size = FIG5_FNT - 2
  ) + fig5_tag("B", hjust = 2)
)

## 3. load the winning model (M2), cache small draws extracts -----------------------
win_mod_group_draws_path <- "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha_group_draws.rds"
win_mod_indiv_draws_path <- "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha_indiv_draws.rds"

if (file.exists(win_mod_group_draws_path) && file.exists(win_mod_indiv_draws_path)) {
  win_mod_group_draws <- readr::read_rds(win_mod_group_draws_path)
  win_mod_i_draws     <- readr::read_rds(win_mod_indiv_draws_path)
} else {
  fit_win_mod <- readr::read_rds("./analyses/stan-fits/learning/fit_learn-s1-free-valalpha.rds")
  win_mod_group_draws <- fit_win_mod$draws(format = "df", variables = c("p_alpha_neg", "p_alpha_pos", "delta_alpha"))
  win_mod_i_draws     <- fit_win_mod$draws(format = "df", variables = c("alpha_neg", "alpha_pos", "alpha_mu"))
  saveRDS(win_mod_group_draws, win_mod_group_draws_path)
  saveRDS(win_mod_i_draws, win_mod_indiv_draws_path)
  rm(fit_win_mod)
  gc(verbose = FALSE)
}

## 4. panel C: posterior-predictive check for the winner (M2), session 1 -----------------
learn_df_s1 <- prep_data(
  learning = list(control = control, causal = causal),
  learning_session = 1, learning_blocks = 1:3,
  self_report = self_report_df, filter_by_catch = TRUE, ret = "df"
)

ppc_cache_path <- "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha_ppc_bands.rds"
# recompute if the cache predates the current (hdi) band columns
ppc_cache_ok <- file.exists(ppc_cache_path) &&
  all(c("pred_med", "pred_lo", "pred_hi") %in% names(readr::read_rds(ppc_cache_path)))
if (ppc_cache_ok) {
  win_mod_ppc_bands <- readr::read_rds(ppc_cache_path)
} else {
  if (file.exists(ppc_cache_path)) {
    message("ppc band cache predates the HDI bands -- recomputing")
  }
  fit_win_mod_ppc <- readr::read_rds("./analyses/stan-fits/learning/fit_learn-s1-free-valalpha.rds")
  cp_draws <- fit_win_mod_ppc$draws(format = "df", variables = "correct_pred")
  win_mod_ppc_bands <- make_ppc_intervals(cp_draws, learn_df_s1, group_lookup, block = TRUE)
  saveRDS(win_mod_ppc_bands, ppc_cache_path)
  rm(fit_win_mod_ppc, cp_draws)
  gc(verbose = FALSE)
}

# plot panel C against the actual session trial (1-30), not the within-valence index (the
# model's own clock, which hides gaps). design slots come from each itemNo's modal position,
# since timed-out trials are re-presented and row order counts presentations
s1_sched <- learn_df_s1 |>
  tidyr::drop_na(chosen_attr_type, valence) |>
  dplyr::mutate(group = ifelse(condition01 == 1, ARM_INT, ARM_REF)) |>
  dplyr::arrange(learn_id, blockNo, trialNo) |>
  dplyr::group_by(learn_id, blockNo) |>
  dplyr::mutate(obs_pos = dplyr::row_number()) |>
  dplyr::group_by(group, blockNo, itemNo, valence) |>
  dplyr::summarise(modal_pos = stats::median(obs_pos), .groups = "drop") |>
  # rank within arm x block (not within the per-item groups above)
  dplyr::group_by(group, blockNo) |>
  dplyr::mutate(design_pos = rank(modal_pos, ties.method = "first")) |>
  dplyr::ungroup()
# fixed design: 10 slots per arm x block, 5 per valence, each itemNo in one slot
stopifnot(
  dplyr::count(s1_sched, group, blockNo)$n == 10L,
  dplyr::count(s1_sched, group, blockNo, valence)$n == 5L,
  s1_sched |> dplyr::group_by(group, blockNo) |>
    dplyr::summarise(ok = setequal(design_pos, 1:10), .groups = "drop") |> dplyr::pull(ok)
)

# k-th same-valence trial of a block -> the slot it occupied
s1_slot_map <- s1_sched |>
  dplyr::arrange(group, blockNo, valence, design_pos) |>
  dplyr::group_by(group, blockNo, valence) |>
  dplyr::mutate(trial = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::select(group, blockNo, valence, trial, design_pos)

n_bands <- nrow(win_mod_ppc_bands)
win_mod_ppc_bands <- win_mod_ppc_bands |>
  dplyr::inner_join(s1_slot_map, by = c("group", "blockNo", "valence", "trial")) |>
  dplyr::mutate(trial = design_pos + 10L * (blockNo - 1L)) |>
  dplyr::select(-design_pos)
stopifnot(nrow(win_mod_ppc_bands) == n_bands, range(win_mod_ppc_bands$trial) == c(1L, 30L))

# the block-to-foil mapping is fixed (and identical across arms) in session 1 only
s1_block_type_labels <- stats::setNames(
  paste0(
    "<b>block ", 1:3, "</b><br>", BLOCK_ALT_LABS_2L[c("internal_specific", "external_global", "external_specific")]
  ),
  as.character(1:3)
)

figure_5c <- wrap_elements(
  plot_ppc_intervals(
    win_mod_ppc_bands, title = NULL, font_size = FIG5_FNT - 2, x_breaks = seq(2, 30, by = 2),
    block_type_labels = s1_block_type_labels
  ) +
    # points mark the trials each valence actually occurred on (lines interpolate across gaps)
    ggplot2::geom_point(ggplot2::aes(y = raw_acc, colour = valence), size = 1.5) +
    # darker rules at the bounds, drawn over the ribbons so the ceiling stays visible
    ggplot2::geom_hline(yintercept = c(0, 1), colour = "grey35", linewidth = 0.45) +
    ggplot2::labs(y = "per-trial proportion correct", x = "session 1 trial number") +
    ggplot2::theme(
      legend.position = "bottom",
      # must stay element_markdown() to merge with plot_ppc_intervals()'s own strip theme
      strip.text.x = ggtext::element_markdown(size = FIG5_FNT * 0.8, lineheight = 1.05)
    ) +
    fig5_tag("C")
)

## 5. panel D: group-level learning-rate slabs, negative and positive valence -------
alpha_chg_df <- function(group_draws, val_suffix) {
  delta_idx <- if (grepl("neg", val_suffix)) 1 else 2
  dplyr::bind_rows(
    data.frame(level = "control",    value = group_draws[[paste0("p_", val_suffix, "[1]")]]),
    data.frame(level = "causal",     value = group_draws[[paste0("p_", val_suffix, "[2]")]]),
    data.frame(level = "difference", value = group_draws[[paste0("delta_alpha[", delta_idx, "]")]])
  ) |>
    dplyr::mutate(level = factor(level, levels = c("difference", "control", "causal")))
}

# shared x-range so the valence asymmetry is comparable across panels
alpha_slab <- function(val_suffix, lab, suppress_labels) {
  plot_change_slabs(
    alpha_chg_df(win_mod_group_draws, val_suffix),
    x_lab = lab, xlim = c(-0.22, 0.52), fnt_sz = 1.2,
    grp_cols = ARM_COLS, grp_shapes = ARM_SHP,
    interval_size_range = c(0.5, 2), fatten_point = 2,
    suppress_labels = suppress_labels, flip_order = TRUE,
    subtitle = if (suppress_labels) NULL else "group-level posterior"
  ) +
    ggplot2::theme(axis.title.x = ggtext::element_markdown()) +
    # conditional, or it would undo plot_change_slabs()'s own blanking
    (if (suppress_labels) NULL else ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)))
}

figure_5d <- wrap_elements(
  alpha_slab("alpha_pos", "session 1 α<sub>pos</sub>", FALSE) +
    alpha_slab("alpha_neg", "session 1 α<sub>neg</sub>", TRUE) +
    plot_layout(nrow = 1)
) + fig5_tag("D")

## 6. panel E: errors-in-variables slopes, alpha -> attribution change --------------
# alpha is indexed by learn_id (324 ppts), attribution change by id (361 ppts) -- join via id_lookup
win_mod_alpha_indiv <- extract_indiv_pars(win_mod_i_draws, stan_ls_learn_s1) |>
  dplyr::rename(learn_id = id) |>
  dplyr::select(variable, learn_id, delta_par = mean, delta_par_sd = sd)

ca_i_draws <- readr::read_rds("./analyses/stan-fits/attr/fit_attr_multisess_indiv_draws.rds")
ca_theta_indiv <- extract_indiv_pars(ca_i_draws, stan_ls_attr) |>
  dplyr::select(variable, id, delta_symptom = mean)

# learn_id numbering must match between ids_learn_s1 and id_lookup
stopifnot(identical(
  ids_learn_s1 |> dplyr::arrange(learn_id) |> dplyr::pull(subID),
  id_lookup |> dplyr::filter(!is.na(learn_id)) |> dplyr::arrange(learn_id) |> dplyr::pull(subID)
))

build_alpha_theta_df <- function(alpha_var, theta_var) {
  alpha_df <- win_mod_alpha_indiv |> dplyr::filter(variable == alpha_var) |> dplyr::select(-variable)
  theta_df <- ca_theta_indiv |> dplyr::filter(variable == theta_var) |> dplyr::select(-variable)

  id_lookup |>
    dplyr::filter(!is.na(learn_id)) |>
    dplyr::inner_join(alpha_df, by = "learn_id") |>
    dplyr::inner_join(theta_df, by = "id") |>
    dplyr::mutate(group = factor(group, levels = c("control", "causal")))
}

alpha_theta_domains <- list(
  list(alpha_var = "alpha_neg", theta_var = "delta_internal_neg", theta_lab = "Δθ internal-negative"),
  list(alpha_var = "alpha_pos", theta_var = "delta_internal_pos", theta_lab = "Δθ internal-positive"),
  list(alpha_var = "alpha_neg", theta_var = "delta_global_neg",   theta_lab = "Δθ global-negative"),
  list(alpha_var = "alpha_pos", theta_var = "delta_global_pos",   theta_lab = "Δθ global-positive")
)

# standardised slopes from learning_attr_assoc.R -- attenuated by measurement error on both
# sides, so a conservative test of association rather than effect sizes
assoc_slope_csv <- "analyses/outputs/alpha_theta_assoc_slopes.csv"
if (!file.exists(assoc_slope_csv)) {
  stop("run analyses/learning_attr_assoc.R first -- panel E reads ", assoc_slope_csv)
}
assoc_slopes_tbl <- readr::read_csv(assoc_slope_csv, col_types = readr::cols()) |>
  dplyr::mutate(
    outcome = factor(paste0("Δθ ", domain), levels = paste0("Δθ ", unique(domain))),
    level = factor(level, levels = c("difference", ARM_REF, ARM_INT))
  )

figure_5e <- wrap_elements(
  plot_slope_forest(
    assoc_slopes_tbl,
    x_lab = "Δθ (logits) per SD of session 1 learning rate",
    subtitle = "slopes on individual-level posterior means (valence-matched \u03b1)",
    flip_order = TRUE, legend_rows = 1,
    fnt_sz = 1.2
  ) +
    ggplot2:::theme(legend.text = ggplot2::element_text(size = 13)) +
    fig5_tag("E")
)

## 7. Figure 5 assembly ------------------------------------------------------------
fig_5 <- figure_5a /
  (figure_5b + figure_5c + plot_layout(widths = c(0.4, 0.6))) /
  (figure_5d + figure_5e + plot_layout(widths = c(0.4, 0.6))) +
  plot_layout(heights = c(0.36, 0.34, 0.31))
fig_5

## 8. supplementary: the raw scatters behind panel E --------------------------------
# plain ols lines (not panel E's model); point size = 1/sd(alpha) is a display cue, not a weight
alpha_theta_long <- purrr::map_dfr(alpha_theta_domains, function(dm) {
  build_alpha_theta_df(dm$alpha_var, dm$theta_var) |>
    dplyr::mutate(
      domain_type = ifelse(grepl("internal", dm$theta_var), "Δθ internal", "Δθ global"),
      valence = ifelse(grepl("neg", dm$alpha_var), "negative", "positive")
    )
}) |>
  dplyr::mutate(
    domain_type = factor(domain_type, levels = c("Δθ internal", "Δθ global")),
    arm = factor(ARM_LABS[as.character(group)], levels = ARM_LABS[c("control", "causal")])
  )

fig_s3_alpha_theta <- alpha_theta_long |>
  ggplot2::ggplot(ggplot2::aes(x = delta_par, y = delta_symptom, colour = group, fill = group)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_point(ggplot2::aes(size = 1 / delta_par_sd, shape = group), alpha = 0.3) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15, linewidth = 1.1) +
  ggplot2::scale_colour_manual(values = ARM_COLS, guide = "none") +
  ggplot2::scale_fill_manual(values = ARM_COLS, guide = "none") +
  ggplot2::scale_shape_manual(values = ARM_SHP[c("control", "causal")], guide = "none") +
  ggplot2::scale_size_continuous(range = c(1, 3), guide = "none") +
  ggplot2::scale_x_continuous(n.breaks = 4) +
  ggplot2::labs(x = "session 1 α", y = "Δθ (t2 − t1, logits)") +
  ggplot2::facet_grid(domain_type ~ arm + valence, scales = "free") +
  cowplot::theme_minimal_hgrid(font_family = "Open Sans", font_size = SUPPL_FNT)
fig_s3_alpha_theta

## 9. supplementary: parameter and model recovery for M2 -----------------------------
# reads the tables written by learning_recovery.R. rows: (A, B) individual-level recovery,
# (C, D) group-level recovery, (E, F) observed reliability and model recovery. note the replicate
# counts differ (10 for parameter recovery, 5 for model recovery)
rec_paths <- c(
  group = "analyses/outputs/recovery_win_mod_group.csv",
  indiv = "analyses/outputs/recovery_win_mod_indiv.csv",
  rel   = "analyses/outputs/recovery_win_mod_reliability.csv",
  mrec  = "analyses/outputs/recovery_model_confusion.csv"
)
if (!all(file.exists(rec_paths))) {
  stop("run analyses/learning_recovery.R first -- panels A-F read ",
       paste(rec_paths[!file.exists(rec_paths)], collapse = ", "))
}
rec_group_tbl <- readr::read_csv(rec_paths[["group"]], col_types = readr::cols())
rec_indiv_tbl <- readr::read_csv(rec_paths[["indiv"]], col_types = readr::cols())
rec_rel_tbl   <- readr::read_csv(rec_paths[["rel"]], col_types = readr::cols())
mrec_fig_tbl  <- readr::read_csv(rec_paths[["mrec"]], col_types = readr::cols())

# plotmath labels (stack superscript over subscript); REC_PAL is positional, same order
REC_PARAM_LABS <- c(
  alpha_pos = "alpha[pos]^i", alpha_neg = "alpha[neg]^i", beta = "beta^i"
)
REC_PAL <- c(VAL_COLS[["positive"]], VAL_COLS[["negative"]], "#6f6f76")

# per-replicate correlations on the probability scale, to match the plotted points
rec_r_cells <- rec_indiv_tbl |>
  dplyr::group_by(param, arm, rep) |>
  dplyr::summarise(r = stats::cor(true, recovered), .groups = "drop")

# panels A-B show the single replicate whose correlations are closest to the across-replicate means
rec_rep_shown <- rec_r_cells |>
  dplyr::group_by(param, arm) |>
  dplyr::mutate(dev = abs(r - mean(r))) |>
  dplyr::group_by(rep) |>
  dplyr::summarise(dev = mean(dev), .groups = "drop") |>
  dplyr::slice_min(dev, n = 1) |>
  dplyr::pull(rep)

rec_arm_fct <- function(x) factor(x, levels = c(ARM_REF, ARM_INT))
rec_rep1 <- rec_indiv_tbl |>
  dplyr::filter(rep == rec_rep_shown) |>
  dplyr::mutate(id = learn_id, arm = rec_arm_fct(arm))

# annotated with the spread over all replicates
rec_r_labs <- rec_r_cells |>
  dplyr::group_by(param, arm) |>
  dplyr::summarise(
    label = sprintf("bar(italic(r))~'='~%.2f~'[%.2f, %.2f]'", mean(r), min(r), max(r)),
    .groups = "drop"
  ) |>
  dplyr::mutate(arm = rec_arm_fct(arm))

# by arm, not pooled (a pooled r would count the group difference as individual recovery).
# error bars are +/- 1 posterior sd, unclamped
figure_s3_rec_a <- wrap_elements(
  plot_recovery_scatter(
    rec_rep1, param_labs = REC_PARAM_LABS, pal = REC_PAL, row_var = "arm",
    r_labs = rec_r_labs, sd_col = "recovered_sd", parse_labs = TRUE,
    font_size = SUPPL_FNT, x_lab = "true (generating) parameter value"
  ) + fig5_tag("A")
)

# centred within arm, so the heatmap shows within-arm parameter trade-offs
figure_s3_rec_b <- wrap_elements(
  plot_recovery_heatmap(
    rec_rep1 |>
      dplyr::group_by(param, arm) |>
      dplyr::mutate(true = true - mean(true), recovered = recovered - mean(recovered)) |>
      dplyr::ungroup(),
    param_labs = REC_PARAM_LABS, parse_labs = TRUE, font_size = SUPPL_FNT * 1.25
  ) + fig5_tag("B")
)

## panel C: the group-level quantities Figure 5D reports ----
# arm x parameter, with the arm contrast as a third row
REC_GROUP_MAP <- tibble::tribble(
  ~variable,         ~row,         ~col,
  "p_alpha_neg[1]",  "control",    "alpha_neg",
  "p_alpha_neg[2]",  "causal",     "alpha_neg",
  "p_alpha_pos[1]",  "control",    "alpha_pos",
  "p_alpha_pos[2]",  "causal",     "alpha_pos",
  "delta_alpha[1]",  "difference", "alpha_neg",
  "delta_alpha[2]",  "difference", "alpha_pos"
)
REC_GROUP_ROWS <- c(control = "control", causal = "causal", difference = "difference")
REC_GROUP_COLS <- c(alpha_pos = "alpha[pos]", alpha_neg = "alpha[neg]")

rec_group_grid <- rec_group_tbl |>
  dplyr::inner_join(REC_GROUP_MAP, by = "variable") |>
  dplyr::mutate(
    row = factor(REC_GROUP_ROWS[row], levels = unname(REC_GROUP_ROWS)),
    col = factor(REC_GROUP_COLS[col], levels = unname(REC_GROUP_COLS))
  )
stopifnot(nrow(rec_group_grid) == nrow(REC_GROUP_MAP) * dplyr::n_distinct(rec_group_tbl$rep))

figure_s3_rec_c <- wrap_elements(
  plot_recovery_group(
    rec_group_grid, row_var = "row", col_var = "col", parse_labs = TRUE, font_size = SUPPL_FNT * 1.1
  ) +
    fig5_tag("C")
)

## panel D: the free starting values that won M2 the model comparison ----
# all 24 group-level starts (2 arms x 3 blocks x 2 valences, foil and internal-global) x
# replicates, by arm, coloured by block type as in figure 5A (session-1 mapping). the 0.5 line
# is where the anchored variants (M1 / M3) fix the foil
REC_Q0_LABS <- c(ig = "q[0]~'(internal-global)'", foil = "q[0]~'(alternative)'")
Q0_BLOCK_TYPE <- c(`1` = "internal_specific", `2` = "external_global", `3` = "external_specific")
Q0_BLOCK_NUM_LABS <- stats::setNames(names(Q0_BLOCK_TYPE), Q0_BLOCK_TYPE)
Q0_BLOCK_PAL <- stats::setNames(MetBrewer::met.brewer(BLOCK_PAL_NM, 3), names(BLOCK_ALT_LABS))

rec_q0 <- rec_group_tbl |>
  dplyr::filter(grepl("^p_q0_(foil|ig)\\[", variable)) |>
  dplyr::mutate(param = ifelse(grepl("^p_q0_ig", variable), "ig", "foil"), recovered = mean)

# variable is "p_q0_ig[group,block,valence]"; valence stays pooled
q0_idx <- stringr::str_match(rec_q0$variable, "\\[(\\d+),(\\d+),(\\d+)\\]")
rec_q0 <- rec_q0 |>
  dplyr::mutate(
    arm = rec_arm_fct(ifelse(q0_idx[, 2] == "2", ARM_INT, ARM_REF)),
    block_type = factor(Q0_BLOCK_TYPE[q0_idx[, 3]], levels = names(BLOCK_ALT_LABS)),
    param_lab = factor(REC_Q0_LABS[param], levels = REC_Q0_LABS[c("ig", "foil")])
  )
stopifnot(
  !anyNA(rec_q0$arm), !anyNA(rec_q0$block_type),
  nrow(rec_q0) == 2L * 2L * 3L * 2L * dplyr::n_distinct(rec_q0$rep) # {ig,foil} x arm x block x valence x reps
)

rec_q0_labs <- rec_q0 |>
  dplyr::group_by(param_lab, arm) |>
  dplyr::summarise(
    label = sprintf("italic(r)~'='~%.2f", stats::cor(true, recovered)), .groups = "drop"
  )

# fixed scales (q0 is on [0, 1] everywhere); facet_grid2() only for the coloured arm strips
figure_s3_rec_d <- wrap_elements(
  rec_q0 |>
    ggplot2::ggplot(ggplot2::aes(x = true, y = recovered)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dotted", colour = "grey55") +
    ggplot2::geom_point(
      ggplot2::aes(colour = block_type), alpha = 0.55, size = SUPPL_FNT * 1.1 / 7
    ) +
    ggplot2::geom_smooth(
      method = "lm", formula = y ~ x, se = TRUE, linewidth = 1, colour = "grey30"
    ) +
    ggplot2::geom_text(
      data = rec_q0_labs, ggplot2::aes(x = -Inf, y = Inf, label = label),
      hjust = -0.05, vjust = 1.1, inherit.aes = FALSE,
      size = SUPPL_FNT * 1.1 / 3.2, family = "Open Sans", parse = TRUE
    ) +
    ggplot2::scale_x_continuous(n.breaks = 4) +
    ggplot2::scale_colour_manual(values = Q0_BLOCK_PAL, labels = Q0_BLOCK_NUM_LABS, name = "block") +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(arm), cols = ggplot2::vars(param_lab),
      labeller = ggplot2::labeller(param_lab = ggplot2::label_parsed),
      strip = row_strip_themed(rec_q0$arm, SUPPL_FNT * 1.1)
    ) +
    ggplot2::labs(
      x = "true (generating) parameter value", y = "recovered posterior mean",
      subtitle = "dotted line = 0.5, where anchored variants fix the alternative"
    ) +
    cowplot::theme_minimal_grid(font_family = "Open Sans", font_size = SUPPL_FNT * 1.1) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = ggplot2::element_rect(
        fill = "white", colour = "#cacaca", linewidth = 0.5
      ),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
      legend.title = ggplot2::element_text(size = SUPPL_FNT * 0.9),
      legend.text = ggplot2::element_text(size = SUPPL_FNT * 0.9),
      plot.subtitle = ggplot2::element_text(size = SUPPL_FNT, colour = "slategrey")
    ) +
    fig5_tag("D")
)

## panel E: reliability of the individual learning rate ----
# observed lambda = tau^2 / (tau^2 + sigma^2) on the real data (lambda_obs in rec_rel_tbl)
rec_lambda_tbl <- rec_rel_tbl |>
  dplyr::filter(arm != "both") |>
  dplyr::distinct(param, arm, lambda = lambda_obs) |>
  dplyr::mutate(
    arm = factor(arm, levels = c(ARM_REF, ARM_INT)),
    param_lab = factor(REC_PARAM_LABS[param], levels = REC_PARAM_LABS[c("alpha_pos", "alpha_neg")])
  )

rec_lambda_tbl |>
  dplyr::transmute(param, arm, lambda = sprintf("%.2f", lambda)) |>
  as.data.frame() |>
  print(row.names = FALSE)

figure_s3_rec_e <- wrap_elements(
  rec_lambda_tbl |>
    ggplot2::ggplot(ggplot2::aes(x = lambda, y = arm, fill = param)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", lambda)), hjust = 0, nudge_x = 0.02,
      family = "Open Sans", size = SUPPL_FNT / ggplot2::.pt, colour = "grey20"
    ) +
    ggplot2::scale_fill_manual(values = stats::setNames(REC_PAL[1:2], c("alpha_pos", "alpha_neg")), guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::facet_wrap(~ param_lab, nrow = 2, labeller = ggplot2::label_parsed) +
    ggplot2::labs(
      x = "reliability (λ) of the individual learning rate", y = NULL,
      subtitle = "estimated proportion of between-participant variance that is signal versus noise"
    ) +
    cowplot::theme_minimal_vgrid(font_family = "Open Sans", font_size = SUPPL_FNT) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = SUPPL_FNT * 0.85, colour = "slategrey")
    ) +
    fig5_tag("E")
)

## panel F: does LOO recover M2, and are Figure 5B's gaps the size the design implies? ----
# M2-generated replicates only; plot_model_confusion() in model_fns.R covers a full MREC_GEN run
mrec_ref_key <- "m2" # must match MREC_REF in learning_recovery.R
mrec_sel <- mrec_fig_tbl |>
  dplyr::filter(gen == mrec_ref_key) |>
  dplyr::summarise(n_rep = dplyr::n_distinct(rep), hit = sum(selected & is_true))

# rival list derived from learn_model_labels, so a new model can't silently drop out
mrec_calib <- mrec_fig_tbl |>
  dplyr::filter(gen == mrec_ref_key, fitted != mrec_ref_key) |>
  dplyr::mutate(
    fitted_lab = factor(
      learn_model_labels[fitted],
      levels = rev(learn_model_labels[setdiff(names(learn_model_labels), mrec_ref_key)])
    )
  )
stopifnot(!anyNA(mrec_calib$fitted_lab))
mrec_obs <- mrec_calib |> dplyr::distinct(fitted_lab, obs_diff_vs_ref, obs_se)

figure_s3_rec_f <- wrap_elements(
  ggplot2::ggplot(mrec_calib, ggplot2::aes(y = fitted_lab)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_point(
      ggplot2::aes(x = diff_vs_ref, shape = "simulated (one per replicate, M2 true)"),
      colour = "grey35", size = 2.4, alpha = 0.8,
      position = ggplot2::position_jitter(height = 0.1, width = 0, seed = 1)
    ) +
    ggplot2::geom_pointrange(
      data = mrec_obs,
      ggplot2::aes(
        x = obs_diff_vs_ref, xmin = obs_diff_vs_ref - obs_se, xmax = obs_diff_vs_ref + obs_se,
        shape = "observed (real data, ± s.e.)"
      ),
      colour = "#b2182b", size = 0.6, linewidth = 0.8
    ) +
    ggplot2::scale_shape_manual(
      values = c(`simulated (one per replicate, M2 true)` = 16, `observed (real data, ± s.e.)` = 18),
      name = NULL
    ) +
    ggplot2::labs(
      x = "ELPD-LOO difference from winning model (M2)", y = NULL,
      subtitle = sprintf(
        "winning model (M2) selected by ELPD-LOO in %d/%d replicates",
        mrec_sel$hit, mrec_sel$n_rep
      )
    ) +
    cowplot::theme_minimal_vgrid(font_family = "Open Sans", font_size = SUPPL_FNT) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = ggplot2::element_rect(
        fill = "white", colour = "#cacaca", linewidth = 0.5
      ),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
      legend.text = ggplot2::element_text(size = SUPPL_FNT * 0.8),
      plot.subtitle = ggplot2::element_text(size = SUPPL_FNT * 0.85, colour = "slategrey"),
      axis.text.y = ggtext::element_markdown(size = 12, lineheight = 1.2)
    ) +
    fig5_tag("F")
)

fig_s3_recovery <-
  (figure_s3_rec_a + figure_s3_rec_b + plot_layout(widths = c(0.7, 0.3))) /
  (figure_s3_rec_c + figure_s3_rec_d + plot_layout(widths = c(0.55, 0.45))) /
  (figure_s3_rec_e + figure_s3_rec_f + plot_layout(widths = c(0.5, 0.5))) +
  plot_layout(heights = c(0.28, 0.42, 0.3))
fig_s3_recovery

## the numbers to quote alongside the figure, rather than reading them off it ----
# probit scale, as in learning_attr_assoc.R (panel A is on the probability scale)
cat("\nM2 recovery, individual alpha (within arm, probit scale, across replicates):\n")
rec_rel_tbl |>
  dplyr::filter(arm != "both") |>
  dplyr::group_by(param, arm) |>
  dplyr::summarise(
    r = sprintf("%.2f [%.2f, %.2f]", mean(r_probit), min(r_probit), max(r_probit)),
    lambda_sim = sprintf("%.2f", mean(lambda_hat)),
    lambda_obs = sprintf("%.2f", dplyr::first(lambda_obs)),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

# how much of the true between-participant spread survives estimation
cat("\nM2 recovery, individual spread retained (replicate ", rec_rep_shown, "):\n", sep = "")
rec_rep1 |>
  dplyr::group_by(param, arm) |>
  dplyr::summarise(
    true_range = sprintf("%.2f-%.2f", min(true), max(true)),
    recovered_range = sprintf("%.2f-%.2f", min(recovered), max(recovered)),
    retained = sprintf("%.0f%%", 100 * diff(range(recovered)) / diff(range(true))),
    median_post_sd = sprintf("%.3f", stats::median(recovered_sd)),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

# bias is the one thing panels C and D cannot show
cat("\nM2 recovery, group-level bias and HDI coverage:\n")
rec_group_tbl |>
  dplyr::filter(variable %in% REC_GROUP_MAP$variable) |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    bias = sprintf("%+.3f", mean(mean - true)),
    covered = sprintf("%d/%d", sum(covered), dplyr::n()), .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\nM2 recovery, free q0 starts:\n")
rec_q0 |>
  dplyr::group_by(param) |>
  dplyr::summarise(
    n = dplyr::n(), r = sprintf("%.2f", stats::cor(true, recovered)),
    bias = sprintf("%+.3f", mean(mean - true)),
    covered = sprintf("%d/%d", sum(covered), dplyr::n()), .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat(sprintf(
  "\nmodel recovery: M2 selected by LOO in %d/%d replicates generated from M2\n",
  mrec_sel$hit, mrec_sel$n_rep
))
cat("elpd difference from M2 -- simulated (M2 true) vs observed:\n")
mrec_calib |>
  dplyr::group_by(fitted) |>
  dplyr::summarise(
    simulated = sprintf("%+.1f [%+.1f, %+.1f]",
                        mean(diff_vs_ref), min(diff_vs_ref), max(diff_vs_ref)),
    observed = sprintf("%+.1f ± %.1f",
                       dplyr::first(obs_diff_vs_ref), dplyr::first(obs_se)),
    obs_inside_sim = dplyr::first(obs_diff_vs_ref) >= min(diff_vs_ref) &
      dplyr::first(obs_diff_vs_ref) <= max(diff_vs_ref),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)
