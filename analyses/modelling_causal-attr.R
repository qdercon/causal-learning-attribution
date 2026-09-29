# Figure 2 -- attribution change =================================================
# hierarchical model of t1 -> t2 change in the four attribution tendencies, plus
# model-free checks and completer vs. non-completer comparisons.

library(patchwork)
source("analyses/model_fns.R")
source("analyses/study_data.R")

## 0. fit -----------------------------------------------------------------------
ca_group_draws_path <- "./analyses/stan-fits/attr/fit_attr_multisess_group_draws.rds"
ca_indiv_draws_path <- "./analyses/stan-fits/attr/fit_attr_multisess_indiv_draws.rds"

if (file.exists(ca_group_draws_path) && file.exists(ca_indiv_draws_path)) {
  ca_group_draws <- readr::read_rds(ca_group_draws_path)
  ca_i_draws <- readr::read_rds(ca_indiv_draws_path)
} else {
  mod_ca <- cmdstanr::cmdstan_model("analyses/stan-models/causAttr-multisess-ITT-intervention-additive.stan")
  fit_ca <- mod_ca$sample(
    data = stan_ls_attr, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 4000, refresh = 1000
  )
  fit_ca$save_object("./analyses/stan-fits/attr/fit_attr_multisess.rds")
  fit_ca <- readr::read_rds("./analyses/stan-fits/attr/fit_attr_multisess.rds")

  ca_group_vars <- c(
    "mu_internal_theta_neg", "mu_internal_theta_pos", "mu_global_theta_neg", "mu_global_theta_pos",
    "theta_int_internal_neg", "theta_int_internal_pos", "theta_int_global_neg", "theta_int_global_pos",
    "delta_internal_neg", "delta_internal_pos", "delta_global_neg", "delta_global_pos",
    "R_theta_pos", "R_theta_neg"
  )
  ca_indiv_vars <- c(
    "theta_internal_neg", "theta_internal_pos", "theta_global_neg", "theta_global_pos",
    "delta_internal_neg_p", "delta_internal_pos_p", "delta_global_neg_p", "delta_global_pos_p"
  )

  ca_group_draws <- fit_ca$draws(format = "df", variables = ca_group_vars)
  ca_i_draws <- fit_ca$draws(format = "df", variables = ca_indiv_vars)
  saveRDS(ca_group_draws, ca_group_draws_path)
  saveRDS(ca_i_draws, ca_indiv_draws_path)
  rm(fit_ca)
  gc(verbose = FALSE)
}

## 1. panels A-B: model-free attribution behaviour -----------------------------------
ca_behav_df <- prep_data(
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  self_report = self_report_df, filter_by_catch = TRUE, ret = "df"
) |>
  dplyr::mutate(timepoint = sessionA - 1L, group = ifelse(condition01 == 1L, ARM_INT, ARM_REF)) |>
  dplyr::group_by(subID) |>
  dplyr::mutate(non_completed = !any(sessionA == 2L)) |>
  dplyr::ungroup()

ca_behav_plot_pos <- cattr_behav_plot(
  ca_behav_df, plot_type = "positive", mode = "randomised", plot_style = "slope", error_type = "se",
  line_width = 1.2, err_width = 0.3, dodge_width = 0.45, point_size = 4,
  label_colour = VAL_COLS[["positive"]], ylim = c(0, 0.6), fnt_sz = 1.3, axis_fnt_sc = 14
)
ca_behav_plot_neg <- cattr_behav_plot(
  ca_behav_df, plot_type = "negative", mode = "randomised", plot_style = "slope", error_type = "se",
  line_width = 1.2, err_width = 0.3, dodge_width = 0.45, point_size = 4,
  label_colour = VAL_COLS[["negative"]], ylim = c(0, 0.6), fnt_sz = 1.3, axis_fnt_sc = 14
) + ggplot2::theme(legend.position = "none")

figure_3ab <- wrap_elements(
  ca_behav_plot_pos + ca_behav_plot_neg +
    plot_layout(nrow = 2, heights = c(0.5, 0.5)) &
    plot_annotation(tag_levels = list(c("A", "B"))) &
    ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 28))
)

## 2. panels C-F: group-level change in theta, per domain ---------------------------
# retest_col indexes R_theta_{neg,pos}'s [internal_t1, global_t1, internal_t2, global_t2] ordering
ca_domains <- list(
  list(
    param = "internal_pos", param_nm = "θ internal-positive", mu_prefix = "mu_internal_theta_pos",
    retest_col = "R_theta_pos[1,3]"
  ),
  list(
    param = "global_pos", param_nm = "θ global-positive", mu_prefix = "mu_global_theta_pos",
    retest_col = "R_theta_pos[2,4]"
  ),
  list(
    param = "internal_neg", param_nm = "θ internal-negative", mu_prefix = "mu_internal_theta_neg",
    retest_col = "R_theta_neg[1,3]"
  ),
  list(
    param = "global_neg", param_nm = "θ global-negative", mu_prefix = "mu_global_theta_neg",
    retest_col = "R_theta_neg[2,4]"
  )
)
ca_change_tags <- c("C", "D", "E", "F")

ca_change_panels <- lapply(seq_along(ca_domains), function(i) {
  dm <- ca_domains[[i]]
  plot_ca_change(
    ca_group_draws, dm$param, dm$param_nm, mu_prefix = dm$mu_prefix, fnt_sz = 1.5,
    suppress_labels = ifelse(i %% 2 == 0, TRUE, FALSE), flip_order = TRUE
  )$change_panel +
    ggplot2::labs(tag = ca_change_tags[i]) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        family = "Open Sans", face = "bold", size = 28, hjust = ifelse(i %% 2 == 0, 1.5, 0)
      )
    )
})

figure_3cdef <- wrap_elements(
  (ca_change_panels[[1]] + ca_change_panels[[2]]) / (ca_change_panels[[3]] + ca_change_panels[[4]]) &
    plot_annotation(tag_levels = list(ca_change_tags)) &
    ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 28))
)

figure_3top <- figure_3ab + figure_3cdef + plot_layout(nrow = 1, widths = c(0.42, 0.58))

# in-text figures
get_hdi_pd(ca_group_draws, "theta_int_internal_pos")
get_hdi_pd(ca_group_draws, "theta_int_global_pos")
get_hdi_pd(ca_group_draws, "theta_int_internal_neg")
get_hdi_pd(ca_group_draws, "theta_int_global_neg")

# test-retest correlations (t1 vs. t2) per domain, off R_theta_{neg,pos}'s
# [internal_t1, global_t1, internal_t2, global_t2] ordering
get_hdi_pd(ca_group_draws, "R_theta_neg[1,3]") # internal-negative
get_hdi_pd(ca_group_draws, "R_theta_neg[2,4]") # global-negative
get_hdi_pd(ca_group_draws, "R_theta_pos[1,3]") # internal-positive
get_hdi_pd(ca_group_draws, "R_theta_pos[2,4]") # global-positive

# full R_theta_{neg,pos} correlation structure: all 6 unique pairs per valence
ca_theta_idx_labs <- c("internal_t1", "global_t1", "internal_t2", "global_t2")
ca_corr_tbl <- dplyr::bind_rows(
  get_corr_matrix_tbl(ca_group_draws, "R_theta_neg", ca_theta_idx_labs) |> dplyr::mutate(valence = "negative"),
  get_corr_matrix_tbl(ca_group_draws, "R_theta_pos", ca_theta_idx_labs) |> dplyr::mutate(valence = "positive")
) |>
  dplyr::mutate(
    pair_type = dplyr::case_when(
      label_i == "internal_t1" & label_j == "internal_t2" ~ "test-retest (internal)",
      label_i == "global_t1"   & label_j == "global_t2"   ~ "test-retest (global)",
      label_i == "internal_t1" & label_j == "global_t1"   ~ "within-timepoint (t1)",
      label_i == "internal_t2" & label_j == "global_t2"   ~ "within-timepoint (t2)",
      TRUE ~ "cross (timepoint x domain)"
    ),
    dplyr::across(c(mean, hdi_95_lo, hdi_95_hi, pd), ~ round(.x, 3))
  ) |>
  dplyr::select(valence, label_i, label_j, pair_type, mean, hdi_95_lo, hdi_95_hi, pd)

print(ca_corr_tbl, n = 12)
readr::write_csv(ca_corr_tbl, "analyses/outputs/attr_theta_retest_corr.csv")

## 3. panel G: individual-level theta, pre vs. post, with per-participant lines -------
ca_hdi <- extract_indiv_pars(ca_i_draws, stan_ls_attr)

# reliability (lambda) of individual delta-theta, within arm -- pooling would count the arm
# effect itself as between-participant signal. pooled row kept for reference
ca_delta_lambda_tbl <- purrr::map_dfr(ca_domains, function(dm) {
  d <- ca_hdi[ca_hdi$variable == paste0("delta_", dm$param), ]
  purrr::map_dfr(c(ARM_REF, ARM_INT, "pooled"), function(g) {
    dd <- if (g == "pooled") d else d[d$group == g, ]
    tibble::tibble(
      param = dm$param, param_nm = dm$param_nm, arm = g, n = nrow(dd),
      lambda = reliability(dd$mean, dd$sd)
    )
  })
})

# the arm split must match stan_ls_attr$condition in id order
ca_delta_check <- ca_hdi[ca_hdi$variable == paste0("delta_", ca_domains[[1]]$param), ]
stopifnot(
  all(ca_hdi$group %in% c(ARM_REF, ARM_INT)),
  identical(
    ca_delta_check$group[order(ca_delta_check$id)],
    ifelse(stan_ls_attr$condition == 1L, ARM_INT, ARM_REF)
  ),
  nrow(ca_delta_lambda_tbl) == length(ca_domains) * 3L,
  all(ca_delta_lambda_tbl$n[ca_delta_lambda_tbl$arm == "pooled"] == nrow(ids_attr))
)

print(ca_delta_lambda_tbl, n = 12)
readr::write_csv(ca_delta_lambda_tbl, "analyses/outputs/attr_theta_delta_reliability.csv")

# shared y-range across sub-panels so magnitudes are comparable
ca_box_panels <- lapply(ca_domains, function(dm) {
  r_retest <- mean(ca_group_draws[[dm$retest_col]])
  p <- plot_indiv_pars(
    ca_hdi, paste0("theta_", dm$param), dm$param_nm, type = "box",
    custom_pal = unname(ARM_COLS[c("control", "causal")]),
    compl = FALSE, indiv_lines = TRUE, fnt_sz = 1.3, sz_scale = c(0.5, 2)
  ) +
    ggplot2::coord_cartesian(ylim = c(-2.5, 4)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "slategray") +
    ggplot2::annotate(
      "text", x = 1, y = 4,
      label = sprintf("test-retest r = %.2f", r_retest), family = "Open Sans", size = 6, colour = "slateblue"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 20, family = "Open Sans"),
    )
  p + ggplot2::labs(y = gsub("\\*\\*", "", p$labels$y))
})

figure_3g <- wrap_elements(
  (ca_box_panels[[1]] + ca_box_panels[[2]]) / (ca_box_panels[[3]] + ca_box_panels[[4]]) +
    plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom") &
    plot_annotation(tag_levels = list("G")) &
    ggplot2::theme(
      plot.tag.position = c(0.025, 0.975),
      plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 28)
    )
)

## 4. panel H: theta-change x questionnaire-change correlation heatmaps -------------
ca_par_labels <- c(
  "internal_pos" = "θ internal-<br>positive",
  "global_pos"   = "θ global-<br>positive",
  "internal_neg" = "θ internal-<br>negative",
  "global_neg"   = "θ global-<br>negative"
)
ca_qn_labels <- c(
  "PHQ9" = "ΔPHQ-9", "DAQ" = "ΔDAQ",
  "DAS" = "ΔDAS", "ERQCR" = "ΔERQ-CR<sup>†</sup>", "miniSPIN" = "ΔminiSPIN"
)
# raw per-timepoint draw columns are named theta_{param}[id,t], not {param}[id,t] --
# map them onto the same keys used in ca_par_labels
ca_indiv_par_map <- c(
  "theta_internal_pos" = "internal_pos", "theta_global_pos" = "global_pos",
  "theta_internal_neg" = "internal_neg", "theta_global_neg" = "global_neg"
)

symptom_corrs_ca <- get_symptom_corrs(
  ids = ids_attr,
  draws = ca_i_draws,
  qqnrs = qqnrs_t2 |> dplyr::select(-tidyr::contains("PHQ2")),
  delta_par_pattern = "delta_(.*)_p\\[(.*)\\]",
  par_labels = ca_par_labels,
  indiv_par_map = ca_indiv_par_map,
  qns_to_invert = c("ERQCR")
)

control_heatmap <- plot_correlation_heatmap(
  symptom_corrs_ca$summary_df, int_group = "control",
  par_labels = ca_par_labels, qn_labels = ca_qn_labels,
  primary_qns = c("PHQ9", "DAQ"),
  cor_lims = c(-0.25, 0.25),
  htmp_title = "control learning", title_clr = ARM_COLS[["control"]],
  label_type = "r_only", fnt_sz = 1.25, txt_sz = 7.5
)
causal_heatmap <- plot_correlation_heatmap(
  symptom_corrs_ca$summary_df, int_group = "causal",
  par_labels = ca_par_labels, qn_labels = ca_qn_labels,
  primary_qns = c("PHQ9", "DAQ"),
  cor_lims = c(-0.25, 0.25),
  htmp_title = "causal learning", title_clr = ARM_COLS[["causal"]],
  label_type = "r_only", fnt_sz = 1.25, txt_sz = 7.5
)

symptom_corrs_ca$summary_df |>
  dplyr::filter(
    group == "causal", questionnaire %in% c("PHQ9_delta", "DAQ_delta"), grepl("internal", par)
  )
symptom_corrs_ca$summary_df |>
  dplyr::filter(
    group == "difference", questionnaire %in% c("PHQ9_delta", "DAQ_delta"), grepl("internal", par)
  )

symptom_corrs_ca$summary_df |>
  dplyr::filter(
    group == "causal", questionnaire %in% c("miniSPIN_delta"), grepl("internal", par)
  ) |>
  dplyr::filter(grepl("negative", par))

figure_3h <- wrap_elements(
  control_heatmap + causal_heatmap +
    plot_layout(nrow = 2, guides = "collect") &
    ggplot2::theme(legend.position = "right") &
    plot_annotation(tag_levels = list("H")) &
    ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 28))
)

## 5. Figure 2 assembly ----------------------------------------------------------
fig_3 <- figure_3top / (figure_3g + figure_3h + plot_layout(widths = c(0.52, 0.48)))
fig_3

## 6. model-free logistic regressions of the four tendencies -------------------------
CA_MF_SEED <- 20260814
ca_mf_cache_dir <- "analyses/stan-fits/attr/model-free"
dir.create(ca_mf_cache_dir, showWarnings = FALSE, recursive = TRUE)

ca_mf_df <- ca_behav_df |> dplyr::mutate(group = factor(group, levels = c(ARM_REF, ARM_INT)))

stopifnot(
  dplyr::n_distinct(ca_mf_df$subID) == 361,
  levels(ca_mf_df$group)[1] == ARM_REF,     # so b_group{INT} IS the causal - control effect
  all(ca_mf_df$timepoint %in% 0:1),         # 0 = prescreen, 1 = postscreen
  all(ca_mf_df$internalChosen %in% 0:1), all(ca_mf_df$globalChosen %in% 0:1),
  all(dplyr::count(ca_mf_df, subID, timepoint, valence)$n == 16)
)

ca_mf_domains <- tibble::tribble(
  ~key,           ~outcome,         ~valence,   ~domain,
  "internal_pos", "internalChosen", "positive", "internal-positive attribution",
  "global_pos",   "globalChosen",   "positive", "global-positive attribution",
  "internal_neg", "internalChosen", "negative", "internal-negative attribution",
  "global_neg",   "globalChosen",   "negative", "global-negative attribution"
)

# hdis on the log-odds scale, then exponentiated. d_naive = b * sqrt(3) / pi (chinn, 2000);
# d_ml also adds the between-subject intercept variance to the denominator (hox, 2010)
ca_mf_or <- function(fit, term) {
  v <- coef_draws(fit, term)
  sg_subj <- as.vector(posterior::as_draws_matrix(fit$fit)[, "sd_subID__Intercept"])
  h <- bayestestR::hdi(v, ci = 0.95)

  d_naive <- v * sqrt(3) / pi
  d_ml <- v / sqrt(sg_subj^2 + pi^2 / 3)
  hn <- bayestestR::hdi(d_naive, ci = 0.95)
  hm <- bayestestR::hdi(d_ml, ci = 0.95)

  tibble::tibble(
    OR = exp(mean(v)), OR_lo95 = exp(h$CI_low), OR_hi95 = exp(h$CI_high),
    pd = as.numeric(bayestestR::p_direction(v, method = "direct")),
    d_naive = mean(d_naive), d_naive_lo95 = hn$CI_low, d_naive_hi95 = hn$CI_high,
    d_ml = mean(d_ml), d_ml_lo95 = hm$CI_low, d_ml_hi95 = hm$CI_high
  )
}

ca_mf_brm <- function(formula, data, cache) {
  brms::brm(
    formula = formula, data = data, family = brms::bernoulli(link = "logit"),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = CA_MF_SEED,
    file = cache, file_refit = "on_change"
  )
}
ca_mf_fmt <- function(tbl) {
  tbl |>
    dplyr::transmute(
      domain, effect,
      OR = sprintf("%.3f", OR),
      HDI95 = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
      pd = sprintf("%.3f", pd),
      d_naive = sprintf("%.3f", d_naive),
      d_naive_HDI95 = sprintf("[%.3f, %.3f]", d_naive_lo95, d_naive_hi95),
      d_ml = sprintf("%.3f", d_ml),
      d_ml_HDI95 = sprintf("[%.3f, %.3f]", d_ml_lo95, d_ml_hi95)
    )
}

# effect labels/order
ca_mf_terms <- tibble::tibble(
  term   = c(paste0("group", ARM_INT), "timepoint", paste0("group", ARM_INT, ":timepoint")),
  effect = c(
    paste0("group (", ARM_INT, " vs. ", ARM_REF, ")"), "timepoint", "group × timepoint"
  )
)

ca_mf_fits <- list()
ca_mf_rows <- list()

for (i in seq_len(nrow(ca_mf_domains))) {
  key <- ca_mf_domains$key[i]
  vlnc <- ca_mf_domains$valence[i]
  df <- dplyr::filter(ca_mf_df, valence == vlnc)
  fm <- stats::as.formula(paste(ca_mf_domains$outcome[i], "~ group * timepoint + trialNo + (1 | subID)"))

  fit <- ca_mf_brm(fm, df, file.path(ca_mf_cache_dir, paste0("mf_", key)))

  ca_mf_fits[[key]] <- fit
  ca_mf_rows[[key]] <- dplyr::bind_cols(
    tibble::tibble(domain = ca_mf_domains$domain[i], effect = ca_mf_terms$effect),
    dplyr::bind_rows(lapply(paste0("b_", ca_mf_terms$term), function(tm) ca_mf_or(fit, tm)))
  )
}
ca_mf_tbl <- dplyr::bind_rows(ca_mf_rows)

print(ca_mf_fmt(ca_mf_tbl), n = 12)
readr::write_csv(ca_mf_tbl, "analyses/outputs/attr_modelfree_or.csv")

## 7. baseline (t1) tendencies: non-completers vs. completers -----------------------
ca_ncpl_df <- ca_mf_df |>
  dplyr::filter(timepoint == 0L) |>
  dplyr::left_join(
    self_report_df |> dplyr::distinct(subID = prolificSubID, sessions_completed), by = "subID"
  ) |>
  dplyr::mutate(ncpl_no_t2 = as.integer(non_completed))

ca_ncpl_defs <- tibble::tribble(
  ~key,    ~var,         ~definition,
  "no_t2", "ncpl_no_t2", "non-completer (no postscreen)",
)

stopifnot(
  !anyNA(ca_ncpl_df$sessions_completed),
  dplyr::n_distinct(ca_ncpl_df$subID) == 361,
  # pinned so a change upstream is caught rather than silently re-baselining the comparison
  dplyr::n_distinct(ca_ncpl_df$subID[ca_ncpl_df$ncpl_no_t2 == 1L]) == 69
)

ca_ncpl_fits <- list()
ca_ncpl_rows <- list()

for (i in seq_len(nrow(ca_mf_domains))) {
  vlnc <- ca_mf_domains$valence[i]
  df <- dplyr::filter(ca_ncpl_df, valence == vlnc)
  ncpl_var <- "ncpl_no_t2"
  key <- paste0(ca_mf_domains$key[i], "_no_t2")
  fm <- stats::as.formula(paste(ca_mf_domains$outcome[i], "~", ncpl_var, "+ trialNo + (1 | subID)"))
  fit <- ca_mf_brm(fm, df, file.path(ca_mf_cache_dir, paste0("mf_ncpl_", key)))

  ca_ncpl_fits[[key]] <- fit
  ca_ncpl_rows[[key]] <- dplyr::bind_cols(
    tibble::tibble(
      domain = ca_mf_domains$domain[i],
      effect = "non-completer (no postscreen)",
      n_ncpl = dplyr::n_distinct(df$subID[df[[ncpl_var]] == 1L])
    ),
    ca_mf_or(fit, paste0("b_", ncpl_var))
  )
}

ca_ncpl_tbl <- dplyr::bind_rows(ca_ncpl_rows)

print(ca_mf_fmt(ca_ncpl_tbl), n = 4)
readr::write_csv(ca_ncpl_tbl, "analyses/outputs/attr_modelfree_dropout_baseline.csv")

## 8. associations between changes in the four attribution tendencies ---------------
# per-draw correlations between the four delta-theta domains, within arm (the arm drives the
# deltas, so pooling would add between-arm differences); motivates per-domain mediator fits
ca_delta_vars <- c(
  internal_pos = "delta_internal_pos_p", global_pos = "delta_global_pos_p",
  internal_neg = "delta_internal_neg_p", global_neg = "delta_global_neg_p"
)
ca_delta_pairs <- utils::combn(names(ca_delta_vars), 2)
ca_arm_ids <- list(
  control = which(stan_ls_attr$condition == 0),
  causal  = which(stan_ls_attr$condition == 1)
)

ca_delta_assoc_tbl <- lapply(seq_len(ncol(ca_delta_pairs)), function(p) {
  d1 <- ca_delta_pairs[1, p]
  d2 <- ca_delta_pairs[2, p]
  r_control <- get_draws_corr_raw(ca_i_draws, ca_delta_vars[[d1]], ca_delta_vars[[d2]], ids = ca_arm_ids$control)
  r_causal  <- get_draws_corr_raw(ca_i_draws, ca_delta_vars[[d1]], ca_delta_vars[[d2]], ids = ca_arm_ids$causal)
  dplyr::bind_rows(
    summarise_draws_r(r_control) |> dplyr::mutate(arm = "control", .before = 1),
    summarise_draws_r(r_causal) |> dplyr::mutate(arm = "causal", .before = 1),
    summarise_draws_r(r_causal - r_control) |> dplyr::mutate(arm = "difference", .before = 1)
  ) |>
    dplyr::mutate(domain_1 = d1, domain_2 = d2, .before = 1)
}) |>
  dplyr::bind_rows() |>
  dplyr::mutate(dplyr::across(c(mean, hdi_95_lo, hdi_95_hi, pd), ~ round(.x, 3)))

print(ca_delta_assoc_tbl, n = 18)
readr::write_csv(ca_delta_assoc_tbl, "analyses/outputs/attr_delta_assoc.csv")

## 9. supplementary: attribution differences by outcome-completion status -----------
# do non-completers (no t2, n = 69) differ at baseline from completers (n = 292)?

### (a) raw behaviour: t1 attribution choices, completers vs. non-completers
ca_ncompl_behav_plot <- cattr_behav_plot(
  ca_behav_df, plot_type = "both", mode = "completer_status", plot_style = "bar",
  error_type = "se", fnt_sz = 1.1, axis_fnt_sc = 13
) +
  ggplot2::labs(tag = "A") +
  ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 28))

### (b) model-estimated non-completion offset (theta_ncompl_*), per domain
ca_ncompl_draws_path <- "./analyses/stan-fits/attr/fit_attr_multisess_ncompl_draws.rds"
ca_ncompl_vars <- c(
  "theta_ncompl_internal_pos", "theta_ncompl_global_pos",
  "theta_ncompl_internal_neg", "theta_ncompl_global_neg"
)

if (file.exists(ca_ncompl_draws_path)) {
  ca_ncompl_draws <- readr::read_rds(ca_ncompl_draws_path)
} else {
  fit_ca_full <- readr::read_rds("./analyses/stan-fits/attr/fit_attr_multisess.rds")
  ca_ncompl_draws <- fit_ca_full$draws(format = "df", variables = ca_ncompl_vars)
  saveRDS(ca_ncompl_draws, ca_ncompl_draws_path)
  rm(fit_ca_full)
  gc(verbose = FALSE)
}

get_hdi_pd(ca_ncompl_draws, "theta_ncompl_internal_pos")
get_hdi_pd(ca_ncompl_draws, "theta_ncompl_global_pos")
get_hdi_pd(ca_ncompl_draws, "theta_ncompl_internal_neg")
get_hdi_pd(ca_ncompl_draws, "theta_ncompl_global_neg")

ca_ncompl_long <- ca_ncompl_draws |>
  dplyr::select(tidyselect::all_of(ca_ncompl_vars)) |>
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value") |>
  dplyr::mutate(
    domain = factor(
      sub("^theta_ncompl_", "", variable),
      levels = c("internal_pos", "global_pos", "internal_neg", "global_neg"),
      labels = c("θ internal-positive", "θ global-positive", "θ internal-negative", "θ global-negative")
    ),
    valence = ifelse(grepl("pos", variable), "positive", "negative")
  )

ca_ncompl_offset_plot <- ca_ncompl_long |>
  ggplot2::ggplot(ggplot2::aes(x = value, y = domain, fill = valence, colour = valence)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
  ggdist::stat_pointinterval(
    .width = c(0.95, 0.99), point_interval = "mean_hdci",
    interval_size_range = c(1, 3), fatten_point = 2
  ) +
  ggplot2::scale_colour_manual(values = VAL_COLS, guide = "none") +
  ggplot2::scale_fill_manual(values = VAL_COLS, guide = "none") +
  ggplot2::scale_y_discrete(limits = rev) +
  ggplot2::labs(
    x = "non-returner offset, θ (non-returner − returner, a.u.)", y = NULL, tag = "B"
  ) +
  cowplot::theme_half_open(font_family = "Open Sans", font_size = 18) +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 14),
    plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 28)
  )

suppl_fig_ncompl <- ca_ncompl_behav_plot + ca_ncompl_offset_plot + plot_layout(widths = c(0.6, 0.4))
suppl_fig_ncompl
