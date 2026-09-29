# Figure 4 (mediation, both outcomes) + supplementary decompositions ==============
# single-mediator fits, one per attribution domain x pre-registered outcome: DAQ
# (proximal, H1b) and PHQ-9 (distal, H1a). symptom scores are scaled by sd(t2) with an
# ANCOVA symptom submodel (the "_sdt2-ancova-" fits). figure 4 shows both outcomes at
# once (section 2); section 1's per-outcome builder makes figures S1-S2.

library(patchwork)
source("analyses/study_data.R")

## 0. shared setup -----------------------------------------------------------------
domain_short_long <- c(
  int_neg = "internal_neg", int_pos = "internal_pos",
  glob_neg = "global_neg", glob_pos = "global_pos"
)
domain_nms <- c(
  internal_neg = "θ internal-negative", internal_pos = "θ internal-positive",
  global_neg = "θ global-negative", global_pos = "θ global-positive"
)
# short labels for legends / diagram boxes -- always subset by name, never positionally
mediator_nms <- c(
  internal_neg = "internal-negative", internal_pos = "internal-positive",
  global_neg = "global-negative", global_pos = "global-positive"
)
mediator_cols <- stats::setNames(
  MetBrewer::met.brewer("Hokusai1")[c(3, 4, 6, 7)],
  c("internal_neg", "global_neg", "internal_pos", "global_pos")
)
# display order only (positive valence first, matching the other figures)
domain_order <- c("internal_pos", "global_pos", "internal_neg", "global_neg")
stopifnot(setequal(domain_order, names(domain_nms)))

# joint (4-mediator) model's generated quantities -- unused by the single-mediator path below
med_gq_vars <- c(
  paste0("a_", names(domain_nms), "_control"), paste0("a_", names(domain_nms), "_causal"),
  paste0("b_", names(domain_nms), "_control"), paste0("b_", names(domain_nms), "_causal"),
  "direct_arm", "total_arm",
  paste0("indirect_", names(domain_nms), "_control"), paste0("indirect_", names(domain_nms), "_causal"),
  paste0("indirect_", names(domain_nms), "_diff"), paste0("index_mod_med_", names(domain_nms)),
  paste0("prop_med_", names(domain_nms))
)
beta_symp_vars <- as.vector(outer(
  paste0("beta_symp_", names(domain_short_long)), c("B", "B_group", "W", "W_group"), paste0
))
delta_p_vars <- paste0("delta_", names(domain_nms), "_p")

## 1. build mediation figures from individual fits -----------------------------
load_single_mediator_domain <- function(fit_rds, cache_stub, domain) {
  group_path <- paste0(cache_stub, "_group_draws.rds")
  indiv_path <- paste0(cache_stub, "_indiv_draws.rds")

  if (file.exists(group_path) && file.exists(indiv_path)) {
    gd <- readr::read_rds(group_path)
    id <- readr::read_rds(indiv_path)
  } else {
    fit <- readr::read_rds(fit_rds)
    # a_med_*_obs: a-paths over participants with observed t2 attribution only
    gd <- fit$draws(format = "df", variables = c(
      "a_med_control", "a_med_causal", "a_med_control_obs", "a_med_causal_obs",
      "b_med_control", "b_med_causal", "direct_arm", "total_arm",
      "indirect_med_control", "indirect_med_causal", "indirect_med_diff", "index_mod_med_med",
      "beta_medB", "beta_medB_group", "beta_medW", "beta_medW_group"
    ))
    id <- fit$draws(format = "df", variables = paste0("delta_", domain, "_p"))
    saveRDS(gd, group_path)
    saveRDS(id, indiv_path)
    rm(fit)
    gc(verbose = FALSE)
  }

  combined <- dplyr::bind_cols(gd, id |> dplyr::select(-tidyselect::any_of(names(gd))))
  combined <- combined |> dplyr::rename(
    !!paste0("a_", domain, "_control") := a_med_control,
    !!paste0("a_", domain, "_causal") := a_med_causal,
    !!paste0("b_", domain, "_control") := b_med_control,
    !!paste0("b_", domain, "_causal") := b_med_causal,
    !!paste0("indirect_", domain, "_control") := indirect_med_control,
    !!paste0("indirect_", domain, "_causal") := indirect_med_causal,
    !!paste0("indirect_", domain, "_diff") := indirect_med_diff,
    !!paste0("index_mod_med_", domain) := index_mod_med_med,
    !!paste0("direct_arm_", domain) := direct_arm,
    !!paste0("total_arm_", domain) := total_arm
  )

  # between/within symptom-attribution association columns for plot_sympt_decomp()
  prefix <- paste0("symp_", domain)
  combined[[paste0(prefix, "_t1_group_control")]] <- combined$beta_medB
  combined[[paste0(prefix, "_t1_group_causal")]]  <- combined$beta_medB + combined$beta_medB_group
  combined[[paste0(prefix, "_t1_diff")]]          <- combined$beta_medB_group
  combined[[paste0(prefix, "_t2_change_control")]] <- combined$beta_medW
  combined[[paste0(prefix, "_t2_change_causal")]]  <- combined$beta_medW + combined$beta_medW_group
  combined[[paste0(prefix, "_t2_change_diff")]]    <- combined$beta_medW_group

  combined
}

# `sections`: "decomp" (per-domain symptom decomposition), "direct_total" (direct c' /
# indirect / total-arm per domain), "diagrams" (path diagrams), "indirect" (all-domain
# indirect effects). panel tags run consecutively over the requested sections.
build_single_mediator_figure <- function(fit_dir, cache_dir, scale_stub, symptom_nm, tag_offset = 0,
                                         sections = c("decomp", "direct_total", "diagrams", "indirect"),
                                         interval_size_range = c(1, 3), fatten_point = 2,
                                         x_n_breaks = NULL, scatter_n_breaks = 4, fnt_sz = 1) {
  sections <- match.arg(sections, several.ok = TRUE)
  tag_i <- tag_offset
  next_tags <- function(n) {
    tags <- LETTERS[(tag_i + 1):(tag_i + n)]
    tag_i <<- tag_i + n
    tags
  }
  single_med_draws <- lapply(names(domain_nms), function(d) {
    load_single_mediator_domain(
      sprintf("%s/fit_attr_symp_%s-ancova-singlemed-%s.rds", fit_dir, scale_stub, d),
      sprintf("%s/fit_attr_symp_%s-ancova-singlemed-%s", cache_dir, scale_stub, d),
      d
    )
  })
  names(single_med_draws) <- names(domain_nms)
  panels <- list()
  heights <- c()

  # each fit must be paired with the stan list it was sampled on; unknown scales error
  stan_ls_symp <- switch(scale_stub,
    daq_sdt2 = stan_ls_symp_daq_sdt2, phq_sdt2 = stan_ls_symp_phq_sdt2,
    stop("unknown scale_stub: ", scale_stub)
  )

  if ("decomp" %in% sections) {
    ## panels A-D: per-domain symptom decomposition, each from that domain's own fit --
    decomp_plts <- lapply(names(domain_nms), function(d) {
      plot_sympt_decomp(
        single_med_draws[[d]], stan_ls = stan_ls_symp, prefix = paste0("symp_", d),
        param_nm = domain_nms[[d]], symptom_nm = symptom_nm,
        interval_size_range = interval_size_range, fatten_point = fatten_point,
        x_n_breaks = x_n_breaks, scatter_n_breaks = scatter_n_breaks, flip_order = TRUE,
        fnt_sz = 0.85 * fnt_sz
      )
    })
    names(decomp_plts) <- names(domain_nms)

    decomp_panels <- Map(function(d, tag) {
      wrap_elements(
        decomp_plts[[d]]$scatter_panel + decomp_plts[[d]]$within_panel + plot_layout(widths = c(0.4, 0.6))
      ) +
        ggplot2::labs(tag = tag) +
        ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 24))
    }, domain_order, next_tags(4))

    panels$decomp <- wrap_elements(
      (decomp_panels[[domain_order[1]]] + decomp_panels[[domain_order[2]]]) /
        (decomp_panels[[domain_order[3]]] + decomp_panels[[domain_order[4]]])
    )
    heights["decomp"] <- 0.4
  }

  ## panel E: direct (c'), indirect and total-arm, one mini-panel per domain ---------------
  # separate panels since each domain comes from its own fit. direct + indirect need not sum
  # to the arm effect (baseline theta is arm-imbalanced), so total_arm is shown alongside
  combined_direct_total <- dplyr::bind_cols(lapply(names(domain_nms), function(d) {
    single_med_draws[[d]] |>
      dplyr::select(tidyselect::all_of(c(
        paste0("direct_arm_", d), paste0("total_arm_", d), paste0("indirect_", d, "_diff")
      )))
  }))

  if ("direct_total" %in% sections) {
    direct_total_plts <- lapply(domain_order, function(d) {
      plot_direct_total_n(
        combined_direct_total, suffixes = d, mediator_nms = mediator_nms[d],
        fnt_sz = fnt_sz * 0.7
      ) +
        ggplot2::xlim(c(-1, 1)) +
        ggplot2::labs(title = domain_nms[[d]]) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            family = "Open Sans SemiBold", size = 11 * fnt_sz, hjust = 0.5,
            colour = if (grepl("neg", d)) VAL_COLS[["negative"]] else VAL_COLS[["positive"]]
          ),
          axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()
        )
    })
    names(direct_total_plts) <- domain_order

    direct_total <- wrap_elements(
      direct_total_plts[[domain_order[1]]] + direct_total_plts[[domain_order[2]]] +
        direct_total_plts[[domain_order[3]]] + direct_total_plts[[domain_order[4]]] +
        plot_layout(nrow = 1) +
        patchwork::plot_layout(guides = "collect") &
        ggplot2::theme(legend.position = "bottom")
    ) +
      ggplot2::labs(tag = next_tags(1)) +
      ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 24))

    panels$direct_total <- plot_spacer() + direct_total + plot_spacer() +
      plot_layout(nrow = 1, widths = c(0.0025, 0.995, 0.0025))

    heights["direct_total"] <- 0.2
  }

  ## panels F-I: single-mediator path diagrams, 2x2 grid ------------------------------------
  # short box labels (full names overflow); the panel title states the valence
  mediator_box_nms <- c(
    internal_neg = "internal attribution", internal_pos = "internal attribution",
    global_neg = "global attribution", global_pos = "global attribution"
  )
  build_single_diagram <- function(d, tag) {
    plts <- plot_single_mediation(
      single_med_draws[[d]], mediator_suffix = d, mediator_nm = mediator_box_nms[[d]],
      mediator_col = mediator_cols[[d]], title = gsub("_", "-", d),
      title_col = if (grepl("neg", d)) VAL_COLS[["negative"]] else VAL_COLS[["positive"]],
      flip_order = TRUE, fnt_sz = fnt_sz
    )
    diagram <- wrap_elements(
      plts$diagram +
        patchwork::inset_element(
          plts$a_path, left = 0.065, bottom = 0.55, right = 0.32, top = 0.975, align_to = "panel"
        ) +
        patchwork::inset_element(
          plts$b_path, left = 0.68, bottom = 0.55, right = 0.935, top = 0.975, align_to = "panel"
        ) +
        # patchwork::inset_element(
        #   plts$direct_path, left = 0.38, bottom = 0.05, right = 0.62, top = 0.4, align_to = "panel"
        # ) +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
    )
    wrap_elements(
      diagram + wrap_elements(plts$indirect) + plot_layout(widths = c(0.62, 0.38))
    ) +
      ggplot2::labs(tag = tag) +
      ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 26))
  }

  if ("diagrams" %in% sections) {
    single_diagrams <- Map(build_single_diagram, domain_order, next_tags(4))

    panels$diagrams <- wrap_elements(
      (single_diagrams[[domain_order[1]]] + single_diagrams[[domain_order[2]]]) /
        (single_diagrams[[domain_order[3]]] + single_diagrams[[domain_order[4]]])
    )
    heights["diagrams"] <- 0.42
  }

  ## panel I: combined indirect effect, all four domains at once --------------------
  # only domain-suffixed columns are combined across fits (unsuffixed names would clash)
  combined_indirect <- dplyr::bind_cols(lapply(names(domain_nms), function(d) {
    single_med_draws[[d]] |>
      dplyr::select(
        tidyselect::starts_with(paste0("indirect_", d)), tidyselect::starts_with(paste0("index_mod_med_", d))
      )
  }))

  if ("indirect" %in% sections) {
    panels$indirect <- wrap_elements(
      plot_paths_n(
        combined_indirect, path = "indirect", suffixes = domain_order,
        mediator_nms = mediator_nms, mediator_cols = mediator_cols,
        flip_order = TRUE,
        interval_size_range = interval_size_range, fatten_point = fatten_point, fnt_sz = 1.1 * fnt_sz
      )
    ) +
      ggplot2::labs(tag = next_tags(1)) +
      ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 28))
    heights["indirect"] <- 0.24
  }

  ## honest reporting -----------------------------------------------------------------
  cat(sprintf("\n========== %s (single-mediator): indirect effects ==========\n", symptom_nm))
  for (d in domain_order) {
    for (col in c(
      paste0("indirect_", d, "_control"), paste0("indirect_", d, "_causal"), paste0("index_mod_med_", d)
    )) {
      v <- combined_indirect[[col]]
      h <- bayestestR::hdi(v, ci = 0.95)
      pd <- bayestestR::p_direction(v, method = "direct")
      cat(sprintf(
        "%-32s mean=%+.3f  95%%HDI=[%+.3f, %+.3f]  pd=%.1f%%\n",
        col, mean(v), h$CI_low, h$CI_high, 100 * as.numeric(pd)
      ))
    }
  }
  cat(sprintf("\n========== %s (single-mediator): direct (c') + total-arm ==========\n", symptom_nm))
  for (d in domain_order) {
    for (col in c(paste0("direct_arm_", d), paste0("total_arm_", d))) {
      v <- combined_direct_total[[col]]
      h <- bayestestR::hdi(v, ci = 0.95)
      pd <- bayestestR::p_direction(v, method = "direct")
      cat(sprintf(
        "%-32s mean=%+.3f  95%%HDI=[%+.3f, %+.3f]  pd=%.1f%%\n",
        col, mean(v), h$CI_low, h$CI_high, 100 * as.numeric(pd)
      ))
    }
  }

  Reduce(`/`, panels) + plot_layout(nrow = length(panels), heights = unname(heights[names(panels)]))
}

## 2. one figure for both outcomes ------------------------------------------------
# a-paths are (near-)identical across outcomes, so they are drawn once (checked below);
# b-paths and indirect effects are drawn per outcome.
#   scales: named vector of scale_stub = display name, in facet order.
build_combined_med_figure <- function(fit_dir, cache_dir, scales, a_path_tol = 0.02,
                                      interval_size_range = c(0.5, 2), fatten_point = 1.5,
                                      panel_heights = c(0.26, 0.38, 0.36), outcome_cols = NULL,
                                      fnt_sz = 1) {
  # facet-strip colours match figure 1; unrecognised stubs fall back to plain strips
  if (is.null(outcome_cols)) {
    outcome_cols <- stats::setNames(unname(SYMP_COLS[sub("_.*$", "", names(scales))]), unname(scales))
    if (anyNA(outcome_cols)) outcome_cols <- NULL
  }
  # per-outcome draws, domain-suffixed path columns only
  scale_draws <- lapply(names(scales), function(scale_stub) {
    dplyr::bind_cols(lapply(names(domain_nms), function(d) {
      load_single_mediator_domain(
        sprintf("%s/fit_attr_symp_%s-ancova-singlemed-%s.rds", fit_dir, scale_stub, d),
        sprintf("%s/fit_attr_symp_%s-ancova-singlemed-%s", cache_dir, scale_stub, d),
        d
      ) |>
        dplyr::select(tidyselect::matches(
          sprintf("^(a|b|indirect)_%s_(control|causal|diff)$|^index_mod_med_%s$", d, d)
        ))
    }))
  })
  names(scale_draws) <- unname(scales)

  ## a-path agreement across outcomes ------------------------------------------------
  # panel B shows the first scale's a-paths only; warn if posterior means differ by > a_path_tol
  # (a researcher choice -- separate mcmc runs, so exact equality isn't expected)
  a_cols <- as.vector(outer(paste0("a_", names(domain_nms), "_"), c("control", "causal"), paste0))
  a_means <- vapply(
    scale_draws,
    function(d) vapply(a_cols, function(col) mean(d[[col]]), numeric(1)),
    numeric(length(a_cols))
  )
  a_gap <- apply(a_means, 1, function(r) max(r) - min(r))
  cat(sprintf(
    "\n========== a-path agreement across %s ==========\nmax |Δ posterior mean| = %.4f (%s); tolerance %.3f\n",
    paste(unname(scales), collapse = " / "), max(a_gap), names(a_gap)[which.max(a_gap)], a_path_tol
  ))
  if (max(a_gap) > a_path_tol) {
    warning(
      "a-paths differ across outcomes by up to ", signif(max(a_gap), 3), " (> ", a_path_tol,
      ") -- panel B can no longer be shown once for both outcomes", call. = FALSE
    )
  }

  ## panel A: structure only -- every estimate lives in B-D ---------------------------
  # the notes flag that each domain x outcome is a separate fit, and that c' / c are
  # estimated but reported elsewhere (section 4, panel E of S1-S2)
  panel_a <- plot_multi_mediation_diagram(
    mediator_nms = mediator_nms[domain_order], mediator_cols = mediator_cols,
    outcome_nms = paste0(unname(scales), "<sub>std</sub>"),
    note = paste(
      "mediation estimated separately for each",
      "attribution domain and symptom score", sep = "<br>"
    ),
    # ~3 lines of ~35 chars fit this corner at the full-figure size
    note_tr = paste(
      "*c*′ (direct) and *c* (total) also",
      "estimated but reported separately:",
      "*c*′ extrapolates to no Δ attribution", sep = "<br>"
    ),
    fnt_sz = 1.25 * fnt_sz
  )

  ## panels B-D: a once, b and indirect per outcome ----------------------------------
  # legend only on D (shared mediator colours)
  paths_panel <- function(d, path, ...) {
    plot_paths_n(
      d, path = path, suffixes = domain_order, mediator_nms = mediator_nms,
      mediator_cols = mediator_cols, interval_size_range = interval_size_range,
      fatten_point = fatten_point, outcome_cols = outcome_cols, flip_order = TRUE, fnt_sz = fnt_sz, ...
    )
  }
  panel_b <- paths_panel(
    scale_draws[[1]], "a", legend = FALSE, x_lab = "a path (learning group → Δ attribution)"
  )
  # "std" = the sd(t2) scaling, as in figure 1
  panel_c <- paths_panel(
    scale_draws, "b", legend = FALSE, x_lab = "b path (Δ attribution → Δ symptoms<sub>std</sub>)"
  )
  panel_d <- paths_panel(scale_draws, "indirect", legend = TRUE)

  ## honest reporting ----------------------------------------------------------------
  for (s in names(scale_draws)) {
    cat(sprintf("\n========== %s (single-mediator): indirect effects ==========\n", s))
    for (d in domain_order) {
      for (col in c(
        paste0("indirect_", d, "_control"), paste0("indirect_", d, "_causal"), paste0("index_mod_med_", d)
      )) {
        v <- scale_draws[[s]][[col]]
        h <- bayestestR::hdi(v, ci = 0.95)
        pd <- bayestestR::p_direction(v, method = "direct")
        cat(sprintf(
          "%-32s mean=%+.3f  95%%HDI=[%+.3f, %+.3f]  pd=%.1f%%\n",
          col, mean(v), h$CI_low, h$CI_high, 100 * as.numeric(pd)
        ))
      }
    }
  }
  panel_a <- panel_a +
    ggplot2::labs(tag = "A") +
    ggplot2::theme(plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 26 * fnt_sz))
  panel_bcd <- panel_b + panel_c + panel_d +
    plot_layout(design = "AB\nCC", heights = panel_heights[c(2:3)], widths = c(0.36, 0.64)) +
    plot_annotation(tag_levels = list(c("B", "C", "D"))) &
    ggplot2::theme(
      plot.tag = ggplot2::element_text(family = "Open Sans", face = "bold", size = 26 * fnt_sz)
    )
  hts <- c(panel_heights[1], sum(panel_heights[c(2:3)]))
  wrap_elements(
    panel_a + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0.01, unit = "npc"))
  ) / wrap_elements(panel_bcd) + plot_layout(heights = hts)
}

## 3. Figure 4 (main text) & Figures S1-S2 (supplementary) ------------------------

fig_4_combined <- build_combined_med_figure(
  "./analyses/stan-fits/attr", "./analyses/stan-fits/attr",
  scales =c(daq_sdt2 = "DAQ", phq_sdt2 = "PHQ-9")
)
fig_4_combined

# supplementary: per-participant decomposition panels + direct/indirect/total-arm, per outcome
fig_s1_daq_decomp <- build_single_mediator_figure(
  "./analyses/stan-fits/attr", "./analyses/stan-fits/attr", "daq_sdt2",
  symptom_nm = "DAQ<sub>std</sub>", tag_offset = 0, sections = c("decomp", "direct_total"),
  interval_size_range = c(0.6, 1.8), fatten_point = 2, x_n_breaks = 4, scatter_n_breaks = 4, fnt_sz = 1.6
)
fig_s1_daq_decomp

fig_s2_phq_decomp <- build_single_mediator_figure(
  "./analyses/stan-fits/attr", "./analyses/stan-fits/attr", "phq_sdt2",
  symptom_nm = "PHQ-9<sub>std</sub>", tag_offset = 0, sections = c("decomp", "direct_total"),
  interval_size_range = c(0.6, 1.8), fatten_point = 2, x_n_breaks = 4, scatter_n_breaks = 4, fnt_sz = 1.6
)
fig_s2_phq_decomp

## 4. table: a/b paths (by group), direct (c'), indirect and total-arm, all domains x outcomes --
# total_arm should track the mediator-free primary ANCOVA (analyses/outputs/ancova_itt.csv)
dir_tot_scales <- c(daq_sdt2 = "DAQ", phq_sdt2 = "PHQ-9")
dir_tot_draws <- lapply(names(dir_tot_scales), function(scale_stub) {
  dplyr::bind_cols(lapply(names(domain_nms), function(d) {
    load_single_mediator_domain(
      sprintf("./analyses/stan-fits/attr/fit_attr_symp_%s-ancova-singlemed-%s.rds", scale_stub, d),
      sprintf("./analyses/stan-fits/attr/fit_attr_symp_%s-ancova-singlemed-%s", scale_stub, d),
      d
    ) |>
      dplyr::select(tidyselect::all_of(c(
        paste0("a_", d, "_control"), paste0("a_", d, "_causal"),
        paste0("b_", d, "_control"), paste0("b_", d, "_causal"),
        paste0("direct_arm_", d), paste0("total_arm_", d), paste0("indirect_", d, "_diff")
      )))
  }))
})
names(dir_tot_draws) <- unname(dir_tot_scales)

## table: mean / 95% hdi / pd, plus the gap to the primary ANCOVA ("total" rows only) ------
summarise_med_draws <- function(v) {
  h <- bayestestR::hdi(v, ci = 0.95)
  tibble::tibble(
    mean = mean(v), hdi_lo95 = h$CI_low, hdi_hi95 = h$CI_high,
    pd = as.numeric(bayestestR::p_direction(v, method = "direct"))
  )
}

primary_ancova_itt <- readr::read_csv("analyses/outputs/ancova_itt.csv", col_types = readr::cols()) |>
  dplyr::filter(frame == "ITT", measure %in% c("PHQ-9", "DAQ"))
primary_total_sd <- stats::setNames(primary_ancova_itt$d_sd_t2, primary_ancova_itt$measure)

dir_tot_tbl <- dplyr::bind_rows(lapply(names(dir_tot_draws), function(outcome) {
  d <- dir_tot_draws[[outcome]]
  dplyr::bind_rows(lapply(domain_order, function(dom) {
    dplyr::bind_rows(
      summarise_med_draws(d[[paste0("a_", dom, "_control")]]) |> dplyr::mutate(component = "a_control", .before = 1),
      summarise_med_draws(d[[paste0("a_", dom, "_causal")]]) |> dplyr::mutate(component = "a_causal", .before = 1),
      summarise_med_draws(d[[paste0("b_", dom, "_control")]]) |> dplyr::mutate(component = "b_control", .before = 1),
      summarise_med_draws(d[[paste0("b_", dom, "_causal")]]) |> dplyr::mutate(component = "b_causal", .before = 1),
      summarise_med_draws(d[[paste0("direct_arm_", dom)]]) |> dplyr::mutate(component = "direct", .before = 1),
      summarise_med_draws(d[[paste0("indirect_", dom, "_diff")]]) |> dplyr::mutate(component = "indirect", .before = 1),
      summarise_med_draws(d[[paste0("total_arm_", dom)]]) |> dplyr::mutate(component = "total", .before = 1)
    ) |>
      dplyr::mutate(domain = dom, outcome = outcome, .before = 1)
  }))
})) |>
  dplyr::left_join(
    tibble::tibble(outcome = names(primary_total_sd), primary_ancova_total_sd = unname(primary_total_sd)),
    by = "outcome"
  ) |>
  dplyr::mutate(gap_vs_primary = dplyr::if_else(component == "total", mean - primary_ancova_total_sd, NA_real_))

readr::write_csv(dir_tot_tbl, "analyses/outputs/mediation_direct_total.csv")

## warn if total_arm drifts from the primary ANCOVA by > total_arm_tol sd (researcher choice)
total_arm_tol <- 0.03
cat("\n========== a / b / direct / indirect / total-arm vs. primary ANCOVA (all domains x outcomes) ==========\n")
for (i in seq_len(nrow(dir_tot_tbl))) {
  r <- dir_tot_tbl[i, ]
  cat(sprintf(
    "%-8s %-14s %-9s mean=%+.3f  95%%HDI=[%+.3f, %+.3f]  pd=%.1f%%%s\n",
    r$outcome, r$domain, r$component, r$mean, r$hdi_lo95, r$hdi_hi95, 100 * r$pd,
    if (!is.na(r$gap_vs_primary)) {
      sprintf("  (primary=%+.3f, gap=%+.3f)", r$primary_ancova_total_sd, r$gap_vs_primary)
    } else {
      ""
    }
  ))
}
total_arm_gaps <- dir_tot_tbl |> dplyr::filter(component == "total")
if (any(abs(total_arm_gaps$gap_vs_primary) > total_arm_tol)) {
  warning(
    "total_arm drifts from the primary ANCOVA by more than ", total_arm_tol, " SD in at least one domain x ",
    "outcome -- check analyses/outputs/mediation_direct_total.csv", call. = FALSE
  )
}
