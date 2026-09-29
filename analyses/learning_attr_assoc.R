# Does the session-1 learning rate predict attribution change? ======================
# four regressions, one per attribution domain:
#
#   delta_theta_d ~ 1 + arm * alpha_z,   sigma ~ 0 + arm
#
# alpha is the valence-matched session-1 learning rate from the winning model (M2), z-scored
# within arm. both sides are shrunken posterior means, so slopes are attenuated by roughly
# sqrt(lambda_alpha) * lambda_dtheta: read them as a conservative test of association, not as
# effect sizes.
# run before modelling_learning.R -- panel E reads the slope table written here.

source("analyses/study_data.R")

ASSOC_FIT_DIR   <- "analyses/stan-fits/learning-attr"
ASSOC_SLOPE_CSV <- "analyses/outputs/alpha_theta_assoc_slopes.csv"
ASSOC_REL_CSV   <- "analyses/outputs/alpha_theta_reliability.csv"
ASSOC_SEED      <- 20260814

# valence-matched: negative domains take alpha_neg, positive domains alpha_pos
ASSOC_DOMAINS <- tibble::tribble(
  ~variable,             ~domain_lab,          ~valence,   ~alpha_var,
  "delta_internal_neg",  "internal-negative",  "negative", "alpha_neg",
  "delta_internal_pos",  "internal-positive",  "positive", "alpha_pos",
  "delta_global_neg",    "global-negative",    "negative", "alpha_neg",
  "delta_global_pos",    "global-positive",    "positive", "alpha_pos"
)

## 1. per-participant measurements from the two cached fits ------------------------
# alpha is summarised on the probit scale (near-symmetric posteriors); Phi_approx can saturate
# to exactly 0/1, so clamp before qnorm() and report how many draws that affects
PROBIT_CLAMP <- 1e-10

# M2 draws, cached by modelling_learning.R section 3
alpha_val_draws <- readr::read_rds(
  "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha_indiv_draws.rds"
) |>
  dplyr::select(dplyr::matches("^alpha_(neg|pos)\\["))

n_clamped <- sum(alpha_val_draws <= PROBIT_CLAMP | alpha_val_draws >= 1 - PROBIT_CLAMP)
cat(sprintf(
  "probit clamp: %d / %d alpha draws (%.4f%%) at the 0/1 boundary\n",
  n_clamped, prod(dim(alpha_val_draws)), 100 * n_clamped / prod(dim(alpha_val_draws))
))

z_alpha_draws <- alpha_val_draws |>
  dplyr::mutate(dplyr::across(
    dplyr::everything(),
    ~ qnorm(pmin(pmax(.x, PROBIT_CLAMP), 1 - PROBIT_CLAMP))
  ))

alpha_meas <- extract_indiv_pars(z_alpha_draws, stan_ls_learn_s1) |>
  dplyr::rename(learn_id = id) |>
  dplyr::select(variable, learn_id, a_hat = mean, a_sd = sd)

theta_draws <- readr::read_rds("./analyses/stan-fits/attr/fit_attr_multisess_indiv_draws.rds") |>
  dplyr::select(dplyr::matches("^delta_(internal|global)_(neg|pos)_p\\["))

# extract_indiv_pars()'s `_p(?!\w)` split maps delta_internal_neg_p[i] -> "delta_internal_neg"
theta_meas <- extract_indiv_pars(theta_draws, stan_ls_attr) |>
  dplyr::select(variable, id, d_hat = mean, d_sd = sd, completed)

## 2. join on subID and assert the id <-> learn_id frames line up ------------------
# alpha is indexed by learn_id (324 ppts), delta-theta by id (361 ppts) -- join via id_lookup
stopifnot(identical(
  ids_learn_s1 |> dplyr::arrange(learn_id) |> dplyr::pull(subID),
  id_lookup |> dplyr::filter(!is.na(learn_id)) |> dplyr::arrange(learn_id) |> dplyr::pull(subID)
))

assoc_ppts <- id_lookup |>
  dplyr::filter(!is.na(learn_id)) |>
  dplyr::arrange(id) |>
  dplyr::mutate(arm = factor(group, levels = c(ARM_REF, ARM_INT)))

# the two fits must agree on every joined participant's arm
stopifnot(
  all(stan_ls_learn_s1$condition[assoc_ppts$learn_id] == stan_ls_attr$condition[assoc_ppts$id]),
  all((stan_ls_attr$condition[assoc_ppts$id] == 1L) == (assoc_ppts$arm == ARM_INT))
)

# one row per participant x domain. alpha_z is within arm, so `arm` is the arm difference at each
# arm's own mean learning rate and `arm:alpha_z` the slope difference
assoc_df <- purrr::pmap_dfr(ASSOC_DOMAINS, function(variable, domain_lab, valence, alpha_var) {
  a <- alpha_meas[alpha_meas$variable == alpha_var, ]
  t <- theta_meas[theta_meas$variable == variable, ]
  assoc_ppts |>
    dplyr::mutate(
      domain = domain_lab, valence = valence, alpha_var = alpha_var,
      a_hat = a$a_hat[match(learn_id, a$learn_id)],
      a_sd  = a$a_sd[match(learn_id, a$learn_id)],
      d_hat = t$d_hat[match(id, t$id)],
      d_sd  = t$d_sd[match(id, t$id)]
    )
}) |>
  dplyr::group_by(domain, arm) |>
  dplyr::mutate(alpha_z = (a_hat - mean(a_hat)) / sd(a_hat)) |>
  dplyr::ungroup()

stopifnot(
  nrow(assoc_df) == nrow(assoc_ppts) * nrow(ASSOC_DOMAINS),
  all(is.finite(assoc_df$alpha_z)), all(is.finite(assoc_df$d_hat)),
  all(assoc_df$a_sd > 0), all(is.finite(assoc_df$d_sd)),
  all(abs(tapply(assoc_df$alpha_z, list(assoc_df$domain, assoc_df$arm), mean)) < 1e-10),
  all(abs(tapply(assoc_df$alpha_z, list(assoc_df$domain, assoc_df$arm), sd) - 1) < 1e-10)
)

## 3. reliability of the two measurements -- the real caveat -----------------------
# lambda = tau^2 / (tau^2 + sigma^2), by arm (pooled rows kept for reference); every caveat
# below reads the per-arm rows of this one table
assoc_lambda <- function(hat, se, quantity, param, arm_lab) {
  tibble::tibble(
    quantity = quantity, param = param, arm = arm_lab,
    n = length(hat), lambda = reliability(hat, se)
  )
}

assoc_rel_tbl <- dplyr::bind_rows(
  # two domains share each alpha, so de-duplicate to one row per participant first
  purrr::map_dfr(c("alpha_neg", "alpha_pos"), function(av) {
    d <- assoc_df[assoc_df$alpha_var == av, ]
    d <- d[!duplicated(d$id), ]
    purrr::map_dfr(c(ARM_REF, ARM_INT, "pooled"), function(g) {
      dd <- if (g == "pooled") d else d[d$arm == g, ]
      assoc_lambda(dd$a_hat, dd$a_sd, "alpha", av, g)
    })
  }),
  purrr::map_dfr(ASSOC_DOMAINS$domain_lab, function(dm) {
    d <- assoc_df[assoc_df$domain == dm, ]
    purrr::map_dfr(c(ARM_REF, ARM_INT, "pooled"), function(g) {
      dd <- if (g == "pooled") d else d[d$arm == g, ]
      assoc_lambda(dd$d_hat, dd$d_sd, "delta_theta", dm, g)
    })
  })
)

lam <- function(quantity, param, arm_lab) {
  v <- assoc_rel_tbl$lambda[assoc_rel_tbl$quantity == quantity &
                              assoc_rel_tbl$param == param & assoc_rel_tbl$arm == arm_lab]
  stopifnot(length(v) == 1L)
  v
}

stopifnot(
  nrow(assoc_rel_tbl) == (2L + nrow(ASSOC_DOMAINS)) * 3L,
  all(is.finite(assoc_rel_tbl$lambda)),
  all(assoc_rel_tbl$lambda > 0 & assoc_rel_tbl$lambda < 1),
  all(assoc_rel_tbl$n[assoc_rel_tbl$arm == "pooled"] == nrow(assoc_ppts))
)

readr::write_csv(assoc_rel_tbl, ASSOC_REL_CSV)

cat("\nmeasurement reliability (lambda), by arm; pooled rows include the arm mean difference:\n")
assoc_rel_tbl |>
  dplyr::mutate(lambda = sprintf("%.2f", lambda)) |>
  dplyr::select(-n) |>
  tidyr::pivot_wider(names_from = arm, values_from = lambda) |>
  as.data.frame() |>
  print(row.names = FALSE)

# implied attenuation: a caveat on the slopes, not a correction to divide by
cat("\nimplied attenuation of the reported slopes (sqrt(lambda_a) * lambda_dtheta):\n")
for (k in seq_len(nrow(ASSOC_DOMAINS))) {
  dm <- ASSOC_DOMAINS$domain_lab[k]
  for (g in c(ARM_REF, ARM_INT)) {
    cat(sprintf(
      "  %-18s %-7s x%.2f\n", dm, g,
      sqrt(lam("alpha", ASSOC_DOMAINS$alpha_var[k], g)) * lam("delta_theta", dm, g)
    ))
  }
}
cat("  -> read the slopes as a conservative test of association, not as effect sizes.\n")

# why the outcome's measurement error isn't modelled additively: brms se(d_sd, sigma = TRUE)
# needs var(d_hat) - mean(d_sd^2) > 0 (lambda_dtheta > 0.5) within each arm
cat("\nadditive measurement-error model (se(d_sd, sigma = TRUE)): var(d_hat) - mean(d_sd^2)\n")
for (dm in ASSOC_DOMAINS$domain_lab) {
  for (g in c(ARM_REF, ARM_INT)) {
    d <- assoc_df[assoc_df$domain == dm & assoc_df$arm == g, ]
    resid_var <- var(d$d_hat) - mean(d$d_sd^2)
    cat(sprintf(
      "  %-18s %-7s lambda=%.2f  var(d_hat)=%.3f  mean(d_sd^2)=%.3f  sigma^2=%+.3f%s\n",
      dm, g, lam("delta_theta", dm, g), var(d$d_hat), mean(d$d_sd^2), resid_var,
      if (resid_var <= 0) "  <- not identified" else ""
    ))
  }
}
cat("  -> sigma^2 <= 0 means d_sd overstates the noise in a shrunken posterior mean, so the\n")
cat("     additive form is unavailable and these regressions stay unweighted.\n")

## 4. the four regressions -----------------------------------------------------------
# arm-specific sigma: sd(delta-theta) is 1.35-1.8x larger in the causal arm in every domain
dir.create(ASSOC_FIT_DIR, showWarnings = FALSE, recursive = TRUE)

assoc_brm <- function(dat, cache) {
  brms::brm(
    formula = brms::bf(d_hat ~ 1 + arm * alpha_z, sigma ~ 0 + arm),
    data = dat, family = stats::gaussian(),
    prior = c(
      brms::prior(normal(0, 1), class = "b"),
      brms::prior(normal(0, 1.5), class = "Intercept"),
      brms::prior(normal(0, 1), class = "b", dpar = "sigma")
    ),
    chains = 4, cores = 4, iter = 6000, warmup = 2000,
    backend = "cmdstanr", refresh = 0, seed = ASSOC_SEED,
    file = cache, file_refit = "on_change"
  )
}

assoc_fits <- lapply(ASSOC_DOMAINS$domain_lab, function(dm) {
  assoc_brm(
    dplyr::filter(assoc_df, domain == dm),
    file.path(ASSOC_FIT_DIR, paste0("assoc_", gsub("-", "_", dm)))
  )
})
names(assoc_fits) <- ASSOC_DOMAINS$domain_lab

## 5. diagnostics -------------------------------------------------------------------
# per-arm sd of the outcome printed alongside (the case for arm-specific sigma)
for (dm in names(assoc_fits)) {
  s <- posterior::summarise_draws(
    brms::as_draws_df(assoc_fits[[dm]], variable = "^b_", regex = TRUE)
  )
  np <- brms::nuts_params(assoc_fits[[dm]])
  sds <- tapply(assoc_df$d_hat[assoc_df$domain == dm], assoc_df$arm[assoc_df$domain == dm], sd)
  cat(sprintf(
    "%-18s div %d, rhat %.4f, ess %.0f | sd(Δθ): %s %.2f, %s %.2f\n",
    dm, sum(np$Value[np$Parameter == "divergent__"]),
    max(s$rhat, na.rm = TRUE), min(s$ess_bulk, na.rm = TRUE),
    ARM_REF, sds[[ARM_REF]], ARM_INT, sds[[ARM_INT]]
  ))
}

## 6. slope table + the cor.test() it replaces ---------------------------------------
slope_row <- function(v, ...) {
  h95 <- bayestestR::hdi(v, ci = 0.95)
  h99 <- bayestestR::hdi(v, ci = 0.99)
  tibble::tibble(
    ..., slope = mean(v), lo95 = h95$CI_low, hi95 = h95$CI_high,
    lo99 = h99$CI_low, hi99 = h99$CI_high,
    pd = as.numeric(bayestestR::p_direction(v, method = "direct"))
  )
}

assoc_slope_tbl <- purrr::pmap_dfr(ASSOC_DOMAINS, function(variable, domain_lab, valence, alpha_var) {
  dr <- brms::as_draws_df(assoc_fits[[domain_lab]])
  b_con  <- dr[["b_alpha_z"]]                                  # control is the reference level
  b_diff <- dr[[paste0("b_arm", ARM_INT, ":alpha_z")]]
  dplyr::bind_rows(
    slope_row(b_con, domain = domain_lab, valence = valence, alpha_var = alpha_var,
              level = ARM_REF),
    slope_row(b_con + b_diff, domain = domain_lab, valence = valence, alpha_var = alpha_var,
              level = ARM_INT),
    slope_row(b_diff, domain = domain_lab, valence = valence, alpha_var = alpha_var,
              level = "difference")
  )
}) |>
  dplyr::mutate(level = factor(level, levels = c("difference", ARM_REF, ARM_INT)))

readr::write_csv(assoc_slope_tbl, ASSOC_SLOPE_CSV)

# cor.test() on the same posterior means / participants -- should agree closely with the model
naive_corrs <- purrr::pmap_dfr(ASSOC_DOMAINS, function(variable, domain_lab, valence, alpha_var) {
  purrr::map_dfr(c(ARM_REF, ARM_INT), function(g) {
    d <- assoc_df[assoc_df$domain == domain_lab & assoc_df$arm == g, ]
    ct <- stats::cor.test(d$a_hat, d$d_hat)
    tibble::tibble(
      domain = domain_lab, level = g, n = nrow(d),
      r_naive = unname(ct$estimate), p_naive = ct$p.value
    )
  })
})

cat("\nΔθ (logits) per SD of session-1 α, vs the cor.test() on the same posterior means:\n")
assoc_slope_tbl |>
  dplyr::filter(level != "difference") |>
  dplyr::left_join(naive_corrs, by = c("domain", "level")) |>
  dplyr::transmute(
    domain, level, n,
    cor_test = sprintf("r=%+.3f p=%.3f", r_naive, p_naive),
    model = sprintf("b=%+.3f [%+.3f, %+.3f] pd=%.1f%%", slope, lo95, hi95, 100 * pd)
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\ncausal - control slope differences (the arm x learning-rate interaction):\n")
assoc_slope_tbl |>
  dplyr::filter(level == "difference") |>
  dplyr::transmute(
    domain, diff = sprintf("%+.3f [%+.3f, %+.3f] pd=%.1f%%", slope, lo95, hi95, 100 * pd)
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

## 7. sensitivity: the two researcher choices ----------------------------------------
# (i) sigma pooled across arms, (ii) the retired 1 / sd(alpha) weighting -- each varies one
# thing from the main model. reported for transparency, not adopted
assoc_sens <- function(dat, form, prior, cache) {
  fit <- brms::brm(
    formula = form, data = dat, family = stats::gaussian(), prior = prior,
    chains = 4, cores = 4, iter = 4000, warmup = 1000,
    backend = "cmdstanr", refresh = 0, seed = ASSOC_SEED,
    file = cache, file_refit = "on_change"
  )
  dr <- brms::as_draws_df(fit)
  dr[["b_alpha_z"]] + dr[[paste0("b_arm", ARM_INT, ":alpha_z")]]   # causal-arm slope
}

pri_pooled <- c(
  brms::prior(normal(0, 1), class = "b"),
  brms::prior(normal(0, 1.5), class = "Intercept"),
  brms::prior(normal(0, 1), class = "sigma")
)
pri_armsig <- c(
  brms::prior(normal(0, 1), class = "b"),
  brms::prior(normal(0, 1.5), class = "Intercept"),
  brms::prior(normal(0, 1), class = "b", dpar = "sigma")
)

fmt_hdi <- function(v) {
  h <- bayestestR::hdi(v, ci = 0.95)
  sprintf(
    "%+.3f [%+.3f, %+.3f] pd=%.1f%%", mean(v), h$CI_low, h$CI_high,
    100 * as.numeric(bayestestR::p_direction(v, method = "direct"))
  )
}

# kish ess / n of the weights (1 = equal weighting)
kish <- function(w) sum(w)^2 / (length(w) * sum(w^2))

cat("\nsensitivity of the CAUSAL-arm slope:\n")
for (dm in ASSOC_DOMAINS$domain_lab) {
  tg <- file.path(ASSOC_FIT_DIR, paste0("assoc_sens_", gsub("-", "_", dm)))
  d <- dplyr::filter(assoc_df, domain == dm) |> dplyr::mutate(wt = 1 / a_sd)
  main <- assoc_slope_tbl |> dplyr::filter(domain == dm, level == ARM_INT)
  pooled <- assoc_sens(
    d, brms::bf(d_hat ~ 1 + arm * alpha_z), pri_pooled, paste0(tg, "_pooledsig")
  )
  wtd <- assoc_sens(
    d, brms::bf(d_hat | weights(wt, scale = TRUE) ~ 1 + arm * alpha_z, sigma ~ 0 + arm),
    pri_armsig, paste0(tg, "_armsig_w1")
  )
  w1 <- d$wt / mean(d$wt)
  cat(sprintf(
    "  %-18s main         %+.3f [%+.3f, %+.3f] pd=%.1f%%\n",
    dm, main$slope, main$lo95, main$hi95, 100 * main$pd
  ))
  cat(sprintf("  %-18s pooled sigma %s\n", "", fmt_hdi(pooled)))
  cat(sprintf(
    "  %-18s w=1/sd       %s  (mean-1 range %.2f-%.2f, ESS/n %.3f)\n",
    "", fmt_hdi(wtd), min(w1), max(w1), kish(w1)
  ))
}
