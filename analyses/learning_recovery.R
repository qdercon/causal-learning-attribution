# Parameter and model recovery for M2 (session-1 learning) ==========================
# 1. parameter recovery: simulate from M2 on the real session-1 structure, with truths drawn
#    from its fitted posterior (fresh individuals each time), refit M2, and compare group- and
#    individual-level estimates to the truth -- incl. checking the lambda estimator against
#    cor(true, recovered)^2.
# 2. model recovery (section 9): can LOO pick M2 out of the six-model family when M2 is true,
#    and where do the observed elpd gaps sit in the distribution the design produces?
# not simulation-based calibration: coverage counts are descriptive. both halves use the
# union simulator; results are cached per replicate as small summaries.
# ~10 min per refit. writes analyses/outputs/recovery_*.csv for modelling_learning.R.

source("analyses/study_data.R")

## 0. config -------------------------------------------------------------------------
REC_N_REP    <- 10
REC_SEED     <- 20260817
REC_CHAINS   <- 4
# shorter than the reported fits (2000/4000): replicates buy more than draws here, and every
# replicate's diagnostics are checked below
REC_WARMUP   <- 1000
REC_SAMPLING <- 2000

REC_DIR      <- "analyses/stan-fits/learning/recovery"
# the union simulator, run in its M2 configuration
REC_SIM_STAN <- "analyses/stan-models/causAttr-session1-learning-modelrec-sim.stan"
REC_FIT_STAN <- "analyses/stan-models/causAttr-session1-learning-s1-free-valalpha.stan"
WIN_MOD_NAME  <- "causAttr-session1-learning-s1-free-valalpha.stan" # stamped into the csvs
WIN_FIT_RDS   <- "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha.rds"
WIN_HYPER_RDS <- "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha_hyper_draws.rds"
WIN_INDIV_RDS <- "./analyses/stan-fits/learning/fit_learn-s1-free-valalpha_indiv_draws.rds"

OUT_GROUP <- "analyses/outputs/recovery_win_mod_group.csv"
OUT_INDIV <- "analyses/outputs/recovery_win_mod_indiv.csv"
OUT_REL   <- "analyses/outputs/recovery_win_mod_reliability.csv"
OUT_DIAG  <- "analyses/outputs/recovery_win_mod_diagnostics.csv"

# group-level parameters compared to truth, on the reported (probability) scale
REC_GROUP_VARS <- c(
  "p_alpha_neg", "p_alpha_pos", "delta_alpha", "p_beta", "p_q0_foil", "p_q0_ig",
  "sigma_alpha_p", "sigma_beta"
)
REC_INDIV_VARS <- c("alpha_neg", "alpha_pos", "beta")

# bump when the per-replicate summary tables change shape (md5s version the stan models)
REC_SCHEMA <- 2L

PROBIT_CLAMP <- 1e-10 # as in learning_attr_assoc.R -- Phi_approx saturates in fp
probit_z <- function(x) stats::qnorm(pmin(pmax(x, PROBIT_CLAMP), 1 - PROBIT_CLAMP))

# both halves cache extracts from the same fit with different variable lists, so check the
# cache has every variable asked for
has_draw_vars <- function(dr, vars) {
  nm <- names(dr)
  all(vapply(vars, function(v) any(nm == v | startsWith(nm, paste0(v, "["))), logical(1)))
}

# read a cached draws extract, or pull it from the (~1.5GB) fit and cache it
cached_draws <- function(cache_rds, fit_rds, vars, label) {
  if (file.exists(cache_rds)) {
    dr <- readr::read_rds(cache_rds)
    if (has_draw_vars(dr, vars)) return(dr)
    cat(sprintf("cached draws for %s lack requested variables -- re-extracting\n", label))
  }
  cat(sprintf("extracting hyperparameter draws for %s...\n", label))
  fit <- readr::read_rds(fit_rds)
  dr <- fit$draws(format = "df", variables = vars)
  saveRDS(dr, cache_rds)
  rm(fit)
  gc(verbose = FALSE)
  dr
}

dir.create(REC_DIR, showWarnings = FALSE, recursive = TRUE)

## 1. ground truth: hyperparameter draws from the fitted M2 --------------------------
# small cached extract; the full fit is loaded once and dropped
win_hyper <- cached_draws(
  WIN_HYPER_RDS, WIN_FIT_RDS,
  c(
    "mu_alpha_p", "mu_q0_foil", "mu_q0_ig", "mu_beta", "sigma_alpha_p", "sigma_beta",
    "alpha_int", "q0_foil_int", "q0_ig_int", "beta_int",
    "p_alpha_neg", "p_alpha_pos", "p_beta", "p_q0_foil", "p_q0_ig", "delta_alpha"
  ),
  "M2"
)

# evenly spaced over the (chain-major) draw index, so truths span all chains
truth_idx <- round(seq(1, nrow(win_hyper), length.out = REC_N_REP))
stopifnot(!anyDuplicated(truth_idx), length(truth_idx) == REC_N_REP)

## 2. the invariants the simulator depends on ----------------------------------------
n_p    <- stan_ls_learn_s1$nPpts
n_tmax <- stan_ls_learn_s1$nTrials_max
n_blk  <- as.integer(stan_ls_learn_s1$nBlocks)

# observed vs padded cells of the [nPpts, nTrials_max] arrays
obs_cell <- outer(seq_len(n_p), seq_len(n_tmax), function(p, t) t <= stan_ls_learn_s1$nT_ppts[p])

# the deterministic contingency the simulator encodes (internal-global = choice 2 is correct
# iff positive valence), checked on the real data
rule_outcome <- function(choice, valence) as.integer((choice == 2L) == (valence == 1L))
stopifnot(
  all(stan_ls_learn_s1$outcome[obs_cell] ==
        rule_outcome(stan_ls_learn_s1$choice[obs_cell], stan_ls_learn_s1$valence[obs_cell])),
  n_blk == 3L,
  all(rowSums(stan_ls_learn_s1$new_block) == n_blk),
  all(stan_ls_learn_s1$new_block[, 1] == 1L),
  all(stan_ls_learn_s1$condition %in% 0:1)
)

## 3. simulate one dataset per truth draw --------------------------------------------
sim_mod <- cmdstanr::cmdstan_model(REC_SIM_STAN)
fit_mod <- cmdstanr::cmdstan_model(REC_FIT_STAN)
REC_MD5 <- c(sim = unname(tools::md5sum(REC_SIM_STAN)), fit = unname(tools::md5sum(REC_FIT_STAN)))

# pull a vector[2] / array[nBlocks] vector[2] parameter out of a one-row draws df.
# array[nBlocks] vector[2] maps to an R matrix with dim (nBlocks, 2).
row_vec2 <- function(row, stub) c(row[[paste0(stub, "[1]")]], row[[paste0(stub, "[2]")]])
row_b2 <- function(row, stub) {
  out <- matrix(NA_real_, nrow = n_blk, ncol = 2)
  for (b in seq_len(n_blk)) {
    for (v in 1:2) out[b, v] <- row[[sprintf("%s[%d,%d]", stub, b, v)]]
  }
  out
}

# the real task structure, shared by every simulator call in this script
REC_TASK <- stan_ls_learn_s1[c("nPpts", "nTrials_max", "nChoices", "nT_ppts", "condition",
                               "new_block", "valence")]

# one hyperparameter draw -> the union simulator's data list, given the generating model's
# four structural flags. the single mapping used by both halves of this script
union_sim_data <- function(row, free_q0, val_alpha, counterfactual, beta_grp) {
  zeros <- matrix(0, nrow = n_blk, ncol = 2)
  single_alpha <- val_alpha == 0
  c(
    REC_TASK,
    list(
      nBlocks = n_blk, val_alpha = val_alpha, counterfactual = counterfactual,
      # single-alpha models: both valences take the same hyperparameters, and val_alpha = 0
      # makes them share the individual deviation too
      mu_alpha_p    = if (single_alpha) rep(row$mu_alpha, 2)    else row_vec2(row, "mu_alpha_p"),
      alpha_int     = if (single_alpha) rep(row$alpha_int, 2)   else row_vec2(row, "alpha_int"),
      sigma_alpha_p = if (single_alpha) rep(row$sigma_alpha, 2) else row_vec2(row, "sigma_alpha_p"),
      mu_q0_ig  = row_b2(row, if (free_q0 == 1) "mu_q0_ig"  else "mu_q0"),
      q0_ig_int = row_b2(row, if (free_q0 == 1) "q0_ig_int" else "q0_int"),
      # anchored models: zeros give Phi_approx(0) = 0.5
      mu_q0_foil  = if (free_q0 == 1) row_b2(row, "mu_q0_foil")  else zeros,
      q0_foil_int = if (free_q0 == 1) row_b2(row, "q0_foil_int") else zeros,
      mu_beta = row$mu_beta, beta_int = row$beta_int,
      sigma_beta = if (beta_grp == 1) row_vec2(row, "sigma_beta") else rep(row$sigma_beta, 2)
    )
  )
}

# M2's flags: free q0, valence-specific alpha, chosen-option-only updating, shared sigma_beta
truth_stan_data <- function(row) {
  union_sim_data(row, free_q0 = 1L, val_alpha = 1L, counterfactual = 0L, beta_grp = 0L)
}

# [nPpts, nTrials_max] int array from a one-row draws df, indexed by parsed column names
sim_mat <- function(dr, var) {
  dr <- as.data.frame(dr) # subsetting columns off a draws_df warns about lost metadata
  cols <- grep(sprintf("^%s\\[", var), names(dr), value = TRUE)
  ij <- stringr::str_match(cols, "\\[(\\d+),(\\d+)\\]")
  out <- matrix(NA_integer_, n_p, n_tmax)
  out[cbind(as.integer(ij[, 2]), as.integer(ij[, 3]))] <- as.integer(round(unlist(dr[1, cols])))
  stopifnot(length(cols) == n_p * n_tmax, !anyNA(out))
  out
}
sim_vec <- function(dr, var) {
  dr <- as.data.frame(dr)
  cols <- sprintf("%s[%d]", var, seq_len(n_p))
  stopifnot(all(cols %in% names(dr)))
  as.numeric(unlist(dr[1, cols]))
}

simulate_rep <- function(r) {
  row <- win_hyper[truth_idx[r], ]
  sim <- sim_mod$sample(
    data = truth_stan_data(row), fixed_param = TRUE, chains = 1,
    iter_warmup = 0, iter_sampling = 1, seed = REC_SEED + r, refresh = 0,
    show_messages = FALSE
  )
  dr <- sim$draws(format = "df")

  # the simulator's p_* must reproduce the fitted M2 draw (32 quantities; 1e-6 relative
  # tolerance allows for cmdstan's csv rounding)
  chk <- grep(
    "^(p_alpha_neg|p_alpha_pos|p_beta|delta_alpha|p_q0_foil|p_q0_ig)\\[", names(dr), value = TRUE
  )
  stopifnot(length(chk) == 32L, all(chk %in% names(row)))
  sim_p <- unlist(as.data.frame(dr)[1, chk])
  fit_p <- unlist(as.data.frame(row)[, chk])
  stopifnot(max(abs(sim_p - fit_p) / pmax(abs(fit_p), 1e-12)) < 1e-6)
  # M2's foil start must not come back pinned at the anchored 0.5
  stopifnot(max(abs(sim_p[grep("^p_q0_foil\\[", names(sim_p))] - 0.5)) > 1e-6)

  choice  <- sim_mat(dr, "choice")
  outcome <- sim_mat(dr, "outcome")

  # -1 sentinels mark exactly the padded cells; outcomes obey the real contingency
  stopifnot(
    all(choice[!obs_cell] == -1L), all(outcome[!obs_cell] == -1L),
    all(choice[obs_cell] %in% 1:2), all(outcome[obs_cell] %in% 0:1),
    all(outcome[obs_cell] == rule_outcome(choice[obs_cell], stan_ls_learn_s1$valence[obs_cell]))
  )
  # refill padding with prep_data()'s learning-only init values
  choice[!obs_cell]  <- 1L
  outcome[!obs_cell] <- 0L

  stan_ls_sim <- stan_ls_learn_s1
  stan_ls_sim$choice  <- choice
  stan_ls_sim$outcome <- outcome
  # task structure unchanged, behaviour changed (i.e. not silently refitting the real data)
  stopifnot(
    identical(stan_ls_sim$nT_ppts, stan_ls_learn_s1$nT_ppts),
    identical(stan_ls_sim$valence, stan_ls_learn_s1$valence),
    identical(stan_ls_sim$new_block, stan_ls_learn_s1$new_block),
    identical(stan_ls_sim$condition, stan_ls_learn_s1$condition),
    !identical(stan_ls_sim$choice, stan_ls_learn_s1$choice)
  )

  list(
    stan_ls = stan_ls_sim,
    truth_group = c(sim_p, stats::setNames(
      c(row[["sigma_alpha_p[1]"]], row[["sigma_alpha_p[2]"]], row$sigma_beta),
      c("sigma_alpha_p[1]", "sigma_alpha_p[2]", "sigma_beta")
    )),
    truth_indiv = lapply(stats::setNames(REC_INDIV_VARS, REC_INDIV_VARS),
                         function(v) sim_vec(dr, v)),
    accuracy = mean(outcome[obs_cell])
  )
}

## 4. refit M2 to each simulated dataset ---------------------------------------------
# each fit is summarised and dropped; caches carry both models' md5s so edits invalidate them
recover_rep <- function(r) {
  cache <- file.path(REC_DIR, sprintf("rec_win_mod_rep%02d.rds", r))
  if (file.exists(cache)) {
    out <- readr::read_rds(cache)
    if (!identical(out$md5, REC_MD5) || out$truth_draw != truth_idx[r] ||
          !identical(out$schema, REC_SCHEMA)) {
      stop("stale recovery cache: ", cache, " -- delete it and re-run")
    }
    cat(sprintf("[rep %02d] cached\n", r))
    return(out)
  }

  cat(sprintf("\n[rep %02d] simulating (truth draw %d)...\n", r, truth_idx[r]))
  sim <- simulate_rep(r)
  cat(sprintf("[rep %02d] simulated accuracy %.3f (real data %.3f)\n",
              r, sim$accuracy, mean(stan_ls_learn_s1$outcome[obs_cell])))

  t0 <- Sys.time()
  fit <- fit_mod$sample(
    data = sim$stan_ls, seed = REC_SEED + 1000 + r, chains = REC_CHAINS,
    parallel_chains = REC_CHAINS, iter_warmup = REC_WARMUP, iter_sampling = REC_SAMPLING,
    refresh = 1000
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

  grp_arr  <- fit$draws(variables = REC_GROUP_VARS)
  grp_conv <- posterior::summarise_draws(grp_arr, posterior::default_convergence_measures())
  grp_mat  <- posterior::as_draws_matrix(grp_arr)
  group_tbl <- tibble::tibble(
    rep = r,
    variable = colnames(grp_mat),
    true = unname(sim$truth_group[colnames(grp_mat)]),
    mean = colMeans(grp_mat),
    sd = apply(grp_mat, 2, stats::sd),
    hdi_low = apply(grp_mat, 2, function(x) bayestestR::hdi(x, ci = 0.95)$CI_low),
    hdi_high = apply(grp_mat, 2, function(x) bayestestR::hdi(x, ci = 0.95)$CI_high)
  ) |>
    dplyr::mutate(
      covered = true >= hdi_low & true <= hdi_high,
      bias = mean - true,
      z_err = (mean - true) / sd
    )
  stopifnot(!anyNA(group_tbl$true))

  ind_arr  <- fit$draws(variables = REC_INDIV_VARS)
  ind_conv <- posterior::summarise_draws(ind_arr, posterior::default_convergence_measures())
  ind_mat  <- posterior::as_draws_matrix(ind_arr)
  indiv_tbl <- purrr::map_dfr(REC_INDIV_VARS, function(v) {
    m <- ind_mat[, sprintf("%s[%d]", v, seq_len(n_p)), drop = FALSE]
    is_alpha <- grepl("^alpha", v)
    z <- if (is_alpha) probit_z(m) else NULL
    tibble::tibble(
      rep = r, learn_id = seq_len(n_p), condition = stan_ls_learn_s1$condition, param = v,
      true = sim$truth_indiv[[v]],
      recovered = colMeans(m), recovered_sd = apply(m, 2, stats::sd),
      true_z = if (is_alpha) probit_z(sim$truth_indiv[[v]]) else NA_real_,
      recovered_z = if (is_alpha) colMeans(z) else NA_real_,
      recovered_z_sd = if (is_alpha) apply(z, 2, stats::sd) else NA_real_
    )
  })

  dg <- fit$diagnostic_summary(quiet = TRUE)
  diag_tbl <- tibble::tibble(
    rep = r, truth_draw = truth_idx[r], runtime_min = elapsed,
    divergences = sum(dg$num_divergent), max_treedepth = sum(dg$num_max_treedepth),
    min_ebfmi = min(dg$ebfmi),
    worst_rhat = max(c(grp_conv$rhat, ind_conv$rhat), na.rm = TRUE),
    min_ess_bulk = min(c(grp_conv$ess_bulk, ind_conv$ess_bulk), na.rm = TRUE),
    sim_accuracy = sim$accuracy
  ) |>
    dplyr::mutate(clean = divergences == 0 & worst_rhat < 1.01)

  out <- list(
    schema = REC_SCHEMA, md5 = REC_MD5, truth_draw = truth_idx[r],
    group = group_tbl, indiv = indiv_tbl, diag = diag_tbl,
    settings = list(chains = REC_CHAINS, warmup = REC_WARMUP, sampling = REC_SAMPLING)
  )
  saveRDS(out, cache)
  cat(sprintf(
    "[rep %02d] %.1f min | div %d | rhat %.3f | min ess %.0f\n",
    r, elapsed, diag_tbl$divergences, diag_tbl$worst_rhat, diag_tbl$min_ess_bulk
  ))
  rm(fit, grp_arr, grp_mat, ind_arr, ind_mat)
  gc(verbose = FALSE)
  out
}

cat(sprintf(
  "\nrecovery: %d replicates, %d chains x (%d warmup + %d sampling), %d participants\n",
  REC_N_REP, REC_CHAINS, REC_WARMUP, REC_SAMPLING, n_p
))
rec <- lapply(seq_len(REC_N_REP), recover_rep)

rec_group <- purrr::map_dfr(rec, "group")
rec_indiv <- purrr::map_dfr(rec, "indiv")
rec_diag  <- purrr::map_dfr(rec, "diag")

## 5. diagnostics --------------------------------------------------------------------
cat("\nper-replicate diagnostics:\n")
rec_diag |>
  dplyr::transmute(
    rep, min = round(runtime_min, 1), div = divergences, treedepth = max_treedepth,
    rhat = round(worst_rhat, 3), ess = round(min_ess_bulk), acc = round(sim_accuracy, 3), clean
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)
clean_reps <- rec_diag$rep[rec_diag$clean]
if (length(clean_reps) < nrow(rec_diag)) {
  cat(sprintf(
    "  NOTE %d/%d replicates had divergences or rhat >= 1.01 -- the group table is printed\n",
    nrow(rec_diag) - length(clean_reps), nrow(rec_diag)
  ))
  cat("  for all replicates and again for the clean subset.\n")
}

## 6. group-level recovery -----------------------------------------------------------
# output names differ from input names: summarise() evaluates sequentially
group_summary <- function(df) {
  df |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      n = dplyr::n(), true_range = sprintf("%.3f-%.3f", min(true), max(true)),
      mean_bias = mean(bias), mad_bias = mean(abs(bias)), sd_bias = stats::sd(bias),
      mean_z = mean(z_err), n_covered = sum(covered), .groups = "drop"
    )
}
print_group_summary <- function(df, header) {
  cat(header)
  group_summary(df) |>
    dplyr::mutate(
      dplyr::across(c(mean_bias, mad_bias, sd_bias, mean_z), ~ round(.x, 3)),
      covered = sprintf("%d/%d", n_covered, n)
    ) |>
    dplyr::select(-n, -n_covered) |>
    as.data.frame() |>
    print(row.names = FALSE)
}
print_group_summary(
  rec_group, "\ngroup-level recovery (bias = recovered - true, z = bias / posterior sd):\n"
)
if (length(clean_reps) < nrow(rec_diag) && length(clean_reps) > 0) {
  print_group_summary(
    rec_group |> dplyr::filter(rep %in% clean_reps),
    sprintf("\ngroup-level recovery, clean replicates only (n = %d):\n", length(clean_reps))
  )
}

## 7. individual-level recovery + reliability ----------------------------------------
# lambda = tau^2 / (tau^2 + sigma^2) from posterior summaries (as in model_fns.R), checked
# against what it approximates, cor(true, recovered)^2
reliability <- function(hat, se) {
  ratio <- stats::var(hat) / mean(se^2)
  ratio / (1 + ratio)
}

rec_alpha <- rec_indiv |> dplyr::filter(grepl("^alpha", param))
# within-arm rows are the individual-level numbers; arm = "both" also carries the group difference
rel_tbl <- dplyr::bind_rows(
  rec_alpha |> dplyr::mutate(arm = ifelse(condition == 1L, ARM_INT, ARM_REF)),
  rec_alpha |> dplyr::mutate(arm = "both")
) |>
  dplyr::group_by(rep, param, arm) |>
  dplyr::summarise(
    n = dplyr::n(),
    r_prob = stats::cor(true, recovered),
    r_probit = stats::cor(true_z, recovered_z),
    slope_probit = stats::coef(stats::lm(recovered_z ~ true_z))[[2]],
    rmse_prob = sqrt(mean((recovered - true)^2)),
    sd_true_prob = stats::sd(true),
    sd_recovered_prob = stats::sd(recovered),
    lambda_hat = reliability(recovered_z, recovered_z_sd),
    r2_probit = stats::cor(true_z, recovered_z)^2,
    .groups = "drop"
  )

# the observed lambda from the real fit, computed the same way, for reference
obs_alpha_draws <- readr::read_rds(WIN_INDIV_RDS) |>
  as.data.frame() |>
  dplyr::select(dplyr::matches("^alpha_(neg|pos)\\["))
obs_rel <- purrr::map_dfr(c("alpha_neg", "alpha_pos"), function(v) {
  m <- as.matrix(obs_alpha_draws[, sprintf("%s[%d]", v, seq_len(n_p))])
  z <- probit_z(m)
  d <- tibble::tibble(
    arm = ifelse(stan_ls_learn_s1$condition == 1L, ARM_INT, ARM_REF),
    hat = colMeans(z), se = apply(z, 2, stats::sd)
  )
  dplyr::bind_rows(d, dplyr::mutate(d, arm = "both")) |>
    dplyr::group_by(arm) |>
    dplyr::summarise(param = v, lambda_obs = reliability(hat, se), .groups = "drop")
})
rel_tbl <- rel_tbl |> dplyr::left_join(obs_rel, by = c("param", "arm"))

cat("\nindividual-level alpha recovery (mean across replicates, [min, max]):\n")
rel_tbl |>
  dplyr::group_by(param, arm) |>
  dplyr::summarise(
    r = sprintf("%.2f [%.2f, %.2f]", mean(r_probit), min(r_probit), max(r_probit)),
    slope = sprintf("%.2f", mean(slope_probit)),
    lambda_sim = sprintf("%.2f [%.2f, %.2f]", mean(lambda_hat), min(lambda_hat), max(lambda_hat)),
    r2 = sprintf("%.2f", mean(r2_probit)),
    lambda_obs = sprintf("%.2f", dplyr::first(lambda_obs)),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)
cat("  r/slope/lambda on the probit scale; lambda_sim vs r2 is the check on the estimator,\n")
cat("  lambda_sim vs lambda_obs the check on whether the real data behaves like the design.\n")

## 8. write the tables modelling_learning.R reads -------------------------------------
# filenames are generic, so every table is stamped with the model it came from
readr::write_csv(rec_group |> dplyr::mutate(model = WIN_MOD_NAME), OUT_GROUP)
readr::write_csv(
  rec_indiv |>
    dplyr::mutate(arm = ifelse(condition == 1L, ARM_INT, ARM_REF), model = WIN_MOD_NAME),
  OUT_INDIV
)
readr::write_csv(rel_tbl |> dplyr::mutate(model = WIN_MOD_NAME), OUT_REL)
readr::write_csv(rec_diag |> dplyr::mutate(model = WIN_MOD_NAME), OUT_DIAG)
cat(sprintf("\nwrote %s, %s, %s, %s\n", OUT_GROUP, OUT_INDIV, OUT_REL, OUT_DIAG))

## 9. model recovery: can LOO tell the six models apart? ------------------------------
# default: generate from M2 only and fit all six (a parametric bootstrap of the observed gaps).
# MREC_GEN <- MREC_MODELS$key gives the full 6 x 6 confusion matrix (~22 h at 3 replicates).
# loops replicate-major, so an interrupted run leaves a balanced grid
RUN_MODEL_RECOVERY <- TRUE

# 5 with only M2 generating; 3 is affordable for the full 6 x 6 grid
MREC_N_REP <- 5
MREC_SEED  <- 20260818
MREC_DIR   <- "analyses/stan-fits/learning/recovery-model"
MREC_SIM_STAN <- "analyses/stan-models/causAttr-session1-learning-modelrec-sim.stan"
OBS_LOO_RDS   <- "./analyses/stan-fits/learning/loo_compare_s1_allblocks.rds"
OUT_MREC      <- "analyses/outputs/recovery_model_confusion.csv"
OUT_MREC_DIAG <- "analyses/outputs/recovery_model_diagnostics.csv"

# reference model for every elpd difference
MREC_REF <- "m2"
# the model family and the structural flags that place each model in the union simulator --
# adding a family member means adding a row here and nothing else
MREC_MODELS <- tibble::tibble(
  key = c("m0", "m1", "m2", "m3", "m4", "m5"),
  label = c(
    "M0: free q0, single a", "M1: anchored q0, single a", "M2: free q0, valence a",
    "M3: anchored q0, valence a", "M4: M2 + counterfactual updating",
    "M5: M3 + counterfactual updating"
  ),
  stan = c(
    "causAttr-session1-learning-s1-single.stan",
    "causAttr-session1-learning-s1-anchored.stan",
    "causAttr-session1-learning-s1-free-valalpha.stan",
    "causAttr-session1-learning-anchored-valalpha.stan",
    "causAttr-session1-learning-s1-free-valalpha-cf.stan",
    "causAttr-session1-learning-s1-cf.stan"
  ),
  fit_rds = c(
    "fit_learn-s1-single.rds", "fit_learn-s1-anchored.rds",
    "fit_learn-s1-free-valalpha.rds", "fit_learn-s1-anchored-valalpha.rds",
    "fit_learn-s1-free-valalpha-cf.rds", "fit_learn-s1-cf.rds"
  ),
  free_q0        = c(1L, 0L, 1L, 0L, 1L, 0L),
  val_alpha      = c(0L, 0L, 1L, 1L, 1L, 1L),
  counterfactual = c(0L, 0L, 0L, 0L, 1L, 1L),
  beta_grp       = c(0L, 0L, 0L, 0L, 0L, 0L)
)
# generating models (MREC_MODELS$key for the full confusion matrix); all six are fitted
MREC_GEN <- "m2"
MREC_FIT <- MREC_MODELS$key

# truth hyperparameters + the p_* used to verify the simulator mapping (names vary by model)
MREC_HYPER_VARS <- stats::setNames(lapply(MREC_MODELS$key, function(k) {
  m <- MREC_MODELS[MREC_MODELS$key == k, ]
  c(
    if (m$val_alpha == 1) c("mu_alpha_p", "alpha_int", "sigma_alpha_p", "p_alpha_neg", "p_alpha_pos")
    else c("mu_alpha", "alpha_int", "sigma_alpha", "p_alpha"),
    if (m$free_q0 == 1) c("mu_q0_foil", "q0_foil_int", "mu_q0_ig", "q0_ig_int", "p_q0_foil", "p_q0_ig")
    else c("mu_q0", "q0_int", "p_q0"),
    "sigma_beta", "mu_beta", "beta_int", "p_beta"
  )
}), MREC_MODELS$key)
# convergence is judged on each model's own hyperparameters (the p_* are their transforms)
MREC_CONV_VARS <- lapply(MREC_HYPER_VARS, function(v) v[!grepl("^p_", v)])

if (RUN_MODEL_RECOVERY) {

  dir.create(MREC_DIR, showWarnings = FALSE, recursive = TRUE)

  ## 10. truth draws for the generating models -----------------------------------------------
  # only generating models need truth draws (and a cached fit)
  mrec_hyper_draws <- lapply(stats::setNames(MREC_GEN, MREC_GEN), function(k) {
    fit_rds <- MREC_MODELS$fit_rds[MREC_MODELS$key == k]
    cached_draws(
      file.path("./analyses/stan-fits/learning", sub("\\.rds$", "_hyper_draws.rds", fit_rds)),
      file.path("./analyses/stan-fits/learning", fit_rds),
      MREC_HYPER_VARS[[k]], toupper(k)
    )
  })

  # the same draw index for every model, so replicate r is a consistent "population"
  mrec_truth_idx <- round(seq(1, min(sapply(mrec_hyper_draws, nrow)), length.out = MREC_N_REP))
  stopifnot(!anyDuplicated(mrec_truth_idx))

  ## 11. simulate from each model, fit all six -----------------------------------------
  mrec_sim_mod <- cmdstanr::cmdstan_model(MREC_SIM_STAN)
  mrec_fit_mods <- lapply(stats::setNames(MREC_FIT, MREC_FIT), function(k) {
    cmdstanr::cmdstan_model(file.path("analyses/stan-models", MREC_MODELS$stan[MREC_MODELS$key == k]))
  })
  MREC_MD5 <- c(
    sim = unname(tools::md5sum(MREC_SIM_STAN)),
    stats::setNames(
      unname(tools::md5sum(file.path("analyses/stan-models", MREC_MODELS$stan))), MREC_MODELS$key
    )
  )

  mrec_union_data <- function(key, row) {
    m <- MREC_MODELS[MREC_MODELS$key == key, ]
    union_sim_data(row, m$free_q0, m$val_alpha, m$counterfactual, m$beta_grp)
  }

  # sim variable -> the generating fit's own variable, for the mapping check below
  mrec_name_map <- function(key) {
    m <- MREC_MODELS[MREC_MODELS$key == key, ]
    idx3 <- expand.grid(g = 1:2, b = seq_len(n_blk), v = 1:2)
    ig_fit <- if (m$free_q0 == 1) "p_q0_ig" else "p_q0"
    q0_map <- stats::setNames(
      sprintf("%s[%d,%d,%d]", ig_fit, idx3$g, idx3$b, idx3$v),
      sprintf("p_q0_ig[%d,%d,%d]", idx3$g, idx3$b, idx3$v)
    )
    if (m$free_q0 == 1) {
      q0_map <- c(q0_map, stats::setNames(
        sprintf("p_q0_foil[%d,%d,%d]", idx3$g, idx3$b, idx3$v),
        sprintf("p_q0_foil[%d,%d,%d]", idx3$g, idx3$b, idx3$v)
      ))
    }
    # single-alpha models emit one p_alpha, which both simulated valences must reproduce
    a_fit <- if (m$val_alpha == 0) "p_alpha" else NULL
    alpha_map <- c(
      stats::setNames(sprintf("%s[%d]", if (is.null(a_fit)) "p_alpha_neg" else a_fit, 1:2),
                      sprintf("p_alpha_neg[%d]", 1:2)),
      stats::setNames(sprintf("%s[%d]", if (is.null(a_fit)) "p_alpha_pos" else a_fit, 1:2),
                      sprintf("p_alpha_pos[%d]", 1:2))
    )
    c(alpha_map, q0_map, stats::setNames(sprintf("p_beta[%d]", 1:2), sprintf("p_beta[%d]", 1:2)))
  }

  mrec_simulate <- function(key, r) {
    row <- mrec_hyper_draws[[key]][mrec_truth_idx[r], ]
    sim <- mrec_sim_mod$sample(
      data = mrec_union_data(key, row), fixed_param = TRUE, chains = 1,
      iter_warmup = 0, iter_sampling = 1,
      seed = MREC_SEED + 100 * match(key, MREC_MODELS$key) + r, refresh = 0, show_messages = FALSE
    )
    dr <- sim$draws(format = "df")

    # the simulator must reproduce the generating model's own reported quantities
    map <- mrec_name_map(key)
    stopifnot(all(names(map) %in% names(dr)), all(map %in% names(row)))
    sim_v <- unlist(as.data.frame(dr)[1, names(map)])
    fit_v <- unlist(as.data.frame(row)[, unname(map)])
    stopifnot(max(abs(sim_v - fit_v) / pmax(abs(fit_v), 1e-12)) < 1e-6)

    # anchored models: foil at exactly 0.5; single-alpha models: alpha_neg == alpha_pos
    foil <- unlist(as.data.frame(dr)[1, grep("^p_q0_foil\\[", names(dr), value = TRUE)])
    m <- MREC_MODELS[MREC_MODELS$key == key, ]
    if (m$free_q0 == 0) stopifnot(max(abs(foil - 0.5)) < 1e-12)
    a_neg <- sim_vec(dr, "alpha_neg")
    a_pos <- sim_vec(dr, "alpha_pos")
    if (m$val_alpha == 0) stopifnot(max(abs(a_neg - a_pos)) < 1e-12)

    choice  <- sim_mat(dr, "choice")
    outcome <- sim_mat(dr, "outcome")
    stopifnot(
      all(choice[!obs_cell] == -1L), all(outcome[!obs_cell] == -1L),
      all(choice[obs_cell] %in% 1:2), all(outcome[obs_cell] %in% 0:1),
      all(outcome[obs_cell] == rule_outcome(choice[obs_cell], stan_ls_learn_s1$valence[obs_cell]))
    )
    choice[!obs_cell]  <- 1L
    outcome[!obs_cell] <- 0L

    stan_ls_sim <- stan_ls_learn_s1
    stan_ls_sim$choice  <- choice
    stan_ls_sim$outcome <- outcome
    stopifnot(
      identical(stan_ls_sim$nT_ppts, stan_ls_learn_s1$nT_ppts),
      identical(stan_ls_sim$valence, stan_ls_learn_s1$valence),
      !identical(stan_ls_sim$choice, stan_ls_learn_s1$choice)
    )
    list(stan_ls = stan_ls_sim, accuracy = mean(outcome[obs_cell]))
  }

  # one grid cell = one (generating model, replicate, fitted model). only the loo object
  # and diagnostics are kept; the fit itself is dropped.
  mrec_cell <- function(gen_key, r, fit_key, dat) {
    cache <- file.path(MREC_DIR, sprintf("mrec_gen-%s_rep%02d_fit-%s.rds", gen_key, r, fit_key))
    if (file.exists(cache)) {
      out <- readr::read_rds(cache)
      if (!identical(out$md5, MREC_MD5) || out$truth_draw != mrec_truth_idx[r] ||
            !identical(out$schema, REC_SCHEMA)) {
        stop("stale model-recovery cache: ", cache, " -- delete it and re-run")
      }
      return(out)
    }
    t0 <- Sys.time()
    fit <- mrec_fit_mods[[fit_key]]$sample(
      data = dat, seed = MREC_SEED + 5000 + 100 * match(gen_key, MREC_MODELS$key) + r,
      chains = REC_CHAINS, parallel_chains = REC_CHAINS,
      iter_warmup = REC_WARMUP, iter_sampling = REC_SAMPLING, refresh = 2000
    )
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    lo <- fit$loo(variables = "log_lik", cores = REC_CHAINS)
    conv <- posterior::summarise_draws(
      fit$draws(variables = MREC_CONV_VARS[[fit_key]]), posterior::default_convergence_measures()
    )
    dg <- fit$diagnostic_summary(quiet = TRUE)
    out <- list(
      schema = REC_SCHEMA, md5 = MREC_MD5, truth_draw = mrec_truth_idx[r], loo = lo,
      diag = tibble::tibble(
        gen = gen_key, rep = r, fitted = fit_key, runtime_min = elapsed,
        divergences = sum(dg$num_divergent), max_treedepth = sum(dg$num_max_treedepth),
        worst_rhat = max(conv$rhat, na.rm = TRUE),
        min_ess_bulk = min(conv$ess_bulk, na.rm = TRUE),
        # leave-one-participant-out, so psis can strain: record p_loo and k > 0.7 counts
        p_loo = lo$estimates["p_loo", "Estimate"],
        n_bad_k = sum(lo$diagnostics$pareto_k > 0.7),
        n_obs = nrow(lo$pointwise)
      )
    )
    saveRDS(out, cache)
    cat(sprintf(
      "  [gen %s rep %02d fit %s] %.1f min | div %d | rhat %.3f | bad k %d\n",
      gen_key, r, fit_key, elapsed, out$diag$divergences, out$diag$worst_rhat, out$diag$n_bad_k
    ))
    rm(fit)
    gc(verbose = FALSE)
    out
  }

  # MREC_REF must be fitted everywhere (and generated from, for the calibration table)
  stopifnot(MREC_REF %in% MREC_FIT, all(MREC_GEN %in% MREC_MODELS$key))
  if (!MREC_REF %in% MREC_GEN) {
    cat(sprintf("NOTE %s is not among the generating models -- skipping the calibration table\n",
                toupper(MREC_REF)))
  }

  n_cells <- length(MREC_GEN) * length(MREC_FIT) * MREC_N_REP
  cat(sprintf(
    "\nmodel recovery: %d generating x %d fitted x %d replicates = %d fits (~%.0f h at 12 min/fit)\n",
    length(MREC_GEN), length(MREC_FIT), MREC_N_REP, n_cells, n_cells * 12 / 60
  ))

  # elpd difference between two loo objects, with the pointwise SE loo_compare() would use
  elpd_diff_vs <- function(lo, lo_ref) {
    d <- lo$pointwise[, "elpd_loo"] - lo_ref$pointwise[, "elpd_loo"]
    c(diff = sum(d), se = stats::sd(d) * sqrt(length(d)))
  }

  mrec_rows <- list()
  mrec_diags <- list()
  # replicate-major: an interrupted run leaves a complete matrix at fewer replicates
  for (r in seq_len(MREC_N_REP)) {
    for (gen_key in MREC_GEN) {
      cells_exist <- file.exists(file.path(
        MREC_DIR, sprintf("mrec_gen-%s_rep%02d_fit-%s.rds", gen_key, r, MREC_FIT)
      ))
      dat <- NULL
      if (!all(cells_exist)) {
        cat(sprintf("\n[gen %s, rep %02d] simulating...\n", gen_key, r))
        sim <- mrec_simulate(gen_key, r)
        dat <- sim$stan_ls
        cat(sprintf("  simulated accuracy %.3f\n", sim$accuracy))
      }
      cells <- lapply(stats::setNames(MREC_FIT, MREC_FIT), function(f) mrec_cell(gen_key, r, f, dat))

      loos <- lapply(cells, `[[`, "loo")
      # loo_compare pairs pointwise contributions positionally, so every fit in a cell must
      # cover the same participants
      stopifnot(length(unique(sapply(loos, function(l) nrow(l$pointwise)))) == 1L)
      elpd <- sapply(loos, function(l) l$estimates["elpd_loo", "Estimate"])
      ref <- loos[[MREC_REF]]

      mrec_rows[[length(mrec_rows) + 1]] <- tibble::tibble(
        gen = gen_key, rep = r, fitted = MREC_FIT,
        elpd_loo = unname(elpd[MREC_FIT]),
        se_elpd_loo = sapply(loos[MREC_FIT], function(l) l$estimates["elpd_loo", "SE"]),
        diff_vs_ref = sapply(loos[MREC_FIT], function(l) elpd_diff_vs(l, ref)[["diff"]]),
        se_diff_vs_ref = sapply(loos[MREC_FIT], function(l) elpd_diff_vs(l, ref)[["se"]]),
        selected = MREC_FIT == MREC_FIT[which.max(elpd[MREC_FIT])],
        is_true = MREC_FIT == gen_key
      )
      mrec_diags[[length(mrec_diags) + 1]] <- purrr::map_dfr(cells, "diag")
    }
  }
  mrec_tbl  <- dplyr::bind_rows(mrec_rows)
  mrec_diag <- dplyr::bind_rows(mrec_diags)

  ## 12. confusion matrix, decisiveness, and calibration of the observed gaps -----------
  cat("\nmodel-recovery diagnostics (worst per generating model):\n")
  mrec_diag |>
    dplyr::group_by(gen) |>
    dplyr::summarise(
      fits = dplyr::n(), div = sum(divergences), worst_rhat = round(max(worst_rhat), 3),
      min_ess = round(min(min_ess_bulk)), max_p_loo = round(max(p_loo)),
      max_bad_k = max(n_bad_k), n_obs = dplyr::first(n_obs),
      total_h = round(sum(runtime_min) / 60, 1), .groups = "drop"
    ) |>
    as.data.frame() |>
    print(row.names = FALSE)
  if (any(mrec_diag$n_bad_k > 0.1 * mrec_diag$n_obs)) {
    cat(sprintf(
      "  NOTE PSIS-LOO is straining -- up to %d/%d participants have Pareto k > 0.7.\n",
      max(mrec_diag$n_bad_k), mrec_diag$n_obs[1]
    ))
    cat("  Read the confusion matrix below as the test of whether the ranking survives it.\n")
  }

  conf_tbl <- mrec_tbl |>
    dplyr::filter(selected) |>
    dplyr::count(gen, fitted, name = "n") |>
    tidyr::complete(gen = MREC_GEN, fitted = MREC_FIT, fill = list(n = 0L))

  cat(sprintf("\nconfusion matrix -- rows generate, columns selected by LOO (%d replicates each):\n",
              MREC_N_REP))
  conf_tbl |>
    tidyr::pivot_wider(names_from = fitted, values_from = n) |>
    as.data.frame() |>
    print(row.names = FALSE)

  # also report whether the winner is distinguishable from the runner-up
  cat("\nper generating model: true model selected, and whether the winner was decisive\n")
  cat("(|elpd diff to the best rival| > 2 SE):\n")
  mrec_tbl |>
    dplyr::group_by(gen, rep) |>
    dplyr::summarise(
      true_selected = any(selected & is_true),
      win_margin = {
        e <- elpd_loo
        sorted <- sort(e, decreasing = TRUE)
        sorted[1] - sorted[2]
      },
      .groups = "drop"
    ) |>
    dplyr::group_by(gen) |>
    dplyr::summarise(
      selected = sprintf("%d/%d", sum(true_selected), dplyr::n()),
      mean_margin = round(mean(win_margin), 1), .groups = "drop"
    ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  # where the observed elpd differences (figure 5B) sit relative to the M2-generated ones
  obs_cmp <- readr::read_rds(OBS_LOO_RDS)
  obs_diff <- tibble::tibble(
    fitted = rownames(obs_cmp),
    obs_diff_vs_ref = as.numeric(obs_cmp[, "elpd_diff"]),
    obs_se = as.numeric(obs_cmp[, "se_diff"])
  )
  # loo_compare's best model must be the reference used above
  stopifnot(rownames(obs_cmp)[1] == MREC_REF)

  if (MREC_REF %in% MREC_GEN) {
    cat(sprintf("\nelpd difference from %s: observed vs simulated with %s as the truth:\n",
                toupper(MREC_REF), toupper(MREC_REF)))
    mrec_tbl |>
      dplyr::filter(gen == MREC_REF, fitted != MREC_REF) |>
      dplyr::group_by(fitted) |>
      dplyr::summarise(
        sim = sprintf("%+.1f [%+.1f, %+.1f]", mean(diff_vs_ref), min(diff_vs_ref), max(diff_vs_ref)),
        .groups = "drop"
      ) |>
      dplyr::left_join(obs_diff, by = "fitted") |>
      dplyr::transmute(
        fitted, simulated_if_ref_true = sim,
        observed = sprintf("%+.1f +/- %.1f", obs_diff_vs_ref, obs_se)
      ) |>
      stats::setNames(c("fitted", sprintf("simulated_if_%s_true", MREC_REF), "observed")) |>
      as.data.frame() |>
      print(row.names = FALSE)
  }

  readr::write_csv(mrec_tbl |> dplyr::left_join(obs_diff, by = "fitted"), OUT_MREC)
  readr::write_csv(mrec_diag, OUT_MREC_DIAG)
  cat(sprintf("\nwrote %s, %s\n", OUT_MREC, OUT_MREC_DIAG))

} else {
  cat("\nmodel recovery skipped (RUN_MODEL_RECOVERY = FALSE)\n")
}
