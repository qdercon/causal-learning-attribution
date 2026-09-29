// Joint attribution + symptom model: SINGLE mediator, ANCOVA symptom submodel, within-chain
// parallel (reduce_sum over participants).
//
// the attribution submodel is identical to the multisess ITT model. one attribution domain
// (mediator_domain, data) mediates the arm effect on the follow-up symptom score, which is
// modelled conditional on baseline (ANCOVA) -- one outcome row per participant, so there is no
// t1/t2 covariance to misspecify. theta_med[:,1] is grand-mean centred; direct_arm is the
// baseline-adjusted arm contrast net of the mediator, and total_arm reconstructs the total arm
// effect exactly.
functions {
  real partial_sum_lpmf(
    array[] int p_slice,
    int start,
    int end,
    int nSess,
    array[,] int nT_ppts,
    array[,,] int internal_neg,
    array[,,] int internal_pos,
    array[,,] int global_neg,
    array[,,] int global_pos,
    matrix theta_internal_neg,
    matrix theta_internal_pos,
    matrix theta_global_neg,
    matrix theta_global_pos,
    array[,] real symp,
    matrix theta_med,
    matrix theta_med_within,
    real theta_med_t1_mean,
    array[] int condition,
    real alpha_symp,
    real beta_t1_symp,
    real beta_cond_symp,
    real beta_medB,
    real beta_medB_group,
    real beta_medW,
    real beta_medW_group,
    real sigma_symp
  ) {
    real total = 0;
    for (i in 1:size(p_slice)) {
      int p = p_slice[i];

      // ---- attribution task likelihood for participant p (guarded per-trial form --
      // see the non-threaded file's header for why the vectorised form is unsafe here)
      for (s in 1:nSess) {
        if (nT_ppts[p, s] > 0) {
          for (t in 1:nT_ppts[p, s]) {
            if (internal_neg[p, s, t] != -1) {
              total += bernoulli_logit_lpmf(internal_neg[p, s, t] | theta_internal_neg[p, s]);
            }
            if (internal_pos[p, s, t] != -1) {
              total += bernoulli_logit_lpmf(internal_pos[p, s, t] | theta_internal_pos[p, s]);
            }
            if (global_neg[p, s, t] != -1) {
              total += bernoulli_logit_lpmf(global_neg[p, s, t] | theta_global_neg[p, s]);
            }
            if (global_pos[p, s, t] != -1) {
              total += bernoulli_logit_lpmf(global_pos[p, s, t] | theta_global_pos[p, s]);
            }
          }
        }
      }

      // ---- symptom ANCOVA for participant p (one domain mediates) ----
      // t2 conditional on t1: needs both timepoints (everyone still enters the attribution part)
      {
        real cond_ind = (condition[p] == 1) ? 1.0 : 0.0;
        if (symp[p, 1] != -999 && symp[p, 2] != -999) {
          total += normal_lpdf(symp[p, 2] |
              alpha_symp
            + beta_t1_symp   * symp[p, 1]
            + beta_cond_symp * cond_ind
            + (beta_medB + beta_medB_group * cond_ind) * (theta_med[p, 1] - theta_med_t1_mean)
            + (beta_medW + beta_medW_group * cond_ind) * theta_med_within[p, 2],
            sigma_symp);
        }
      }
    }
    return total;
  }
}
data {
  int nSess;
  int nPpts;
  array[nPpts] int condition;
  array[nPpts] int completed;
  int nTrials_max;
  array[nPpts, nSess] int nT_ppts;
  array[nPpts, nSess, nTrials_max] int<lower=-1, upper=1> internal_neg;
  array[nPpts, nSess, nTrials_max] int<lower=-1, upper=1> internal_pos;
  array[nPpts, nSess, nTrials_max] int<lower=-1, upper=1> global_neg;
  array[nPpts, nSess, nTrials_max] int<lower=-1, upper=1> global_pos;

  // symptom data (standardized); use -999 for missing values
  array[nPpts, nSess] real symp;

  // which attribution domain mediates the symptom regression in THIS fit:
  // 1 = internal_neg, 2 = internal_pos, 3 = global_neg, 4 = global_pos
  int<lower=1, upper=4> mediator_domain;
}

transformed data {
  // participant ids looked up inside partial_sum_lpmf() (slices needn't start at 1)
  array[nPpts] int p_index;
  for (p in 1:nPpts) {
    p_index[p] = p;
  }
}

parameters {
  // group-level correlation matrix (cholesky factor for faster computation)
  cholesky_factor_corr[4] R_chol_theta_neg;
  cholesky_factor_corr[4] R_chol_theta_pos;

  // group-level parameters
  vector[nSess] mu_internal_theta_neg;
  vector[nSess] mu_internal_theta_pos;
  vector[nSess] mu_global_theta_neg;
  vector[nSess] mu_global_theta_pos;

  // sds for each parameter and timepoint
  vector<lower=0>[4] pars_sigma_neg;
  vector<lower=0>[4] pars_sigma_pos;

  // individual-level parameters (raw/untransformed values)
  matrix[4, nPpts] pars_pr_neg;
  matrix[4, nPpts] pars_pr_pos;

  // group-level effects of active intervention at t2 (group level)
  real theta_int_internal_neg;
  real theta_int_internal_pos;
  real theta_int_global_neg;
  real theta_int_global_pos;

  // group-level effects of non-completion (t1 and t2)
  real theta_ncompl_internal_neg;
  real theta_ncompl_internal_pos;
  real theta_ncompl_global_neg;
  real theta_ncompl_global_pos;

  // fixed effects for symptom ANCOVA
  real alpha_symp;      // intercept
  real beta_t1_symp;    // baseline (t1) symptom slope -- freely estimated, where the old
                        // model's random intercept forced it to whatever the compound-
                        // symmetry variance ratio implied
  real beta_cond_symp;  // c' path: baseline-adjusted arm contrast, net of the mediator

  // ==================== symptom regression: between/within, ONE domain ====================
  // B = association with baseline theta (between-person); W = with change from baseline
  // (within-person, the b-path); _group = causal - control difference
  real beta_medB;
  real beta_medB_group;
  real beta_medW;
  real beta_medW_group;

  // residual s.d. of the symptom ANCOVA (one outcome row per participant)
  real<lower=0> sigma_symp;
}

transformed parameters {
  // individual-level parameter off-sets (for non-centered parameterization)
  matrix[4, nPpts] pars_tilde_neg;
  matrix[4, nPpts] pars_tilde_pos;

  // individual-level parameters (transformed values)
  matrix[nPpts, nSess] theta_internal_neg;
  matrix[nPpts, nSess] theta_global_neg;
  matrix[nPpts, nSess] theta_internal_pos;
  matrix[nPpts, nSess] theta_global_pos;

  // construct individual offsets (for non-centered parameterization)
  pars_tilde_neg = diag_pre_multiply(pars_sigma_neg, R_chol_theta_neg) * pars_pr_neg;
  pars_tilde_pos = diag_pre_multiply(pars_sigma_pos, R_chol_theta_pos) * pars_pr_pos;

  // compute individual-level parameters from non-centered parameterization
  for (p in 1:nPpts) {
    // negative events
    theta_internal_neg[p, 1] = mu_internal_theta_neg[1] + pars_tilde_neg[1, p];
    theta_global_neg[p, 1]   = mu_global_theta_neg[1]   + pars_tilde_neg[2, p];

    if (condition[p] == 1) {
      theta_internal_neg[p, 2] = mu_internal_theta_neg[2] + pars_tilde_neg[3, p] + theta_int_internal_neg;
      theta_global_neg[p, 2]   = mu_global_theta_neg[2]   + pars_tilde_neg[4, p] + theta_int_global_neg;
    } else {
      theta_internal_neg[p, 2] = mu_internal_theta_neg[2] + pars_tilde_neg[3, p];
      theta_global_neg[p, 2]   = mu_global_theta_neg[2]   + pars_tilde_neg[4, p];
    }

    // positive events
    theta_internal_pos[p, 1] = mu_internal_theta_pos[1] + pars_tilde_pos[1, p];
    theta_global_pos[p, 1]   = mu_global_theta_pos[1]   + pars_tilde_pos[2, p];

    if (condition[p] == 1) {
      theta_internal_pos[p, 2] = mu_internal_theta_pos[2] + pars_tilde_pos[3, p] + theta_int_internal_pos;
      theta_global_pos[p, 2]   = mu_global_theta_pos[2]   + pars_tilde_pos[4, p] + theta_int_global_pos;
    } else {
      theta_internal_pos[p, 2] = mu_internal_theta_pos[2] + pars_tilde_pos[3, p];
      theta_global_pos[p, 2]   = mu_global_theta_pos[2]   + pars_tilde_pos[4, p];
    }

    if (completed[p] == 0) {
      theta_internal_neg[p, :] += theta_ncompl_internal_neg;
      theta_internal_pos[p, :] += theta_ncompl_internal_pos;
      theta_global_neg[p, :]   += theta_ncompl_global_neg;
      theta_global_pos[p, :]   += theta_ncompl_global_pos;
    }
  }

  // within-person deviations from baseline -- 0 at t=1 by construction.
  matrix[nPpts, nSess] theta_internal_neg_within;
  matrix[nPpts, nSess] theta_internal_pos_within;
  matrix[nPpts, nSess] theta_global_neg_within;
  matrix[nPpts, nSess] theta_global_pos_within;
  for (p in 1:nPpts) {
    for (s in 1:nSess) {
      theta_internal_neg_within[p, s] = theta_internal_neg[p, s] - theta_internal_neg[p, 1];
      theta_internal_pos_within[p, s] = theta_internal_pos[p, s] - theta_internal_pos[p, 1];
      theta_global_neg_within[p, s]   = theta_global_neg[p, s]   - theta_global_neg[p, 1];
      theta_global_pos_within[p, s]   = theta_global_pos[p, s]   - theta_global_pos[p, 1];
    }
  }

  // the ONE domain that mediates in this fit (mediator_domain is data)
  matrix[nPpts, nSess] theta_med;
  matrix[nPpts, nSess] theta_med_within;
  if (mediator_domain == 1) {
    theta_med = theta_internal_neg;
    theta_med_within = theta_internal_neg_within;
  } else if (mediator_domain == 2) {
    theta_med = theta_internal_pos;
    theta_med_within = theta_internal_pos_within;
  } else if (mediator_domain == 3) {
    theta_med = theta_global_neg;
    theta_med_within = theta_global_neg_within;
  } else {
    theta_med = theta_global_pos;
    theta_med_within = theta_global_pos_within;
  }

  // grand-mean centre baseline theta (pooled over arms) before it enters the between-person
  // term -- a reparameterisation only; see total_arm for the exact total
  real theta_med_t1_mean = mean(theta_med[:, 1]);
}

model {
  // ==================== attribution task priors ====================
  R_chol_theta_neg ~ lkj_corr_cholesky(1);
  R_chol_theta_pos ~ lkj_corr_cholesky(1);

  mu_internal_theta_neg ~ normal(0, 1);
  mu_internal_theta_pos ~ normal(0, 1);
  mu_global_theta_neg   ~ normal(0, 1);
  mu_global_theta_pos   ~ normal(0, 1);

  pars_sigma_neg ~ cauchy(0, 1);
  pars_sigma_pos ~ cauchy(0, 1);

  to_vector(pars_pr_neg) ~ normal(0, 1);
  to_vector(pars_pr_pos) ~ normal(0, 1);

  theta_int_internal_neg ~ normal(0, 1);
  theta_int_internal_pos ~ normal(0, 1);
  theta_int_global_neg   ~ normal(0, 1);
  theta_int_global_pos   ~ normal(0, 1);

  theta_ncompl_internal_neg ~ normal(0, 1);
  theta_ncompl_internal_pos ~ normal(0, 1);
  theta_ncompl_global_neg   ~ normal(0, 1);
  theta_ncompl_global_pos   ~ normal(0, 1);

  // ==================== symptom ANCOVA priors ====================
  // outcome is z-scaled by sd(t2); beta_t1_symp (same instrument, t1 -> t2) is centred near 0.5-0.8
  alpha_symp     ~ normal(0, 1);
  beta_t1_symp   ~ normal(0.6, 0.5);
  beta_cond_symp ~ normal(0, 1);

  beta_medB       ~ normal(0, 1);
  beta_medB_group ~ normal(0, 1);
  beta_medW       ~ normal(0, 1);
  beta_medW_group ~ normal(0, 1);

  // residual about a baseline-adjusted mean, ~ sd(t2) * sqrt(1 - r^2)
  sigma_symp ~ normal(0, 0.5);

  // ==================== attribution + symptom likelihood (within-chain parallel) =====
  // one reduce_sum over participants; grainsize = 1 lets the scheduler partition
  target += reduce_sum(
    partial_sum_lpmf, p_index, 1,
    nSess, nT_ppts, internal_neg, internal_pos, global_neg, global_pos,
    theta_internal_neg, theta_internal_pos, theta_global_neg, theta_global_pos,
    symp, theta_med, theta_med_within, theta_med_t1_mean, condition,
    alpha_symp, beta_t1_symp, beta_cond_symp,
    beta_medB, beta_medB_group, beta_medW, beta_medW_group, sigma_symp
  );
}

generated quantities {
  // ==================== correlation matrices ====================
  corr_matrix[4] R_theta_neg = multiply_lower_tri_self_transpose(R_chol_theta_neg);
  corr_matrix[4] R_theta_pos = multiply_lower_tri_self_transpose(R_chol_theta_pos);

  // ==================== probability transforms ====================
  matrix[nPpts, nSess] p_internal_neg = inv_logit(theta_internal_neg);
  matrix[nPpts, nSess] p_internal_pos = inv_logit(theta_internal_pos);
  matrix[nPpts, nSess] p_global_neg = inv_logit(theta_global_neg);
  matrix[nPpts, nSess] p_global_pos = inv_logit(theta_global_pos);

  // ==================== individual-level differences (delta_p), all 4 domains =======
  // emitted for every domain regardless of mediator_domain
  vector[nPpts] delta_internal_neg_p = theta_internal_neg[:, 2] - theta_internal_neg[:, 1];
  vector[nPpts] delta_internal_pos_p = theta_internal_pos[:, 2] - theta_internal_pos[:, 1];
  vector[nPpts] delta_global_neg_p   = theta_global_neg[:, 2] - theta_global_neg[:, 1];
  vector[nPpts] delta_global_pos_p   = theta_global_pos[:, 2] - theta_global_pos[:, 1];

  // ==================== mediation: A path (this fit's mediator domain only) =========
  real a_med_causal = 0;
  real a_med_control = 0;
  {
    vector[nPpts] delta_med_p = theta_med[:, 2] - theta_med[:, 1];
    int n_causal = 0;
    int n_control = 0;
    for (p in 1:nPpts) {
      if (condition[p] == 1) {
        a_med_causal += delta_med_p[p];
        n_causal += 1;
      } else {
        a_med_control += delta_med_p[p];
        n_control += 1;
      }
    }
    a_med_causal /= n_causal;
    a_med_control /= n_control;
  }

  // a-path over participants with observed t2 attribution only (a_med_* is the ITT quantity,
  // with missing t2 theta imputed by the model)
  real a_med_causal_obs = 0;
  real a_med_control_obs = 0;
  {
    vector[nPpts] delta_med_p = theta_med[:, 2] - theta_med[:, 1];
    int n_causal_obs = 0;
    int n_control_obs = 0;
    for (p in 1:nPpts) {
      if (nT_ppts[p, 2] > 0) {
        if (condition[p] == 1) {
          a_med_causal_obs += delta_med_p[p];
          n_causal_obs += 1;
        } else {
          a_med_control_obs += delta_med_p[p];
          n_control_obs += 1;
        }
      }
    }
    a_med_causal_obs  /= n_causal_obs;
    a_med_control_obs /= n_control_obs;
  }

  // ==================== mediation: B path ====================
  real b_med_control = beta_medW;
  real b_med_causal  = beta_medW + beta_medW_group;

  // ==================== mediation: direct (c') path ====================
  // one baseline-adjusted arm contrast (ANCOVA outcome), at the pooled-mean baseline theta, net
  // of this one mediator. not in general total_arm - indirect_med_diff (see below)
  real direct_arm = beta_cond_symp;

  // ==================== mediation: indirect effect (a x b) ====================
  real indirect_med_control = a_med_control * b_med_control;
  real indirect_med_causal  = a_med_causal  * b_med_causal;
  real indirect_med_diff    = indirect_med_causal - indirect_med_control;

  // moderated-mediation index: causal - control difference in the indirect effect.
  real index_mod_med_med = indirect_med_diff;

  // ==================== mediation: total effect (exact reconstruction) ====================
  // averages the model's prediction over each arm's own baseline theta, adding back the term that
  // direct_arm + indirect_med_diff omits when baseline theta is arm-imbalanced. centring-invariant;
  // compared with the mediator-free primary ANCOVA in modelling_mediation.R
  real theta_med_t1_causal = 0;
  real theta_med_t1_control = 0;
  {
    int n_causal = 0;
    int n_control = 0;
    for (p in 1:nPpts) {
      if (condition[p] == 1) {
        theta_med_t1_causal += theta_med[p, 1];
        n_causal += 1;
      } else {
        theta_med_t1_control += theta_med[p, 1];
        n_control += 1;
      }
    }
    theta_med_t1_causal  /= n_causal;
    theta_med_t1_control /= n_control;
  }
  real total_arm = direct_arm
    + beta_medB * (theta_med_t1_causal - theta_med_t1_control)
    + beta_medB_group * (theta_med_t1_causal - theta_med_t1_mean)
    + indirect_med_diff;

  // no prop_med_*: indirect effects from separate single-mediator fits must not be summed
  // (shared variance between domains)

  // ==================== log-likelihoods (attribution only) ====================
  array[nPpts, nSess] real sum_log_lik_ppt;
  vector[nPpts] log_lik;

  for (p in 1:nPpts) {
    sum_log_lik_ppt[p, 1] = 0;
    sum_log_lik_ppt[p, 2] = 0;

    for (s in 1:nSess) {
      if (nT_ppts[p, s] > 0) {
        for (t in 1:nT_ppts[p, s]) {
          if (internal_neg[p, s, t] != -1) {
            sum_log_lik_ppt[p, s] += bernoulli_logit_lpmf(internal_neg[p, s, t] | theta_internal_neg[p, s]);
          }
          if (internal_pos[p, s, t] != -1) {
            sum_log_lik_ppt[p, s] += bernoulli_logit_lpmf(internal_pos[p, s, t] | theta_internal_pos[p, s]);
          }
          if (global_neg[p, s, t] != -1) {
            sum_log_lik_ppt[p, s] += bernoulli_logit_lpmf(global_neg[p, s, t] | theta_global_neg[p, s]);
          }
          if (global_pos[p, s, t] != -1) {
            sum_log_lik_ppt[p, s] += bernoulli_logit_lpmf(global_pos[p, s, t] | theta_global_pos[p, s]);
          }
        }
      }
    }

    log_lik[p] = sum(sum_log_lik_ppt[p, :]);
  }
}
