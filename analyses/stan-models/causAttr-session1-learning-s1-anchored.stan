// Session-1, all-blocks (1-3) learning model -- M1: anchored q0 + single learning rate.
// group-level q0 per block x valence with the foil fixed at 0.5 (only the IG start estimated),
// and one valence-collapsed alpha -- differs from M3 only in the alpha split.
// generated quantities: one-step-ahead choice_pred / correct_pred over the observed
// trajectory, and per-participant log_lik.
data {
  int<lower=1> nPpts;
  int<lower=1> nTrials_max;
  int<lower=1> nChoices;
  int<lower=1> nBlocks;         // number of blocks per session (e.g. 3)
  array[nPpts] int<lower=0, upper=nTrials_max> nT_ppts;
  array[nPpts] int<lower=0, upper=1> condition; // 0 = control, 1 = causal

  array[nPpts, nTrials_max] int<lower=0, upper=1> new_block; // first trial of each block
  array[nPpts, nTrials_max] int<lower=0, upper=1> valence;
  array[nPpts, nTrials_max] int<lower=1, upper=2> choice;
  array[nPpts, nTrials_max] int<lower=0, upper=1> outcome;
}

parameters {
  // Control-group baseline means. Single valence-collapsed alpha (cf. M3's alpha_neg/
  // alpha_pos split).
  real mu_alpha;                   // learning rate (probit), control baseline
  array[nBlocks] vector[2] mu_q0;  // [block][1=IG_neg, 2=IG_pos] start values (probit)
  real mu_beta;                    // inverse temperature (probit, *20)

  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;

  // Causal-group offsets from the control baseline
  real alpha_int;                  // intervention effect on alpha
  array[nBlocks] vector[2] q0_int; // intervention effect on each block's q0 (neg, pos)
  real beta_int;                   // intervention effect on beta

  // Individual-level non-centred deviations (q0 has none -- group-level only)
  vector[nPpts] alpha_raw;
  vector[nPpts] beta_raw;
}

transformed parameters {
  vector[nPpts] alpha;
  vector[nPpts] beta;

  // Group-level initial IG Q-values (probability scale), [group, block][valence]:
  // group 1 = control, 2 = causal; valence 1 = neg, 2 = pos.
  array[2, nBlocks] vector[2] q0_grp;
  for (g in 1:2) {
    for (b in 1:nBlocks) {
      for (v in 1:2) {
        q0_grp[g, b][v] = Phi_approx(mu_q0[b][v] + (g == 2 ? q0_int[b][v] : 0));
      }
    }
  }

  for (p in 1:nPpts) {
    int g = condition[p] + 1;
    real ma = mu_alpha + (g == 2 ? alpha_int : 0);
    real mb = mu_beta  + (g == 2 ? beta_int  : 0);

    alpha[p] = Phi_approx(ma + sigma_alpha * alpha_raw[p]);
    beta[p]  = 20 * Phi_approx(mb + sigma_beta * beta_raw[p]);
  }
}

model {
  mu_alpha ~ normal(0, 1);
  mu_beta  ~ normal(0, 1);
  for (b in 1:nBlocks) {
    mu_q0[b]  ~ normal(0, 1);
    q0_int[b] ~ normal(0, 0.5);
  }

  sigma_alpha ~ normal(0, 1);
  sigma_beta  ~ normal(0, 1);

  alpha_int ~ normal(0, 0.5);
  beta_int  ~ normal(0, 0.25);

  alpha_raw ~ normal(0, 1);
  beta_raw  ~ normal(0, 1);

  for (p in 1:nPpts) {
    array[nT_ppts[p] + 1] vector[nChoices] q_neg;
    array[nT_ppts[p] + 1] vector[nChoices] q_pos;
    int g = condition[p] + 1;
    int blk = 1; // running block index, derived from new_block

    for (t in 1:nT_ppts[p]) {
      // Reset Q to this block's anchored q0 at each block onset.
      if (t > 1 && new_block[p, t] == 1) blk += 1;
      if (t == 1 || new_block[p, t] == 1) {
        q_neg[t] = [0.5, q0_grp[g, blk][1]]';
        q_pos[t] = [0.5, q0_grp[g, blk][2]]';
      }

      q_neg[t + 1] = q_neg[t];
      q_pos[t + 1] = q_pos[t];

      // Single per-participant alpha used regardless of valence.
      if (valence[p, t] == 0) {
        choice[p, t] ~ categorical_logit(beta[p] * q_neg[t]);
        q_neg[t + 1, choice[p, t]] = q_neg[t, choice[p, t]]
          + alpha[p] * (outcome[p, t] - q_neg[t, choice[p, t]]);
      } else {
        choice[p, t] ~ categorical_logit(beta[p] * q_pos[t]);
        q_pos[t + 1, choice[p, t]] = q_pos[t, choice[p, t]]
          + alpha[p] * (outcome[p, t] - q_pos[t, choice[p, t]]);
      }
    }
  }
}

generated quantities {
  // per-participant summary, named alpha_mu for consistency with M3's downstream
  // mediation predictor naming (here it is simply alpha, not a valence average).
  vector[nPpts] alpha_mu = alpha;

  // ==================== group-level summaries (probability scale) ====================
  // [1] = control, [2] = causal.
  real p_alpha_ctrl  = Phi_approx(mu_alpha);
  real p_alpha_causal = Phi_approx(mu_alpha + alpha_int);
  vector[2] p_alpha = [p_alpha_ctrl, p_alpha_causal]';
  array[2, nBlocks] vector[2] p_q0 = q0_grp; // [group, block][neg, pos]
  vector[2] p_beta = 20 * Phi_approx([mu_beta, mu_beta + beta_int]');

  // causal - control contrasts on the probability scale
  real delta_alpha = p_alpha[2] - p_alpha[1];
  array[nBlocks] vector[2] delta_q0; // [block][neg, pos]
  for (b in 1:nBlocks) {
    for (v in 1:2) delta_q0[b][v] = q0_grp[2, b][v] - q0_grp[1, b][v];
  }

  // ==================== log-likelihood and posterior predictions ====================
  array[nPpts, nTrials_max] int choice_pred;
  array[nPpts, nTrials_max] int correct_pred;
  array[nPpts, nTrials_max] real log_lik_trial;
  vector[nPpts] log_lik;

  for (p in 1:nPpts) {
    array[nT_ppts[p] + 1] vector[nChoices] q_neg;
    array[nT_ppts[p] + 1] vector[nChoices] q_pos;
    int g = condition[p] + 1;
    int blk = 1;
    log_lik[p] = 0;

    for (t in 1:nTrials_max) {
      choice_pred[p, t] = -1;
      correct_pred[p, t] = -1;
      log_lik_trial[p, t] = 0;
    }

    for (t in 1:nT_ppts[p]) {
      if (t > 1 && new_block[p, t] == 1) blk += 1;
      if (t == 1 || new_block[p, t] == 1) {
        q_neg[t] = [0.5, q0_grp[g, blk][1]]';
        q_pos[t] = [0.5, q0_grp[g, blk][2]]';
      }

      q_neg[t + 1] = q_neg[t];
      q_pos[t + 1] = q_pos[t];

      if (valence[p, t] == 0) {
        vector[nChoices] logits = beta[p] * q_neg[t];
        log_lik_trial[p, t] = categorical_logit_lpmf(choice[p, t] | logits);
        choice_pred[p, t] = categorical_logit_rng(logits);
        q_neg[t + 1, choice[p, t]] = q_neg[t, choice[p, t]]
          + alpha[p] * (outcome[p, t] - q_neg[t, choice[p, t]]);
      } else {
        vector[nChoices] logits = beta[p] * q_pos[t];
        log_lik_trial[p, t] = categorical_logit_lpmf(choice[p, t] | logits);
        choice_pred[p, t] = categorical_logit_rng(logits);
        q_pos[t + 1, choice[p, t]] = q_pos[t, choice[p, t]]
          + alpha[p] * (outcome[p, t] - q_pos[t, choice[p, t]]);
      }

      // one-step-ahead: was the predicted choice the correct option?
      correct_pred[p, t] =
        (choice[p, t] == choice_pred[p, t]) ? outcome[p, t] : (1 - outcome[p, t]);
      log_lik[p] += log_lik_trial[p, t];
    }
  }
}
