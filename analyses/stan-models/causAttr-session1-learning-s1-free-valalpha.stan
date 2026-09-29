// Session-1, all-blocks (1-3) learning model -- M2 (the winner): free q0 + valence-specific
// alpha.
//
//                       single alpha        valence-specific alpha
//   free q0             M0                  M2  (this file)
//   anchored q0         M1                  M3
//
// q0 as in M0 (both the foil and the IG start estimated per block x valence x group, rather
// than anchoring the foil at 0.5); learning rates as in M3 (individual alpha_neg / alpha_pos).
// chosen-option-only updating and a shared sigma_beta.
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
  // Control-group baseline means. Valence-specific alpha (as M2), free q0 (as M0).
  vector[2] mu_alpha_p;                // [1]=alpha_neg, [2]=alpha_pos (probit)
  array[nBlocks] vector[2] mu_q0_foil; // [block][1=neg, 2=pos] FREE foil start (probit)
  array[nBlocks] vector[2] mu_q0_ig;   // [block][1=neg, 2=pos] IG start (probit)
  real mu_beta;                        // inverse temperature (probit, *20)

  vector<lower=0>[2] sigma_alpha_p;
  real<lower=0> sigma_beta;

  // Causal-group offsets from the control baseline
  vector[2] alpha_int;                  // intervention effect on alpha_neg, alpha_pos
  array[nBlocks] vector[2] q0_foil_int; // intervention effect on each block's foil q0
  array[nBlocks] vector[2] q0_ig_int;   // intervention effect on each block's IG q0
  real beta_int;                        // intervention effect on beta

  // Individual-level non-centred deviations (q0 has none -- group-level only)
  vector[nPpts] alpha_neg_raw;
  vector[nPpts] alpha_pos_raw;
  vector[nPpts] beta_raw;
}

transformed parameters {
  vector[nPpts] alpha_neg;
  vector[nPpts] alpha_pos;
  vector[nPpts] beta;

  // group-level initial Q-values (probability scale), [group, block][valence]; both options
  // free (index 1 = foil, 2 = IG). group 1 = control, 2 = causal; valence 1 = neg, 2 = pos.
  array[2, nBlocks] vector[2] q0_foil_grp;
  array[2, nBlocks] vector[2] q0_ig_grp;
  for (g in 1:2) {
    for (b in 1:nBlocks) {
      for (v in 1:2) {
        q0_foil_grp[g, b][v] = Phi_approx(mu_q0_foil[b][v] + (g == 2 ? q0_foil_int[b][v] : 0));
        q0_ig_grp[g, b][v]   = Phi_approx(mu_q0_ig[b][v]   + (g == 2 ? q0_ig_int[b][v]   : 0));
      }
    }
  }

  for (p in 1:nPpts) {
    int g = condition[p] + 1;
    real ma_neg = mu_alpha_p[1] + (g == 2 ? alpha_int[1] : 0);
    real ma_pos = mu_alpha_p[2] + (g == 2 ? alpha_int[2] : 0);
    real mb     = mu_beta       + (g == 2 ? beta_int     : 0);

    alpha_neg[p] = Phi_approx(ma_neg + sigma_alpha_p[1] * alpha_neg_raw[p]);
    alpha_pos[p] = Phi_approx(ma_pos + sigma_alpha_p[2] * alpha_pos_raw[p]);
    beta[p]      = 20 * Phi_approx(mb + sigma_beta * beta_raw[p]);
  }
}

model {
  mu_alpha_p ~ normal(0, 1);
  mu_beta    ~ normal(0, 1);
  for (b in 1:nBlocks) {
    mu_q0_foil[b]  ~ normal(0, 1);
    mu_q0_ig[b]    ~ normal(0, 1);
    q0_foil_int[b] ~ normal(0, 0.5);
    q0_ig_int[b]   ~ normal(0, 0.5);
  }

  sigma_alpha_p ~ normal(0, 1);
  sigma_beta    ~ normal(0, 1);

  alpha_int ~ normal(0, 0.5);
  beta_int  ~ normal(0, 0.25);

  alpha_neg_raw ~ normal(0, 1);
  alpha_pos_raw ~ normal(0, 1);
  beta_raw      ~ normal(0, 1);

  for (p in 1:nPpts) {
    array[nT_ppts[p] + 1] vector[nChoices] q_neg;
    array[nT_ppts[p] + 1] vector[nChoices] q_pos;
    int g = condition[p] + 1;
    int blk = 1; // running block index, derived from new_block

    for (t in 1:nT_ppts[p]) {
      // Reset Q to this block's free-q0 start at each block onset.
      if (t > 1 && new_block[p, t] == 1) blk += 1;
      if (t == 1 || new_block[p, t] == 1) {
        q_neg[t] = [q0_foil_grp[g, blk][1], q0_ig_grp[g, blk][1]]';
        q_pos[t] = [q0_foil_grp[g, blk][2], q0_ig_grp[g, blk][2]]';
      }

      q_neg[t + 1] = q_neg[t];
      q_pos[t + 1] = q_pos[t];

      if (valence[p, t] == 0) {
        choice[p, t] ~ categorical_logit(beta[p] * q_neg[t]);
        q_neg[t + 1, choice[p, t]] = q_neg[t, choice[p, t]]
          + alpha_neg[p] * (outcome[p, t] - q_neg[t, choice[p, t]]);
      } else {
        choice[p, t] ~ categorical_logit(beta[p] * q_pos[t]);
        q_pos[t + 1, choice[p, t]] = q_pos[t, choice[p, t]]
          + alpha_pos[p] * (outcome[p, t] - q_pos[t, choice[p, t]]);
      }
    }
  }
}

generated quantities {
  // per-participant valence-collapsed learning-rate summary (for diagnostics /
  // downstream); alpha_neg / alpha_pos remain available for the valence split.
  vector[nPpts] alpha_mu = 0.5 * (alpha_neg + alpha_pos);

  // ==================== group-level summaries (probability scale) ====================
  // [1] = control, [2] = causal.
  vector[2] p_alpha_neg = Phi_approx([mu_alpha_p[1], mu_alpha_p[1] + alpha_int[1]]');
  vector[2] p_alpha_pos = Phi_approx([mu_alpha_p[2], mu_alpha_p[2] + alpha_int[2]]');
  array[2, nBlocks] vector[2] p_q0_foil = q0_foil_grp; // [group, block][neg, pos]
  array[2, nBlocks] vector[2] p_q0_ig   = q0_ig_grp;   // [group, block][neg, pos]
  vector[2] p_beta = 20 * Phi_approx([mu_beta, mu_beta + beta_int]');

  // causal - control contrasts on the probability scale
  vector[2] delta_alpha = [p_alpha_neg[2] - p_alpha_neg[1], p_alpha_pos[2] - p_alpha_pos[1]]';
  array[nBlocks] vector[2] delta_q0_foil;
  array[nBlocks] vector[2] delta_q0_ig;
  for (b in 1:nBlocks) {
    for (v in 1:2) {
      delta_q0_foil[b][v] = q0_foil_grp[2, b][v] - q0_foil_grp[1, b][v];
      delta_q0_ig[b][v]   = q0_ig_grp[2, b][v]   - q0_ig_grp[1, b][v];
    }
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
        q_neg[t] = [q0_foil_grp[g, blk][1], q0_ig_grp[g, blk][1]]';
        q_pos[t] = [q0_foil_grp[g, blk][2], q0_ig_grp[g, blk][2]]';
      }

      q_neg[t + 1] = q_neg[t];
      q_pos[t + 1] = q_pos[t];

      if (valence[p, t] == 0) {
        vector[nChoices] logits = beta[p] * q_neg[t];
        log_lik_trial[p, t] = categorical_logit_lpmf(choice[p, t] | logits);
        choice_pred[p, t] = categorical_logit_rng(logits);
        q_neg[t + 1, choice[p, t]] = q_neg[t, choice[p, t]]
          + alpha_neg[p] * (outcome[p, t] - q_neg[t, choice[p, t]]);
      } else {
        vector[nChoices] logits = beta[p] * q_pos[t];
        log_lik_trial[p, t] = categorical_logit_lpmf(choice[p, t] | logits);
        choice_pred[p, t] = categorical_logit_rng(logits);
        q_pos[t + 1, choice[p, t]] = q_pos[t, choice[p, t]]
          + alpha_pos[p] * (outcome[p, t] - q_pos[t, choice[p, t]]);
      }

      // one-step-ahead: was the predicted choice the correct option?
      correct_pred[p, t] =
        (choice[p, t] == choice_pred[p, t]) ? outcome[p, t] : (1 - outcome[p, t]);
      log_lik[p] += log_lik_trial[p, t];
    }
  }
}
