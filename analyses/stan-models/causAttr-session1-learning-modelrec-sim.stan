// Forward simulator for the session-1 learning family (M0-M5), used by both halves of
// analyses/learning_recovery.R. fixed_param sampling on the real task structure (each
// participant's arm, trial count, valence sequence and block onsets), with fresh individual
// parameters drawn from the hierarchy implied by the hyperparameters passed as data.
//
// six models, two data switches:
//   * val_alpha      0 = single learning rate (M0, M1): alpha_neg[p] == alpha_pos[p];
//                    1 = valence-specific (M2-M5).
//   * counterfactual 0 = chosen option updates only (M0-M3); 1 = both options update (M4, M5).
// anchored q0 (M1, M3, M5) is the free-q0 path with the foil parameters set to zero, since
// Phi_approx(0) = 0.5. sigma_beta is per group, but every current model passes equal entries.
data {
  int<lower=1> nPpts;
  int<lower=1> nTrials_max;
  int<lower=1> nChoices;
  int<lower=1> nBlocks;
  array[nPpts] int<lower=0, upper=nTrials_max> nT_ppts;
  array[nPpts] int<lower=0, upper=1> condition; // 0 = control, 1 = causal

  array[nPpts, nTrials_max] int<lower=0, upper=1> new_block;
  array[nPpts, nTrials_max] int<lower=0, upper=1> valence;

  // which member of the family to generate from
  int<lower=0, upper=1> val_alpha;
  int<lower=0, upper=1> counterfactual;

  // ground truth: one posterior draw of the generating model's hyperparameters, mapped
  // into the union parameterisation described above
  vector[2] mu_alpha_p;                 // [neg, pos]; equal entries when val_alpha = 0
  vector[2] alpha_int;
  vector<lower=0>[2] sigma_alpha_p;
  array[nBlocks] vector[2] mu_q0_ig;    // internal-global start (probit)
  array[nBlocks] vector[2] q0_ig_int;
  array[nBlocks] vector[2] mu_q0_foil;  // zeros => foil anchored at 0.5
  array[nBlocks] vector[2] q0_foil_int;
  real mu_beta;
  real beta_int;
  vector<lower=0>[2] sigma_beta;        // per group; equal entries when shared
}

transformed data {
  if (val_alpha == 0 &&
      (mu_alpha_p[1] != mu_alpha_p[2] ||
       alpha_int[1]  != alpha_int[2]  ||
       sigma_alpha_p[1] != sigma_alpha_p[2])) {
    reject("val_alpha = 0 requires equal [neg, pos] alpha hyperparameters");
  }
}

generated quantities {
  // ==================== group-level truth, reporting scale ====================
  array[2, nBlocks] vector[2] p_q0_ig;   // [group, block][neg, pos]
  array[2, nBlocks] vector[2] p_q0_foil;
  vector[2] p_alpha_neg = Phi_approx([mu_alpha_p[1], mu_alpha_p[1] + alpha_int[1]]');
  vector[2] p_alpha_pos = Phi_approx([mu_alpha_p[2], mu_alpha_p[2] + alpha_int[2]]');
  vector[2] p_beta = 20 * Phi_approx([mu_beta, mu_beta + beta_int]');
  vector[2] delta_alpha = [p_alpha_neg[2] - p_alpha_neg[1], p_alpha_pos[2] - p_alpha_pos[1]]';

  // ==================== individual truth ====================
  vector[nPpts] alpha_neg;
  vector[nPpts] alpha_pos;
  vector[nPpts] beta;

  // ==================== simulated behaviour (-1 = padding) ====================
  array[nPpts, nTrials_max] int choice = rep_array(-1, nPpts, nTrials_max);
  array[nPpts, nTrials_max] int outcome = rep_array(-1, nPpts, nTrials_max);

  for (g in 1:2) {
    for (b in 1:nBlocks) {
      for (v in 1:2) {
        p_q0_ig[g, b][v]   = Phi_approx(mu_q0_ig[b][v]   + (g == 2 ? q0_ig_int[b][v] : 0));
        p_q0_foil[g, b][v] = Phi_approx(mu_q0_foil[b][v] + (g == 2 ? q0_foil_int[b][v] : 0));
      }
    }
  }

  for (p in 1:nPpts) {
    int g = condition[p] + 1;
    int blk = 1;
    vector[nChoices] q_neg;
    vector[nChoices] q_pos;
    real ma_neg = mu_alpha_p[1] + (g == 2 ? alpha_int[1] : 0);
    real ma_pos = mu_alpha_p[2] + (g == 2 ? alpha_int[2] : 0);
    real mb     = mu_beta       + (g == 2 ? beta_int     : 0);

    // fixed draw order (neg, pos, beta) for reproducible rng streams; with val_alpha = 0 the
    // single deviation is shared
    real a_neg_raw = std_normal_rng();
    real a_pos_raw = a_neg_raw;
    if (val_alpha == 1) a_pos_raw = std_normal_rng();

    alpha_neg[p] = Phi_approx(ma_neg + sigma_alpha_p[1] * a_neg_raw);
    alpha_pos[p] = Phi_approx(ma_pos + sigma_alpha_p[2] * a_pos_raw);
    beta[p]      = 20 * Phi_approx(mb + sigma_beta[g] * std_normal_rng());

    for (t in 1:nT_ppts[p]) {
      int c;
      int un;
      int correct_opt = valence[p, t] == 1 ? 2 : 1;

      if (t > 1 && new_block[p, t] == 1) blk += 1;
      if (t == 1 || new_block[p, t] == 1) {
        q_neg = [p_q0_foil[g, blk][1], p_q0_ig[g, blk][1]]';
        q_pos = [p_q0_foil[g, blk][2], p_q0_ig[g, blk][2]]';
      }

      if (valence[p, t] == 0) {
        c = categorical_logit_rng(beta[p] * q_neg);
        un = 3 - c;
        outcome[p, t] = (c == correct_opt);
        q_neg[c] += alpha_neg[p] * (outcome[p, t] - q_neg[c]);
        if (counterfactual == 1) {
          q_neg[un] += alpha_neg[p] * ((1 - outcome[p, t]) - q_neg[un]);
        }
      } else {
        c = categorical_logit_rng(beta[p] * q_pos);
        un = 3 - c;
        outcome[p, t] = (c == correct_opt);
        q_pos[c] += alpha_pos[p] * (outcome[p, t] - q_pos[c]);
        if (counterfactual == 1) {
          q_pos[un] += alpha_pos[p] * ((1 - outcome[p, t]) - q_pos[un]);
        }
      }
      choice[p, t] = c;
    }
  }
}
