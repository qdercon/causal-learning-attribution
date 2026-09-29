## helper functions for modelling
# data prep (prep_data()), stan-output processing, plotting, recovery plots and the questionnaire
# ANCOVA helpers shared by the analysis scripts
`%>%` <- dplyr::`%>%`

## arm encoding + palette -------------------------------------------------------
# condition 0 = control (reference arm), 1 = causal. hexes are hardcoded (metbrewer indices shift with n)
ARM_REF  <- "control"
ARM_INT  <- "causal"
ARM_LABS <- c(control = "control learning", causal = "causal learning")
ARM_BRK  <- c(control = "control\nlearning", causal = "causal\nlearning")
# control/causal are Demuth[2,5] (n = 10); `difference` is their rgb midpoint
ARM_COLS <- c(control = "#9b332b", causal = "#f7c267", difference = "#C97A49")
ARM_SHP  <- c(control = 16, causal = 17, difference = 15)
DIFF_LAB <- "difference\n(causal − control)"
VAL_COLS <- c(positive = "#ef8a47", negative = "#72bcd5") # Hiroshige[2,7] @ n=10
# symptom measures, keyed so a measure keeps one colour across figures. DAQ matches the
# `total` row of Figure 1's DAQ sub-scale forest (which still hardcodes the same hex).
SYMP_COLS <- c(daq = "#b695bc", phq = "#574571")

# Prepare symptom scores with catch question filtering
prep_symptom_data <- function(
  self_report_df = NULL,
  attr_ids = NULL,
  z_symptom = TRUE,
  symptom_scale = NULL,
  z_sd_ref = c("t1", "t2")
) {
  # `z_sd_ref`: the sd both timepoints are divided by ("t1", the default for existing fits, or
  # "t2"); the mean is always t1's. not a pure reparameterisation -- the symptom priors are fixed
  z_sd_ref <- match.arg(z_sd_ref)
  # Returns: list with
  #   - symp: [nPpts, 2] matrix of symptom scores (t1, t2) with -999 for missing, or NULL if symptom_scale is NULL
  #   - pass_catch: [nPpts] logical for whether to use this person's data (passed both catch Qs at t2)
  if (is.null(self_report_df)) {
    return(NULL)
  }

  # Ensure we have catch question columns
  required_cols <- c("prolificSubID", "sessionNo", "catch_1_das_corr", "catch_2_erqcr_corr")
  missing_cols <- setdiff(required_cols, names(self_report_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # If symptom_scale is provided, add it to required columns
  if (!is.null(symptom_scale)) {
    if (!(symptom_scale %in% names(self_report_df))) {
      stop(paste("Missing column:", symptom_scale))
    }
  }

  # Get t2 catch question data (needed for all cases)
  sr_t2_catch <- self_report_df |>
    dplyr::filter(sessionNo == 1) |>
    dplyr::select(prolificSubID, catch_1_das_corr, catch_2_erqcr_corr) |>
    dplyr::mutate(
      pass_catch = as.integer((catch_1_das_corr == 1L) & (catch_2_erqcr_corr == 1L))
    )

  # If no symptom_scale provided, just return catch filtering info
  if (is.null(symptom_scale)) {
    sr_combined <- sr_t2_catch
    if (!is.null(attr_ids)) {
      sr_combined <- sr_combined |>
        dplyr::rename(subID = prolificSubID) |>
        dplyr::right_join(
          attr_ids |> dplyr::select(id, subID),
          by = "subID"
        ) |>
        dplyr::arrange(id)
    }

    return(list(
      symp = NULL,
      pass_catch = sr_combined$pass_catch,
      sr_df = sr_combined
    ))
  }

  # Get t1 (sessionNo=0) and t2 (sessionNo=1) symptom data
  sr_t1 <- self_report_df |>
    dplyr::filter(sessionNo == 0) |>
    dplyr::select(prolificSubID, !!rlang::sym(symptom_scale)) |>
    dplyr::rename(symp_t1 = !!rlang::sym(symptom_scale))

  sr_t2_symp <- self_report_df |>
    dplyr::filter(sessionNo == 1) |>
    dplyr::select(prolificSubID, !!rlang::sym(symptom_scale)) |>
    dplyr::rename(symp_t2 = !!rlang::sym(symptom_scale))

  # Join and handle missingness
  sr_combined <- sr_t1 |>
    dplyr::full_join(sr_t2_symp, by = "prolificSubID") |>
    dplyr::full_join(sr_t2_catch, by = "prolificSubID")

  # Match to attr_ids and align with output participant order
  if (!is.null(attr_ids)) {
    sr_combined <- sr_combined |>
      dplyr::rename(subID = prolificSubID) |>
      dplyr::right_join(
        attr_ids |> dplyr::select(id, subID),
        by = "subID"
      ) |>
      dplyr::arrange(id)
  }

  # Optionally standardize (t1 mean as the centre; z_sd_ref picks the divisor)
  if (z_symptom) {
    t1_mean <- mean(sr_combined$symp_t1, na.rm = TRUE)
    # t2 divisor over the participants the model sees, i.e. after catch exclusion
    z_sd <- if (z_sd_ref == "t1") {
      sd(sr_combined$symp_t1, na.rm = TRUE)
    } else {
      sd(sr_combined$symp_t2[sr_combined$pass_catch == 1L], na.rm = TRUE)
    }
    stopifnot(is.finite(z_sd), z_sd > 0)
    sr_combined <- sr_combined |>
      dplyr::mutate(
        symp_t1 = (symp_t1 - t1_mean) / z_sd,
        symp_t2 = (symp_t2 - t1_mean) / z_sd
      )
  }

  # Create output matrices
  nPpts <- nrow(sr_combined)
  symp <- array(-999, dim = c(nPpts, 2))

  symp[, 1] <- ifelse(is.na(sr_combined$symp_t1), -999, sr_combined$symp_t1)
  # Set t2 to -999 (missing) for those who failed catch questions
  symp[, 2] <- ifelse(
    sr_combined$pass_catch == 0L | is.na(sr_combined$symp_t2),
    -999,
    sr_combined$symp_t2
  )

  list(
    symp = symp,
    pass_catch = sr_combined$pass_catch,
    sr_df = sr_combined  # for inspection
  )
}

prep_data <- function(
  learning = list(control = NULL, causal = NULL),
  attribution = list(t1 = NULL, t2 = NULL),
  learning_session = 1,
  learning_blocks = 1:3,
  self_report = NULL,
  symptom_scale = NULL,
  filter_by_catch = FALSE,
  z_symptom = TRUE,
  z_sd_ref = c("t1", "t2"),
  ret = c("ids", "df", "stan")
) {
  ret <- match.arg(ret)
  z_sd_ref <- match.arg(z_sd_ref) # see prep_symptom_data() -- changes fits, not just units

  learn <- !is.null(learning$control) && !is.null(learning$causal)
  attr  <- !is.null(attribution$t1)   && !is.null(attribution$t2)
  joint <- learn && attr
  attr_symp <- attr && !is.null(symptom_scale) && !learn

  if (!learn && !attr) stop("Invalid input.")
  if (attr && is.null(self_report)) stop("Need self_report defined to get condition.")

  multi_session_l <- length(learning_session) > 1   # TRUE when e.g. c(1, 2)
  nSess_l         <- length(learning_session)

  learning_df <- NULL
  learn_ids   <- NULL
  attr_df     <- NULL
  attr_ids    <- NULL

  n_ppts_l      <- 0L
  n_ppts_c      <- 0L
  n_trials_max_l <- 1L
  n_trials_max_c <- 1L

  # ── Learning data ──────────────────────────────────────────────────────────
  if (learn) {
    learning_df <- dplyr::bind_rows(learning$control, learning$causal, .id = "condition") |>
      dplyr::mutate(
        sessionL   = as.integer(sessionNo),
        condition01 = as.integer(condition) - 1L
      ) |>
      dplyr::filter(
        sessionL %in% learning_session, blockNo %in% learning_blocks
      ) |>
      tidyr::drop_na(response) |>
      dplyr::mutate(
        valence01  = ifelse(valence == "positive", 1L, 0L),
        choice12   = as.integer(chosen_attr_type == "internal_global") + 1L,
        outcome01  = as.integer(correct)
      ) |>
      dplyr::arrange(subID, sessionL, blockNo, trialNo) |>
      dplyr::group_by(subID) |>
      dplyr::mutate(
        learn_id       = dplyr::cur_group_id(),
        trial_no_ovl   = dplyr::row_number(),
        new_block      = ifelse(blockNo != dplyr::lag(blockNo, default = 0), 1L, 0L),
        # new_session: 1 on the first trial of each session after the very first
        new_session    = dplyr::if_else(
          multi_session_l &
            sessionL != dplyr::lag(sessionL, default = dplyr::first(sessionL)) &
            trial_no_ovl != 1L,   # trial 1 gets a hard reset, not a carry-over reset
          1L, 0L
        )
      ) |>
      dplyr::ungroup()

    learn_ids <- learning_df |>
      dplyr::distinct(learn_id, subID, condition01) |>
      dplyr::arrange(learn_id)

    if (!joint && ret == "df") return(learning_df)
    if (!joint && ret == "ids") return(learn_ids)

    n_ppts_l       <- dplyr::n_distinct(learning_df$subID)
    n_trials_l     <- learning_df |> dplyr::count(learn_id, name = "n_trials")
    n_trials_max_l <- max(c(n_trials_l$n_trials, 1L))
  }

  # ── Attribution data ────────────────────────────────────────────────────────
  if (attr) {
    randomisation <- dplyr::distinct(self_report, prolificSubID, interventionCondition) |>
      dplyr::mutate(condition01 = ifelse(interventionCondition == "causal", 1L, 0L))

    attr_df <- dplyr::bind_rows(attribution$t1, attribution$t2, .id = "sessionA") |>
      dplyr::mutate(
        sessionA    = as.integer(sessionA),
        condition01 = randomisation$condition01[match(subID, randomisation$prolificSubID)],
        neg_pos     = ifelse(valence == "negative", 0L, 1L),
        internalChosen = as.integer(internalChosen),
        globalChosen   = as.integer(globalChosen)
      ) |>
      dplyr::arrange(subID, sessionA) |>
      dplyr::group_by(subID) |>
      dplyr::mutate(
        id           = dplyr::cur_group_id(),
        trial_no_ovl = dplyr::row_number()
      ) |>
      dplyr::ungroup()

    attr_ids <- attr_df |>
      dplyr::group_by(id, subID) |>
      dplyr::summarise(
        completed   = as.integer(any(sessionA == 2L)),
        condition01 = as.integer(dplyr::first(condition01)),
        .groups = "drop"
      ) |>
      dplyr::arrange(id)

    # Derive nSess from the data (usually 2, but robust to extension)
    nSess <- dplyr::n_distinct(attr_df$sessionA)

    if (filter_by_catch && !is.null(self_report)) {
      symp_obj <- prep_symptom_data(
        self_report, attr_ids, z_symptom = FALSE, symptom_scale = symptom_scale
      )
      pass_catch_vec <- symp_obj$pass_catch

      # Remove t2 data for those who failed catch (ITT: keep all participants)
      failed_catch_ids <- attr_ids$subID[pass_catch_vec == 0L]
      attr_df <- attr_df |>
        dplyr::filter(!(subID %in% failed_catch_ids & sessionA == 2L)) |>
        dplyr::arrange(id, sessionA)

      # `completed` must reflect t2 data after the catch filter
      has_t2 <- attr_df |>
        dplyr::group_by(id) |>
        dplyr::summarise(completed = as.integer(any(sessionA == 2L)), .groups = "drop")
      attr_ids$completed <- has_t2$completed[match(attr_ids$id, has_t2$id)]
    }

    if (!joint && ret == "df") return(attr_df)
    if (!joint && ret == "ids") return(attr_ids)

    n_ppts_c       <- dplyr::n_distinct(attr_df$subID)
    n_trials_c     <- attr_df |> dplyr::count(id, name = "n_trials")
    n_trials_max_c <- max(c(n_trials_c$n_trials, 1L))
  }

  # ── Joint id matching ───────────────────────────────────────────────────────
  if (joint) {
    learn_key   <- paste(learn_ids$subID, learn_ids$condition01, sep = "::")
    attr_key    <- paste(attr_ids$subID,  attr_ids$condition01,  sep = "::")
    learn_match <- match(attr_key, learn_key)

    if (ret == "df")  return(list(learning_df = learning_df, attr_df = attr_df))
    if (ret == "ids") {
      return(
        attr_ids |>
          dplyr::mutate(learn_id = learn_ids$learn_id[learn_match]) |>
          dplyr::select(id, learn_id, subID, condition01, completed)
      )
    }
  }

  # ── Fill attribution arrays ─────────────────────────────────────────────────
  if (attr) {
    n_t_ppts_c  <- array(0L,  dim = c(n_ppts_c, nSess))
    internal_neg <- array(-1L, dim = c(n_ppts_c, nSess, n_trials_max_c))
    internal_pos <- array(-1L, dim = c(n_ppts_c, nSess, n_trials_max_c))
    global_neg   <- array(-1L, dim = c(n_ppts_c, nSess, n_trials_max_c))
    global_pos   <- array(-1L, dim = c(n_ppts_c, nSess, n_trials_max_c))

    for (p in seq_len(n_ppts_c)) {
      for (s in seq_len(nSess)) {
        p_c_df <- attr_df[attr_df$id == p & attr_df$sessionA == s, , drop = FALSE]
        n_t    <- nrow(p_c_df)
        n_t_ppts_c[p, s] <- n_t
        if (n_t == 0) next

        idx <- seq_len(n_t)
        internal_neg[p, s, idx] <- as.integer(ifelse(p_c_df$neg_pos == 0L, p_c_df$internalChosen, -1L))
        internal_pos[p, s, idx] <- as.integer(ifelse(p_c_df$neg_pos == 1L, p_c_df$internalChosen, -1L))
        global_neg[p, s, idx]   <- as.integer(ifelse(p_c_df$neg_pos == 0L, p_c_df$globalChosen,   -1L))
        global_pos[p, s, idx]   <- as.integer(ifelse(p_c_df$neg_pos == 1L, p_c_df$globalChosen,   -1L))
      }
    }
  }

  # ── Fill learning arrays ────────────────────────────────────────────────────
  if (learn) {
    n_ppts_l_out <- if (joint) n_ppts_c else n_ppts_l

    init_new_block   <- 0L
    init_new_session <- 0L
    init_valence     <- if (joint) -1L else 0L
    init_choice      <- if (joint)  0L else 1L
    init_outcome     <- if (joint) -1L else 0L

    n_t_ppts_l   <- array(0L,              dim = n_ppts_l_out)
    new_block    <- array(init_new_block,   dim = c(n_ppts_l_out, n_trials_max_l))
    new_session  <- array(init_new_session, dim = c(n_ppts_l_out, n_trials_max_l))
    valence_l    <- array(init_valence,     dim = c(n_ppts_l_out, n_trials_max_l))
    choice_l     <- array(init_choice,      dim = c(n_ppts_l_out, n_trials_max_l))
    outcome_l    <- array(init_outcome,     dim = c(n_ppts_l_out, n_trials_max_l))

    if (joint) {
      learn_key   <- paste(learn_ids$subID, learn_ids$condition01, sep = "::")
      attr_key    <- paste(attr_ids$subID,  attr_ids$condition01,  sep = "::")
      learn_match <- match(attr_key, learn_key)
    }

    for (p in seq_len(n_ppts_l_out)) {
      learn_id_p <- if (joint) {
        li <- learn_match[p]
        if (is.na(li)) next
        learn_ids$learn_id[li]
      } else {
        p
      }

      p_l_df <- learning_df[learning_df$learn_id == learn_id_p, , drop = FALSE]
      n_t    <- nrow(p_l_df)
      n_t_ppts_l[p] <- n_t
      if (n_t == 0) next

      idx <- seq_len(n_t)
      new_block[p, idx]   <- as.integer(p_l_df$new_block)
      new_session[p, idx] <- as.integer(p_l_df$new_session)
      valence_l[p, idx]   <- as.integer(p_l_df$valence01)
      choice_l[p, idx]    <- as.integer(p_l_df$choice12)
      outcome_l[p, idx]   <- as.integer(p_l_df$outcome01)
    }
  }

  # ── Symptom data ────────────────────────────────────────────────────────────
  symp_data <- NULL
  if ((joint || attr_symp) && !is.null(self_report) && !is.null(symptom_scale)) {
    symp_data <- prep_symptom_data(
      self_report_df = self_report,
      attr_ids       = attr_ids,
      z_symptom      = z_symptom,
      symptom_scale  = symptom_scale,
      z_sd_ref       = z_sd_ref
    )
  }

  has_learning_data <- if (joint) as.integer(!is.na(match(attr_ids$subID, learn_ids$subID))) else NULL

  # ── Return: attribution-only ────────────────────────────────────────────────
  if (!joint && !attr_symp && attr) {
    return(list(
      nSess          = nSess,
      nPpts          = as.integer(n_ppts_c),
      condition      = as.integer(attr_ids$condition01),
      completed      = as.integer(attr_ids$completed),
      nTrials_max    = as.integer(n_trials_max_c),
      nT_ppts        = n_t_ppts_c,
      internal_neg   = internal_neg,
      internal_pos   = internal_pos,
      global_neg     = global_neg,
      global_pos     = global_pos
    ))
  }

  # ── Return: learning-only ───────────────────────────────────────────────────
  if (!joint && learn) {
    out <- list(
      nPpts          = as.integer(n_ppts_l),
      nSess_l        = as.integer(nSess_l),
      nBlocks_l      = 3L,
      nTrials_max    = as.integer(n_trials_max_l),
      nChoices       = 2L,
      nT_ppts        = n_t_ppts_l,
      condition      = as.integer(learn_ids$condition01),
      new_block      = new_block,
      valence        = valence_l,
      choice         = choice_l,
      outcome        = outcome_l
    )
    if (multi_session_l) out$new_session <- new_session
    return(out)
  }

  # ── Return: attribution + symptom, no learning ─────────────────────────────
  if (attr_symp) {
    if (ret == "df")  return(list(attr_df = attr_df, symp_data = symp_data))
    if (ret == "ids") return(attr_ids)

    out <- list(
      nSess          = nSess,
      nPpts          = as.integer(n_ppts_c),
      condition      = as.integer(attr_ids$condition01),
      completed      = as.integer(attr_ids$completed),
      nTrials_max    = as.integer(n_trials_max_c),
      nT_ppts        = n_t_ppts_c,
      internal_neg   = internal_neg,
      internal_pos   = internal_pos,
      global_neg     = global_neg,
      global_pos     = global_pos
    )
    if (!is.null(symp_data)) out$symp <- symp_data$symp
    return(out)
  }

  # ── Return: full joint Stan list ────────────────────────────────────────────
  joint_list <- list(
    nSess          = nSess,
    nPpts          = as.integer(n_ppts_c),
    nSess_l        = as.integer(nSess_l),
    nBlocks_l      = 3L,
    nChoices_l     = 2L,
    condition      = as.integer(attr_ids$condition01),
    completed      = as.integer(attr_ids$completed),
    nTrials_max_c  = as.integer(n_trials_max_c),
    nTrials_max_l  = as.integer(n_trials_max_l),
    nT_ppts_c      = n_t_ppts_c,
    nT_ppts_l      = n_t_ppts_l,
    internal_neg   = internal_neg,
    internal_pos   = internal_pos,
    global_neg     = global_neg,
    global_pos     = global_pos,
    new_block      = new_block,
    valence        = valence_l,
    choice         = choice_l,
    outcome        = outcome_l,
    has_learning_data = has_learning_data
  )

  if (multi_session_l)   joint_list$new_session <- new_session
  if (!is.null(symp_data)) joint_list$symp       <- symp_data$symp

  joint_list
}

# Extract draws and return tidy factor data.frame

extract_draws <- function(draws, gathered, hdi = 0.95) {
  int_eff_terms <- list(
    delta_internal_neg_int = c("delta_internal_neg", "theta_int_internal_neg"),
    delta_internal_pos_int = c("delta_internal_pos", "theta_int_internal_pos"),
    delta_global_neg_int = c("delta_global_neg", "theta_int_global_neg"),
    delta_global_pos_int = c("delta_global_pos", "theta_int_global_pos")
  )

  diff_terms <- list(
    beta_eta_alpha_int = c("beta_eta_alpha_causal", "beta_eta_alpha_control"),
    beta_symp_eta_int = c("beta_symp_eta_causal", "beta_symp_eta_control"),
    beta_symp_time_eta_int = c("beta_symp_time_eta_causal", "beta_symp_time_eta_control"),
    indirect_path_int = c("indirect_path_causal", "indirect_path_control")
  )

  for (new_term in names(int_eff_terms)) {
    source_terms <- int_eff_terms[[new_term]]
    if (all(source_terms %in% names(draws))) {
      draws[[new_term]] <- draws[[source_terms[1]]] + draws[[source_terms[2]]]
    }
  }

  for (new_term in names(diff_terms)) {
    source_terms <- diff_terms[[new_term]]
    if (all(source_terms %in% names(draws))) {
      draws[[new_term]] <- draws[[source_terms[1]]] - draws[[source_terms[2]]]
    }
  }

  inferred_cols <- c(intersect(names(int_eff_terms), names(draws)), intersect(names(diff_terms), names(draws)))
  int_eff_df <- if (length(inferred_cols) > 0) {
    tibble::tibble(
      .variable = rep(inferred_cols, each = nrow(draws)),
      .value = unlist(draws[inferred_cols], use.names = FALSE)
    )
  } else {
    tibble::tibble(.variable = character(), .value = numeric())
  }
  lvl_map <- c(
    # group-mean thetas at each timepoint
    "mu_internal_theta_neg[1]" = "mean θ<sup>(int, neg)</sup>: t1",
    "mu_internal_theta_neg[2]" = "mean θ<sup>(int, neg)</sup>: t2",
    "mu_internal_theta_pos[1]" = "mean θ<sup>(int, pos)</sup>: t1",
    "mu_internal_theta_pos[2]" = "mean θ<sup>(int, pos)</sup>: t2",
    "mu_global_theta_neg[1]"   = "mean θ<sup>(glob, neg)</sup>: t1",
    "mu_global_theta_neg[2]"   = "mean θ<sup>(glob, neg)</sup>: t2",
    "mu_global_theta_pos[1]"   = "mean θ<sup>(glob, pos)</sup>: t1",
    "mu_global_theta_pos[2]"   = "mean θ<sup>(glob, pos)</sup>: t2",
    # intervention effect on Δθ (causal vs. control group contrast)
    "theta_int_internal_neg" = "causal vs. control: Δθ<sup>(int, neg)</sup>",
    "theta_int_internal_pos" = "causal vs. control: Δθ<sup>(int, pos)</sup>",
    "theta_int_global_neg"   = "causal vs. control: Δθ<sup>(glob, neg)</sup>",
    "theta_int_global_pos"   = "causal vs. control: Δθ<sup>(glob, pos)</sup>",
    # non-completion adjustment
    "theta_ncompl_internal_neg" = "non-completion: θ<sup>(int, neg)</sup>",
    "theta_ncompl_internal_pos" = "non-completion: θ<sup>(int, pos)</sup>",
    "theta_ncompl_global_neg"   = "non-completion: θ<sup>(glob, neg)</sup>",
    "theta_ncompl_global_pos"   = "non-completion: θ<sup>(glob, pos)</sup>",
    # within-group t2 - t1 change in θ
    "delta_internal_neg"     = "Δθ<sup>(int, neg)</sup>: control",
    "delta_internal_neg_int" = "Δθ<sup>(int, neg)</sup>: causal",
    "delta_internal_pos"     = "Δθ<sup>(int, pos)</sup>: control",
    "delta_internal_pos_int" = "Δθ<sup>(int, pos)</sup>: causal",
    "delta_global_neg"       = "Δθ<sup>(glob, neg)</sup>: control",
    "delta_global_neg_int"   = "Δθ<sup>(glob, neg)</sup>: causal",
    "delta_global_pos"       = "Δθ<sup>(glob, pos)</sup>: control",
    "delta_global_pos_int"   = "Δθ<sup>(glob, pos)</sup>: causal",
    # α × Δθ: within-group slope of block-1 learning rate on Δθ
    # (legacy single-group version from full-block model, causal only)
    "beta_alpha_internal_neg" = "α × Δθ<sup>(int, neg)</sup>: causal",
    "beta_alpha_internal_pos" = "α × Δθ<sup>(int, pos)</sup>: causal",
    "beta_alpha_global_neg"   = "α × Δθ<sup>(glob, neg)</sup>: causal",
    "beta_alpha_global_pos"   = "α × Δθ<sup>(glob, pos)</sup>: causal",
    # single-block model: per-group slopes and causal − control difference
    "beta_alpha_internal_neg_causal"  = "α × Δθ<sup>(int, neg)</sup>: causal",
    "beta_alpha_internal_pos_causal"  = "α × Δθ<sup>(int, pos)</sup>: causal",
    "beta_alpha_global_neg_causal"    = "α × Δθ<sup>(glob, neg)</sup>: causal",
    "beta_alpha_global_pos_causal"    = "α × Δθ<sup>(glob, pos)</sup>: causal",
    "beta_alpha_internal_neg_control" = "α × Δθ<sup>(int, neg)</sup>: control",
    "beta_alpha_internal_pos_control" = "α × Δθ<sup>(int, pos)</sup>: control",
    "beta_alpha_global_neg_control"   = "α × Δθ<sup>(glob, neg)</sup>: control",
    "beta_alpha_global_pos_control"   = "α × Δθ<sup>(glob, pos)</sup>: control",
    "beta_alpha_internal_neg_diff"    = "causal vs. control: α × Δθ<sup>(int, neg)</sup>",
    "beta_alpha_internal_pos_diff"    = "causal vs. control: α × Δθ<sup>(int, pos)</sup>",
    "beta_alpha_global_neg_diff"      = "causal vs. control: α × Δθ<sup>(glob, neg)</sup>",
    "beta_alpha_global_pos_diff"      = "causal vs. control: α × Δθ<sup>(glob, pos)</sup>",
    # indirect effects (α → Δθ → symptom)
    "indirect_internal_neg_control" = "indirect effect via θ<sup>(int, neg)</sup>: control",
    "indirect_internal_neg_causal"  = "indirect effect via θ<sup>(int, neg)</sup>: causal",
    "indirect_internal_neg_diff"    = "causal vs. control: indirect effect via θ<sup>(int, neg)</sup>",
    "indirect_internal_pos_control" = "indirect effect via θ<sup>(int, pos)</sup>: control",
    "indirect_internal_pos_causal"  = "indirect effect via θ<sup>(int, pos)</sup>: causal",
    "indirect_internal_pos_diff"    = "causal vs. control: indirect effect via θ<sup>(int, pos)</sup>",
    "indirect_global_neg_control"   = "indirect effect via θ<sup>(glob, neg)</sup>: control",
    "indirect_global_neg_causal"    = "indirect effect via θ<sup>(glob, neg)</sup>: causal",
    "indirect_global_neg_diff"      = "causal vs. control: indirect effect via θ<sup>(glob, neg)</sup>",
    "indirect_global_pos_control"   = "indirect effect via θ<sup>(glob, pos)</sup>: control",
    "indirect_global_pos_causal"    = "indirect effect via θ<sup>(glob, pos)</sup>: causal",
    "indirect_global_pos_int"       = "causal vs. control: indirect effect via θ<sup>(glob, pos)</sup>",
    "indirect_global_pos_diff"      = "causal vs. control: indirect effect via θ<sup>(glob, pos)</sup>",
    # A paths: intervention-driven change in θ
    "a_ctrl_internal_neg"   = "A path: Δθ<sup>(int, neg)</sup> (control)",
    "a_causal_internal_neg" = "A path: Δθ<sup>(int, neg)</sup> (causal)",
    "a_ctrl_internal_pos"   = "A path: Δθ<sup>(int, pos)</sup> (control)",
    "a_causal_internal_pos" = "A path: Δθ<sup>(int, pos)</sup> (causal)",
    "a_ctrl_global_neg"     = "A path: Δθ<sup>(glob, neg)</sup> (control)",
    "a_causal_global_neg"   = "A path: Δθ<sup>(glob, neg)</sup> (causal)",
    "a_ctrl_global_pos"     = "A path: Δθ<sup>(glob, pos)</sup> (control)",
    "a_causal_global_pos"   = "A path: Δθ<sup>(glob, pos)</sup> (causal)",
    # B paths: sensitivity of symptoms to θ
    "b_ctrl_internal_neg_path"   = "B path: symptom ← θ<sup>(int, neg)</sup> (control)",
    "b_causal_internal_neg_path" = "B path: symptom ← θ<sup>(int, neg)</sup> (causal)",
    "b_ctrl_internal_pos_path"   = "B path: symptom ← θ<sup>(int, pos)</sup> (control)",
    "b_causal_internal_pos_path" = "B path: symptom ← θ<sup>(int, pos)</sup> (causal)",
    "b_ctrl_global_neg_path"     = "B path: symptom ← θ<sup>(glob, neg)</sup> (control)",
    "b_causal_global_neg_path"   = "B path: symptom ← θ<sup>(glob, neg)</sup> (causal)",
    "b_ctrl_global_pos_path"     = "B path: symptom ← θ<sup>(glob, pos)</sup> (control)",
    "b_causal_global_pos_path"   = "B path: symptom ← θ<sup>(glob, pos)</sup> (causal)",
    # decomposed mediation model (causAttr-multisess-ITT-joint-attr-symp-decomp.stan):
    # A/B paths, indirect effects, direct (c') path, totals -- one block per domain.
    "a_internal_neg_control" = "A path: Δθ<sup>(int, neg)</sup> (control)",
    "a_internal_neg_causal"  = "A path: Δθ<sup>(int, neg)</sup> (causal)",
    "a_internal_pos_control" = "A path: Δθ<sup>(int, pos)</sup> (control)",
    "a_internal_pos_causal"  = "A path: Δθ<sup>(int, pos)</sup> (causal)",
    "a_global_neg_control"   = "A path: Δθ<sup>(glob, neg)</sup> (control)",
    "a_global_neg_causal"    = "A path: Δθ<sup>(glob, neg)</sup> (causal)",
    "a_global_pos_control"   = "A path: Δθ<sup>(glob, pos)</sup> (control)",
    "a_global_pos_causal"    = "A path: Δθ<sup>(glob, pos)</sup> (causal)",
    "b_internal_neg_control" = "B path (within): symptom ← θ<sup>(int, neg)</sup> (control)",
    "b_internal_neg_causal"  = "B path (within): symptom ← θ<sup>(int, neg)</sup> (causal)",
    "b_internal_pos_control" = "B path (within): symptom ← θ<sup>(int, pos)</sup> (control)",
    "b_internal_pos_causal"  = "B path (within): symptom ← θ<sup>(int, pos)</sup> (causal)",
    "b_global_neg_control"   = "B path (within): symptom ← θ<sup>(glob, neg)</sup> (control)",
    "b_global_neg_causal"    = "B path (within): symptom ← θ<sup>(glob, neg)</sup> (causal)",
    "b_global_pos_control"   = "B path (within): symptom ← θ<sup>(glob, pos)</sup> (control)",
    "b_global_pos_causal"    = "B path (within): symptom ← θ<sup>(glob, pos)</sup> (causal)",
    "direct_control" = "direct (c') path: control",
    "direct_causal"  = "direct (c') path: causal",
    "indirect_internal_neg_control" = "indirect effect via θ<sup>(int, neg)</sup>: control",
    "indirect_internal_neg_causal"  = "indirect effect via θ<sup>(int, neg)</sup>: causal",
    "indirect_internal_neg_diff"    = "causal vs. control: indirect effect via θ<sup>(int, neg)</sup>",
    "indirect_internal_pos_control" = "indirect effect via θ<sup>(int, pos)</sup>: control",
    "indirect_internal_pos_causal"  = "indirect effect via θ<sup>(int, pos)</sup>: causal",
    "indirect_internal_pos_diff"    = "causal vs. control: indirect effect via θ<sup>(int, pos)</sup>",
    "indirect_global_neg_control"   = "indirect effect via θ<sup>(glob, neg)</sup>: control",
    "indirect_global_neg_causal"    = "indirect effect via θ<sup>(glob, neg)</sup>: causal",
    "indirect_global_neg_diff"      = "causal vs. control: indirect effect via θ<sup>(glob, neg)</sup>",
    "indirect_global_pos_control"   = "indirect effect via θ<sup>(glob, pos)</sup>: control",
    "indirect_global_pos_causal"    = "indirect effect via θ<sup>(glob, pos)</sup>: causal",
    "indirect_global_pos_diff"      = "causal vs. control: indirect effect via θ<sup>(glob, pos)</sup>",
    "index_mod_med_internal_neg" = "moderated mediation index: θ<sup>(int, neg)</sup>",
    "index_mod_med_internal_pos" = "moderated mediation index: θ<sup>(int, pos)</sup>",
    "index_mod_med_global_neg"   = "moderated mediation index: θ<sup>(glob, neg)</sup>",
    "index_mod_med_global_pos"   = "moderated mediation index: θ<sup>(glob, pos)</sup>",
    "total_control" = "total effect: control",
    "total_causal"  = "total effect: causal",
    "total_diff"    = "causal vs. control: total effect"
  )
  df_ret <- gathered |>
    dplyr::bind_rows(int_eff_df) |>
    dplyr::mutate(
      var_type = ifelse(
        grepl("^delta.*_int$", .variable) | grepl("^theta_int_", .variable) | grepl("_int$", .variable),
        "intervention effect", "group mean"
      )
    )
  df_ret <- df_ret |> dplyr::mutate(
    pos_neg = ifelse(grepl("pos", .variable), "positive", "negative"),
    int_glob = ifelse(grepl("internal", .variable), "internal", "global"),
    var_group = factor(
      dplyr::case_when(
        grepl("^theta_int_|_causal$|_cr$", .variable) ~ "causal",
        grepl("^delta|_ba$|_control$", .variable) ~ "control",
        grepl("_int$|diff$|^index_mod_med_", .variable) ~ "group difference",
        grepl("ncpl|ncompl", .variable) ~ "non-completion",
        grepl("^lambda_", .variable) ~ "group mean",
        TRUE ~ "group mean"
      ),
      levels = c("causal", "control", "group difference", "non-completion", "group mean")
    ),
    prim_int = ifelse(var_group == "causal", TRUE, FALSE),
  )
  # Map labels using variables actually present in gathered draws.
  present_vars <- unique(df_ret[[".variable"]])
  mapped_lvls <- names(lvl_map)[names(lvl_map) %in% present_vars]
  unmapped_lvls <- setdiff(present_vars, names(lvl_map))
  plot_levels <- unique(c(unname(lvl_map[mapped_lvls]), unmapped_lvls))

  df_ret <- df_ret |>
    dplyr::mutate(
      variable = dplyr::case_when(
        .variable %in% mapped_lvls ~ unname(lvl_map[.variable]),
        TRUE ~ .variable
      ),
      variable = factor(
        variable,
        levels = plot_levels
      )
    )

  hdi_ret <- df_ret |>
    dplyr::group_by(variable, var_group) |>
    dplyr::mutate(
      mean = mean(.value),
      hdi_low = bayestestR::hdi(.value, ci = hdi)[[2]],
      hdi_high = bayestestR::hdi(.value, ci = hdi)[[3]]
    ) |>
    dplyr::distinct(variable, mean, hdi_low, hdi_high, var_group)

  list("df" = df_ret, "hdi" = hdi_ret)
}

plot_distr <- function(tidy_draws,
                       var_filt = "",
                       var_filt_excl = NULL,
                       brew_col = "Hokusai3",
                       col_nums = NULL,
                       custom_pal = NULL,
                       fnt_sz = 1,
                       fnt_fm = "Open Sans",
                       hdi_w = "95%",
                       hdi_lab_adj = 0.25) {
  filter_vars <- function(df) {
    df <- dplyr::filter(df, grepl(var_filt, variable))
    if (!is.null(var_filt_excl)) {
      df <- dplyr::filter(df, !grepl(var_filt_excl, variable))
    }
    df
  }

  to_plt <- tidy_draws$df |>
    filter_vars() |>
    dplyr::mutate(variable = factor(variable, levels = rev(levels(variable))))
  hdis <- tidy_draws$hdi |>
    filter_vars() |>
    dplyr::mutate(variable = factor(variable, levels = rev(levels(variable))))
  nvar <- length(unique(to_plt$variable))
  if (!is.null(custom_pal)) pal <- custom_pal
  else if (!is.null(col_nums)) pal <- MetBrewer::met.brewer(brew_col)[col_nums]
  else pal <- sample(MetBrewer::met.brewer(brew_col), length(unique(to_plt[[clr]])))

  to_plt |>
    ggplot2::ggplot(
      ggplot2::aes(y = variable, x = .value)
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linewidth = 1.1, colour = "slategrey",
      linetype = "dashed", alpha = 0.2
    ) +
    ggdist::stat_slabinterval(
      ggplot2::aes(fill = var_group, colour = var_group), .width = c(0.95, 0.99),
      width = 2, point_size = 3, slab_alpha = 0.5,
      position = ggpp::position_dodgenudge(width = 0.1, y = 0.1)
    ) +
    ggplot2::geom_text(
      data = hdis,
      ggplot2::aes(
        x = mean,
        y = variable,  # Use the variable name directly for positioning
        label = paste0(
          hdi_w,
          " HDI = (",
          format(round(hdi_low, digits = 3), nsmall = 3, scientific = FALSE), ", ",
          format(round(hdi_high, digits = 3), nsmall = 3, scientific = FALSE),
          ")"
        ),
        colour = var_group
      ),
      size = fnt_sz * 5, family = fnt_fm, alpha = 0.8,
      position = ggpp::position_dodgenudge(
        width = hdi_lab_adj * 2, y = -hdi_lab_adj,
        kept.origin = "none"
      )
    ) +
    ggplot2::scale_color_manual(name = "", values = pal) +
    ggplot2::scale_fill_manual(name = "", values = pal) +
    ggplot2::labs(x = "estimated posterior value (a.u.)", y = "") +
    cowplot::theme_half_open(
      font_family = fnt_fm, font_size = fnt_sz * 20
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = fnt_sz * 16),
      axis.text.y = ggtext::element_markdown(size = fnt_sz * 14, lineheight = 1.2),
      legend.text = ggplot2::element_text(size = fnt_sz * 16),
      legend.position = "bottom",
      legend.location = "plot",
      legend.justification = "center"
    )
}

extract_indiv_pars <- function(draws, stan_ls, hdi = 0.95,
                               index_order = c("id", "timepoint")) {
  # drop columns of draws starting with a "."
  indiv_pars <- draws |> dplyr::select(-starts_with("."))
  nms_parsed <- strsplit(colnames(indiv_pars), "\\[|,|\\]|_p(?!\\w)", perl = TRUE)
  conds <- stan_ls$condition
  compl <- stan_ls$completed

  # Extract indices for each column parameter
  extract_indices <- function(nms_parts) {
    # Extract all numeric parts between variable and trailing empty string
    indices <- nms_parts[2:length(nms_parts)]
    indices <- indices[indices != ""]
    as.integer(indices)
  }

  indices_list <- lapply(nms_parsed, extract_indices)

  # Start building the tibble with basic statistics
  ret_df <- tibble::tibble(
    "variable" = sapply(nms_parsed, function(x) x[1]),
    "mean" = colMeans(indiv_pars),
    "sd" = apply(indiv_pars, 2, sd),
    "sem" = sd / sqrt(nrow(indiv_pars)),
    "hdi_low" = apply(indiv_pars, 2, function(x) bayestestR::hdi(x, ci = hdi)$CI_low),
    "hdi_high" = apply(indiv_pars, 2, function(x) bayestestR::hdi(x, ci = hdi)$CI_high),
  )

  # Add columns for each index in the specified order
  for (i in seq_along(index_order)) {
    idx_name <- index_order[i]
    ret_df[[idx_name]] <- sapply(indices_list, function(idx_vec) {
      if (i <= length(idx_vec)) idx_vec[i] else NA_integer_
    })
  }

  # Add group and completed based on "id" index if present
  if ("id" %in% index_order) {
    id_pos <- which(index_order == "id")
    ret_df <- ret_df |>
      dplyr::mutate(
        group = sapply(seq_along(indices_list), function(j) {
          if (id_pos <= length(indices_list[[j]])) {
            conds[indices_list[[j]][id_pos]]
          } else {
            NA_integer_
          }
        }),
        completed = sapply(seq_along(indices_list), function(j) {
          if (id_pos <= length(indices_list[[j]])) {
            compl[indices_list[[j]][id_pos]]
          } else {
            NA_integer_
          }
        })
      )
  }

  # Compute hdi_width
  ret_df <- ret_df |>
    dplyr::mutate(hdi_width = hdi_high - hdi_low)

  # Apply timepoint-specific mutations if timepoint index exists
  if ("timepoint" %in% index_order) {
    ret_df <- ret_df |>
      dplyr::mutate(
        timepoint = dplyr::case_when(
          grepl("delta", variable) ~ "t2-t1",
          timepoint == 1 ~ "t1",
          timepoint == 2 ~ "t2",
          .default = ""
        ),
        t = factor(
          timepoint,
          levels = c("t1", "t2", "t2-t1"),
          labels = c("t1", "t2", "change")
        )
      )
  }

  # Apply group factor conversions if group column exists
  if ("group" %in% names(ret_df)) {
    ret_df <- ret_df |>
      dplyr::mutate(
        group = ifelse(group == 0, "control", "causal"),
        grp = factor(
          group,
          levels = c("control", "causal"),
          labels = c("bsl", "int")
        )
      )
  }

  ret_df
}

# per-participant posterior-mean learning rates from a fitted learning model; id_col is
# "learn_id" for learning-alone fits, "id" for the joint fit
extract_alpha_table <- function(fit, stan_ls, id_col = c("learn_id", "id")) {
  id_col <- match.arg(id_col)
  stan_vars <- fit$metadata()$stan_variables
  alpha_vars <- intersect(c("alpha_mu", "alpha_neg", "alpha_pos"), stan_vars)
  if (length(alpha_vars) == 0) stop("fit has none of alpha_mu / alpha_neg / alpha_pos")

  parse_idx <- function(v) as.integer(stringr::str_match(v, "\\[(\\d+)\\]")[, 2])

  alpha_vars |>
    lapply(function(v) {
      fit$summary(v, "mean") |>
        dplyr::transmute(idx = parse_idx(variable), !!v := mean)
    }) |>
    purrr::reduce(dplyr::full_join, by = "idx") |>
    dplyr::mutate(group = ifelse(stan_ls$condition[idx] == 1, "causal", "control")) |>
    dplyr::rename(!!id_col := idx)
}

# wide per-participant t1 / t2 / diff table of the four theta domains from an attribution fit
extract_theta_change_table <- function(fit, stan_ls) {
  theta_vars <- c("theta_internal_neg", "theta_internal_pos", "theta_global_neg", "theta_global_pos")
  draws <- fit$draws(format = "df", variables = theta_vars)
  theta_indiv <- extract_indiv_pars(draws, stan_ls, index_order = c("id", "timepoint"))

  theta_indiv |>
    dplyr::select(-t) |>
    tidyr::pivot_wider(
      names_from = c(variable, timepoint),
      values_from = c("mean", "sd", "sem", "hdi_low", "hdi_high", "hdi_width")
    ) |>
    dplyr::mutate(
      mean_theta_internal_neg_diff = mean_theta_internal_neg_t2 - mean_theta_internal_neg_t1,
      mean_theta_internal_pos_diff = mean_theta_internal_pos_t2 - mean_theta_internal_pos_t1,
      mean_theta_global_neg_diff   = mean_theta_global_neg_t2   - mean_theta_global_neg_t1,
      mean_theta_global_pos_diff   = mean_theta_global_pos_t2   - mean_theta_global_pos_t1
    )
}

# per-participant shift in alpha_mu between a learning-alone fit (learn_id) and a joint fit (id),
# optionally with attribution change -- a shift that tracks theta change signals induced coupling.
# `id_lookup` needs id + learn_id
compute_alpha_shift <- function(fit_alone, stan_ls_alone, fit_test, stan_ls_test, id_lookup,
                                theta_diff = TRUE) {
  parse_idx <- function(v) as.integer(stringr::str_match(v, "\\[(\\d+)\\]")[, 2])

  mean_alone <- fit_alone$summary("alpha_mu", "mean") |>
    dplyr::transmute(learn_id = parse_idx(variable), alpha_mu_alone = mean)
  mean_test <- fit_test$summary("alpha_mu", "mean") |>
    dplyr::transmute(id = parse_idx(variable), alpha_mu_test = mean)

  # id <-> learn_id consistency: the two fits' own group labels for the same participant
  # must agree, or the shift computed below would be silently comparing the wrong people.
  chk <- id_lookup |> tidyr::drop_na(learn_id) |> dplyr::filter(learn_id %in% mean_alone$learn_id)
  stopifnot(all(stan_ls_alone$condition[chk$learn_id] == stan_ls_test$condition[chk$id]))

  shift_df <- id_lookup |>
    dplyr::inner_join(mean_alone, by = "learn_id") |>
    dplyr::inner_join(mean_test, by = "id") |>
    dplyr::mutate(
      group = ifelse(stan_ls_test$condition[id] == 1, "causal", "control"),
      shift = alpha_mu_test - alpha_mu_alone
    )

  if (theta_diff) {
    theta_test <- extract_theta_change_table(fit_test, stan_ls_test)
    diff_cols <- grep("^mean_theta_.*_diff$", names(theta_test), value = TRUE)
    shift_df <- shift_df |>
      dplyr::inner_join(theta_test |> dplyr::select(id, dplyr::all_of(diff_cols)), by = "id")
  }

  shift_df
}

# Summary of compute_alpha_shift(): mean absolute shift and its correlation with each
# available theta-change column, by group.
summarise_alpha_shift <- function(shift_df) {
  diff_cols <- grep("^mean_theta_.*_diff$", names(shift_df), value = TRUE)
  shift_df |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_abs_shift = mean(abs(shift)),
      dplyr::across(dplyr::all_of(diff_cols), ~ cor(shift, .x), .names = "cor_shift_{.col}"),
      .groups = "drop"
    )
}

plot_indiv_pars <- function(pars,
                            var,
                            var_nm,
                            type = "point",
                            tpt = c("t1", "t2"),
                            qnr = NULL,
                            qnr_nm = NULL,
                            compl = FALSE,
                            fnt_sz = 1,
                            sz_scale = c(1, 4),
                            clr = "group",
                            clr_levs = c("control", "causal"),
                            clr_labs = c("control", "causal"),
                            brew_col = "Hokusai3",
                            col_nums = NULL,
                            custom_pal = NULL,
                            boxp_legend_pos = c(0.3, 0.05),
                            indiv_lines = FALSE) {
  type <- match.arg(type, c("point", "box"))

  if (compl) to_plt <- pars |> dplyr::filter(completed == 1)
  else to_plt <- pars
  if (!is.null(custom_pal)) pal <- custom_pal
  else if (!is.null(col_nums)) pal <- MetBrewer::met.brewer(brew_col)[col_nums]
  else pal <- sample(MetBrewer::met.brewer(brew_col), length(unique(to_plt[[clr]])))
  to_plt[["grp"]] <- factor(to_plt[[clr]], levels = clr_levs, labels = clr_labs)
  if (length(var) > 1) {
    to_plt[["var"]] <- factor(to_plt[["variable"]], levels = var, labels = var_nm)
  }
  if (type == "point") {
    if (is.null(qnr)) {
      plt <- to_plt |>
        dplyr::filter(timepoint %in% tpt & variable == var) |>
        dplyr::select(id, grp, timepoint, mean, sd) |>
        tidyr::pivot_wider(names_from = timepoint, values_from = c(mean, sd)) |>
        ggplot2::ggplot(
          ggplot2::aes(x = mean_t1, y = mean_t2, color = grp, fill = grp)
        ) +
        ggplot2::geom_abline(slope = 1, linetype = "dashed", colour = "grey") +
        ggplot2::geom_point(size = 3, alpha = 0.5) +
        ggplot2::geom_errorbar(
          ggplot2::aes(xmin = mean_t1 - sd_t1, xmax = mean_t1 + sd_t1), alpha = .2, orientation = "y"
        ) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_t2 - sd_t2, ymax = mean_t2 + sd_t2), alpha = .2) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
        ggplot2::labs(
          x = paste0("mean (± s.d.) **", var_nm, "** timepoint 1 (a.u.)"),
          y = paste0("mean (± s.d.) **", var_nm, "**<br>timepoint 2 (a.u.)"),
        ) +
        cowplot::theme_minimal_grid(font_family = "Open Sans", font_size = 16 * fnt_sz)
    } else {
      qn <- rlang::sym(qnr)
      plt <- to_plt |>
        dplyr::filter(timepoint == "t2-t1" & grepl(var, variable)) |>
        ggplot2::ggplot(
          ggplot2::aes(x = !!qn, y = mean, color = grp, fill = grp)
        ) +
        ggplot2::geom_point(ggplot2::aes(size = 1 / sd), alpha = 0.5) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.35, width = 0.6
        ) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1.5) +
        ggplot2::guides(size = "none") +
        ggplot2::labs(
          x = paste0("change in **", qnr_nm, "**"),
          y = paste0("\u0394 **", var_nm, "**"),
        ) +
        ggplot2::scale_size(range = sz_scale) +
        cowplot::theme_minimal_grid(font_family = "Open Sans", font_size = 20 * fnt_sz)
    }
    plt +
      ggplot2::scale_colour_manual(name = NULL, values = pal) +
      ggplot2::scale_fill_manual(name = NULL, values = pal) +
      ggplot2::theme(
        axis.title = ggtext::element_markdown(lineheight = 1.2),
        legend.text = ggplot2::element_text(size = fnt_sz * 18),
        plot.title = ggtext::element_markdown(size = fnt_sz * 20, face = "plain", colour = "slateblue4"),
        plot.subtitle = ggtext::element_markdown(size = fnt_sz * 14, face = "plain", colour = "red4")
      )
  } else if (type == "box") {
    if (length(tpt) == 2) {
      x_aes <- "timepoint"
      to_plt <- to_plt |> dplyr::filter(timepoint %in% tpt & variable == var)
    } else {
      x_aes <- "var"
      to_plt <- to_plt |> dplyr::filter(timepoint %in% tpt)
    }

    plt_base <- to_plt |>
      ggplot2::ggplot(
        ggplot2::aes(x = .data[[x_aes]], y = mean, colour = grp, fill = grp)
      )

    if (length(tpt) == 2) {
      # jitter once per participant, so t1/t2 points (and indiv_lines) share an offset
      set.seed(123) # for reproducible jitter
      to_plt <- to_plt |>
        dplyr::group_by(id) |>
        dplyr::mutate(x_jit = runif(1, -0.1, 0.1)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          x_nudge = dplyr::case_when(
            timepoint == tpt[1] ~ 0.2,
            timepoint == tpt[2] ~ -0.2
          ),
          x_final = as.numeric(factor(timepoint)) + x_nudge + x_jit
        )

      plt_base <- to_plt |>
        ggplot2::ggplot(
          ggplot2::aes(x = .data[[x_aes]], y = mean, colour = grp, fill = grp)
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = x_final, group = id, size = 1 / sd),
          alpha = 0.1
        ) +
        ggplot2::geom_boxplot(
          width = 0.15, alpha = 0.5, outlier.shape = NA,
          position = ggpp::position_dodgenudge(
            width = 0.2, x = c(-0.15, -0.15, 0.15, 0.15)
          )
        )
    } else {
      # For 1 timepoint, points are left, boxplots are right
      plt_base <- plt_base +
        ggplot2::geom_boxplot(
          width = 0.2, alpha = 0.5, outlier.shape = NA,
          position = ggpp::position_dodgenudge(width = 0.25, x = 0.15)
        ) +
        ggplot2::geom_point(
          ggplot2::aes(group = id, size = 1 / sd),
          alpha = 0.2,
          position = ggpp::position_jitternudge(
            width = 0.15, x = -0.2, seed = 123,
            nudge.from = "jittered"
          )
        )
    }

    if (length(tpt) == 2 && indiv_lines) {
      lines_df <- to_plt |>
        dplyr::select(id, grp, timepoint, x_final, mean) |>
        tidyr::pivot_wider(
          names_from = timepoint,
          values_from = c(x_final, mean)
        ) |>
        dplyr::rename(
          x1 = paste0("x_final_", tpt[1]),
          x2 = paste0("x_final_", tpt[2]),
          y1 = paste0("mean_", tpt[1]),
          y2 = paste0("mean_", tpt[2])
        ) |>
        tidyr::drop_na()

      plt_base <- plt_base +
        ggplot2::geom_segment(
          data = lines_df,
          ggplot2::aes(x = x1, xend = x2, y = y1, yend = y2, group = id),
          alpha = 0.02
        )
    }

    if (length(tpt) == 2) {
      y_lab <- var_nm
      x_scale <- ggplot2::scale_x_discrete(
        labels = c("t1" = "pre", "t2" = "post"),
        name = NULL
      )
    } else {
      y_lab <- "mean timepoint 1 estimate"
      x_scale <- ggplot2::scale_x_discrete(name = NULL)
    }

    plt_base <- plt_base +
      ggplot2::ylab(var_nm) +
      x_scale +
      cowplot::theme_half_open(font_family = "Open Sans", font_size = 16 * fnt_sz) +
      ggplot2::scale_colour_manual(name = NULL, labels = clr_labs, values = pal) +
      ggplot2::scale_fill_manual(name = NULL, labels = clr_labs, values = pal) +
      ggplot2::scale_size(range = sz_scale) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(position = "inside", nrow = 1),
        colour = ggplot2::guide_legend(position = "inside", nrow = 1),
        size = "none"
      ) +
      ggplot2::theme(
        legend.text = ggplot2::element_text(size = fnt_sz * 18),
        legend.position.inside = boxp_legend_pos,
        legend.key.spacing.x = grid::unit(16 * fnt_sz, "pt"),
        axis.title = ggtext::element_markdown()
      )

    if (length(tpt) == 1) {
      plt_base <- plt_base + ggplot2::theme(
        axis.title = ggtext::element_markdown(size = fnt_sz * 18),
        axis.text = ggtext::element_markdown(size = fnt_sz * 16)
      )
    }
    plt_base
  }
}

# Clean and process generated quantities predictions
process_gq_predictions <- function(gq_draws) {
  # column means first, then parse indices from colnames (avoids pivoting huge arrays)

  # Extract column means directly (fast vectorized operation)
  choice_cols <- grep("^choice_pred\\[", names(gq_draws), value = TRUE)
  correct_cols <- grep("^correct_pred\\[", names(gq_draws), value = TRUE)

  if (length(choice_cols) == 0 || length(correct_cols) == 0) {
    stop("gq_draws must contain columns starting with 'choice_pred[' and 'correct_pred['")
  }

  # Calculate column means directly on the original dataframe
  choice_means <- colMeans(gq_draws[, choice_cols, drop = FALSE], na.rm = TRUE)
  correct_means <- colMeans(gq_draws[, correct_cols, drop = FALSE], na.rm = TRUE)

  # Extract indices from column names using regex
  # E.g., "choice_pred[1,5]" -> (1, 5)
  parse_indices <- function(col_names) {
    matches <- stringr::str_match(col_names, "\\[(\\d+),(\\d+)\\]")
    list(
      id = as.integer(matches[, 2]),
      trial_idx = as.integer(matches[, 3])
    )
  }

  indices_choice <- parse_indices(choice_cols)
  indices_correct <- parse_indices(correct_cols)

  # Create result dataframe
  pred_df <- tibble::tibble(
    id = indices_choice$id,
    trial_no_ovl = indices_choice$trial_idx,
    choice_pred = as.integer(round(choice_means)),
    correct_pred = correct_means  # keep as posterior-mean probability, not rounded to 0/1
  )

  pred_df
}

make_learning_plot <- function(df,
                               title,
                               palette = "Hiroshige",
                               font_size = 16,
                               by = c("both", "block", "valence", "none", "session"),
                               block_labels = NULL,
                               valence_labels = NULL,
                               legend_title = NULL,
                               x_breaks = seq(0, 10, by = 2),
                               legend_pos = c(0.85, 0.3),
                               font_family = "Open Sans",
                               preds = NULL,
                               ret_data = FALSE,
                               line_colour = "black",
                               row_var = NULL,
                               y_breaks = ggplot2::waiver()) {
  by <- match.arg(by)
  if (is.null(block_labels)) {
    block_labels <- c(
      internal_specific = "internal-specific",
      external_global = "external-global",
      external_specific = "external-specific"
    )
    if (by == "both" || by == "block") { # otherwise doesn't matter
      warning("block_labels not provided, defaulting to block 1 order.")
    }
  }
  if (is.null(valence_labels)) {
    valence_labels <- c(positive = "positive", negative = "negative")
    if (by == "both" || by == "valence") { # otherwise doesn't matter
      warning("valence_labels not provided, defaulting to positive/negative.")
    }
  }
  # both were previously set only inside the is.null() branches, so passing either argument
  # explicitly left the corresponding *_levels undefined and errored at the factor() calls
  block_levels <- names(block_labels)
  valence_levels <- names(valence_labels)

  # Validate preds structure if provided
  if (!is.null(preds)) {
    # Check that preds is already processed (has id, trial_no_ovl, choice_pred, correct_pred)
    required_pred_cols <- c("id", "trial_no_ovl", "choice_pred", "correct_pred")
    missing_cols <- setdiff(required_pred_cols, names(preds))
    if (length(missing_cols) > 0) {
      stop("preds must be processed with process_gq_predictions() and contain columns: ",
           paste(missing_cols, collapse = ", "))
    }

    # Check required columns in df
    required_df_cols <- c("learn_id", "choice12", "outcome01", "valence", "sessionNo", "blockNo")
    missing_cols <- setdiff(required_df_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("df missing required columns for preds plotting: ", paste(missing_cols, collapse = ", "))
    }

    # Prevent "both" option when plotting predictions (too busy)
    if (by == "both") {
      stop("Cannot use by='both' when plotting predictions. Choose 'block', 'valence', or 'none'.")
    }

    # Prepare raw data first
    raw_data <- df |>
      tidyr::drop_na(chosen_attr_type, valence) |>
      dplyr::select(learn_id, sessionNo, blockNo, trialNo, chosen_attr_type, valence,
                    choice12, outcome01, dplyr::everything()) |>
      dplyr::group_by(sessionNo, blockNo) |>
      dplyr::mutate(
        block_type = dplyr::first(chosen_attr_type[chosen_attr_type != "internal_global"], default = NA_character_),
        block_type = factor(block_type, levels = block_levels)
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(learn_id, sessionNo, blockNo, trialNo) |>
      dplyr::group_by(learn_id) |>
      dplyr::mutate(trial_no_ovl = dplyr::row_number()) |>
      dplyr::ungroup()

    # Join pre-processed predictions with raw data to get session, block, valence info
    data_prepped <- preds |>
      dplyr::filter(choice_pred > 0) |>
      dplyr::select(id, trial_no_ovl, correct_pred) |>
      dplyr::group_by(id) |>
      dplyr::mutate(learn_id = dplyr::cur_group_id()) |>
      dplyr::ungroup() |>
      dplyr::select(-id) |>
      dplyr::inner_join(
        raw_data |> dplyr::select(
          learn_id, trial_no_ovl, block_type, sessionNo, blockNo, chosen_attr_type, valence, correct
        ),
        by = c("learn_id", "trial_no_ovl")
      ) |>
      tidyr::pivot_longer(
        cols = c(correct, correct_pred),
        names_to = "data_type",
        values_to = "correct"
      ) |>
      dplyr::mutate(
        data_type = factor(data_type, levels = c("correct", "correct_pred"), labels = c("raw", "predicted")),
        valence = factor(valence, levels = valence_levels),
        block_type = factor(block_type, levels = block_levels)
      ) |>
      dplyr::rename(subID = learn_id, trialNo = trial_no_ovl)
  } else {
    # Original logic when no predictions provided
    data_prepped <- df |>
      tidyr::drop_na(chosen_attr_type, valence) |>
      dplyr::group_by(sessionNo, blockNo) |>
      dplyr::mutate(
        block_type = dplyr::first(chosen_attr_type[chosen_attr_type != "internal_global"], default = NA_character_),
        block_type = factor(block_type, levels = block_levels),
        valence = factor(valence, levels = valence_levels)
      ) |>
      dplyr::arrange(subID, sessionNo, blockNo, trialNo) |>
      dplyr::group_by(subID, sessionNo, block_type, valence) |>
      dplyr::mutate(
        trial_no_block = dplyr::row_number(),
        cuml_accuracy = cumsum(correct) / trial_no_block
      ) |>
      dplyr::ungroup()
  }

  # Prepare grouping variables based on what will be plotted
  grouping_vars <- if (is.null(preds)) {
    switch(
      by,
      valence = rlang::exprs(subID, sessionNo, valence),
      block = rlang::exprs(subID, sessionNo, block_type),
      both = rlang::exprs(subID, sessionNo, block_type, valence),
      rlang::exprs(subID, sessionNo)
    )
  } else {
    switch(
      by,
      valence = rlang::exprs(subID, data_type, sessionNo, valence),
      block = rlang::exprs(subID, data_type, sessionNo, block_type),
      none = rlang::exprs(data_type, sessionNo),
      rlang::exprs(subID, data_type, sessionNo)
    )
  }

  data_prepped <- data_prepped |>
    dplyr::arrange(subID, sessionNo, blockNo, trialNo) |>
    dplyr::group_by(!!!grouping_vars) |>
    dplyr::mutate(
      trial_no_block = dplyr::row_number(),
      cuml_accuracy = cumsum(correct) / trial_no_block
    ) |>
    dplyr::ungroup()

  if (ret_data) return(data_prepped)

  # Build aesthetics map
  aes_map <- ggplot2::aes(x = trial_no_block, y = cuml_accuracy)

  if (is.null(preds)) {
    # Original behavior without predictions
    if (by == "block") {
      aes_map$colour <- quote(block_type)
      aes_map$fill <- quote(block_type)
      aes_map$group <- quote(block_type)
      colour_vals <- MetBrewer::met.brewer(palette, 3)
      colour_labels <- block_labels
      linetype_scale <- NULL
    } else if (by == "valence") {
      aes_map$colour <- quote(valence)
      aes_map$fill <- quote(valence)
      aes_map$group <- quote(valence)
      colour_vals <- MetBrewer::met.brewer(palette, 2)
      colour_labels <- ggplot2::waiver()
      linetype_scale <- NULL
    } else if (by == "session") {
      # one pooled series per session, coloured by `line_colour` (so groups can be overlaid)
      aes_map$group <- 1
      colour_vals <- NULL
      colour_labels <- NULL
      linetype_scale <- NULL
    } else {
      aes_map$colour <- quote(block_type)
      aes_map$fill <- quote(block_type)
      aes_map$linetype <- quote(valence)
      aes_map$group <- quote(interaction(block_type, valence))
      colour_vals <- MetBrewer::met.brewer(palette, 3)
      colour_labels <- block_labels
      linetype_scale <- ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = "valence")
    }
  } else {
    # When plotting predictions, always add data_type to distinguish raw vs predicted
    aes_map$linetype <- quote(data_type)

    if (by == "block") {
      aes_map$colour <- quote(block_type)
      aes_map$fill <- quote(block_type)
      aes_map$group <- quote(interaction(block_type, data_type))
      colour_vals <- MetBrewer::met.brewer(palette, 3)
      colour_labels <- block_labels
      linetype_scale <- ggplot2::scale_linetype_manual(
        values = c("raw" = "solid", "predicted" = "dashed"), name = "data source"
      )
    } else if (by == "valence") {
      aes_map$colour <- quote(valence)
      aes_map$fill <- quote(valence)
      aes_map$group <- quote(interaction(valence, data_type))
      colour_vals <- MetBrewer::met.brewer(palette, 2)
      colour_labels <- ggplot2::waiver()
      linetype_scale <- ggplot2::scale_linetype_manual(
        values = c("raw" = "solid", "predicted" = "dashed"), name = "data source"
      )
    } else if (by == "none") {
      aes_map$colour <- quote(data_type)
      aes_map$fill <- quote(data_type)
      aes_map$group <- quote(data_type)
      colour_vals <- c("raw" = "#440154", "predicted" = "#31688e")
      colour_labels <- ggplot2::waiver()
      linetype_scale <- NULL
    }
  }

  # fixed line_colour only applies when by == "session" (no colour/fill aes mapped);
  # for every other by= mode colour/fill are aes-mapped and must not be overridden here
  line_stat  <- if (by == "session") {
    ggplot2::stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1.2, colour = line_colour)
  } else {
    ggplot2::stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1.2)
  }
  ribbon_stat <- if (by == "session") {
    ggplot2::stat_summary(fun.data = "mean_se", geom = "ribbon", alpha = 0.3, colour = NA, fill = line_colour)
  } else {
    ggplot2::stat_summary(fun.data = "mean_se", geom = "ribbon", alpha = 0.3, colour = NA)
  }

  p <- ggplot2::ggplot(data_prepped, aes_map) +
    line_stat +
    ribbon_stat +
    ggplot2::labs(
      title = title,
      x = "trial number",
      y = "cumulative accuracy"
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    cowplot::theme_minimal_hgrid(font_family = font_family, font_size = font_size)

  # row_var puts groups in facet rows; legend_pos is then a keyword ("bottom", ...)
  p <- if (is.null(row_var)) {
    p +
      ggplot2::guides(
        colour = ggplot2::guide_legend(position = "inside"),
        fill = ggplot2::guide_legend(position = "inside"),
        linetype = ggplot2::guide_legend(position = "inside")
      ) +
      ggplot2::facet_wrap(
        ggplot2::vars(sessionNo),
        nrow = 1,
        labeller = ggplot2::as_labeller(function(x) paste("session", x))
      ) +
      ggplot2::theme(
        legend.position.inside = legend_pos,
        legend.background = ggplot2::element_rect(
          fill = "white", colour = "#cacaca", linewidth = 0.5
        ),
        strip.text.y = ggplot2::element_text(family = "Open Sans SemiBold"),
        #legend.key.width = grid::unit(1.5, "lines"),
        legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
        legend.justification = "center",
      )
  } else {
    p +
      # facet_grid2() for strip_themed(): per-row strip colours
      ggh4x::facet_grid2(
        rows = ggplot2::vars(.data[[row_var]]),
        cols = ggplot2::vars(sessionNo),
        labeller = ggplot2::labeller(sessionNo = function(x) paste("session", x)),
        strip = row_strip_themed(data_prepped[[row_var]], font_size)
      ) +
      ggplot2::theme(
        legend.position = legend_pos,
        legend.background = ggplot2::element_rect(
          fill = "white", colour = "#cacaca", linewidth = 0.5
        ),
        legend.key.width = grid::unit(1.5, "lines"),
        legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
        legend.justification = "center"
      )
  }

  if (!is.null(colour_vals)) {
    lgd_nm <- legend_title %||% if (by == "valence") "valence" else "block option"
    p <- p +
      ggplot2::scale_colour_manual(values = colour_vals, labels = colour_labels, name = lgd_nm) +
      ggplot2::scale_fill_manual(values = colour_vals, labels = colour_labels, name = lgd_nm)
  }

  if (!is.null(linetype_scale)) {
    p <- p + linetype_scale
  }

  p
}

# Posterior-predictive per-trial accuracy with predictive intervals ------------
# per draw, group x valence x trial mean accuracy, summarised as a median + `hdi_ci` hdi band
# (predictive, not mean-of-means, uncertainty). assumes a learning-alone fit (correct_pred[p, t]
# with p = learn_id). `correct_pred_draws` = fit$draws(format = "df", variables = "correct_pred");
# `raw_df` = prep_data(ret = "df"); `group_lookup` maps learn_id -> group
make_ppc_intervals <- function(correct_pred_draws, raw_df, group_lookup,
                               n_draws = 1000, seed = 1, block = FALSE,
                               hdi_ci = 0.95) {
  # `block = TRUE` computes the within-(block, valence) trial index and carries
  # blockNo through, so multi-block fits can be plotted per block.
  keyvars <- if (block) c("valence", "blockNo") else "valence"

  raw_ordered <- raw_df |>
    tidyr::drop_na(chosen_attr_type, valence) |>
    dplyr::arrange(learn_id, sessionNo, blockNo, trialNo) |>
    dplyr::group_by(learn_id) |>
    dplyr::mutate(trial_no_ovl = dplyr::row_number()) |>
    dplyr::ungroup()

  raw_key <- raw_ordered |>
    dplyr::select(dplyr::all_of(c("learn_id", "trial_no_ovl", keyvars)))

  # thin draws for memory, then reshape correct_pred to long form
  cp_cols <- grep("^correct_pred\\[", names(correct_pred_draws), value = TRUE)
  if (length(cp_cols) == 0) stop("correct_pred_draws must contain 'correct_pred[' columns")
  set.seed(seed)
  draws_thin <- correct_pred_draws |>
    dplyr::select(dplyr::any_of(".draw"), dplyr::all_of(cp_cols)) |>
    dplyr::slice_sample(n = min(n_draws, nrow(correct_pred_draws)))
  if (!".draw" %in% names(draws_thin)) draws_thin$.draw <- seq_len(nrow(draws_thin))

  pred_long <- draws_thin |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(cp_cols),
      names_to = c("learn_id", "trial_no_ovl"),
      names_pattern = "correct_pred\\[(\\d+),(\\d+)\\]",
      values_to = "correct_pred"
    ) |>
    dplyr::mutate(
      learn_id = as.integer(learn_id),
      trial_no_ovl = as.integer(trial_no_ovl)
    ) |>
    dplyr::filter(correct_pred >= 0) |>
    dplyr::inner_join(raw_key, by = c("learn_id", "trial_no_ovl")) |>
    dplyr::inner_join(group_lookup, by = "learn_id") |>
    dplyr::arrange(.draw, learn_id, trial_no_ovl) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(".draw", "learn_id", "group", keyvars)))) |>
    dplyr::mutate(trial = dplyr::row_number()) |>
    dplyr::ungroup()

  # hdi() emits expected tie-break messages here (acc takes few distinct values); muffle only those
  quiet_hdi <- function(x, bound) {
    withCallingHandlers(
      bayestestR::hdi(x, ci = hdi_ci)[[bound]],
      message = function(m) {
        if (grepl("Identical densities", conditionMessage(m), fixed = TRUE)) {
          invokeRestart("muffleMessage")
        }
      }
    )
  }

  # draw-level group accuracy, then predictive bands across draws
  pred_bands <- pred_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(".draw", "group", keyvars, "trial")))) |>
    dplyr::summarise(acc = mean(correct_pred), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("group", keyvars, "trial")))) |>
    dplyr::summarise(
      pred_med = stats::median(acc),
      pred_lo = quiet_hdi(acc, "CI_low"),
      pred_hi = quiet_hdi(acc, "CI_high"),
      .groups = "drop"
    ) |>
    dplyr::mutate(hdi_ci = hdi_ci)

  # raw per-trial accuracy (binomial SE across participants)
  raw_bands <- raw_ordered |>
    dplyr::inner_join(group_lookup, by = "learn_id") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("group", "learn_id", keyvars)))) |>
    dplyr::mutate(trial = dplyr::row_number()) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("group", keyvars, "trial")))) |>
    dplyr::summarise(
      raw_acc = mean(as.numeric(correct)), n = dplyr::n(),
      raw_se = sqrt(raw_acc * (1 - raw_acc) / n), .groups = "drop"
    )

  dplyr::left_join(pred_bands, raw_bands, by = c("group", keyvars, "trial"))
}

# Plot the output of make_ppc_intervals(): raw per-trial accuracy (points +/- SE)
# over the model's median + HDI posterior-predictive band, faceted by group.
plot_ppc_intervals <- function(bands, title = NULL, font_family = "Open Sans",
                               font_size = 16, x_breaks = 1:5, block_type_labels = NULL) {
  # cached bands may predate the hdi columns
  missing_cols <- setdiff(c("pred_med", "pred_lo", "pred_hi"), names(bands))
  if (length(missing_cols)) {
    stop(
      "bands is missing ", paste(missing_cols, collapse = ", "),
      if (any(grepl("^pred_(lo|hi)(50|90)$", names(bands)))) {
        " -- it carries the older equal-tailed columns, so it predates the switch to HDI
         bands; delete the cached .rds and re-run make_ppc_intervals()"
      } else {
        " -- was it produced by make_ppc_intervals()?"
      },
      call. = FALSE
    )
  }
  val_cols <- c(positive = VAL_COLS[["positive"]], negative = VAL_COLS[["negative"]])
  bands <- bands |>
    dplyr::mutate(
      valence = factor(valence, levels = c("positive", "negative")),
      # group arrives as a character from group_lookup, which would facet alphabetically
      # (causal first); the manuscript convention is control then causal everywhere
      group = factor(group, levels = c(ARM_REF, ARM_INT))
    )

  # observed and predicted both as line + ribbon; linetype distinguishes them
  p <- ggplot2::ggplot(bands, ggplot2::aes(x = trial, group = valence)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pred_lo, ymax = pred_hi, fill = valence), alpha = 0.2, colour = NA
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = raw_acc - raw_se, ymax = raw_acc + raw_se, fill = valence), alpha = 0.4, colour = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = pred_med, colour = valence, linetype = "posterior predictive (median)"), linewidth = 1.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = raw_acc, colour = valence, linetype = "observed"), alpha = 0.8,linewidth = 1
    ) +
    ggplot2::scale_colour_manual(values = val_cols, name = "valence") +
    ggplot2::scale_fill_manual(values = val_cols, name = "valence") +
    ggplot2::scale_linetype_manual(
      values = c(observed = "solid", "posterior predictive (median)" = "dashed"),
      labels = c(observed = "observed", "posterior predictive (median)" = "predicted"),
      name = "data"
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = title, x = "trial number (within valence)", y = "accuracy") +
    cowplot::theme_minimal_hgrid(font_family = font_family, font_size = font_size) +
    ggplot2::theme(
      legend.background = ggplot2::element_rect(
        fill = "white", colour = "#cacaca", linewidth = 0.5
      ),
      legend.key.width = grid::unit(2, "lines"),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
      legend.justification = "center",
      strip.text.x = ggtext::element_markdown()
    )

  # facet group x block when block bands are supplied. block_type_labels (named by blockNo) is
  # only valid where the foil-to-block mapping is fixed (session 1). strip_themed() colours each
  # arm's row strip
  arm_strip_pal <- unname(ARM_COLS[c(ARM_REF, ARM_INT)])
  # family/size recycled to length(colour): elem_list_text() doesn't recycle
  arm_strip <- ggh4x::strip_themed(
    text_y = ggh4x::elem_list_text(
      colour = arm_strip_pal,
      family = rep("Open Sans SemiBold", length(arm_strip_pal)),
      size = rep(font_size * 1.2, length(arm_strip_pal))
    )
  )
  if ("blockNo" %in% names(bands)) {
    block_labeller <- if (!is.null(block_type_labels)) {
      ggplot2::as_labeller(block_type_labels)
    } else {
      ggplot2::as_labeller(function(x) paste("block", x))
    }
    # free_x so trials can be numbered continuously across blocks
    p +
      ggh4x::facet_grid2(
        rows = ggplot2::vars(group), cols = ggplot2::vars(blockNo), scales = "free_x",
        labeller = ggplot2::labeller(blockNo = block_labeller), strip = arm_strip
      ) +
      ggplot2::theme(strip.text.x = ggtext::element_markdown())
  } else {
    p + ggh4x::facet_wrap2(ggplot2::vars(group), nrow = 1, strip = arm_strip)
  }
}

# Model comparison: LOO ========================================================

# `fits`: named list of fits on identical data (same participants) -- check before calling
compute_loo_compare <- function(fits, cores = 4) {
  loo_list <- lapply(fits, function(f) f$loo(variables = "log_lik", cores = cores))
  loo::loo_compare(loo_list)
}

# elpd_diff +/- se_diff forest plot from loo::loo_compare()'s output (best model,
# elpd_diff = 0, first). `model_labels` optionally renames the list names used in
# compute_loo_compare() (e.g. c(m3 = "M3: anchored q0, valence alpha")).
plot_loo_compare <- function(loo_compare_tbl, model_labels = NULL, title = NULL,
                             font_family = "Open Sans", font_size = 14) {
  # loo_compare()'s result carries a "compare.loo" class that as_tibble() otherwise
  # propagates onto every extracted column (breaks ggplot's scale detection)
  mat <- loo_compare_tbl
  class(mat) <- "matrix"

  df <- tibble::as_tibble(mat, rownames = "model") |>
    dplyr::mutate(
      model = if (!is.null(model_labels)) unname(model_labels[model]) else model,
      # loo_compare() orders rows best -> worst; reverse so the best model plots on top
      model = factor(model, levels = rev(model))
    )

  ggplot2::ggplot(df, ggplot2::aes(x = elpd_diff, y = model)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = elpd_diff - se_diff, xmax = elpd_diff + se_diff),
      orientation = "y", width = 0.15, linewidth = font_size / 26, colour = "slategray"
    ) +
    # point scaled off the base font rather than fixed at 3, which read as a blob against
    # small body text and as a dot against large
    ggplot2::geom_point(size = font_size / 5.5, colour = "slateblue") +
    ggplot2::labs(title = title, x = "ELPD-LOO difference vs. best model (± s.e.)", y = NULL) +
    cowplot::theme_minimal_vgrid(font_family = font_family, font_size = font_size) +
    ggplot2::theme(axis.text.y = ggtext::element_markdown(lineheight = 1.3))
}

# Parameter recovery ==========================================================

# recovery plots (ported from STAND-study's plot_reff_recovery()). all take a long data frame,
# one row per participant x parameter, with `id`, `param`, `true`, `recovered`

# levels/labels shared by both recovery plots: `param_labs` is an optional named vector
# (names = `param` values in the data, values = display labels) that also fixes the
# facet/axis order.
recovery_param_levels <- function(recov_df, param_labs = NULL) {
  lev <- if (!is.null(param_labs)) names(param_labs) else unique(as.character(recov_df$param))
  list(lev = lev, labs = if (!is.null(param_labs)) unname(param_labs) else lev)
}

# row strip styling for facet_grid2(): colours rows with ARM_COLS when every level is an arm name
row_strip_themed <- function(row_values, font_size = 14) {
  lev <- levels(factor(row_values))
  cols <- if (all(lev %in% names(ARM_COLS))) unname(ARM_COLS[lev]) else rep("black", length(lev))
  # family/size recycled to length(cols) (elem_list_text() doesn't recycle)
  ggh4x::strip_themed(
    text_y = ggh4x::elem_list_text(
      colour = cols, family = rep("Open Sans SemiBold", length(cols)), size = rep(font_size * 1.15, length(cols))
    )
  )
}

# true vs recovered posterior mean, one facet per parameter, with r annotated. `row_var` adds a
# facet row (e.g. arm). `r_labs` optionally replaces the annotations (plotmath): a vector named
# by `param`, or a data frame with `param`, `label` (and the row_var column). `sd_col` adds
# +/- 1 sd bars (unclamped).
plot_recovery_scatter <- function(recov_df,
                                  param_labs = NULL,
                                  pal = NULL,
                                  font_family = "Open Sans",
                                  font_size = 14,
                                  point_alpha = 0.3,
                                  r_labs = NULL,
                                  row_var = NULL,
                                  sd_col = NULL,
                                  parse_labs = FALSE,
                                  x_lab = "true parameter value",
                                  y_lab = "recovered posterior mean",
                                  n_row = 1) {
  pl <- recovery_param_levels(recov_df, param_labs)
  if (is.null(pal)) pal <- MetBrewer::met.brewer("Hiroshige", length(pl$lev))
  pal <- stats::setNames(unname(pal)[seq_along(pl$lev)], pl$labs)

  to_plt <- recov_df |>
    dplyr::mutate(param = factor(as.character(param), levels = pl$lev, labels = pl$labs))

  r_df <- to_plt |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("param", row_var)))) |>
    dplyr::summarise(r = stats::cor(true, recovered, use = "pairwise.complete.obs"), .groups = "drop") |>
    dplyr::mutate(label = paste0("italic(r)~'='~", sprintf("%.2f", r)))
  if (!is.null(r_labs)) {
    lab_df <- if (is.data.frame(r_labs)) {
      r_labs
    } else {
      tibble::tibble(param = names(r_labs), label = unname(r_labs))
    }
    lab_df <- lab_df |>
      dplyr::mutate(param = factor(as.character(param), levels = pl$lev, labels = pl$labs))
    # drop the computed label FIRST, so the shared-key intersect below cannot pick it up;
    # joining on the remaining shared keys gives the vector form its recycling across rows
    r_df <- dplyr::select(r_df, -label)
    r_df <- dplyr::left_join(r_df, lab_df, by = intersect(names(r_df), names(lab_df)))
    # a mistyped param or row value would otherwise drop that facet's annotation silently
    stopifnot(!anyNA(r_df$label))
  }

  # parameter strips carry plotmath (e.g. alpha[neg]^i); any row_var strip stays literal
  lab_fn <- if (parse_labs) ggplot2::label_parsed else ggplot2::label_value

  to_plt |>
    ggplot2::ggplot(ggplot2::aes(x = true, y = recovered, colour = param, fill = param)) +
    # identity line, not in the STAND original: with free per-facet scales the lm line
    # alone cannot show whether recovery is unbiased or merely monotonic
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    (if (!is.null(sd_col)) {
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = recovered - .data[[sd_col]], ymax = recovered + .data[[sd_col]]),
        alpha = point_alpha / 2, linewidth = font_size / 56
      )
    }) +
    ggplot2::geom_point(alpha = point_alpha, size = font_size / 7) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1) +
    ggplot2::geom_text(
      data = r_df, ggplot2::aes(x = -Inf, y = Inf, label = label),
      hjust = -0.05, vjust = 1.1, inherit.aes = FALSE,
      size = font_size / 3.2, family = font_family, parse = TRUE
    ) +
    ggplot2::scale_y_continuous(n.breaks = 4) +
    ggplot2::scale_colour_manual(name = "", values = pal) +
    ggplot2::scale_fill_manual(name = "", values = pal) +
    ggplot2::guides(colour = "none", fill = "none") +
    (if (is.null(row_var)) {
      ggplot2::facet_wrap(
        ~ param, scales = "free", nrow = n_row,
        labeller = ggplot2::labeller(param = lab_fn, .default = ggplot2::label_value)
      )
    } else {
      # independent = "all": each panel gets its own scales (parameters differ in range)
      ggh4x::facet_grid2(
        rows = ggplot2::vars(.data[[row_var]]), cols = ggplot2::vars(param),
        scales = "free", independent = "all", axes = "all",
        labeller = ggplot2::labeller(param = lab_fn, .default = ggplot2::label_value),
        strip = row_strip_themed(to_plt[[row_var]], font_size)
      )
    }) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    cowplot::theme_minimal_grid(font_family = font_family, font_size = font_size) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(size = font_size * 1.05)
    )
}

# Between-parameter correlations, true (upper triangle) vs recovered (lower). Reads as a
# trade-off check: a correlation that appears only in the recovered half is one the data
# cannot separate, whatever the generating parameters did.
plot_recovery_heatmap <- function(recov_df,
                                  param_labs = NULL,
                                  font_family = "Open Sans",
                                  font_size = 14,
                                  true_col = "#1b9e77",
                                  rec_col = "#d95f02",
                                  parse_labs = FALSE,
                                  title = NULL) {
  pl <- recovery_param_levels(recov_df, param_labs)

  cor_df <- function(value_col, source) {
    mat <- recov_df |>
      dplyr::select(id, param, dplyr::all_of(value_col)) |>
      tidyr::pivot_wider(names_from = param, values_from = dplyr::all_of(value_col)) |>
      dplyr::select(dplyr::all_of(pl$lev)) |>
      stats::cor(use = "pairwise.complete.obs")
    as.data.frame(as.table(mat)) |>
      dplyr::rename(param_x = Var1, param_y = Var2, correlation = Freq) |>
      dplyr::mutate(
        source = source,
        param_x = factor(as.character(param_x), levels = pl$lev, labels = pl$labs),
        param_y = factor(as.character(param_y), levels = pl$lev, labels = pl$labs)
      )
  }

  tri_df <- dplyr::bind_rows(cor_df("true", "true"), cor_df("recovered", "recovered")) |>
    dplyr::mutate(x_idx = as.integer(param_x), y_idx = as.integer(param_y)) |>
    dplyr::filter(
      (source == "true" & y_idx > x_idx) | (source == "recovered" & y_idx < x_idx)
    )

  n_p <- length(pl$lev)
  label_df <- tibble::tibble(
    source = c("true", "recovered"),
    x = c(pl$labs[1], pl$labs[n_p]),
    y = c(pl$labs[n_p], pl$labs[1]),
    nudge_x = c(-0.3, 0.15),
    nudge_y = c(0.7, -0.7)
  )

  tri_df |>
    ggplot2::ggplot(ggplot2::aes(x = param_x, y = param_y, fill = correlation)) +
    ggplot2::geom_tile(ggplot2::aes(colour = source), linewidth = 1) +
    ggplot2::geom_segment(
      data = tibble::tibble(x = 0.5, y = 0.5, xend = n_p + 0.5, yend = n_p + 0.5),
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE, colour = "white", linewidth = 3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", correlation)),
      size = font_size / 3.6, family = font_family, colour = "black"
    ) +
    ggplot2::geom_text(
      data = label_df, ggplot2::aes(x = x, y = y, label = source, colour = source),
      inherit.aes = FALSE, size = font_size / 3.2, fontface = "bold", family = font_family,
      nudge_x = label_df$nudge_x, nudge_y = label_df$nudge_y
    ) +
    # axis text is parsed, not the tile labels: the corner "true"/"recovered" markers position
    # by factor level, so they are unaffected by how the axis renders
    ggplot2::scale_x_discrete(
      labels = if (parse_labs) function(x) parse(text = x) else ggplot2::waiver()
    ) +
    ggplot2::scale_y_discrete(
      labels = if (parse_labs) function(x) parse(text = x) else ggplot2::waiver()
    ) +
    ggplot2::scale_fill_gradient2(
      limits = c(-1, 1), low = "#2e294e", mid = "white", high = "#e71d36",
      midpoint = 0, name = "r"
    ) +
    ggplot2::scale_colour_manual(values = c(true = true_col, recovered = rec_col), guide = "none") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    cowplot::theme_minimal_grid(font_family = font_family, font_size = font_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      axis.ticks.x = ggplot2::element_line(colour = "white"),
      axis.ticks.y = ggplot2::element_line(colour = "white"),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

# model-recovery confusion matrix. `conf_df` needs `gen`, `fitted`, `n` for every cell. not
# currently plotted (the default run generates from M2 only)
plot_model_confusion <- function(conf_df,
                                 model_labs = NULL,
                                 font_family = "Open Sans",
                                 font_size = 14,
                                 fill_col = "#9b332b",
                                 diag_col = "#1b9e77",
                                 title = NULL) {
  lev <- if (!is.null(model_labs)) names(model_labs) else sort(unique(as.character(conf_df$gen)))
  labs_v <- if (!is.null(model_labs)) unname(model_labs) else lev

  to_plt <- conf_df |>
    dplyr::group_by(gen) |>
    # a generating model with no datasets was not run; drop it rather than drawing a row
    # of 0/0 tiles, which reads as "never selected" instead of "never simulated"
    dplyr::filter(sum(n) > 0) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      gen = factor(as.character(gen), levels = rev(lev), labels = rev(labs_v)),
      fitted = factor(as.character(fitted), levels = lev, labels = labs_v),
      correct = as.character(gen) == as.character(fitted)
    ) |>
    dplyr::mutate(gen = droplevels(gen))

  to_plt |>
    ggplot2::ggplot(ggplot2::aes(x = fitted, y = gen)) +
    ggplot2::geom_tile(ggplot2::aes(fill = prop), colour = "white", linewidth = 1) +
    # the diagonal is outlined after the fill so a correct recovery is legible even when
    # the proportion is low
    ggplot2::geom_tile(
      data = ~ dplyr::filter(.x, correct), fill = NA, colour = diag_col, linewidth = 1.4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = n, colour = prop > 0.6),
      size = font_size / 3.2, family = font_family, fontface = "bold"
    ) +
    ggplot2::scale_fill_gradient(
      low = "white", high = fill_col, limits = c(0, 1), name = "proportion\nselected"
    ) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"), guide = "none") +
    ggplot2::labs(title = title, x = "selected by LOO", y = "generating model") +
    cowplot::theme_minimal_grid(font_family = font_family, font_size = font_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
}

# group-level recovery: posterior mean +/- 95% hdi vs. the generating value. `group_df` needs
# `variable`, `true`, `mean`, `hdi_low`, `hdi_high`. one facet per variable by default, or a
# row_var x col_var grid (supply both as ordered factors)
plot_recovery_group <- function(group_df,
                                param_labs = NULL,
                                font_family = "Open Sans",
                                font_size = 14,
                                point_col = "slateblue",
                                n_row = 1,
                                row_var = NULL,
                                col_var = NULL,
                                parse_labs = FALSE,
                                x_lab = "true (generating) parameter value",
                                y_lab = "recovered posterior mean (95% HDI)") {
  stopifnot(is.null(row_var) == is.null(col_var))
  grid_mode <- !is.null(row_var)
  lev <- if (!is.null(param_labs)) names(param_labs) else unique(as.character(group_df$variable))
  labs_v <- if (!is.null(param_labs)) unname(param_labs) else lev

  to_plt <- group_df |>
    dplyr::mutate(covered = true >= hdi_low & true <= hdi_high)
  if (!grid_mode) {
    to_plt <- to_plt |>
      dplyr::filter(variable %in% lev) |>
      dplyr::group_by(variable) |>
      dplyr::mutate(
        strip = sprintf("%s\n%d/%d covered", labs_v[match(dplyr::first(variable), lev)],
                        sum(covered), dplyr::n())
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(strip = factor(strip, levels = unique(strip[order(match(variable, lev))])))
  }

  # coverage is NOT colour-coded by count: at 10 replicates 9/10 vs 10/10 means nothing
  # (see learning_recovery.R's header), so it is reported as plain text
  cov_df <- if (grid_mode) {
    to_plt |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(row_var, col_var)))) |>
      dplyr::summarise(label = sprintf("%d/%d covered", sum(covered), dplyr::n()), .groups = "drop")
  } else {
    NULL
  }
  lab_fn <- if (parse_labs) ggplot2::label_parsed else ggplot2::label_value

  to_plt |>
    ggplot2::ggplot(ggplot2::aes(x = true, y = mean)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = hdi_low, ymax = hdi_high, colour = covered),
      linewidth = font_size / 20
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = covered), size = font_size / 7) +
    # replicates whose HDI misses the generating value are picked out rather than left to
    # be counted off the strip
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = point_col, `FALSE` = "#b2182b"), guide = "none"
    ) +
    (if (grid_mode) {
      # top-left is empty in every panel: points run bottom-left to top-right on the identity
      ggplot2::geom_text(
        data = cov_df, ggplot2::aes(x = -Inf, y = Inf, label = label),
        hjust = -0.05, vjust = 1.1, inherit.aes = FALSE,
        size = font_size / 3.6, family = font_family
      )
    }) +
    (if (grid_mode) {
      # independent scales per panel, as in plot_recovery_scatter()
      ggh4x::facet_grid2(
        rows = ggplot2::vars(.data[[row_var]]), cols = ggplot2::vars(.data[[col_var]]),
        scales = "free", independent = "all", axes = "all",
        labeller = do.call(
          ggplot2::labeller,
          c(stats::setNames(list(lab_fn), col_var), list(.default = ggplot2::label_value))
        ),
        strip = row_strip_themed(to_plt[[row_var]], font_size)
      )
    } else {
      ggplot2::facet_wrap(~ strip, scales = "free", nrow = n_row)
    }) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    cowplot::theme_minimal_grid(font_family = font_family, font_size = font_size) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = font_size * 0.95, lineheight = 1.05)
    )
}

# Individual-level diagnostics ================================================

# Index (within an ordered 0/1 vector) at which the first run of `streak`
# consecutive correct (== 1) completes; NA if it never happens.
first_streak_trial <- function(correct, streak) {
  s <- 0L
  for (i in seq_along(correct)) {
    s <- if (isTRUE(as.numeric(correct[i]) == 1)) s + 1L else 0L
    if (s >= streak) return(i)
  }
  NA_integer_
}

# Per (learn_id, valence) "rule-learnt" classification: did the participant hit a
# run of `streak` consecutive correct, and on which within-valence trial. Returns a
# tidy table (learn_id, group, valence, n_trials, learnt_trial, learnt). Doubles as
# the censoring rule for a later "while-learning" model (drop trials > learnt_trial).
classify_learnt <- function(raw_df, group_lookup, streak = 5) {
  raw_df |>
    tidyr::drop_na(chosen_attr_type, valence) |>
    dplyr::arrange(learn_id, sessionNo, blockNo, trialNo) |>
    dplyr::inner_join(group_lookup, by = "learn_id") |>
    dplyr::group_by(learn_id, group, valence) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      learnt_trial = first_streak_trial(correct, streak),
      learnt = !is.na(learnt_trial),
      .groups = "drop"
    )
}

# Per-participant observed-vs-predicted fit, individual learning rate, pointwise
# LOO, and learnt flag, for a learning-ALONE fit (correct_pred / alpha_mu indexed
# by learn_id). `gq_draws` = fit$draws(format="df", variables="correct_pred");
# `loo_obj` = fit$loo(variables="log_lik") (optional). Returns list(by_ppt, by_valence).
make_indiv_fit_table <- function(gq_draws, fit, raw_df, group_lookup,
                                 loo_obj = NULL, streak = 5) {
  # posterior-mean predicted accuracy per (learn_id, trial); only needs correct_pred
  cp_cols <- grep("^correct_pred\\[", names(gq_draws), value = TRUE)
  if (length(cp_cols) == 0) stop("gq_draws must contain 'correct_pred[' columns")
  cp_means <- colMeans(gq_draws[, cp_cols, drop = FALSE], na.rm = TRUE)
  cp_idx <- stringr::str_match(cp_cols, "\\[(\\d+),(\\d+)\\]")
  preds <- tibble::tibble(
    learn_id = as.integer(cp_idx[, 2]),
    trial_no_ovl = as.integer(cp_idx[, 3]),
    correct_pred = as.numeric(cp_means)
  )

  raw_ordered <- raw_df |>
    tidyr::drop_na(chosen_attr_type, valence) |>
    dplyr::arrange(learn_id, sessionNo, blockNo, trialNo) |>
    dplyr::group_by(learn_id) |>
    dplyr::mutate(trial_no_ovl = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::inner_join(group_lookup, by = "learn_id") |>
    dplyr::select(learn_id, group, trial_no_ovl, valence, correct)

  joined <- raw_ordered |>
    dplyr::inner_join(
      preds |> dplyr::select(learn_id, trial_no_ovl, correct_pred),
      by = c("learn_id", "trial_no_ovl")
    ) |>
    dplyr::mutate(correct = as.numeric(correct))

  by_valence <- joined |>
    dplyr::group_by(learn_id, group, valence) |>
    dplyr::summarise(
      n = dplyr::n(), obs_acc = mean(correct), pred_acc = mean(correct_pred),
      .groups = "drop"
    )
  by_ppt <- joined |>
    dplyr::group_by(learn_id, group) |>
    dplyr::summarise(
      n = dplyr::n(), obs_acc = mean(correct), pred_acc = mean(correct_pred),
      .groups = "drop"
    )

  # individual learning rates
  stan_vars <- fit$metadata()$stan_variables
  parse_id <- function(v) as.integer(stringr::str_match(v, "\\[(\\d+)\\]")[, 2])
  alpha_mu <- fit$summary("alpha_mu", "mean") |>
    dplyr::transmute(learn_id = parse_id(variable), alpha_mu = mean)
  by_ppt <- dplyr::left_join(by_ppt, alpha_mu, by = "learn_id")

  if (all(c("alpha_neg", "alpha_pos") %in% stan_vars)) {
    alpha_val <- dplyr::bind_rows(
      fit$summary("alpha_neg", "mean") |>
        dplyr::transmute(learn_id = parse_id(variable), valence = "negative", alpha = mean),
      fit$summary("alpha_pos", "mean") |>
        dplyr::transmute(learn_id = parse_id(variable), valence = "positive", alpha = mean)
    )
    by_valence <- dplyr::left_join(by_valence, alpha_val, by = c("learn_id", "valence"))
  }

  # pointwise LOO (log_lik is per-participant -> per-participant elpd_loo)
  if (!is.null(loo_obj)) {
    elpd <- tibble::tibble(
      learn_id = seq_len(nrow(loo_obj$pointwise)),
      elpd_loo = loo_obj$pointwise[, "elpd_loo"]
    )
    by_ppt <- dplyr::left_join(by_ppt, elpd, by = "learn_id")
  }

  # learnt classification
  learnt <- classify_learnt(raw_df, group_lookup, streak = streak)
  by_valence <- dplyr::left_join(
    by_valence, learnt |> dplyr::select(learn_id, valence, learnt, learnt_trial),
    by = c("learn_id", "valence")
  )
  by_ppt <- dplyr::left_join(
    by_ppt,
    learnt |>
      dplyr::group_by(learn_id) |>
      dplyr::summarise(learnt_any = any(learnt), .groups = "drop"),
    by = "learn_id"
  )

  list(by_ppt = by_ppt, by_valence = by_valence)
}

## ================================================================================
## plotting layer ported from STAND-study/model_fns.R (same task design). flip_order: FALSE =
## arm of interest (causal) on top, TRUE = reference arm on top
## ================================================================================

# Group-level absolute-levels panel: pre (open circle) -> post (filled), thick 95% /
# thin 99% HDI linerange, one row per group. `pop_df` needs columns: group (factor),
# tp ("pre"/"post"), m, lo95, hi95, lo99, hi99, ybase.
plot_group_levels <- function(pop_df,
                              xlim = NULL,
                              x_lab,
                              grp_cols,
                              suppress_labels = FALSE,
                              flip_order = FALSE,
                              fnt_sz = 1,
                              subtitle = "individual-level estimates\n○ pre- ● post-intervention") {
  if (flip_order) {
    nudge_sign   <- pop_df$ybase - as.numeric(pop_df$group)
    pop_df$group <- factor(pop_df$group, levels = rev(levels(pop_df$group)))
    pop_df$ybase <- as.numeric(pop_df$group) + nudge_sign
  }
  grp_levels <- levels(pop_df$group)
  pre  <- pop_df[pop_df$tp == "pre",  ]
  post <- pop_df[pop_df$tp == "post", ]
  plt <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = pop_df,
      ggplot2::aes(x = m, y = ybase, group = group, colour = group),
      linewidth = 0.7, alpha = 0.5, linetype = "dotted"
    ) +
    ggplot2::geom_linerange(
      data = pop_df,
      ggplot2::aes(xmin = lo99, xmax = hi99, y = ybase, colour = group),
      linewidth = 0.9
    ) +
    ggplot2::geom_linerange(
      data = pop_df,
      ggplot2::aes(xmin = lo95, xmax = hi95, y = ybase, colour = group),
      linewidth = 2.0
    ) +
    ggplot2::geom_point(
      data = pre,  ggplot2::aes(x = m, y = ybase, colour = group),
      shape = 21, fill = "white", size = 3.4, stroke = 1.2
    ) +
    ggplot2::geom_point(
      data = post, ggplot2::aes(x = m, y = ybase, colour = group, fill = group),
      shape = 21, size = 3.4, stroke = 1.2
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(grp_levels), labels = gsub(" ", "\n", grp_levels),
      limits = c(0.6, length(grp_levels) + 0.4)
    ) +
    ggplot2::scale_colour_manual(values = grp_cols, guide = "none") +
    ggplot2::scale_fill_manual(values = grp_cols, guide = "none") +
    ggplot2::labs(x = x_lab, y = NULL, subtitle = subtitle) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 13 * fnt_sz) +
    cowplot::background_grid(major = "x", minor = "none") +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 12 * fnt_sz, colour = "grey30"),
      axis.text.y   = ggplot2::element_text(size = 10 * fnt_sz)
    )
  if (!is.null(xlim)) plt <- plt + ggplot2::coord_cartesian(xlim = xlim)
  if (suppress_labels) {
    plt <- plt + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }
  plt
}

# Group-level change panel: three rows (difference, control, causal) as slab+interval
# (95%/99% HDI). `chg_df` needs columns: level (factor, levels "difference" <
# ARM_REF < ARM_INT), value.
plot_change_slabs <- function(chg_df,
                              xlim = NULL,
                              x_lab,
                              grp_cols,
                              grp_shapes,
                              diff_comp = DIFF_LAB,
                              flip_order = FALSE,
                              suppress_labels = FALSE,
                              interval_size_range = c(1, 3),
                              fatten_point = 2,
                              x_n_breaks = NULL,
                              fnt_sz = 1,
                              subtitle = "group-level mean pre → post change") {
  if (flip_order) {
    lvls  <- levels(chg_df$level)
    other <- rev(lvls[lvls != "difference"])
    chg_df$level <- factor(chg_df$level, levels = c("difference", other))
  }
  plt <- chg_df |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = level, fill = level, colour = level, shape = level)
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
    ggplot2::geom_hline(
      yintercept = 1.5, linetype = "dotted", colour = "grey70",
      linewidth = max(1, 2 * interval_size_range[2] / 3)
    ) +
    ggdist::stat_pointinterval(
      .width = c(0.95, 0.99), point_interval = "mean_hdci",
      interval_size_range = interval_size_range, fatten_point = fatten_point
    ) +
    ggplot2::scale_colour_manual(values = grp_cols, guide = "none") +
    ggplot2::scale_fill_manual(values = grp_cols, guide = "none") +
    ggplot2::scale_shape_manual(values = grp_shapes, guide = "none") +
    ggplot2::scale_y_discrete(labels = c(
      "difference" = diff_comp,
      "control"    = ARM_BRK[["control"]],
      "causal"     = ARM_BRK[["causal"]]
    )) +
    ggplot2::labs(x = x_lab, y = NULL, subtitle = subtitle) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 14 * fnt_sz) +
    cowplot::background_grid(major = "x", minor = "none") +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 12 * fnt_sz, colour = "grey30"),
      axis.text.y   = ggplot2::element_text(size = 10 * fnt_sz),
      # markdown (same size as before): callers subscript units, e.g. "Δ DAQ<sub>std</sub>"
      axis.title.x   = ggtext::element_markdown(size = 12 * fnt_sz)
    )
  # only touched when asked: at larger `fnt_sz` the default ~5 breaks run into each other
  if (!is.null(x_n_breaks)) plt <- plt + ggplot2::scale_x_continuous(n.breaks = x_n_breaks)
  if (!is.null(xlim)) plt <- plt + ggplot2::coord_cartesian(xlim = xlim)
  if (suppress_labels) {
    plt <- plt + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }
  plt
}

# Forest of regression slopes, one row per outcome with the arms (and their difference)
# dodged within the row. Styled to match plot_change_slabs() so slope and change panels read
# as one family. `slopes_df` needs columns: outcome (factor, rows top-to-bottom), level
# (factor, "difference" < ARM_REF < ARM_INT), and either draws-per-row or the summary columns
# slope / lo95 / hi95 / lo99 / hi99.
plot_slope_forest <- function(slopes_df,
                              x_lab,
                              subtitle = NULL,
                              grp_cols = ARM_COLS,
                              grp_shapes = ARM_SHP,
                              diff_comp = DIFF_LAB,
                              dodge_width = 0.78,
                              legend_rows = 1,
                              flip_order = FALSE,
                              fnt_sz = 1) {
  # same convention as plot_change_slabs(): `flip_order = TRUE` makes the dodged levels
  # READ top-down as control, causal, difference
  if (flip_order) {
    lvls  <- levels(slopes_df$level)
    other <- rev(lvls[lvls != "difference"])
    slopes_df$level <- factor(slopes_df$level, levels = c("difference", other))
  }
  pd <- ggplot2::position_dodge(width = dodge_width)
  plt <- slopes_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = slope, y = outcome, colour = level, shape = level, group = level
    )) +
    # faint rules between outcome rows: with three dodged levels and intervals this wide,
    # neighbouring rows otherwise run into each other
    ggplot2::geom_hline(
      yintercept = seq_len(dplyr::n_distinct(slopes_df$outcome) - 1) + 0.5,
      colour = "grey88", linewidth = 0.5
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lo99, xmax = hi99), position = pd, linewidth = 0.5
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lo95, xmax = hi95), position = pd, linewidth = 1.8
    ) +
    ggplot2::geom_point(position = pd, size = 2.2, fill = "white") +
    ggplot2::scale_colour_manual(
      values = grp_cols,
      breaks = c(ARM_REF, ARM_INT, "difference"),
      labels = c(ARM_LABS[[ARM_REF]], ARM_LABS[[ARM_INT]], gsub("\n", " ", diff_comp)),
      name = NULL
    ) +
    # byrow so a wrapped legend still reads control, causal, difference left-to-right
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = legend_rows, byrow = TRUE)) +
    ggplot2::scale_shape_manual(values = grp_shapes, guide = "none") +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = x_lab, y = NULL, subtitle = subtitle) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 14 * fnt_sz) +
    cowplot::background_grid(major = "x", minor = "none") +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 11 * fnt_sz, colour = "grey30"),
      axis.text.y = ggplot2::element_text(size = 11 * fnt_sz),
      axis.title.x = ggplot2::element_text(size = 12 * fnt_sz),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 11 * fnt_sz)
    )
  plt
}

# Raw scatter: per-participant posterior-mean delta-parameter vs. observed
# delta-symptom, coloured by group with per-group lm fits.
plot_indiv_change_scatter <- function(df,
                                      param_nm,
                                      symptom_lab = "Δ symptoms<sub>std</sub>",
                                      xlim = NULL,
                                      ylim = NULL,
                                      x_n_breaks = 4,
                                      suppress_labels = FALSE,
                                      flip_order = FALSE,
                                      grp_cols = ARM_COLS,
                                      fnt_sz = 1) {
  plt <- df |>
    ggplot2::ggplot(
      ggplot2::aes(x = delta_par, y = delta_symptom, colour = group, shape = group, fill = group)
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_point(ggplot2::aes(size = 1 / delta_par_sd), alpha = 0.3) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15, linewidth = 1.2) +
    ggplot2::scale_colour_manual(values = grp_cols, guide = "none") +
    ggplot2::scale_fill_manual(values = grp_cols, guide = "none") +
    ggplot2::scale_shape_manual(values = ARM_SHP[c("control", "causal")], guide = "none") +
    ggplot2::scale_size_continuous(range = c(1, 3)) +
    # check.overlap drops colliding tick labels (narrow panels)
    ggplot2::scale_x_continuous(n.breaks = x_n_breaks, guide = ggplot2::guide_axis(check.overlap = TRUE)) +
    ggplot2::guides(size = "none") +
    ggplot2::labs(x = paste0("Δ ", param_nm), y = symptom_lab) +
    cowplot::theme_minimal_hgrid(font_family = "Open Sans", font_size = 14 * fnt_sz) +
    # `symptom_lab` carries a subscripted unit. set here, before the suppress_labels block
    # below, which must still win when it blanks the title
    ggplot2::theme(axis.title.y = ggtext::element_markdown())
  if (!is.null(xlim) || !is.null(ylim)) {
    plt <- plt + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }
  if (suppress_labels) {
    plt <- plt + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank()
    )
  }
  plt
}

# Small inset interval plot for one mediation path (causal on top, control below by
# default -- flip_order = TRUE flips it), used by plot_joint_mediation().
mini_interval_plot <- function(control, causal, title, grp_cols, fnt_sz = 1, flip_order = FALSE,
                               x_n_breaks = 3) {
  d <- dplyr::bind_rows(
    data.frame(level = "control", value = control),
    data.frame(level = "causal",  value = causal)
  )
  lvls <- if (flip_order) c("causal", "control") else c("control", "causal")
  grp_shapes <- if (flip_order) c(17, 15) else c(15, 17)
  d$level <- factor(d$level, levels = lvls)
  ggplot2::ggplot(d, ggplot2::aes(x = value, y = level, colour = level, shape = level)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
    ggdist::stat_pointinterval(
      .width = c(0.95, 0.99), point_interval = "mean_hdci", interval_size_range = c(0.75, 2)
    ) +
    ggplot2::scale_colour_manual(values = grp_cols, guide = "none") +
    ggplot2::scale_shape_manual(values = grp_shapes, guide = "none") +
    # small inset panels crowd easily with the default break count -- keep it low
    ggplot2::scale_x_continuous(n.breaks = x_n_breaks) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 12 * fnt_sz) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = 11 * fnt_sz, face = "italic", colour = "slategray", hjust = 0.5
      )
    )
}

# Combined indirect-effect panel for a two-mediator joint model: two dodged
# pointranges per row (mediator 1 above, mediator 2 below, mirroring the path
# diagram). Reads indirect_{suffix}_{control,causal} + index_mod_med_{suffix}.
plot_joint_indirect <- function(draws_med,
                                mediator1_suffix, mediator2_suffix,
                                mediator_cols,
                                diff_lab = DIFF_LAB,
                                flip_order = FALSE,
                                fnt_sz = 1) {
  if (!all(c(mediator1_suffix, mediator2_suffix) %in% names(mediator_cols))) {
    stop(
      "plot_joint_indirect(): `mediator_cols` must be named with `mediator1_suffix`/",
      "`mediator2_suffix` (\"", mediator1_suffix, "\"/\"", mediator2_suffix, "\") -- got names: ",
      paste(names(mediator_cols), collapse = ", ")
    )
  }
  row_vals <- function(mediator, level, value) data.frame(mediator = mediator, level = level, value = value)
  ind_df <- dplyr::bind_rows(
    row_vals(mediator1_suffix, "control", draws_med[[paste0("indirect_", mediator1_suffix, "_control")]]),
    row_vals(mediator1_suffix, "causal",  draws_med[[paste0("indirect_", mediator1_suffix, "_causal")]]),
    row_vals(mediator1_suffix, "difference", draws_med[[paste0("index_mod_med_", mediator1_suffix)]]),
    row_vals(mediator2_suffix, "control", draws_med[[paste0("indirect_", mediator2_suffix, "_control")]]),
    row_vals(mediator2_suffix, "causal",  draws_med[[paste0("indirect_", mediator2_suffix, "_causal")]]),
    row_vals(mediator2_suffix, "difference", draws_med[[paste0("index_mod_med_", mediator2_suffix)]])
  )
  other <- if (flip_order) c("causal", "control") else c("control", "causal")
  ind_df$level <- factor(ind_df$level, levels = c("difference", other))
  nudge <- 0.16
  ind_df$ybase <- as.numeric(ind_df$level) + ifelse(ind_df$mediator == mediator1_suffix, nudge, -nudge)

  ind_df |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = ybase, colour = mediator, group = interaction(mediator, level))
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 1.5, linetype = "dotted", colour = "grey70", linewidth = 2) +
    ggdist::stat_pointinterval(
      .width = c(0.95, 0.99), point_interval = "mean_hdci", interval_size_range = c(1, 3), fatten_point = 2
    ) +
    ggplot2::scale_colour_manual(values = mediator_cols, guide = "none") +
    ggplot2::scale_y_continuous(
      breaks = 1:3,
      labels = c(diff_lab, gsub(" ", "\n", other)),
      limits = c(0.6, 3.4)
    ) +
    ggplot2::labs(
      x = "indirect effect (a × b)",
      y = NULL
    ) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 16 * fnt_sz) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 12 * fnt_sz),
      axis.text.y = ggplot2::element_text(size = 10 * fnt_sz)
    )
}

# N-mediator generalisation of plot_joint_indirect(): one dodged pointrange per
# mediator within each group row, mediators ordered top->bottom by `suffixes`.
# Unlike the 2-mediator version, mediator colours are shown as a small legend.
plot_joint_indirect_n <- function(draws_med,
                                  suffixes,
                                  mediator_nms,
                                  mediator_cols,
                                  diff_lab = DIFF_LAB,
                                  flip_order = FALSE,
                                  legend = TRUE,
                                  fnt_sz = 1) {
  if (!all(suffixes %in% names(mediator_cols))) {
    stop(
      "plot_joint_indirect_n(): `mediator_cols` must be named with every entry of ",
      "`suffixes` (", paste(suffixes, collapse = ", "), ") -- got names: ",
      paste(names(mediator_cols), collapse = ", ")
    )
  }
  n_med <- length(suffixes)
  row_vals <- function(suffix, level, value) data.frame(suffix = suffix, level = level, value = value)
  ind_df <- dplyr::bind_rows(lapply(suffixes, function(sfx) {
    dplyr::bind_rows(
      row_vals(sfx, "control", draws_med[[paste0("indirect_", sfx, "_control")]]),
      row_vals(sfx, "causal",  draws_med[[paste0("indirect_", sfx, "_causal")]]),
      row_vals(sfx, "difference", draws_med[[paste0("index_mod_med_", sfx)]])
    )
  }))
  other <- if (flip_order) c("causal", "control") else c("control", "causal")
  ind_df$level <- factor(ind_df$level, levels = c("difference", other))
  step <- min(0.22, 0.66 / n_med)
  offs <- ((n_med + 1) / 2 - seq_len(n_med)) * step
  names(offs) <- suffixes
  ind_df$ybase <- as.numeric(ind_df$level) + offs[ind_df$suffix]
  ind_df$mediator <- factor(ind_df$suffix, levels = suffixes, labels = mediator_nms)
  leg_cols <- stats::setNames(unname(mediator_cols[suffixes]), mediator_nms)

  ind_df |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = ybase, colour = mediator, group = interaction(mediator, level))
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 1.5, linetype = "dotted", colour = "grey70", linewidth = 2) +
    ggdist::stat_pointinterval(
      .width = c(0.95, 0.99), point_interval = "mean_hdci", interval_size_range = c(1, 3), fatten_point = 2
    ) +
    ggplot2::scale_colour_manual(
      values = leg_cols, name = NULL, guide = if (legend) "legend" else "none"
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1:3, labels = c(diff_lab, gsub(" ", "\n", other)), limits = c(0.5, 3.5)
    ) +
    ggplot2::labs(x = "indirect effect (a × b)", y = NULL) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 16 * fnt_sz) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 12 * fnt_sz),
      axis.text.y = ggplot2::element_text(size = 10 * fnt_sz),
      legend.position = if (legend) "bottom" else "none",
      legend.text = ggplot2::element_text(size = 11 * fnt_sz),
      legend.justification = "center"
    )
}

# a / b / indirect paths per mediator (rows = arms + difference, colour = mediator).
#   draws: a draws df, or a named list of them (names -> facet strips, shared x-scale).
#   path: "a" / "b" / "indirect" -- selects {path}_{suffix}_{control,causal}.
# the difference row is causal - control (index_mod_med_{suffix} for "indirect")
plot_paths_n <- function(draws,
                         path = c("indirect", "a", "b"),
                         suffixes,
                         mediator_nms = NULL,
                         mediator_cols,
                         x_lab = NULL,
                         diff_lab = DIFF_LAB,
                         flip_order = FALSE,
                         legend = TRUE,
                         interval_size_range = c(0.5, 1.5),
                         fatten_point = 1.8,
                         legend_point_size = fatten_point * 1.5,
                         outcome_cols = NULL,
                         strip_family = "Open Sans SemiBold",
                         fnt_sz = 1) {
  path <- match.arg(path)
  if (!all(suffixes %in% names(mediator_cols))) {
    stop(
      "plot_paths_n(): `mediator_cols` must be named with every entry of `suffixes` (",
      paste(suffixes, collapse = ", "), ") -- got names: ", paste(names(mediator_cols), collapse = ", ")
    )
  }
  # `mediator_nms` must be named by suffix, never positional
  if (is.null(mediator_nms)) mediator_nms <- stats::setNames(gsub("_", "-", suffixes), suffixes)
  if (is.null(names(mediator_nms)) || !all(suffixes %in% names(mediator_nms))) {
    stop("plot_paths_n(): `mediator_nms` must be a vector named by suffix, covering every entry of `suffixes`")
  }

  draws_ls <- if (is.data.frame(draws)) list(draws) else draws
  faceted <- length(draws_ls) > 1
  if (faceted && is.null(names(draws_ls))) {
    stop("plot_paths_n(): a list of >1 draws frames must be named (the names label the facets)")
  }
  outcome_keys <- if (is.null(names(draws_ls))) "" else names(draws_ls)

  need <- function(d, col) {
    if (!col %in% names(d)) stop("plot_paths_n(): draws are missing column '", col, "'")
    d[[col]]
  }
  path_df <- dplyr::bind_rows(lapply(seq_along(draws_ls), function(i) {
    d <- draws_ls[[i]]
    dplyr::bind_rows(lapply(suffixes, function(sfx) {
      ctl <- need(d, paste0(path, "_", sfx, "_control"))
      cas <- need(d, paste0(path, "_", sfx, "_causal"))
      mod_col <- paste0("index_mod_med_", sfx)
      dif <- if (path == "indirect" && mod_col %in% names(d)) d[[mod_col]] else cas - ctl
      data.frame(
        outcome = outcome_keys[i], suffix = sfx,
        level = rep(c("control", "causal", "difference"), each = length(ctl)),
        value = c(ctl, cas, dif)
      )
    }))
  }))

  n_med <- length(suffixes)
  # flip_order = TRUE reads top-down as control, causal, difference (as in other figures)
  other <- if (flip_order) c("causal", "control") else c("control", "causal")
  path_df$level <- factor(path_df$level, levels = c("difference", other))
  step <- min(0.22, 0.66 / n_med)
  offs <- stats::setNames(((n_med + 1) / 2 - seq_len(n_med)) * step, suffixes)
  path_df$ybase <- as.numeric(path_df$level) + offs[path_df$suffix]
  path_df$mediator <- factor(path_df$suffix, levels = suffixes, labels = unname(mediator_nms[suffixes]))
  path_df$outcome <- factor(path_df$outcome, levels = outcome_keys)
  leg_cols <- stats::setNames(unname(mediator_cols[suffixes]), unname(mediator_nms[suffixes]))

  if (is.null(x_lab)) {
    x_lab <- switch(path,
      a = "a path (learning group → Δ attribution)",
      b = "b path (Δ attribution → Δ symptoms)",
      indirect = "indirect effect (a × b)"
    )
  }

  plt <- path_df |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = ybase, colour = mediator, group = interaction(mediator, level))
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 1.5, linetype = "dotted", colour = "grey70", linewidth = 1.1) +
    ggdist::stat_pointinterval(
      .width = c(0.95, 0.99), point_interval = "mean_hdci",
      interval_size_range = interval_size_range, fatten_point = fatten_point
    ) +
    ggplot2::scale_colour_manual(
      values = leg_cols, name = NULL, guide = if (legend) "legend" else "none"
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1:3, labels = c(diff_lab, gsub(" ", "\n", other)), limits = c(0.5, 3.5)
    ) +
    ggplot2::labs(x = x_lab, y = NULL) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 19 * fnt_sz) +
    ggplot2::theme(
      axis.title.x = ggtext::element_markdown(size = 16 * fnt_sz),
      axis.text.x = ggplot2::element_text(size = 16 * fnt_sz),
      axis.text.y = ggplot2::element_text(size = 16 * fnt_sz),
      legend.position = if (legend) "bottom" else "none",
      legend.text = ggplot2::element_text(size = 15 * fnt_sz),
      legend.background = ggplot2::element_rect(
        fill = "white", colour = "#cacaca", linewidth = 0.5
      ),
      legend.key.size = grid::unit(16 * fnt_sz, "pt"),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
      legend.justification = "center"
    )
  if (legend) {
    # legend keys follow the layer's geometry, so enlarge point_size / interval_size for them
    plt <- plt + ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          point_size = legend_point_size * fnt_sz,
          interval_size = legend_point_size * 1.5 * fnt_sz
        )
      )
    )
  }
  if (faceted) {
    # per-facet strip colour: `strip.text` is one element for the whole plot, so the colour
    # has to travel with the label. `outcome_cols` is keyed by the draws-list names.
    coloured <- !is.null(outcome_cols) && all(outcome_keys %in% names(outcome_cols))
    plt <- plt +
      ggplot2::facet_wrap(
        ~outcome, nrow = 1,
        labeller = if (coloured) {
          ggplot2::as_labeller(stats::setNames(
            sprintf("<span style='color:%s'>%s</span>", unname(outcome_cols[outcome_keys]), outcome_keys),
            outcome_keys
          ))
        } else {
          ggplot2::label_value
        }
      ) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text = if (coloured) {
          ggtext::element_markdown(family = strip_family, size = 18 * fnt_sz)
        } else {
          ggplot2::element_text(face = "bold", size = 18 * fnt_sz)
        }
      )
  }
  plt
}

# direct (c'), indirect and total-arm effects across mediator domains (rows = domains, colour =
# component). direct_arm / total_arm are single arm contrasts (ANCOVA outcome), and direct +
# indirect need not equal total_arm when baseline theta is arm-imbalanced.
#   draws: a draws df, or a named list of them (names -> facet strips).
#   suffixes/mediator_nms: mediator domains, as in plot_paths_n()
plot_direct_total_n <- function(draws,
                                suffixes,
                                mediator_nms = NULL,
                                component_cols = c(
                                  direct = "#e76254", indirect = "#ffd06f", total = "#376795"
                                ),
                                component_labs = c(
                                  direct = "direct (c′)", indirect = "indirect (a×b)", total = "total"
                                ),
                                x_lab = "effect (SD units)",
                                outcome_cols = NULL,
                                legend = TRUE,
                                interval_size_range = c(1, 2.5),
                                fatten_point = 1.8,
                                legend_point_size = fatten_point * 1.5,
                                strip_family = "Open Sans SemiBold",
                                fnt_sz = 1) {
  if (is.null(mediator_nms)) mediator_nms <- stats::setNames(gsub("_", "-", suffixes), suffixes)
  if (is.null(names(mediator_nms)) || !all(suffixes %in% names(mediator_nms))) {
    stop("plot_direct_total_n(): `mediator_nms` must be a vector named by suffix, covering every entry of `suffixes`")
  }

  draws_ls <- if (is.data.frame(draws)) list(draws) else draws
  faceted <- length(draws_ls) > 1
  if (faceted && is.null(names(draws_ls))) {
    stop("plot_direct_total_n(): a list of >1 draws frames must be named (the names label the facets)")
  }
  outcome_keys <- if (is.null(names(draws_ls))) "" else names(draws_ls)

  need <- function(d, col) {
    if (!col %in% names(d)) stop("plot_direct_total_n(): draws are missing column '", col, "'")
    d[[col]]
  }
  comp_levels <- c("total", "indirect", "direct")
  comp_df <- dplyr::bind_rows(lapply(seq_along(draws_ls), function(i) {
    d <- draws_ls[[i]]
    dplyr::bind_rows(lapply(suffixes, function(sfx) {
      dr  <- need(d, paste0("direct_arm_", sfx))
      ind <- need(d, paste0("indirect_", sfx, "_diff"))
      tot <- need(d, paste0("total_arm_", sfx))
      data.frame(
        outcome = outcome_keys[i], suffix = sfx,
        component = rep(comp_levels, each = length(dr)),
        value = c(tot, ind, dr)
      )
    }))
  }))

  comp_df$mediator <- factor(comp_df$suffix, levels = suffixes, labels = unname(mediator_nms[suffixes]))
  comp_df$component <- factor(comp_df$component, levels = comp_levels)
  comp_df$outcome <- factor(comp_df$outcome, levels = outcome_keys)
  n_comp <- length(comp_levels)
  step <- 0.22
  offs <- stats::setNames(((n_comp + 1) / 2 - seq_along(comp_levels)) * step, comp_levels)
  comp_df$ybase <- as.numeric(comp_df$mediator) + offs[as.character(comp_df$component)]
  leg_cols <- stats::setNames(unname(component_cols[comp_levels]), unname(component_labs[comp_levels]))
  comp_df$component_lab <- factor(
    unname(component_labs[as.character(comp_df$component)]), levels = unname(component_labs[comp_levels])
  )

  plt <- comp_df |>
    ggplot2::ggplot(
      ggplot2::aes(x = value, y = ybase, colour = component_lab, group = interaction(mediator, component))
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
    ggplot2::geom_hline(
      yintercept = seq_len(length(suffixes) - 1) + 0.5, linetype = "dotted", colour = "grey70", linewidth = 1.1
    ) +
    ggdist::stat_pointinterval(
      .width = c(0.95, 0.99), point_interval = "mean_hdci",
      interval_size_range = interval_size_range, fatten_point = fatten_point
    ) +
    ggplot2::scale_colour_manual(values = leg_cols, name = NULL, guide = if (legend) "legend" else "none") +
    ggplot2::scale_y_continuous(
      breaks = seq_along(suffixes), labels = unname(mediator_nms[suffixes]),
      limits = c(0.5, length(suffixes) + 0.5)
    ) +
    ggplot2::labs(x = x_lab, y = NULL) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 19 * fnt_sz) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 16 * fnt_sz),
      axis.text.x = ggplot2::element_text(size = 16 * fnt_sz),
      axis.text.y = ggplot2::element_text(size = 16 * fnt_sz),
      legend.position = if (legend) "bottom" else "none",
      legend.text = ggplot2::element_text(size = 15 * fnt_sz),
      legend.background = ggplot2::element_rect(fill = "white", colour = "#cacaca", linewidth = 0.5),
      legend.key.size = grid::unit(16 * fnt_sz, "pt"),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
      legend.justification = "center"
    )
  if (legend) {
    plt <- plt + ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          point_size = legend_point_size * fnt_sz,
          interval_size = legend_point_size * 1.5 * fnt_sz
        )
      )
    )
  }
  if (faceted) {
    coloured <- !is.null(outcome_cols) && all(outcome_keys %in% names(outcome_cols))
    plt <- plt +
      ggplot2::facet_wrap(
        ~outcome, nrow = 1,
        labeller = if (coloured) {
          ggplot2::as_labeller(stats::setNames(
            sprintf("<span style='color:%s'>%s</span>", unname(outcome_cols[outcome_keys]), outcome_keys),
            outcome_keys
          ))
        } else {
          ggplot2::label_value
        }
      ) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text = if (coloured) {
          ggtext::element_markdown(family = strip_family, size = 18 * fnt_sz)
        } else {
          ggplot2::element_text(face = "bold", size = 18 * fnt_sz)
        }
      )
  }
  plt
}

# two-mediator path diagram for a joint mediation model (mediator 1 above c', mediator 2 below).
#   mediator1_suffix / mediator2_suffix: column suffixes in `draws_med` (e.g. "int_neg").
#   draws_med: a_/b_/indirect_{suffix}_{control,causal}, direct_{control,causal},
#     index_mod_med_{suffix}.
# returns list(diagram, a1_path, b1_path, a2_path, b2_path, direct_path, indirect).
plot_joint_mediation <- function(draws_med,
                                 mediator1_suffix, mediator2_suffix,
                                 mediator1_nm, mediator2_nm,
                                 mediator_cols,
                                 outcome_lab = "Δ symptoms",
                                 diff_lab = DIFF_LAB,
                                 grp_cols = ARM_COLS,
                                 box_fill_alpha = 1,
                                 flip_order = FALSE,
                                 title = NULL,
                                 title_col = "grey15",
                                 med_col_text = "white",
                                 fnt_sz = 1) {
  if (!all(c(mediator1_suffix, mediator2_suffix) %in% names(mediator_cols))) {
    stop(
      "plot_joint_mediation(): `mediator_cols` must be named with `mediator1_suffix`/",
      "`mediator2_suffix` (\"", mediator1_suffix, "\"/\"", mediator2_suffix, "\") -- got names: ",
      paste(names(mediator_cols), collapse = ", ")
    )
  }

  hw <- 0.95
  hh <- 0.52
  mediator1_lab <- sub(" ", "\n", mediator1_nm)
  mediator2_lab <- sub(" ", "\n", mediator2_nm)
  med_cols <- unname(mediator_cols[c(mediator1_suffix, mediator2_suffix)])
  boxes <- data.frame(
    x    = c(0.0, 4.0, 8.0, 4.0),
    y    = c(0.0, 1.8, 0.0, -1.8),
    fill = c("grey97", med_cols[1], "grey97", med_cols[2]),
    alpha = c(1, box_fill_alpha, 1, box_fill_alpha),
    text_col = c("grey15", med_col_text, "grey15", med_col_text),
    lab  = c(
      "learning group", paste0("Δ ", mediator1_lab), outcome_lab, paste0("Δ ", mediator2_lab)
    )
  )
  seg_ab <- data.frame(
    x    = c(0.95, 4.95, 0.95,  4.95),
    y    = c(0.52,  1.28, -0.52, -1.28),
    xend = c(3.05, 7.03, 3.05,  7.03),
    yend = c(1.28,  0.52, -1.28, -0.52)
  )
  seg_c <- data.frame(x = 0.95, y = -0.35, xend = 7.05, yend = -0.35)
  plett_ab <- data.frame(
    x = c(2.1, 5.85, 2.1, 5.85), y = c(0.68, 0.68, -0.68, -0.68),
    lab = c("italic(a[g]^{(k)})", "italic(b[g]^{(k)})", "italic(a[g]^{(k)})", "italic(b[g]^{(k)})")
  )
  plett_c <- data.frame(x = 4, y = -0.55, lab = "c′")

  diagram <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(xmin = x - hw, xmax = x + hw, ymin = y - hh, ymax = y + hh, fill = fill, alpha = alpha),
      colour = "grey45", linewidth = 0.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::geom_segment(
      data = seg_c, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linetype = "dashed", colour = "grey60", linewidth = 0.5,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.018, "npc"), type = "closed")
    ) +
    ggplot2::geom_segment(
      data = seg_ab, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour = "grey25", linewidth = 0.7,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.022, "npc"), type = "closed")
    ) +
    ggplot2::geom_text(
      data = boxes, ggplot2::aes(x = x, y = y, label = lab, colour = text_col),
      family = "Open Sans", size = 4.6 * fnt_sz, lineheight = 0.9
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::geom_text(
      data = plett_ab, ggplot2::aes(x = x, y = y, label = lab),
      family = "Open Sans", colour = "grey35", size = 4.8 * fnt_sz, parse = TRUE
    ) +
    ggplot2::geom_text(
      data = plett_c, ggplot2::aes(x = x, y = y, label = lab),
      family = "Open Sans", fontface = "italic", colour = "grey35", size = 4.8 * fnt_sz
    ) +
    ggplot2::coord_cartesian(
      xlim = c(-1.3, 9.4), ylim = c(-2.55, 2.55), clip = "off", expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  if (!is.null(title)) {
    diagram <- diagram + ggplot2::annotate(
      "text", x = 0, y = 2.7, label = title, colour = title_col,
      family = "Open Sans", fontface = "bold", size = 5.4 * fnt_sz, hjust = 0.5
    )
  }

  a1_path <- mini_interval_plot(
    draws_med[[paste0("a_", mediator1_suffix, "_control")]], draws_med[[paste0("a_", mediator1_suffix, "_causal")]],
    paste0("training → ", mediator1_nm), grp_cols, fnt_sz, flip_order = flip_order
  )
  b1_path <- mini_interval_plot(
    draws_med[[paste0("b_", mediator1_suffix, "_control")]], draws_med[[paste0("b_", mediator1_suffix, "_causal")]],
    paste0(mediator1_nm, " → symptoms"), grp_cols, fnt_sz, flip_order = flip_order
  )
  a2_path <- mini_interval_plot(
    draws_med[[paste0("a_", mediator2_suffix, "_control")]], draws_med[[paste0("a_", mediator2_suffix, "_causal")]],
    paste0("training → ", mediator2_nm), grp_cols, fnt_sz, flip_order = flip_order
  )
  b2_path <- mini_interval_plot(
    draws_med[[paste0("b_", mediator2_suffix, "_control")]], draws_med[[paste0("b_", mediator2_suffix, "_causal")]],
    paste0(mediator2_nm, " → symptoms"), grp_cols, fnt_sz, flip_order = flip_order
  )
  indirect <- plot_joint_indirect(
    draws_med, mediator1_suffix, mediator2_suffix, mediator_cols,
    diff_lab = diff_lab, flip_order = flip_order, fnt_sz = fnt_sz
  )
  direct_path <- mini_interval_plot(
    draws_med$direct_control, draws_med$direct_causal, "residual association",
    grp_cols, fnt_sz, flip_order = flip_order
  )

  list(
    diagram = diagram,
    a1_path = a1_path, b1_path = b1_path,
    a2_path = a2_path, b2_path = b2_path,
    direct_path = direct_path,
    indirect = indirect
  )
}

# structural schematic for k mediators feeding m outcomes (no estimates -- those are in the
# companion plot_paths_n() panels). b-arrows are coloured by mediator. no c' path is drawn
# (there is no clean route for it); `note_tr` says where c' / c are reported instead.
#   mediator_nms / mediator_cols: named by suffix; `mediator_nms` order is top -> bottom.
# returns a single ggplot.
plot_multi_mediation_diagram <- function(mediator_nms,
                                         mediator_cols,
                                         outcome_nms,
                                         arm_lab = "learning group",
                                         a_lab = "italic(a[g]^{(k)})",
                                         b_lab = "italic(b[g]^{(k)})",
                                         note = NULL,
                                         note_col = "slateblue4",
                                         note_tr = NULL,
                                         note_tr_col = note_col,
                                         med_text_col = "white",
                                         med_text_family = "Open Sans SemiBold",
                                         box_fill_alpha = 1,
                                         fnt_sz = 1) {
  sfx <- names(mediator_nms)
  if (is.null(sfx) || !all(sfx %in% names(mediator_cols))) {
    stop("plot_multi_mediation_diagram(): `mediator_nms` must be named by suffix and covered by `mediator_cols`")
  }
  k <- length(mediator_nms)
  m <- length(outcome_nms)

  med_y <- ((k + 1) / 2 - seq_len(k)) * 1.75
  out_y <- ((m + 1) / 2 - seq_len(m)) * 2.6
  arm_x <- 0
  med_x <- 5.1
  out_x <- 10.4
  arm_hw <- 1.15
  med_hw <- 1.62
  out_hw <- 1.25
  box_hh <- 0.62
  med_cols <- unname(mediator_cols[sfx])

  # mediator box labels are their own layer (different text colour / family)
  med_boxes <- data.frame(
    x = med_x, y = med_y, hw = med_hw, fill = med_cols, alpha = box_fill_alpha,
    lab = paste0("Δ ", sub(" ", "\n", unname(mediator_nms)))
  )
  plain_boxes <- dplyr::bind_rows(
    data.frame(x = arm_x, y = 0, hw = arm_hw, fill = "grey97", alpha = 1, lab = arm_lab),
    data.frame(
      x = out_x, y = out_y, hw = out_hw, fill = "grey97", alpha = 1, lab = paste0("Δ ", outcome_nms)
    )
  )
  boxes <- dplyr::bind_rows(plain_boxes, med_boxes)

  # a-arrows fan out of the arm box edge (the small y offset at the start reads as a fan
  # rather than k lines pinned to one point); b-arrows run mediator -> every outcome
  seg_a <- data.frame(
    x = arm_x + arm_hw + 0.05, y = med_y * 0.11,
    xend = med_x - med_hw - 0.06, yend = med_y, col = med_cols
  )
  seg_b <- do.call(rbind, lapply(seq_len(k), function(i) {
    data.frame(
      x = med_x + med_hw + 0.06, y = med_y[i] + out_y * 0.06,
      xend = out_x - out_hw - 0.06, yend = out_y, col = med_cols[i]
    )
  }))

  # `note` gets its own strip below the diagram (no arrow-free gap at k = 4)
  y_bot <- -(max(abs(med_y)) + box_hh + 0.3) - if (is.null(note)) 0 else 1.35
  # `note_tr` sits top-right (the one region no arrow reaches), right edge flush with the outcome
  # boxes. ~3 lines of ~35 chars fit at figure 4's size -- calibrate on the whole figure
  y_top <- max(abs(med_y)) + box_hh + 0.85 + if (is.null(note_tr)) 0 else 1.05
  x_left <- arm_x - arm_hw - 0.4
  x_right <- out_x + out_hw + 0.4
  x_note_tr <- out_x + out_hw

  plt <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg_a, ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = col),
      linewidth = 0.8, arrow = ggplot2::arrow(length = ggplot2::unit(0.016, "npc"), type = "closed")
    ) +
    ggplot2::geom_segment(
      data = seg_b, ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = col),
      linewidth = 0.55, alpha = 0.85,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.014, "npc"), type = "closed")
    ) +
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(xmin = x - hw, xmax = x + hw, ymin = y - box_hh, ymax = y + box_hh, fill = fill, alpha = alpha),
      colour = "grey45", linewidth = 0.5
    ) +
    # richtext for the "<sub>std</sub>" suffix, with an invisible label box
    ggtext::geom_richtext(
      data = plain_boxes, ggplot2::aes(x = x, y = y, label = lab),
      colour = "grey15", family = "Open Sans", size = 4.2 * fnt_sz, lineheight = 0.9,
      fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    ggplot2::geom_text(
      data = med_boxes, ggplot2::aes(x = x, y = y, label = lab),
      colour = med_text_col, family = med_text_family, size = 4.2 * fnt_sz, lineheight = 0.9
    ) +
    ggplot2::annotate(
      "text", x = c((arm_x + med_x) / 2, (med_x + out_x) / 2), y = max(med_y) + box_hh + 0.45,
      label = c(a_lab, b_lab), parse = TRUE, family = "Open Sans", colour = "grey35",
      size = 4.8 * fnt_sz
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(
      xlim = c(x_left, x_right),
      ylim = c(y_bot, y_top),
      clip = "off", expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))

  # geom_textbox: `hjust` places the box and `halign` the text (geom_richtext ignores halign)
  if (!is.null(note)) {
    plt <- plt + ggtext::geom_textbox(
      data = data.frame(x = x_left, y = y_bot + 1.5, label = note),
      ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE,
      hjust = 0, halign = 0.5, vjust = 0, width = NULL, fill = NA, box.colour = note_col,
      box.padding = grid::unit(rep(0.4, 4), "lines"), box.r = grid::unit(0.5, "lines"),
      colour = "grey20", lineheight = 1.25, size = 3.9 * fnt_sz, family = "Open Sans"
    )
  }
  if (!is.null(note_tr)) {
    plt <- plt + ggtext::geom_textbox(
      data = data.frame(x = x_note_tr, y = y_top, label = note_tr),
      ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE,
      hjust = 1, halign = 0.5, vjust = 1, width = NULL, fill = NA, box.colour = note_tr_col,
      box.padding = grid::unit(rep(0.35, 4), "lines"), box.r = grid::unit(0.5, "lines"),
      colour = "grey20", lineheight = 1.25, size = 3.2 * fnt_sz, family = "Open Sans"
    )
  }
  plt
}

# single-mediator path diagram (one domain per fit; its c' is net of that mediator only, so not
# comparable across fits).
#   mediator_suffix: column suffix in `draws_med` (e.g. "internal_neg").
#   draws_med: a_/b_/indirect_{suffix}_{control,causal}, index_mod_med_{suffix}, and
#     optionally direct_{control,causal}.
# returns list(diagram, a_path, b_path, direct_path, indirect).
plot_single_mediation <- function(draws_med,
                                  mediator_suffix,
                                  mediator_nm,
                                  mediator_col,
                                  outcome_lab = "Δ symptoms",
                                  diff_lab = DIFF_LAB,
                                  grp_cols = ARM_COLS,
                                  box_fill_alpha = 1,
                                  flip_order = FALSE,
                                  title = NULL,
                                  title_col = "grey15",
                                  med_col_text = "white",
                                  fnt_sz = 1) {
  hw <- 0.95
  hh <- 0.52
  mediator_lab <- sub(" ", "\n", mediator_nm)
  boxes <- data.frame(
    x    = c(0.0, 4.0, 8.0),
    y    = c(0.0, 1.8, 0.0),
    fill = c("grey97", mediator_col, "grey97"),
    alpha = c(1, box_fill_alpha, 1),
    text_col = c("grey15", med_col_text, "grey15"),
    lab  = c("learning group", paste0("Δ ", mediator_lab), outcome_lab)
  )
  seg_ab <- data.frame(
    x    = c(0.95, 4.95),
    y    = c(0.52,  1.28),
    xend = c(3.05, 7.03),
    yend = c(1.28,  0.52)
  )
  seg_c <- data.frame(x = 0.95, y = 0, xend = 7.05, yend = 0)
  plett_ab <- data.frame(x = c(2.1, 5.85), y = c(0.68, 0.68), lab = c("italic(a[g])", "italic(b[g])"))
  plett_c <- data.frame(x = 4, y = 0.3, lab = "c′")

  diagram <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(xmin = x - hw, xmax = x + hw, ymin = y - hh, ymax = y + hh, fill = fill, alpha = alpha),
      colour = "grey45", linewidth = 0.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::geom_segment(
      data = seg_c, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linetype = "dashed", colour = "grey60", linewidth = 0.5,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.05, "npc"), type = "closed")
    ) +
    ggplot2::geom_segment(
      data = seg_ab, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour = "grey25", linewidth = 0.7,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.05, "npc"), type = "closed")
    ) +
    ggplot2::geom_text(
      data = boxes, ggplot2::aes(x = x, y = y, label = lab, colour = text_col),
      family = "Open Sans", size = 4.6 * fnt_sz, lineheight = 0.9
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::geom_text(
      data = plett_ab, ggplot2::aes(x = x, y = y, label = lab),
      family = "Open Sans", colour = "grey35", size = 4.8 * fnt_sz, parse = TRUE
    ) +
    ggplot2::geom_text(
      data = plett_c, ggplot2::aes(x = x, y = y, label = lab),
      family = "Open Sans", fontface = "italic", colour = "grey35", size = 4.8 * fnt_sz
    ) +
    ggplot2::coord_cartesian(
      xlim = c(-1.3, 9.4), ylim = c(-1.3, 2.55), clip = "off", expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  if (!is.null(title)) {
    # title above the ylim bound (clip = "off") so it clears the a-path inset
    diagram <- diagram + ggplot2::annotate(
      "text", x = 0, y = 2.7, label = title, colour = title_col,
      family = "Open Sans", fontface = "bold", size = 5.4 * fnt_sz, hjust = 0.5
    )
  }

  a_path <- mini_interval_plot(
    draws_med[[paste0("a_", mediator_suffix, "_control")]], draws_med[[paste0("a_", mediator_suffix, "_causal")]],
    paste0("training → ", mediator_nm), grp_cols, fnt_sz, flip_order = flip_order
  )
  b_path <- mini_interval_plot(
    draws_med[[paste0("b_", mediator_suffix, "_control")]], draws_med[[paste0("b_", mediator_suffix, "_causal")]],
    paste0(mediator_nm, " → symptoms"), grp_cols, fnt_sz, flip_order = flip_order
  )
  # per-arm direct paths only exist in the compound-symmetry generation; NULL otherwise
  direct_path <- if (all(c("direct_control", "direct_causal") %in% names(draws_med))) {
    mini_interval_plot(
      draws_med$direct_control, draws_med$direct_causal, "residual association",
      grp_cols, fnt_sz, flip_order = flip_order
    )
  } else {
    NULL
  }

  # one mediator per fit, so reuse plot_change_slabs()
  ind_df <- dplyr::bind_rows(
    data.frame(level = "control", value = draws_med[[paste0("indirect_", mediator_suffix, "_control")]]),
    data.frame(level = "causal",  value = draws_med[[paste0("indirect_", mediator_suffix, "_causal")]]),
    data.frame(level = "difference", value = draws_med[[paste0("index_mod_med_", mediator_suffix)]])
  ) |>
    dplyr::mutate(level = factor(level, levels = c("difference", "control", "causal")))
  indirect <- plot_change_slabs(
    ind_df, x_lab = "indirect effect (a × b)", grp_cols = grp_cols,
    grp_shapes = ARM_SHP, diff_comp = diff_lab, flip_order = flip_order,
    fnt_sz = fnt_sz, subtitle = NULL
  )

  list(diagram = diagram, a_path = a_path, b_path = b_path, direct_path = direct_path, indirect = indirect)
}

# Session-1 completer vs. non-completer parameter comparison. Three slab rows:
# returners, non-returners, difference. `draws` must contain "mu_{param}[1]" and
# "{param}_ncpl". Returns a single ggplot.
plot_ncpl_diff <- function(draws,
                           param,
                           param_nm,
                           mu_col          = NULL,
                           ncpl_col        = NULL,
                           diff_lab        = "difference in\nnon-returners",
                           xlim            = NULL,
                           flip_order      = FALSE,
                           suppress_labels = FALSE,
                           brew_col        = "Archambault",
                           col_nums        = c(2, 5),
                           diff_col        = "#bb9bb4",
                           fnt_sz          = 1) {
  if (is.null(mu_col))   mu_col   <- paste0("mu_", param, "[1]")
  if (is.null(ncpl_col)) ncpl_col <- paste0(param, "_ncpl")

  grp_shapes <- if (flip_order) c(21, 25, 23) else c(21, 23, 25)

  pal      <- MetBrewer::met.brewer(brew_col)[col_nums]
  grp_cols <- c("control" = pal[1], "causal" = pal[2], "difference" = diff_col)
  title <- if (flip_order) {
    paste0("session 1: non-returners vs. returners\n(", param_nm, ")")
  } else {
    paste0("session 1: returners vs. non-returners\n(", param_nm, ")")
  }

  comp_vec  <- draws[[mu_col]]
  ncpl_diff <- draws[[ncpl_col]]

  chg <- dplyr::bind_rows(
    data.frame(level = "control",  value = comp_vec),
    data.frame(level = "causal",   value = comp_vec + ncpl_diff),
    data.frame(level = "difference", value = ncpl_diff)
  )
  chg$level <- factor(chg$level, levels = c("difference", "causal", "control"))

  plot_change_slabs(
    chg, xlim = xlim,
    x_lab = param_nm,
    grp_shapes = grp_shapes,
    grp_cols = grp_cols, fnt_sz = fnt_sz, flip_order = flip_order,
    suppress_labels = suppress_labels,
    subtitle = title
  ) +
    ggplot2::scale_y_discrete(labels = c(
      "control"     = "returners",
      "causal"      = "non-returners",
      "difference"  = diff_lab
    ))
}

# Session-1 baseline symptom/questionnaire scores: returners vs. non-returners, one
# dodged bar pair per measure. `value_col` should already be z-scored within each
# measure (left to the caller).
plot_ncpl_symptom_bars <- function(df,
                                   measure_col = "measure",
                                   value_col = "value_z",
                                   completer_col = "non_completer",
                                   error_type = c("sd", "se"),
                                   bar_width = 0.65,
                                   err_width = 0.18,
                                   dodge_width = 0.75,
                                   alpha_bar = 0.8,
                                   fnt_sz = 1,
                                   legend_pos = "bottom",
                                   brew_col = "Archambault",
                                   col_nums = c(2, 5)) {
  error_type <- match.arg(error_type)
  pal <- MetBrewer::met.brewer(brew_col)[col_nums]
  grp_cols <- c("returners" = pal[1], "non-returners" = pal[2])

  plt_df <- df |>
    dplyr::transmute(
      measure = .data[[measure_col]],
      value = .data[[value_col]],
      group_plot = factor(
        ifelse(as.logical(.data[[completer_col]]), "non-returners", "returners"),
        levels = c("returners", "non-returners")
      )
    ) |>
    dplyr::filter(!is.na(value), !is.na(group_plot))

  sum_df <- plt_df |>
    dplyr::group_by(measure, group_plot) |>
    dplyr::summarise(
      mean = mean(value), sd = stats::sd(value), se = sd / sqrt(dplyr::n()), .groups = "drop"
    ) |>
    dplyr::mutate(err = if (error_type == "sd") sd else se)

  pd <- ggplot2::position_dodge(width = dodge_width)
  ggplot2::ggplot(sum_df, ggplot2::aes(x = measure, y = mean, fill = group_plot, colour = group_plot)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "#cacaca", alpha = 0.8) +
    ggplot2::geom_col(position = pd, width = bar_width, alpha = alpha_bar, linewidth = 0.3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - err, ymax = mean + err),
      position = pd, width = err_width, linewidth = 0.5, alpha = 0.5
    ) +
    ggplot2::scale_colour_manual(name = "", values = grp_cols) +
    ggplot2::scale_fill_manual(name = "", values = grp_cols) +
    ggplot2::labs(
      x = NULL,
      y = paste0("z-score (± ", ifelse(error_type == "sd", "s.d.", "s.e."), ")")
    ) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 14 * fnt_sz) +
    ggplot2::theme(
      legend.position = legend_pos,
      legend.justification = "center"
    )
}

# Model-free attribution plot: proportion internal-global / internal-specific /
# external-global / external-specific chosen, by valence x group x timepoint.
# `data` needs: subID, group ("control"/"causal"), timepoint (0/1), valence
# ("positive"/"negative"), internalGlobalChosen / internalSpecificChosen /
# externalGlobalChosen / externalSpecificChosen, and (for mode = "completer_status")
# non_completed.
cattr_behav_plot <- function(
    data,
    plot_type = c("positive", "negative", "both"),
    mode = c("both", "randomised", "completer_status"),
    plot_style = c("bar", "slope"),
    error_type = c("sd", "se"),
    label_colour = NULL,
    bar_width = 0.65,
    err_width = 0.18,
    dodge_width = 0.75,
    alpha_bar = 0.8,
    line_width = 0.9,
    point_size = 3,
    shape_types = ARM_SHP[c("control", "causal")],
    time_sep = 0.16,
    ylim = NULL,
    fnt_sz = 1,
    axis_fnt_sc = 11,
    grp_cols = ARM_COLS,
    custom_pal = NULL) {

  plot_type  <- match.arg(plot_type)
  mode       <- match.arg(mode)
  plot_style <- match.arg(plot_style)
  error_type <- match.arg(error_type)
  has_non_completed <- "non_completed" %in% names(data)

  to_plt <- data |>
    dplyr::mutate(
      non_completed_plot = if (has_non_completed) as.logical(.data[["non_completed"]]) else FALSE,
      group_plot = dplyr::case_when(
        non_completed_plot ~ "non-returners",
        group == "control" ~ "control",
        group == "causal"  ~ "causal",
        TRUE ~ NA_character_
      ),
      group_plot = factor(group_plot, levels = c("control", "causal", "non-returners")),
      timepoint_plot = factor(
        as.character(.data[["timepoint"]]), levels = c("0", "1"), labels = c("pre-intervention", "post-intervention")
      ),
      valence_plot = tolower(as.character(.data[["valence"]])),
      valence_facet = factor(
        valence_plot,
        levels = c("positive", "negative"),
        labels = c("positive events", "negative events")
      )
    )

  if (plot_type != "both") {
    to_plt <- to_plt |> dplyr::filter(valence_plot == plot_type)
  }

  if (mode == "randomised") {
    to_plt <- to_plt |>
      dplyr::mutate(group_plot = factor(group_plot, levels = c("control", "causal")))
  } else if (mode == "completer_status") {
    to_plt <- to_plt |>
      dplyr::filter(.data[["timepoint"]] == 0) |>
      dplyr::mutate(
        group_plot = dplyr::if_else(group_plot == "non-returners", "non-returners", "returners"),
        group_plot = factor(group_plot, levels = c("returners", "non-returners"))
      )
  }

  if (!is.null(custom_pal)) {
    pal <- custom_pal
  } else if (mode == "completer_status") {
    pal <- unname(MetBrewer::met.brewer("Archambault")[c(2, 5)])
  } else {
    pal <- unname(grp_cols[levels(droplevels(to_plt$group_plot))])
  }

  if (is.null(label_colour)) {
    if (plot_type == "positive") label_colour <- VAL_COLS[["positive"]]
    else if (plot_type == "negative") label_colour <- VAL_COLS[["negative"]]
    else label_colour <- unname(VAL_COLS[c("positive", "negative")])
  }

  if (plot_type == "both") {
    if (length(label_colour) == 1) label_colour <- rep(label_colour, 2)
    if (length(label_colour) >= 2) {
      to_plt <- to_plt |>
        dplyr::mutate(
          valence_facet = factor(
            valence_plot,
            levels = c("positive", "negative"),
            labels = c(
              paste0("<span style='color:", label_colour[1], ";'>positive events</span>"),
              paste0("<span style='color:", label_colour[2], ";'>negative events</span>")
            )
          )
        )
    }
  }

  plot_df <- to_plt |>
    dplyr::transmute(
      id_plot = as.character(.data[["subID"]]),
      group_plot,
      timepoint_plot,
      valence_facet,
      internalGlobalChosen = as.numeric(.data[["internalGlobalChosen"]]),
      internalSpecificChosen = as.numeric(.data[["internalSpecificChosen"]]),
      externalGlobalChosen = as.numeric(.data[["externalGlobalChosen"]]),
      externalSpecificChosen = as.numeric(.data[["externalSpecificChosen"]])
    ) |>
    tidyr::pivot_longer(
      cols = c(internalGlobalChosen, internalSpecificChosen, externalGlobalChosen, externalSpecificChosen),
      names_to = "category",
      values_to = "chosen"
    ) |>
    dplyr::mutate(
      category = factor(
        category,
        levels = c(
          "internalGlobalChosen", "internalSpecificChosen", "externalGlobalChosen", "externalSpecificChosen"
        ),
        labels = c("internal-global", "internal-specific", "external-global", "external-specific")
      )
    ) |>
    dplyr::filter(!is.na(id_plot), !is.na(group_plot), !is.na(chosen), !is.na(timepoint_plot), !is.na(category))

  subj_df <- plot_df |>
    dplyr::group_by(id_plot, group_plot, timepoint_plot, valence_facet, category) |>
    dplyr::summarise(chosen = mean(chosen), .groups = "drop")

  sum_df <- subj_df |>
    dplyr::group_by(group_plot, timepoint_plot, valence_facet, category) |>
    dplyr::summarise(
      chosen_mean = mean(chosen),
      chosen_sd = stats::sd(chosen),
      chosen_se = chosen_sd / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    dplyr::mutate(err = if (error_type == "sd") chosen_sd else chosen_se)

  pd <- ggplot2::position_dodge(width = dodge_width)
  legend_key_h <- grid::unit(max(6, 10 * fnt_sz), "pt")
  legend_spacing_y <- grid::unit(max(0, 2 * fnt_sz), "pt")
  title_lab <- if (plot_type == "both") NULL else paste0(plot_type, " events")

  if (plot_style == "bar") {
    p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = category, y = chosen_mean, fill = group_plot, colour = group_plot)) +
      ggplot2::geom_col(
        ggplot2::aes(group = group_plot), position = pd, width = bar_width, alpha = alpha_bar, linewidth = 0.3
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = pmax(0, chosen_mean - err), ymax = chosen_mean + err, group = group_plot),
        linewidth = line_width * 0.6, width = err_width, alpha = 0.4, position = pd
      ) +
      ggplot2::geom_hline(yintercept = 0.25, linetype = "dashed", colour = "#cacaca", alpha = 0.8) + {
      if (plot_type == "both") {
        if (mode == "completer_status") {
          ggplot2::facet_wrap(~valence_facet, nrow = 2)
        } else {
          ggplot2::facet_grid(rows = ggplot2::vars(valence_facet), cols = ggplot2::vars(timepoint_plot))
        }
      } else {
        if (mode != "completer_status") ggplot2::facet_wrap(~timepoint_plot, nrow = 1)
      }
    } +
      ggplot2::scale_colour_manual(name = "", values = pal) +
      ggplot2::scale_fill_manual(name = "", values = pal) +
      ggplot2::labs(
        title = title_lab, x = NULL,
        y = paste0("proportion of attributions (± ", ifelse(error_type == "sd", "s.d.", "s.e."), ")")
      )
  } else {
    p <- ggplot2::ggplot(
      sum_df,
      ggplot2::aes(
        x = timepoint_plot, y = chosen_mean,
        colour = group_plot, fill = group_plot, shape = group_plot, group = group_plot
      )
    ) +
      ggplot2::geom_line(linewidth = line_width, alpha = 0.7, position = pd) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = chosen_mean - err, ymax = chosen_mean + err),
        width = err_width, linewidth = line_width * 0.6, alpha = 0.6, position = pd
      ) +
      ggplot2::geom_point(size = point_size, stroke = 0.5, alpha = 0.95, position = pd) +
      ggplot2::geom_hline(yintercept = 0.25, linetype = "dashed", colour = "#cacaca", alpha = 0.8) +
      (if (plot_type == "both") {
        ggplot2::facet_grid(rows = ggplot2::vars(valence_facet), cols = ggplot2::vars(category))
      } else {
        ggplot2::facet_wrap(~category, nrow = 1)
      }) +
      ggplot2::scale_x_discrete(labels = c("pre-intervention" = "pre", "post-intervention" = "post")) +
      ggplot2::scale_colour_manual(name = "", values = pal) +
      ggplot2::scale_fill_manual(name = "", values = pal) +
      ggplot2::scale_shape_manual(name = "", values = shape_types) +
      ggplot2::labs(
        title = title_lab, x = NULL,
        y = paste0("prop. chosen (± ", ifelse(error_type == "sd", "s.d.", "s.e."), ")")
      )
  }

  if (!is.null(ylim)) p <- p + ggplot2::coord_cartesian(ylim = ylim)

  p +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = 14 * fnt_sz) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.location = "plot",
      legend.justification = "center",
      legend.key.height = legend_key_h,
      legend.spacing.y = legend_spacing_y,
      legend.justification.bottom = "center",
      legend.background = ggplot2::element_rect(fill = "white", colour = "#cacaca", linewidth = 0.5),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
      strip.background = ggplot2::element_blank(),
      strip.text.x = if (plot_type == "both" && plot_style == "bar" && mode == "completer_status") {
        ggtext::element_markdown(
          family = "Open Sans SemiBold", colour = "darkslategray4",
          size = 13 * fnt_sz, hjust = 0, margin = ggplot2::margin(b = 0.01, unit = "npc")
        )
      } else {
        ggtext::element_markdown(face = "italic", colour = "darkslategray4", size = 13 * fnt_sz)
      },
      strip.text.y = if (plot_type == "both" && plot_style == "bar" && mode != "completer_status") {
        ggtext::element_markdown(
          family = "Open Sans SemiBold", size = 12 * fnt_sz, hjust = 0,
          margin = ggplot2::margin(b = 0.01, unit = "npc")
        )
      } else {
        ggtext::element_markdown(face = "italic", size = 12 * fnt_sz)
      },
      plot.title = if (!plot_type == "both") {
        ggplot2::element_text(size = 14 * fnt_sz, face = "plain", family = "Open Sans SemiBold", colour = label_colour)
      } else {
        ggplot2::element_blank()
      },
      axis.title.y = ggplot2::element_text(size = axis_fnt_sc * fnt_sz),
      axis.title.x = ggtext::element_markdown(size = 12 * fnt_sz),
      axis.text.x = if (plot_style == "slope") {
        ggplot2::element_text(size = 14 * fnt_sz, angle = 0, hjust = 0.5, vjust = 0.5)
      } else {
        ggplot2::element_text(size = 10 * fnt_sz, angle = 20, hjust = 0.75, vjust = 0.85)
      },
      axis.text.y = ggplot2::element_text(size = 11 * fnt_sz),
      legend.text = ggplot2::element_text(size = 12 * fnt_sz),
      legend.box = "vertical"
    )
}

# group-level population mean + pre->post change for an attribution parameter. delta_{param} =
# control change, theta_int_{param} = additional change in causal. `mu_prefix` (e.g.
# "mu_internal_theta_neg") is needed because mu_* names order the words differently.
# returns list(levels_panel, change_panel).
plot_ca_change <- function(draws,
                           param,
                           param_nm,
                           flip_order = FALSE, # causal on top default
                           mu_prefix = NULL,
                           diff_lab = DIFF_LAB,
                           level_xlim = NULL,
                           change_xlim = NULL,
                           suppress_labels = FALSE,
                           grp_cols = ARM_COLS,
                           fnt_sz = 1) {
  if (is.null(mu_prefix)) mu_prefix <- paste0("mu_", param)
  grp_shapes <- if (flip_order) {
    ARM_SHP[c("control", "causal", "difference")]
  } else {
    ARM_SHP[c("causal", "control", "difference")]
  }
  nudge <- 0.16

  # condition 1 = causal (arm of interest, reference here); condition 0 = control
  mu1 <- draws[[paste0(mu_prefix, "[1]")]]  # pre, shared by both arms
  mu2 <- draws[[paste0(mu_prefix, "[2]")]]  # post, control (reference)
  int_col <- paste0("theta_int_", param)
  int <- draws[[int_col]]                   # additional causal effect at post

  pop_rows <- list()
  for (g in c(1, 0)) {
    for (t in 1:2) {
      v <- if (t == 1) mu1 else if (g == 1) mu2 + int else mu2
      h95 <- bayestestR::hdi(v, ci = 0.95)
      h99 <- bayestestR::hdi(v, ci = 0.99)
      pop_rows[[length(pop_rows) + 1]] <- data.frame(
        group = ifelse(g == 1, "causal", "control"),
        tp    = ifelse(t == 1, "pre", "post"),
        m     = median(v),
        lo95  = h95$CI_low, hi95 = h95$CI_high,
        lo99  = h99$CI_low, hi99 = h99$CI_high
      )
    }
  }
  pop <- do.call(rbind, pop_rows)
  pop$group <- factor(pop$group, levels = c("control", "causal"))
  pop$ybase <- as.numeric(pop$group) + ifelse(pop$tp == "pre", nudge, -nudge)

  chg <- dplyr::bind_rows(
    data.frame(level = "control", value = draws[[paste0("delta_", param)]]),
    data.frame(level = "causal",  value = draws[[paste0("delta_", param)]] + draws[[int_col]]),
    data.frame(level = "difference", value = draws[[int_col]])
  )
  chg$level <- factor(chg$level, levels = c("difference", "control", "causal"))

  list(
    levels_panel = plot_group_levels(
      pop, xlim = level_xlim,
      x_lab    = param_nm,
      grp_cols = grp_cols, fnt_sz = fnt_sz,
      flip_order = flip_order,
      suppress_labels = suppress_labels,
      subtitle = "group-level estimates\n○ pre- ● post-intervention"
    ),
    change_panel = plot_change_slabs(
      chg, xlim = change_xlim, diff_comp = diff_lab,
      x_lab    = paste0("change in ", param_nm),
      grp_shapes = grp_shapes, flip_order = flip_order,
      grp_cols = grp_cols, fnt_sz = fnt_sz,
      suppress_labels = suppress_labels
    )
  )
}

# posterior correlations between attribution draws and questionnaire scores (t1 / t2 levels and
# change): within arm, pooled ("both") and causal - control ("difference", per draw).
#   ids: the id/subID frame from the same prep_data() call that built the fit's data.
#   qqnrs: qqnrs_t2-shaped frame (subID, group, {measure}_total_{t1,t2,delta}).
#   qns_to_invert: measures to sign-flip (higher = better, e.g. "ERQCR").
get_symptom_corrs <- function(ids,
                              draws,
                              qqnrs,
                              delta_par_pattern,
                              par_labels,
                              indiv_par_map = NULL,
                              qns_to_invert = c()) {
  invert_pattern <- paste0("^(", paste(qns_to_invert, collapse = "|"), ")_(t1|t2|delta)$")

  qns_wide <- qqnrs |>
    dplyr::select(subID, group, tidyselect::matches("_total_(t1|t2|delta)$")) |>
    dplyr::rename_with(~ gsub("_total_", "_", .)) |>
    dplyr::mutate(dplyr::across(tidyselect::matches(invert_pattern), ~ . * -1))

  ids_dt <- ids |>
    dplyr::mutate(id = as.character(id)) |>
    dplyr::select(id, subID) |>
    data.table::as.data.table()

  corr_from_sums <- function(agg) {
    agg[, den := sqrt(pmax(n * sxx - sx^2, 0) * pmax(n * syy - sy^2, 0))]
    agg[, correlation := data.table::fifelse(n < 2 | den == 0, NA_real_, (n * sxy - sx * sy) / den)]
    agg[, .(group, par, questionnaire, .draw, correlation)]
  }

  correlate_draws <- function(par_draws, qn_long, timepoint_label) {
    pd <- data.table::as.data.table(par_draws)
    pd[, id := as.character(id)]
    pd_ids <- merge(pd, ids_dt, by = "id", all.x = TRUE, allow.cartesian = TRUE)
    joined <- merge(data.table::as.data.table(qn_long), pd_ids, by = "subID", all.x = TRUE, allow.cartesian = TRUE)
    joined[, c("id", "subID") := NULL]
    joined <- joined[!is.na(par_value) & !is.na(score)]

    agg_grp <- joined[
      , .(n = .N, sx = sum(par_value), sy = sum(score), sxy = sum(par_value * score),
          sxx = sum(par_value * par_value), syy = sum(score * score)),
      by = .(group, par, questionnaire, .draw)
    ]
    agg_both <- agg_grp[
      , .(n = sum(n), sx = sum(sx), sy = sum(sy), sxy = sum(sxy), sxx = sum(sxx), syy = sum(syy)),
      by = .(par, questionnaire, .draw)
    ][, group := "both"]

    out <- corr_from_sums(rbind(agg_grp, agg_both, use.names = TRUE, fill = TRUE))
    out[, timepoint := timepoint_label]
    tibble::as_tibble(out)
  }

  qn_long_for <- function(suffix) {
    qns_wide |>
      dplyr::select(subID, group, tidyselect::ends_with(suffix)) |>
      tidyr::drop_na(tidyselect::ends_with(suffix)) |>
      tidyr::pivot_longer(cols = tidyselect::ends_with(suffix), names_to = "questionnaire", values_to = "score")
  }

  delta_draws <- draws |> dplyr::select(tidyselect::starts_with("delta_"), .draw)
  if (length(par_labels) > 1) {
    delta_draws <- delta_draws |>
      tidyr::pivot_longer(
        cols = -".draw", names_to = c("par", "id"), names_pattern = delta_par_pattern, values_to = "par_value"
      )
  } else {
    delta_draws <- delta_draws |>
      tidyr::pivot_longer(
        cols = -".draw", names_to = "id", names_pattern = delta_par_pattern, values_to = "par_value"
      ) |>
      dplyr::mutate(par = names(par_labels))
  }
  delta_draws <- delta_draws |> dplyr::mutate(id = as.integer(id))

  change_cor <- correlate_draws(delta_draws, qn_long_for("_delta"), "change")

  level_draws <- draws |>
    dplyr::select(-tidyselect::starts_with("delta_"), -tidyselect::any_of(c(".chain", ".iteration"))) |>
    tidyr::pivot_longer(
      cols = -".draw", names_to = c("raw_par", "id", "tp"),
      names_pattern = "^(.*)\\[(\\d+),\\s*(\\d+)\\]$", values_to = "par_value"
    ) |>
    dplyr::mutate(
      id = as.integer(id),
      par = if (is.null(indiv_par_map)) raw_par else dplyr::recode(raw_par, !!!indiv_par_map),
      timepoint = dplyr::case_when(tp == "1" ~ "t1", tp == "2" ~ "t2", TRUE ~ NA_character_)
    ) |>
    dplyr::filter(par %in% names(par_labels)) |>
    dplyr::select(-raw_par, -tp)

  level_cor <- lapply(c("t1", "t2"), function(tpt) {
    correlate_draws(
      level_draws |> dplyr::filter(timepoint == tpt) |> dplyr::select(-timepoint),
      qn_long_for(paste0("_", tpt)), tpt
    )
  }) |>
    dplyr::bind_rows()

  per_draw <- dplyr::bind_rows(change_cor, level_cor)

  diff_draw <- per_draw |>
    dplyr::filter(group %in% c("control", "causal")) |>
    tidyr::pivot_wider(names_from = group, values_from = correlation) |>
    dplyr::mutate(correlation = causal - control, group = "difference") |>
    dplyr::select(-causal, -control)

  posterior_cor_df <- dplyr::bind_rows(per_draw, diff_draw) |>
    dplyr::mutate(
      par = factor(par, levels = names(par_labels), labels = par_labels),
      group = factor(group, levels = c("control", "causal", "both", "difference")),
      timepoint = factor(timepoint, levels = c("t1", "t2", "change"))
    )

  cor_summary_df <- posterior_cor_df |>
    dplyr::summarise(
      mean_corr = mean(correlation),
      hdi_95_lo = bayestestR::hdi(correlation, ci = 0.95)$CI_low,
      hdi_95_up = bayestestR::hdi(correlation, ci = 0.95)$CI_high,
      p_direct = bayestestR::p_direction(correlation)$pd,
      .by = c(group, par, questionnaire, timepoint)
    ) |>
    dplyr::rename(correlation = mean_corr) |>
    dplyr::ungroup()

  panels <- split(cor_summary_df, cor_summary_df$group)
  list(cor_df = posterior_cor_df, summary_df = cor_summary_df, panels = panels)
}

# tile heatmap of get_symptom_corrs()$summary_df. `primary_qns` (leading entries of qn_labels)
# draws a primary/secondary rule as in plot_ancova_forest(); `rule_ends` sets its extent (npc)
plot_correlation_heatmap <- function(cor_summary_df,
                                     int_group = NULL,
                                     par_labels,
                                     qn_labels,
                                     primary_qns = NULL,
                                     tier_labels = c("primary", "secondary"),
                                     rule_ends = c(-0.22, 1),
                                     timepoint = c("change", "t1", "t2"),
                                     cor_lims = NULL,
                                     cor_cols = c("#85D4E3", "white", "#E39485"),
                                     htmp_title = NULL,
                                     title_clr = "black",
                                     txt_sz = 4,
                                     fnt_sz = 1,
                                     label_type = c("full", "r_only", "sig_only", "none"),
                                     alpha_range = c(0.15, 1)) {
  label_type <- match.arg(label_type)
  timepoint <- match.arg(timepoint)
  par_prefix <- if (timepoint == "change") "Δ " else ""

  cor_plot_df <- cor_summary_df |>
    dplyr::filter(
      timepoint == !!timepoint,
      if (is.null(int_group)) TRUE else group %in% !!int_group
    ) |>
    dplyr::mutate(
      par = factor(par, labels = paste0(par_prefix, par_labels)),
      questionnaire = gsub("_(delta|t1|t2)$", "", questionnaire),
      questionnaire = factor(questionnaire, levels = rev(names(qn_labels)), labels = rev(qn_labels))
    )

  if (is.null(cor_lims)) {
    cor_lims <- c(min(cor_plot_df$correlation) - 0.1, max(cor_plot_df$correlation) + 0.1)
  }

  cor_plot_df <- cor_plot_df |>
    dplyr::mutate(
      cell_label = if (label_type == "full") {
        sprintf("%.2f\n(%.2f, %.2f)", correlation, .lower, .upper)
      } else if (label_type == "r_only") {
        sprintf("%.2f", correlation)
      } else if (label_type == "sig_only") {
        ifelse(hdi_95_lo > 0 | hdi_95_up < 0, sprintf("%.2f", correlation), "")
      } else {
        ""
      }
    )

  # alpha (not just fill) carries p_direction, so weak-evidence cells fade toward white
  # rather than reading as strongly coloured -- an uncertainty cue with no hard cutoff
  base_plt <- cor_plot_df |>
    ggplot2::ggplot(ggplot2::aes(x = par, y = questionnaire, fill = correlation, alpha = p_direct)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.5)

  # rule above the secondary rows, in npc so it can extend past the tiles (clip = "off")
  if (!is.null(primary_qns)) {
    stopifnot(identical(primary_qns, names(qn_labels)[seq_along(primary_qns)]), length(tier_labels) == 2)
    n_sec <- length(qn_labels) - length(primary_qns)
    tier_ann <- function(lbl, y, vjust) {
      ggplot2::annotate(
        "text", x = I(1.02), y = y, label = lbl, hjust = 0, vjust = vjust,
        family = "Open Sans", size = fnt_sz * 3.9, colour = "grey45", fontface = "italic"
      )
    }
    base_plt <- base_plt +
      ggplot2::annotate(
        "segment", x = I(rule_ends[1]), xend = I(rule_ends[2]), y = n_sec + 0.5, yend = n_sec + 0.5,
        colour = "grey75", linewidth = 0.8
      ) +
      tier_ann(tier_labels[[1]], n_sec + 0.62, 0) +
      tier_ann(tier_labels[[2]], n_sec + 0.38, 1)
  }

  if (label_type != "none") {
    # show.legend = FALSE so this layer's key doesn't replace geom_tile's in the pd legend
    base_plt <- base_plt + ggplot2::geom_text(
      ggplot2::aes(label = cell_label), color = "black", size = txt_sz, family = "Open Sans", lineheight = 0.8,
      show.legend = FALSE
    )
  }

  base_plt +
    ggplot2::scale_fill_gradientn(
      name = "<em>r</em>", limits = cor_lims, colors = cor_cols, 
      values = scales::rescale(c(cor_lims[1], 0, cor_lims[2]))
    ) +
    # pd is bounded in [0.5, 1] (bayestestR::p_direction never dips below 0.5, by
    # definition); breaks span that full range so the legend doesn't imply a wider one
    ggplot2::scale_alpha_continuous(
      name = "<em>P</em><sub>direction</sub>", range = alpha_range, limits = c(0.5, 1), breaks = c(0.5, 0.75, 1),
      guide = ggplot2::guide_legend(override.aes = list(fill = "grey20"))
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(title = htmp_title, x = NULL, y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    cowplot::theme_minimal_grid(font_size = fnt_sz * 18, font_family = "Open Sans") +
    ggplot2::theme(
      axis.text.x = ggtext::element_markdown(size = fnt_sz * 14),
      axis.text.x.top = ggtext::element_markdown(size = fnt_sz * 14),
      axis.text.y = ggtext::element_markdown(size = fnt_sz * 16),
      legend.title = ggtext::element_markdown(size = fnt_sz * 12, face = "plain"),
      legend.text = ggplot2::element_text(size = fnt_sz * 11),
      # legend.key.width  = grid::unit(1, "lines"),
      legend.key.height = grid::unit(1.25, "lines"),
      # legend.spacing.x = grid::unit(0.5, "lines"),
      plot.title = ggtext::element_markdown(
        family = "Open Sans SemiBold", face = "plain", hjust = 0.5, size = fnt_sz * 18, color = title_clr
      ),
      panel.grid.major = ggplot2::element_blank(),
      # right margin reserves space for the tier labels, which sit outside the tiles (and
      # scales with them); without primary_qns nothing is drawn there, so keep the default
      plot.margin = ggplot2::margin(5.5, if (is.null(primary_qns)) 5.5 else 56 * fnt_sz, 5.5, 5.5),
      legend.position = "right"
    )
}

# per-measure pre->post slope panel (means by timepoint x group, beeswarm points, d_av +
# bracket). reads slope_summ, slope_long, panel_stats, grp_cols, grp_labs, grp_shapes, grp_lty
# from the global env
plot_qnr_measure <- function(meas, title, subtitle, ylab, tag, dodge_w,
                             tag_size       = 20,
                             tag_theme      = NULL,
                             err            = c("ci", "se"),
                             show_points    = TRUE,
                             annotate_stats = TRUE,
                             base_size      = 18) {
  err <- match.arg(err)
  # `tag = NULL` leaves the tag for patchwork; `tag_theme` styles it
  if (is.null(tag_theme)) tag_theme <- ggtext::element_markdown(size = tag_size, colour = "black")
  sm  <- slope_summ[slope_summ$measure == meas, ]
  sm$err <- if (err == "ci") sm$ci else sm$se
  iv  <- slope_long[slope_long$measure == meas, ]
  st  <- panel_stats[panel_stats$measure == meas, ]

  pd <- ggplot2::position_dodge(width = dodge_w)

  p <- ggplot2::ggplot(
    sm,
    ggplot2::aes(x = timepoint, y = mean_score, colour = group, group = group)
  )

  if (show_points) {
    p <- p + ggplot2::geom_point(
      data = iv,
      ggplot2::aes(x = timepoint, y = value, colour = group, shape = group),
      position = ggbeeswarm::position_quasirandom(
        width = 0.12, dodge.width = dodge_w
      ),
      alpha = 0.2, size = 1.5, show.legend = FALSE
    )
  }

  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(linetype = group), position = pd, linewidth = 1.1
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_score - err, ymax = mean_score + err),
      position = pd, width = 0.12, linewidth = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = group), position = pd, size = 3
    ) +
    ggplot2::scale_colour_manual(values = grp_cols, labels = grp_labs, name = NULL) +
    ggplot2::scale_shape_manual(values = grp_shapes, labels = grp_labs, name = NULL) +
    ggplot2::scale_linetype_manual(values = grp_lty, labels = grp_labs, name = NULL) +
    ggplot2::labs(x = NULL, y = ylab, title = title, subtitle = subtitle, tag = tag) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = base_size) +
    cowplot::background_grid(major = "y", minor = "none") +
    ggplot2::theme(
      plot.title    = ggtext::element_markdown(size = base_size + 3, hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size - 5, hjust = 0.5, colour = "grey30"),
      plot.tag        = tag_theme,
      axis.title.y    = ggtext::element_markdown(size = base_size - 4),
      legend.position = "bottom",
      legend.justification.bottom = "center",
      legend.background = ggplot2::element_rect(
        fill = "white", colour = "#cacaca", linewidth = 0.5
      ),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm")
    )

  if (annotate_stats) {
    data_range <- range(
      c(sm$mean_score - sm$err, sm$mean_score + sm$err, iv$value), na.rm = TRUE
    )
    bracket_y <- max(sm$mean_score + sm$err) + diff(data_range) * 0.10

    # NB d_av and the bracket are the pooled pre->post change, not the arm contrast
    p <- p +
      ggtext::geom_richtext(
        data = data.frame(
          x = 1.5, y = Inf,
          label = paste0("*d*<sub>av</sub> = ", formatC(st$d_av, format = "f", digits = 2))
        ),
        ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE,
        vjust = 1.3, family = "Open Sans", size = (base_size - 6) / 2,
        fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")
      ) +
      ggplot2::annotate(
        "segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y, linewidth = 0.7, color = "grey70"
      ) +
      ggplot2::annotate(
        "text", x = 1.5, y = bracket_y, label = st$p_star_timepoint,
        vjust = -0.35, size = 4.3, family = "Open Sans", colour = "grey40"
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.22)))
  }
  p
}

# per-measure pre-post change by group, with the group x time interaction p (via the caller's
# compute_interaction()) and per-group d_s. reads qqnrs_t2, delta_cols, grp_cols, grp_labs,
# grp_labs_br, grp_shapes from the global env. `show_pp = TRUE` adds per-protocol columns (needs
# qqnrs_t2$pp and the *_pp globals), with a separate, de-emphasised PP bracket
plot_qnr_change <- function(meas, title, subtitle, ylab, tag,
                            tag_size       = 20,
                            tag_theme      = NULL,
                            err            = c("ci", "se"),
                            show_points    = TRUE,
                            annotate_stats = TRUE,
                            show_pp        = FALSE,
                            base_size      = 18) {
  err <- match.arg(err)
  # see plot_qnr_measure() on `tag = NULL` + `tag_theme` (patchwork-assigned tags)
  if (is.null(tag_theme)) tag_theme <- ggtext::element_markdown(size = tag_size, colour = "black")
  col <- delta_cols[[meas]]

  if (show_pp) {
    dd <- qqnrs_t2[, c("group", "pp", col)]
    names(dd) <- c("group", "pp", "delta")
    dd <- dd[!is.na(dd$delta), ]
    dd$group <- as.character(dd$group)

    itt <- dd
    itt$group_pop <- paste0(itt$group, "_itt")
    ppd <- dd[dd$pp, ]
    ppd$group_pop <- paste0(ppd$group, "_pp")
    dd <- rbind(itt, ppd)
    dd$group <- factor(dd$group_pop, levels = c("control_itt", "causal_itt", "control_pp", "causal_pp"))
    dd$group_pop <- NULL

    cols <- grp_cols_pp
    shps <- grp_shapes_pp
    labs <- grp_labs_pp
    labs_br <- grp_labs_br_pp
  } else {
    dd <- qqnrs_t2[, c("group", col)]
    names(dd) <- c("group", "delta")
    dd <- dd[!is.na(dd$delta), ]

    cols <- grp_cols
    shps <- grp_shapes
    labs <- grp_labs
    labs_br <- grp_labs_br
  }

  dsumm <- dd |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      mean_delta = mean(delta),
      n          = dplyr::n(),
      sd_delta   = stats::sd(delta),
      se         = sd_delta / sqrt(n),
      ci         = stats::qt(0.975, n - 1) * se,
      d_s        = mean_delta / sd_delta,
      .groups    = "drop"
    )
  dsumm$err <- if (err == "ci") dsumm$ci else dsumm$se

  # the ITT comparison ignores show_pp (compute_interaction() reads the unfiltered data)
  intr <- compute_interaction(meas)

  p <- ggplot2::ggplot(dsumm, ggplot2::aes(x = group, y = mean_delta, colour = group))

  if (show_points) {
    p <- p + ggplot2::geom_point(
      data = dd,
      ggplot2::aes(x = group, y = delta, colour = group, shape = group),
      position = ggbeeswarm::position_quasirandom(width = 0.25),
      alpha = 0.2, size = 1.5, show.legend = FALSE
    )
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "32", colour = "grey60") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_delta - err, ymax = mean_delta + err),
      width = 0.12, linewidth = 0.9
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = group), size = 3) +
    ggplot2::scale_colour_manual(values = cols, labels = labs, name = NULL) +
    ggplot2::scale_shape_manual(values = shps, labels = labs, name = NULL) +
    ggplot2::scale_x_discrete(labels = labs_br) +
    ggplot2::labs(x = NULL, y = ylab, title = title, subtitle = subtitle, tag = tag) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = base_size) +
    cowplot::background_grid(major = "y", minor = "none") +
    ggplot2::theme(
      plot.title      = ggtext::element_markdown(size = base_size + 3, hjust = 0.5, face = "bold"),
      plot.subtitle   = ggplot2::element_text(size = base_size - 5, hjust = 0.5, colour = "grey30"),
      plot.tag        = tag_theme,
      axis.title.y    = ggtext::element_markdown(size = base_size - 4),
      legend.position = "bottom",
      legend.justification.bottom = "center",
      legend.background = ggplot2::element_rect(
        fill = "white", colour = "#cacaca", linewidth = 0.5
      ),
      legend.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm")
    )

  if (show_pp) {
    p <- p + ggplot2::geom_vline(xintercept = 2.5, linetype = "dotted", colour = "grey70")
  }

  if (annotate_stats) {
    data_range <- range(
      c(dsumm$mean_delta - dsumm$err, dsumm$mean_delta + dsumm$err, dd$delta), na.rm = TRUE
    )
    bracket_y <- max(dsumm$mean_delta + dsumm$err) + diff(data_range) * 0.10

    # one d_s label per column
    d_lab_df <- dsumm |>
      dplyr::mutate(
        x = as.numeric(group),
        label = paste0(
          "<span style='color:", cols[as.character(group)], "'>*d*<sub>s</sub> = ",
          formatC(d_s, format = "f", digits = 2), "</span>"
        )
      )

    p <- p +
      ggtext::geom_richtext(
        data = d_lab_df, ggplot2::aes(x = x, y = Inf, label = label), inherit.aes = FALSE,
        vjust = 1.3, family = "Open Sans", size = (base_size - 6) / 2,
        fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")
      ) +
      ggplot2::annotate(
        "segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y, linewidth = 0.7, color = "grey70"
      ) +
      ggplot2::annotate(
        "text", x = 1.5, y = bracket_y, label = intr$star,
        vjust = -0.35, size = 4.3, family = "Open Sans", colour = "grey40"
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.22)))

    if (show_pp) {
      dd_pp_pair <- dd[dd$group %in% c("control_pp", "causal_pp"), ]
      intr_pp <- compute_interaction(meas, ref = "control_pp", comp = "causal_pp", data = dd_pp_pair)

      p <- p +
        ggplot2::annotate(
          "segment", x = 3, xend = 4, y = bracket_y, yend = bracket_y,
          linewidth = 0.7, linetype = "42", color = "grey85"
        ) +
        ggplot2::annotate(
          "text", x = 3.5, y = bracket_y, label = intr_pp$star,
          vjust = -0.35, size = 4.3, family = "Open Sans", colour = "grey55"
        )
    }
  }
  p
}

# companion to plot_ca_change() for the mediation models. returns:
#   scatter_panel -- per-participant delta-parameter vs. delta-symptom (z), by group
#   between_panel -- between-person (baseline) association, beta_symp_{par}B(+_group)
#   within_panel  -- within-person (change) association, beta_symp_{par}W(+_group)
# `draws` needs the caller-built {prefix}_t1_group_* / {prefix}_t2_change_* columns and
# delta_{param}_p[1..nPpts]; `stan_ls` must be the list the fit was sampled on.
plot_sympt_decomp <- function(draws,
                              stan_ls,
                              prefix,
                              param_nm,
                              symptom_key = "symp",
                              symptom_nm = "symptoms<sub>std</sub>",
                              diff_lab = DIFF_LAB,
                              between_xlim = NULL,
                              within_xlim = NULL,
                              scatter_xlim = NULL,
                              scatter_ylim = NULL,
                              flip_order = FALSE,
                              suppress_labels = FALSE,
                              grp_cols = ARM_COLS,
                              interval_size_range = c(1, 3),
                              fatten_point = 2,
                              x_n_breaks = NULL,
                              scatter_n_breaks = 4,
                              fnt_sz = 1) {
  grp_shapes <- if (flip_order) {
    ARM_SHP[c("control", "causal", "difference")]
  } else {
    ARM_SHP[c("causal", "control", "difference")]
  }

  mediator_par <- sub("^symp_", "", prefix)
  required_cols <- paste0(prefix, c(
    "_t1_group_control", "_t1_group_causal", "_t1_diff",
    "_t2_change_control", "_t2_change_causal", "_t2_change_diff"
  ))
  missing_cols <- setdiff(required_cols, names(draws))
  if (length(missing_cols) > 0) {
    stop("plot_sympt_decomp(): `draws` is missing expected columns: ", paste(missing_cols, collapse = ", "))
  }
  delta_cols <- grep(paste0("^delta_", mediator_par, "_p\\["), names(draws), value = TRUE)
  if (length(delta_cols) == 0) {
    stop(
      "plot_sympt_decomp(): `draws` has no columns matching 'delta_", mediator_par, "_p[...]' -- ",
      "make sure 'delta_", mediator_par, "_p' was included in the variables pulled from the fit."
    )
  }

  slab_df <- function(causal_col, control_col, diff_col_nm) {
    d <- dplyr::bind_rows(
      data.frame(level = "causal",  value = draws[[causal_col]]),
      data.frame(level = "control", value = draws[[control_col]]),
      data.frame(level = "difference", value = draws[[diff_col_nm]])
    )
    d$level <- factor(d$level, levels = c("difference", "control", "causal"))
    d
  }

  between <- slab_df(
    paste0(prefix, "_t1_group_causal"), paste0(prefix, "_t1_group_control"), paste0(prefix, "_t1_diff")
  )
  within <- slab_df(
    paste0(prefix, "_t2_change_causal"), paste0(prefix, "_t2_change_control"), paste0(prefix, "_t2_change_diff")
  )

  # --- raw within-person scatter: delta-parameter (posterior mean/participant) vs. delta-symptom (z) ---
  # extract_indiv_pars() already returns group as "control"/"causal"
  indiv_pars <- extract_indiv_pars(
    draws |> dplyr::select(tidyselect::all_of(delta_cols)), stan_ls, index_order = "id"
  ) |>
    dplyr::filter(variable == paste0("delta_", mediator_par))

  symp_mat <- stan_ls[[symptom_key]]
  symp_delta <- tibble::tibble(
    id = seq_len(nrow(symp_mat)),
    delta_symptom = ifelse(symp_mat[, 1] == -999 | symp_mat[, 2] == -999, NA, symp_mat[, 2] - symp_mat[, 1])
  )

  scatter_df <- indiv_pars |>
    dplyr::select(id, group, delta_par = mean, delta_par_sd = sd) |>
    dplyr::left_join(symp_delta, by = "id") |>
    tidyr::drop_na(delta_symptom)

  list(
    scatter_panel = plot_indiv_change_scatter(
      scatter_df, param_nm = param_nm, symptom_lab = paste0("Δ ", symptom_nm),
      xlim = scatter_xlim, ylim = scatter_ylim, x_n_breaks = scatter_n_breaks,
      suppress_labels = suppress_labels, grp_cols = grp_cols, fnt_sz = fnt_sz
    ),
    between_panel = plot_change_slabs(
      between, xlim = between_xlim,
      x_lab      = paste0(param_nm, " → ", symptom_nm, " (baseline)"),
      grp_cols   = grp_cols, fnt_sz = fnt_sz, diff_comp = diff_lab,
      grp_shapes = grp_shapes,
      interval_size_range = interval_size_range, fatten_point = fatten_point,
      x_n_breaks = x_n_breaks,
      flip_order = flip_order, suppress_labels = suppress_labels,
      subtitle   = "between-person (baseline) association"
    ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60"),
    within_panel = plot_change_slabs(
      within, xlim = within_xlim,
      x_lab      = paste0("Δ ", param_nm, " → Δ ", symptom_nm),
      grp_cols   = grp_cols, fnt_sz = fnt_sz, diff_comp = diff_lab,
      grp_shapes = grp_shapes,
      interval_size_range = interval_size_range, fatten_point = fatten_point,
      x_n_breaks = x_n_breaks,
      flip_order = flip_order, suppress_labels = suppress_labels,
      subtitle = NULL
    )
  )
}

## Questionnaire ANCOVA: pre-registered primary analysis ==========================
## final score ~ group + baseline, in brms, on raw scores; priors scaled per measure by sd(t2)

# per-measure constants, always from the ITT frame (reused for every frame). sd_t2 is both the
# prior scale and the standardising divisor (not sd(t1): baseline is range-restricted)
ancova_constants <- function(dat, prefix) {
  t1 <- dat[[paste0(prefix, "_total_t1")]]
  t2 <- dat[[paste0(prefix, "_total_t2")]]
  keep <- !is.na(t1) & !is.na(t2)
  stopifnot(sum(keep) > 0)
  sd_t1 <- stats::sd(t1[keep])
  sd_t2 <- stats::sd(t2[keep])
  list(
    t1_mean = mean(t1[keep]), t2_mean = mean(t2[keep]),
    sd_t1 = sd_t1, sd_t2 = sd_t2,
    # d_av denominator: once per measure over both arms (descriptive only)
    sd_av = (sd_t1 + sd_t2) / 2,
    n = sum(keep)
  )
}

# modelling frame for one measure x analysis frame. complete cases only (an ANCOVA needs
# both timepoints); `t1_mean` comes from ancova_constants() on ITT, never recomputed here.
make_ancova_df <- function(dat, prefix, t1_mean, extra_cols = NULL) {
  d <- data.frame(
    subID = dat$subID,
    group = factor(as.character(dat$group), levels = c(ARM_REF, ARM_INT)),
    t1    = dat[[paste0(prefix, "_total_t1")]],
    t2    = dat[[paste0(prefix, "_total_t2")]]
  )
  for (nm in extra_cols) d[[nm]] <- dat[[nm]]
  d <- d[stats::complete.cases(d), ]
  d$t1_c <- d$t1 - t1_mean
  stopifnot(nrow(d) > 0, levels(d$group)[1] == ARM_REF, ARM_INT %in% levels(d$group))
  d
}

# long frame for the cLDA: keeps participants missing t2. `post` must be numeric 0/1 -- with
# `~ post + post:group` the group term is 0 at baseline (shared baseline mean); a factor time
# variable would silently restore a free baseline arm difference
make_clda_df <- function(dat, prefix) {
  base <- data.frame(
    subID = dat$subID,
    group = factor(as.character(dat$group), levels = c(ARM_REF, ARM_INT))
  )
  d <- rbind(
    cbind(base, post = 0L, score = dat[[paste0(prefix, "_total_t1")]]),
    cbind(base, post = 1L, score = dat[[paste0(prefix, "_total_t2")]])
  )
  d <- d[!is.na(d$score), ]
  # factor twin of `post`, used by unstr(), sigma ~ 0 + tf and the gls varIdent()
  d$tf <- factor(d$post, levels = c(0L, 1L), labels = c("t1", "t2"))
  stopifnot(nrow(d) > 0, levels(d$group)[1] == ARM_REF, all(d$post %in% 0:1))
  d
}

# posterior draws for a sum of named coefficients -- every reported quantity is one or two
# coefficients (e.g. causal-arm sessions slope = b_sessions_c + b_group{INT}:sessions_c)
coef_draws <- function(fit, ...) {
  nms <- c(...)
  m <- posterior::as_draws_matrix(fit$fit)
  stopifnot(all(nms %in% colnames(m)))
  as.vector(rowSums(m[, nms, drop = FALSE]))
}

# the causal - control arm effect (the group coefficient), as list(contrast = ...)
ancova_contrast_draws <- function(fit) {
  v <- coef_draws(fit, paste0("b_group", ARM_INT))
  list(contrast = tibble::tibble(.draw = seq_along(v), .value = v))
}

# one row per fit: raw-point effect (reported) plus d_sd_t2 (divided by the constant sd(t2)) and
# d_sigma (draw-wise residual sigma, for comparison). mean + hdi, as in the figures.
# `rope_thresh` (b/SD_t2 units): one-sided P(d > -rope_thresh), the posterior share short of the
# pre-registered directional target; NULL gives NA
summarise_ancova_draws <- function(fit, contrast_draws, const, measure, frame, n_obs, rope_thresh = NULL) {
  v <- contrast_draws$.value
  sg <- as.vector(posterior::as_draws_matrix(fit$fit)[, "sigma"])
  h95 <- bayestestR::hdi(v, ci = 0.95)
  h90 <- bayestestR::hdi(v, ci = 0.90)
  d95 <- bayestestR::hdi(v / const$sd_t2, ci = 0.95)
  rope_pct <- if (is.null(rope_thresh)) {
    NA_real_
  } else {
    mean(v / const$sd_t2 > -rope_thresh)
  }

  np <- brms::nuts_params(fit)
  dg <- posterior::summarise_draws(
    posterior::subset_draws(posterior::as_draws_df(fit$fit), variable = c("b_", "sigma"), regex = TRUE),
    "rhat", "ess_bulk", "ess_tail"
  )

  tibble::tibble(
    measure = measure, frame = frame, n = n_obs, sd_t2 = const$sd_t2,
    est = mean(v), lo95 = h95$CI_low, hi95 = h95$CI_high,
    lo90 = h90$CI_low, hi90 = h90$CI_high,
    d_sd_t2 = mean(v) / const$sd_t2, d_lo95 = d95$CI_low, d_hi95 = d95$CI_high,
    d_sigma = mean(v / sg),
    pd = as.numeric(bayestestR::p_direction(v, method = "direct")),
    rope_thresh = rope_thresh %||% NA_real_, rope_pct = rope_pct,
    rhat_max = max(dg$rhat), ess_bulk_min = min(dg$ess_bulk), ess_tail_min = min(dg$ess_tail),
    divergent = sum(np$Value[np$Parameter == "divergent__"])
  )
}

# forest of standardised arm-effect posteriors, primary tier above secondary. `draws_df` is long:
# measure (factor, top -> bottom), tier, .value (standardised). `dodge_by` dodges series within a
# row; `colour_by` defaults to measure (pass the dodge column for arm/difference series).
# slab+interval for a single series, pointinterval when dodged
ANCOVA_WIDTHS <- c(0.95, 0.99)

plot_ancova_forest <- function(draws_df, labels, measure_cols, dodge_by = NULL, tag = "G",
                               xlab = NULL, base_size = 18, dodge_w = 0.55,
                               colour_by = NULL, colour_values = NULL,
                               dodge_shapes = NULL, dodge_alphas = NULL, dodge_labels = NULL,
                               tier_labels = c("primary", "secondary")) {
  # `tier_labels` names the two blocks either side of the rule
  stopifnot(length(tier_labels) == 2)
  n_sec <- draws_df |>
    dplyr::distinct(measure, tier) |>
    dplyr::filter(tier == "secondary") |>
    nrow()

  clr <- colour_by %||% "measure"
  clr_vals <- colour_values %||% measure_cols
  # when the dodge is the colour, colour/shape/alpha share one legend; otherwise colour is hidden
  clr_in_legend <- !is.null(dodge_by) && identical(clr, dodge_by)

  p <- ggplot2::ggplot(draws_df, ggplot2::aes(x = .value, y = measure, colour = .data[[clr]])) +
    ggplot2::geom_vline(xintercept = 0, linetype = "32", colour = "grey50")

  if (is.null(dodge_by)) {
    p <- p + ggdist::stat_pointinterval(
      .width = ANCOVA_WIDTHS, point_interval = "mean_hdci", interval_size_range = c(1, 3), fatten_point = 2
    ) +
      ggplot2::scale_fill_manual(values = clr_vals, guide = "none")
  } else {
    n_lvl <- nlevels(draws_df[[dodge_by]])
    # n == 2 values are pinned rather than derived so the ITT-vs-PP panel is unchanged by
    # the generalisation; 3+ steps down from opaque without fading any series to illegible
    shapes <- dodge_shapes %||% c(16, 21, 15, 17, 18)[seq_len(n_lvl)]
    alphas <- dodge_alphas %||% if (n_lvl == 2) c(1, 0.6) else seq(1, 0.65, length.out = n_lvl)
    labs_d <- dodge_labels %||% levels(draws_df[[dodge_by]])

    # reverse = TRUE so the FIRST level sits on top within each measure row, matching the
    # primary-first ordering used everywhere else
    p <- p + ggdist::stat_pointinterval(
      ggplot2::aes(shape = .data[[dodge_by]], alpha = .data[[dodge_by]]),
      .width = ANCOVA_WIDTHS, point_interval = "mean_hdci",
      point_size = 3, interval_size_range = c(1, 3), fatten_point = 2,
      position = ggplot2::position_dodge(width = dodge_w, reverse = TRUE)
    ) +
      ggplot2::scale_shape_manual(values = shapes, name = NULL, labels = labs_d) +
      ggplot2::scale_alpha_manual(values = alphas, name = NULL, labels = labs_d)
  }

  # tier rule sits between the secondary block (bottom) and the primary block (top)
  p <- p +
    ggplot2::geom_hline(yintercept = n_sec + 0.5, linetype = "solid", colour = "grey75", linewidth = 0.5) +
    ggplot2::annotate(
      "text", x = Inf, y = n_sec + 0.62, label = tier_labels[[1]], hjust = 1.05, vjust = 0,
      family = "Open Sans", size = 3.9, colour = "grey45", fontface = "italic"
    ) +
    ggplot2::annotate(
      "text", x = Inf, y = n_sec + 0.38, label = tier_labels[[2]], hjust = 1.05, vjust = 1,
      family = "Open Sans", size = 3.9, colour = "grey45", fontface = "italic"
    ) +
    ggplot2::scale_y_discrete(labels = labels, expand = ggplot2::expansion(add = 0.7)) +
    ggplot2::labs(x = xlab, y = NULL, tag = tag) +
    ggplot2::coord_cartesian(clip = "off")

  p <- p + if (clr_in_legend) {
    ggplot2::scale_colour_manual(values = clr_vals, name = NULL, labels = labs_d)
  } else {
    ggplot2::scale_colour_manual(values = clr_vals, guide = "none")
  }

  p +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = base_size) +
    ggplot2::theme(
      plot.tag     = ggplot2::element_text(face = "bold", size = base_size + 6, colour = "black"),
      axis.title.x = ggtext::element_markdown(size = 16, lineheight = 1.15),
      axis.text.y  = ggtext::element_markdown(),
      legend.position = if (is.null(dodge_by)) "none" else "bottom",
      legend.justification.bottom = "center",
      plot.margin  = ggplot2::margin(2, 30, 2, 10)
    )
}

# per-participant standardised change, all measures on one axis, dodged by arm (beeswarm + mean
# and 95% ci, so it reads as observed data). `df` is long: measure, group, .value (standardised by
# the caller). `note` is optional richtext at `note_pos` ("tr"/"br"); `show_values` prints group
# means; `raw_change` (tibble(measure, label)) adds a pooled per-measure label
plot_change_distributions <- function(df, labels, tag = "A", ylab = NULL, base_size = 18,
                                      grp_cols = ARM_COLS, grp_labs = ARM_LABS,
                                      grp_shapes = ARM_SHP, dodge_w = 0.6, note = NULL,
                                      note_pos = c("tr", "br"),
                                      show_values = FALSE, value_digits = 2, value_nudge = 0.28,
                                      raw_change = NULL, raw_change_nudge = 0.55) {
  note_pos <- match.arg(note_pos)
  dsumm <- df |>
    dplyr::group_by(measure, group) |>
    dplyr::summarise(
      mean_chg = mean(.value), n = dplyr::n(), sd_chg = stats::sd(.value),
      se = sd_chg / sqrt(n), ci = stats::qt(0.975, n - 1) * se, .groups = "drop"
    )

  pd <- ggplot2::position_dodge(width = dodge_w)

  p <- ggplot2::ggplot(dsumm, ggplot2::aes(x = measure, y = mean_chg, colour = group)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "32", colour = "grey50") +
    ggplot2::geom_point(
      data = df, ggplot2::aes(x = measure, y = .value, colour = group, shape = group),
      position = ggbeeswarm::position_quasirandom(width = 0.13, dodge.width = dodge_w),
      alpha = 0.18, size = 1.3, show.legend = FALSE
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_chg - ci, ymax = mean_chg + ci),
      position = pd, width = 0.14, linewidth = 0.9
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = group), position = pd, size = 3) +
    ggplot2::scale_colour_manual(values = grp_cols, labels = grp_labs, name = NULL) +
    ggplot2::scale_shape_manual(values = grp_shapes, labels = grp_labs, name = NULL) +
    ggplot2::scale_x_discrete(labels = labels) +
    # headroom at the top for the value / raw-change labels, plus extra on whichever side
    # `note` sits; the corner it is given is empty in the data either way
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(if (!is.null(note) && note_pos == "br") 0.12 else 0.05, 0.14))
    ) +
    ggplot2::labs(x = NULL, y = ylab, tag = tag)

  # values from `dsumm`, so they match the plotted points
  if (show_values) {
    p <- p + ggtext::geom_richtext(
      data = dsumm,
      ggplot2::aes(
        x = measure, y = mean_chg + ci + value_nudge, colour = group,
        label = formatC(mean_chg, format = "f", digits = value_digits, flag = "+")
      ),
      position = pd, inherit.aes = FALSE, show.legend = FALSE,
      size = 5, family = "Open Sans", fontface = "plain", lineheight = 1.2,
      fill = grDevices::adjustcolor("white", alpha.f = 0.72), label.color = NA,
      label.padding = grid::unit(c(0.06, 0.12, 0.06, 0.12), "lines"),
      label.r = grid::unit(0.1, "lines")
    )
  }

  if (!is.null(raw_change)) {
    # above the taller of the group labels / cis
    rc_y <- dsumm |>
      dplyr::group_by(measure) |>
      dplyr::summarise(y = max(mean_chg + ci), .groups = "drop") |>
      dplyr::mutate(y = y + (if (show_values) value_nudge else 0) + raw_change_nudge)
    rc <- dplyr::left_join(raw_change, rc_y, by = "measure")

    p <- p + ggtext::geom_richtext(
      data = rc, ggplot2::aes(x = measure, y = y, label = label), inherit.aes = FALSE,
      size = 3.8, family = "Open Sans", fontface = "italic", colour = "grey30", lineheight = 1.15,
      fill = grDevices::adjustcolor("white", alpha.f = 0.72), label.color = NA,
      label.padding = grid::unit(c(0.06, 0.12, 0.06, 0.12), "lines"),
      label.r = grid::unit(0.1, "lines")
    )
  }

  if (!is.null(note)) {
    p <- p + ggtext::geom_richtext(
      data = data.frame(x = Inf, y = if (note_pos == "br") -Inf else Inf, label = note),
      ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE,
      hjust = 1, vjust = if (note_pos == "br") -0.5 else 1.5, fill = NA, label.color = "slategrey",
      label.padding = grid::unit(rep(0.2, 4), "lines"), label.r = grid::unit(0.4, "lines"),
      colour = "grey20", lineheight = 1.2, size = 4.5, family = "Open Sans"
    )
  }

  p +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = base_size) +
    cowplot::background_grid(major = "y", minor = "none") +
    ggplot2::theme(
      plot.tag        = ggplot2::element_text(face = "bold", size = base_size + 6, colour = "black"),
      axis.title.y    = ggtext::element_markdown(size = 16),
      axis.text.x     = ggtext::element_markdown(),
      legend.position = "bottom",
      legend.justification.bottom = "center",
      plot.margin     = ggplot2::margin(2, 10, 2, 10)
    )
}

# sessions completed, by arm (why the continuous H2 slope is weakly identified)
plot_sessions_histogram <- function(df, tag = "E", base_size = 18,
                                    grp_cols = ARM_COLS, grp_labs = ARM_LABS) {
  ggplot2::ggplot(df, ggplot2::aes(x = factor(sessions_completed), fill = group)) +
    ggplot2::geom_bar(position = ggplot2::position_dodge(preserve = "single"), width = 0.75) +
    ggplot2::scale_fill_manual(values = grp_cols, labels = grp_labs, name = NULL) +
    ggplot2::labs(x = "training sessions completed", y = "participants", tag = tag) +
    cowplot::theme_half_open(font_family = "Open Sans", font_size = base_size) +
    cowplot::background_grid(major = "y", minor = "none") +
    ggplot2::theme(
      plot.tag        = ggplot2::element_text(face = "bold", size = base_size + 6, colour = "black"),
      legend.position = "bottom",
      plot.margin     = ggplot2::margin(2, 10, 2, 10)
    )
}

get_hdi_pd <- function(draws, var) {
  idx <- draws[[var]]
  mean_idx <- mean(idx)
  hdi_idx <- paste0(
    "[",
    signif(bayestestR::hdi(idx, ci = 0.95)[[2]], 4),
    ", ",
    signif(bayestestR::hdi(idx, ci = 0.95)[[3]], 4),
    "]"
  )
  pd_idx <- bayestestR::p_direction(idx, method = "direct")[[2]]
  print(
    paste0(
      var, ": mean = ", signif(mean_idx, 3),
      ", 95\\% HDI = ", hdi_idx,
      ", p_direction = ", signif(pd_idx, 3)
    )
  )
}

# reliability (lambda = tau^2 / (tau^2 + sigma^2)) of an individual-level measurement from its
# posterior summaries: var(m) / mean(v) = tau^2 / sigma^2 in a normal-normal hierarchy, so
# lambda = ratio / (1 + ratio). approximate (assumes homogeneous lambda)
reliability <- function(hat, se) {
  ratio <- var(hat) / mean(se^2)
  ratio / (1 + ratio)
}

# tidy (mean, 95% HDI, pd) summary of every unique off-diagonal entry of a k x k
# correlation-matrix parameter's draws (e.g. var = "R_theta_neg", with columns named
# "R_theta_neg[i,j]" in `draws`). `labels` names the k row/col indices. One row per
# unique pair (upper triangle, i < j) -- the diagonal (always 1) is dropped.
get_corr_matrix_tbl <- function(draws, var, labels) {
  pairs <- utils::combn(length(labels), 2)
  rows <- lapply(seq_len(ncol(pairs)), function(p) {
    i <- pairs[1, p]
    j <- pairs[2, p]
    v <- draws[[sprintf("%s[%d,%d]", var, i, j)]]
    hdi <- bayestestR::hdi(v, ci = 0.95)
    tibble::tibble(
      var = var, label_i = labels[i], label_j = labels[j],
      mean = mean(v), hdi_95_lo = hdi$CI_low, hdi_95_hi = hdi$CI_high,
      pd = as.numeric(bayestestR::p_direction(v, method = "direct"))
    )
  })
  dplyr::bind_rows(rows)
}

# per-draw correlation across participants between two per-participant quantities (propagates
# their uncertainty, unlike correlating posterior means). `ids` restricts to a subset (e.g. one arm)
get_draws_corr_raw <- function(draws, var_x, var_y, ids = NULL) {
  extract_sorted <- function(v) {
    cols <- grep(paste0("^", v, "\\[\\d+\\]$"), names(draws), value = TRUE)
    col_ids <- as.integer(sub(".*\\[(\\d+)\\]$", "\\1", cols))
    ord <- order(col_ids)
    cols <- cols[ord]
    col_ids <- col_ids[ord]
    if (!is.null(ids)) cols <- cols[col_ids %in% ids]
    as.matrix(draws[, cols])
  }
  x <- extract_sorted(var_x)
  y <- extract_sorted(var_y)
  stopifnot(ncol(x) == ncol(y), ncol(x) > 0)
  n <- ncol(x)
  sx <- rowSums(x)
  sy <- rowSums(y)
  sxx <- rowSums(x^2)
  syy <- rowSums(y^2)
  sxy <- rowSums(x * y)
  (n * sxy - sx * sy) / sqrt((n * sxx - sx^2) * (n * syy - sy^2))
}

# mean/95% HDI/pd summary of a raw per-draw statistic (e.g. from get_draws_corr_raw(), or
# the elementwise difference of two such vectors, to test whether an association differs
# credibly between two subsets).
summarise_draws_r <- function(r) {
  hdi <- bayestestR::hdi(r, ci = 0.95)
  tibble::tibble(
    mean = mean(r), hdi_95_lo = hdi$CI_low, hdi_95_hi = hdi$CI_high,
    pd = as.numeric(bayestestR::p_direction(r, method = "direct"))
  )
}

get_draws_corr <- function(draws, var_x, var_y, ids = NULL) {
  summarise_draws_r(get_draws_corr_raw(draws, var_x, var_y, ids = ids))
}