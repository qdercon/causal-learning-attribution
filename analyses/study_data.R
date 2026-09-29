# Data loading ===================================================================
# single source of truth for data loading and the canonical prep_data() calls: every
# figure / modelling script sources this file, so all fits share one id <-> subID map.
# run from the repo root. data are pseudonymised (prolific ids replaced by sub_NNNNN).

source("analyses/model_fns.R")

## 0. raw data ---------------------------------------------------------------------
read_data <- function(f) readr::read_csv(file.path("data", f), col_types = readr::cols())
causal  <- read_data("causal_training/causal-training-task-data-long_scrambled.csv")
control <- read_data("control_training/control-training-task-data-long_scrambled.csv")
caus_attr_t1 <- read_data("prescreen/causal-attr-task-data-long-pre_scrambled.csv")
caus_attr_t2 <- read_data("postscreen/causal-attr-task-data-long-post_scrambled.csv")
self_report_df <- read_data("self-report-combined-data_scrambled.csv")
qqnrs_t2 <- read_data("qqnrs_t2_scrambled.csv")

# researcher's own prolific test account (not a participant): session-4 rows in both arms
prolific_test_subid <- "sub_45092"
causal  <- causal  |> dplyr::filter(subID != prolific_test_subid)
control <- control |> dplyr::filter(subID != prolific_test_subid)

## 1. canonical prep_data() calls ---------------------------------------------------
ids_attr <- prep_data(
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  self_report = self_report_df, filter_by_catch = TRUE, ret = "ids"
) |>
  dplyr::mutate(group = ifelse(condition01 == 1, ARM_INT, ARM_REF))

stan_ls_attr <- prep_data(
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  self_report = self_report_df, filter_by_catch = TRUE, ret = "stan"
)

ids_learn_s1 <- prep_data(
  learning = list(control = control, causal = causal),
  learning_session = 1, learning_blocks = 1:3,
  self_report = self_report_df, filter_by_catch = TRUE, ret = "ids"
) |>
  dplyr::mutate(group = ifelse(condition01 == 1, ARM_INT, ARM_REF))

stan_ls_learn_s1 <- prep_data(
  learning = list(control = control, causal = causal),
  learning_session = 1, learning_blocks = 1:3,
  self_report = self_report_df, filter_by_catch = TRUE, ret = "stan"
)
# the all-blocks learning models need plain nBlocks (prep_data() only returns nBlocks_l)
stan_ls_learn_s1$nBlocks <- max(rowSums(stan_ls_learn_s1$new_block))

ids_joint <- prep_data(
  learning = list(control = control, causal = causal),
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  learning_session = 1, learning_blocks = 1:3,
  self_report = self_report_df, filter_by_catch = TRUE, ret = "ids"
) |>
  dplyr::mutate(group = ifelse(condition01 == 1, ARM_INT, ARM_REF))

stan_ls_joint <- prep_data(
  learning = list(control = control, causal = causal),
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  learning_session = 1, learning_blocks = 1:3,
  self_report = self_report_df, filter_by_catch = TRUE, ret = "stan"
)

stan_ls_symp_das <- prep_data(
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  self_report = self_report_df, symptom_scale = "DAS_total",
  filter_by_catch = TRUE, ret = "stan"
)
stan_ls_symp_phq <- prep_data(
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  self_report = self_report_df, symptom_scale = "PHQ9_total",
  filter_by_catch = TRUE, ret = "stan"
)
# the lists above are scaled by sd(t1) (superseded fits); those below by sd(t2), as used by
# the figure 4 mediation fits -- t1 phq-9 is range-restricted by the screening criterion
stan_ls_symp_phq_sdt2 <- prep_data(
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  self_report = self_report_df, symptom_scale = "PHQ9_total",
  filter_by_catch = TRUE, z_sd_ref = "t2", ret = "stan"
)
stan_ls_symp_daq_sdt2 <- prep_data(
  attribution = list(t1 = caus_attr_t1, t2 = caus_attr_t2),
  self_report = self_report_df, symptom_scale = "DAQ_total",
  filter_by_catch = TRUE, z_sd_ref = "t2", ret = "stan"
)

## 2. lookups -------------------------------------------------------------------
group_lookup <- ids_learn_s1 |> dplyr::select(learn_id, group)

id_lookup <- ids_joint |>
  dplyr::select(id, learn_id, subID, group, completed)

## 3. invariants ------------------------------------------------------------------
causal_sub_ids <- self_report_df |>
  dplyr::filter(interventionCondition == "causal") |>
  dplyr::pull(prolificSubID)

# no participant should have learning-task rows in both arms' csvs
dup_learn_subs <- intersect(
  unique(causal$subID[causal$sessionNo %in% 1:6]),
  unique(control$subID[control$sessionNo %in% 1:6])
)

# symptom-scale stan lists must differ only in `symp`, by a constant rescale
symp_ls_common <- function(x) x[setdiff(names(x), "symp")]
symp_obs <- function(x) x$symp[x$symp != -999]
phq_ratio <- symp_obs(stan_ls_symp_phq) / symp_obs(stan_ls_symp_phq_sdt2)

stopifnot(
  nrow(ids_attr) == 361,
  identical(symp_ls_common(stan_ls_symp_phq_sdt2), symp_ls_common(stan_ls_symp_das)),
  identical(symp_ls_common(stan_ls_symp_daq_sdt2), symp_ls_common(stan_ls_symp_das)),
  identical(symp_ls_common(stan_ls_symp_phq), symp_ls_common(stan_ls_symp_das)),
  identical(stan_ls_symp_phq$symp == -999, stan_ls_symp_phq_sdt2$symp == -999),
  length(unique(round(phq_ratio, 8))) == 1L,
  identical(ids_attr$id, seq_len(nrow(ids_attr))),
  all(stan_ls_attr$condition %in% 0:1),
  all(ids_attr$condition01[ids_attr$subID %in% causal_sub_ids] == 1L),
  sum(stan_ls_joint$has_learning_data) == sum(stan_ls_joint$nT_ppts_l > 0),
  all((stan_ls_joint$has_learning_data == 1) == (stan_ls_joint$nT_ppts_l > 0)),
  # everyone flagged completed must have t2 attribution trials
  sum(stan_ls_attr$completed == 1 & stan_ls_attr$nT_ppts[, 2] == 0) == 0,
  length(dup_learn_subs) == 0
)
