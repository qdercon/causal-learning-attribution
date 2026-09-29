# causal-learning-attribution

## Data and analysis code for a randomised online study of causal learning training

Participants with mild-to-moderate depressive symptoms were randomised to a causal learning training or a structurally matched control learning training, completed over six online sessions across roughly two weeks. A causal attribution task and a set of questionnaires were completed before (prescreen) and after (postscreen) training. The analyses ask whether causal learning training shifts attributional style, whether changes in attribution mediate changes in symptoms, and whether the learning rate during training predicts attribution change.

Eligibility at prescreen required a PHQ-2 score (the first two PHQ-9 items) of at least 2, and a PHQ-9 score of 5–9, or of 10–14 if item 9 (thoughts of self-harm) was scored 0 or 1.

### This study builds on:

> Norbury, A., Dercon, Q., Hauser, T. U., Dolan, R. J., & Huys, Q. J. M. (2025). Learning training as a cognitive restructuring intervention. *Biological Psychiatry: Cognitive Neuroscience and Neuroimaging*, S2451-9022(25)00136-3. Advance online publication. https://doi.org/10.1016/j.bpsc.2025.04.008

The tasks and interventions are adapted from those in the accompanying repository: [agnesnorbury/cognitive-restructuring-learning](https://github.com/agnesnorbury/cognitive-restructuring-learning).

## Repository structure

| Path | Contents |
| --- | --- |
| `analyses/study_data.R` | loads the data and builds the Stan data lists (sourced by the modelling scripts) |
| `analyses/model_fns.R` | data preparation, modelling and plotting functions (sourced by every script) |
| `analyses/qnr_analysis_complete.R` | questionnaire outcomes: baseline-adjusted (ANCOVA) arm effects, effects of the number of sessions completed, and sensitivity analyses (Figure 2) |
| `analyses/modelling_causal-attr.R` | causal attribution task: model-based change in attribution tendencies, model-free checks, and completer vs. non-completer comparisons (Figure 3) |
| `analyses/modelling_mediation.R` | attribution change as a mediator of the training effect on symptoms, DAQ and PHQ-9 (Figure 4) |
| `analyses/modelling_learning.R` | learning task: learning curves, model comparison, posterior predictive checks and learning-rate estimates (Figure 5) |
| `analyses/learning_attr_assoc.R` | whether the session-1 learning rate predicts attribution change (Figure 5E) |
| `analyses/learning_recovery.R` | parameter and model recovery for the learning models |
| `analyses/stan-models/` | Stan models (attribution, mediation, and the session-1 learning model family) |
| `analyses/setup/` | scripts and stimulus files used to build the training trial sets |
| `analyses/data-download.py` | downloads study data from Firestore (requires your own Firebase credentials) |
| `public/` | web code for the study (built with [jsPsych](https://www.jspsych.org/)) |

`public/` holds a single web app for all parts of the study: the prescreen, the causal and control training sessions, and the postscreen. The version to deploy is selected in `public/js/versionInfo.js`. The Firebase configuration in `public/index.html` and the Prolific completion code in `versionInfo.js` are placeholders, to be replaced with your own.

## Data

All data are provided as `.csv` files ending `_scrambled.csv`. Participant IDs have been replaced with random IDs (`sub_XXXXX`), consistent across all files. Platform identifiers, timestamps and free-text responses have been removed. The random IDs preserve the sort order of the original IDs, so participant indices in the models match the original analysis.

| File | Contents |
| --- | --- |
| `data/prescreen/causal-attr-task-data-long-pre_scrambled.csv` | causal attribution task at prescreen (baseline), trial-level |
| `data/postscreen/causal-attr-task-data-long-post_scrambled.csv` | causal attribution task at postscreen (follow-up), trial-level |
| `data/causal_training/causal-training-task-data-long_scrambled.csv` | causal learning training, trial-level, all six sessions |
| `data/control_training/control-training-task-data-long_scrambled.csv` | control learning training, trial-level, all six sessions |
| `data/self-report-combined-data_scrambled.csv` | questionnaire items, totals and demographics, one row per participant per timepoint (`sessionNo` 0 = prescreen, 1 = postscreen) |
| `data/qqnrs_t2_scrambled.csv` | questionnaire totals at both timepoints and their change, one row per randomised participant |

`sub_45092` is a researcher test account that appears in the training data. It is excluded in `study_data.R`.

## Running the analyses

### Requirements

- [R](https://www.r-project.org/) (tested with 4.5.2)
- [CmdStan](https://mc-stan.org/cmdstanr/) via `cmdstanr` (tested with CmdStan 2.39.0), and [`brms`](https://paul-buerkner.github.io/brms/) (tested with 2.23.0) for the questionnaire and regression models
- the R packages used by the scripts:

```R
install.packages(c(
  "bayestestR", "brms", "cowplot", "data.table", "dplyr", "effectsize", "ggbeeswarm", "ggdist",
  "ggh4x", "ggplot2", "ggpp", "ggtext", "loo", "MetBrewer", "nlme", "patchwork", "posterior",
  "psych", "purrr", "readr", "rlang", "scales", "stringr", "tibble", "tidyr", "tidyselect"
))
install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
cmdstanr::install_cmdstan()
```

Figures use the Open Sans font family, which needs to be installed for them to render as intended. The Python scripts (`data-download.py`, `analyses/setup/`) need `pandas`, `numpy` and `firebase-admin`.

### Order

Run each script from the repository root, which it treats as the working directory. First create the output folders:

```bash
mkdir -p analyses/outputs analyses/stan-fits/attr analyses/stan-fits/learning analyses/stan-fits/qnr
```

1. `qnr_analysis_complete.R`. The mediation script checks its results against this script's output (`analyses/outputs/ancova_itt.csv`).
2. `modelling_causal-attr.R`. It fits the attribution model and saves the draws used by the learning scripts.
3. The learning analyses:
   1. run `modelling_learning.R` up to and including section 3, which caches the model comparison and the winning model's draws;
   2. run `learning_attr_assoc.R` and `learning_recovery.R`;
   3. run the whole of `modelling_learning.R`, which reads tables written by both.

   `learning_recovery.R` refits the winning model to simulated data and takes several hours (roughly 2 hours for parameter recovery and 6 for model recovery). Set `RUN_MODEL_RECOVERY <- FALSE` to skip the model-recovery half.
4. `modelling_mediation.R`.

Model fits are not included in this repository. `qnr_analysis_complete.R`, `modelling_causal-attr.R` and `learning_attr_assoc.R` fit their models by MCMC when no saved fit is found, and `learning_recovery.R` fits the learning models to simulated data. `modelling_learning.R`, `learning_recovery.R` and `modelling_mediation.R` also need previously fitted models, loaded from `analyses/stan-fits/learning/` (`fit_learn-s1-*.rds`, one per learning model) and `analyses/stan-fits/attr/` (`fit_attr_symp_*_sdt2-ancova-singlemed-*.rds`, one per attribution domain and outcome). These are fitted with the corresponding models in `analyses/stan-models/`. Fits and draws are saved to `analyses/stan-fits/`, and tables to `analyses/outputs/`.
