library(devtools); library(tidymodels); library(bonsai); load_all()

# reference example:
# https://workflowsets.tidymodels.org/articles/tuning-and-comparing-models.html

# note: partykit models leverage ordinality when responses are ordered factors

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# resampling method
( house_resamples <- vfold_cv(house_train, v = 3, strata = Sat) )

# --- ordinal_reg engines ---

# polr: ordinal_link (5 values)
polr_spec <-
  ordinal_reg() |>
  set_engine("polr") |>
  set_args(ordinal_link = tune())

# vglm: ordinal_link + odds_link
vglm_spec <-
  ordinal_reg() |>
  set_engine("vglm") |>
  set_args(ordinal_link = tune(), odds_link = tune())

# lrm: penalty
lrm_spec <-
  ordinal_reg() |>
  set_engine("lrm") |>
  set_args(penalty = tune())

# orm: ordinal_link + penalty
orm_spec <-
  ordinal_reg() |>
  set_engine("orm") |>
  set_args(ordinal_link = tune(), penalty = tune())

# ordinalNet: penalty (submodel via multi_predict) + mixture +
#   parallel_penalty_factor
ordinalNet_spec <-
  ordinal_reg() |>
  set_engine("ordinalNet") |>
  set_args(
    penalty = tune(), mixture = tune(),
    parallelPenaltyFactor = tune()
  )

# glmnetcr: penalty (submodel via multi_predict) + mixture
glmnetcr_spec <-
  ordinal_reg() |>
  set_engine("glmnetcr") |>
  set_args(penalty = tune(), mixture = tune())

# --- gen_additive_mod engines ---

# vgam: link + family (named differently from ordinal_reg parameter)
vgam_spec <-
  gen_additive_mod() |>
  set_engine("vgam") |>
  set_mode("classification") |>
  set_args(link = tune(), family = tune())

# --- decision_tree engines ---

# rpartScore: split + prune
rpart_spec <-
  decision_tree() |>
  set_engine("rpartScore") |>
  set_mode("classification") |>
  set_args(split = tune(), prune = tune())

# partykit: tree_depth + mincriterion (using ctree via bonsai)
ctree_spec <-
  decision_tree() |>
  set_engine("partykit") |>
  set_mode("classification") |>
  set_args(tree_depth = tune(), mincriterion = tune())

# --- rand_forest engines ---

# ordinalForest: trees
ordfor_spec <-
  rand_forest() |>
  set_engine("ordinalForest") |>
  set_mode("classification") |>
  set_args(trees = tune())

# orf: min_n + honesty (short runtime via small trees)
orf_spec <-
  rand_forest() |>
  set_engine("orf") |>
  set_mode("classification") |>
  set_args(min_n = tune(), trees = 50, honesty = tune())

# partykit: mtry + trees + mincriterion (using cforest via bonsai)
cforest_spec <-
  rand_forest() |>
  set_engine("partykit") |>
  set_mode("classification") |>
  set_args(trees = tune(), mincriterion = tune())

# --- tuning grids (all small to limit runtime) ---

polr_tune <- extract_parameter_set_dials(polr_spec)
( polr_grid <- grid_regular(polr_tune, levels = Inf) )

vglm_tune <- extract_parameter_set_dials(vglm_spec)
( vglm_grid <- grid_regular(vglm_tune, levels = 2) )

lrm_tune <- extract_parameter_set_dials(lrm_spec)
( lrm_grid <- grid_regular(lrm_tune, levels = 2) )

orm_tune <- extract_parameter_set_dials(orm_spec)
( orm_grid <- grid_regular(orm_tune, levels = c(Inf, 2)) )

ordinalNet_tune <- extract_parameter_set_dials(ordinalNet_spec)
# constrain the default c(-Inf, Inf) range for gridding
idx <- which(ordinalNet_tune$name == "parallelPenaltyFactor")
ordinalNet_tune$object[[idx]] <- parallel_penalty_factor(range = c(-1, 1))
( ordinalNet_grid <- grid_regular(ordinalNet_tune, levels = 2) )

glmnetcr_tune <- extract_parameter_set_dials(glmnetcr_spec)
( glmnetcr_grid <- grid_regular(glmnetcr_tune, levels = 2) )

vgam_tune <- extract_parameter_set_dials(vgam_spec)
( vgam_grid <- grid_regular(vgam_tune, levels = 2) )

rpart_tune <- extract_parameter_set_dials(rpart_spec)
( rpart_grid <- grid_regular(rpart_tune, levels = Inf) )

ctree_tune <- extract_parameter_set_dials(ctree_spec)
( ctree_grid <- grid_regular(ctree_tune, levels = 2) )

ordfor_tune <- extract_parameter_set_dials(ordfor_spec)
( ordfor_grid <- grid_regular(ordfor_tune, levels = 2L) )

orf_tune <- extract_parameter_set_dials(orf_spec)
( orf_grid <- grid_regular(orf_tune, levels = 2) )

cforest_tune <- extract_parameter_set_dials(cforest_spec)
( cforest_grid <- grid_regular(cforest_tune, levels = 2) )

# assemble and tune workflow set
workflow_set(
  preproc = list(formula = Sat ~ Infl + Type + Cont),
  models = list(
    polr = polr_spec,
    vglm = vglm_spec,
    lrm = lrm_spec,
    orm = orm_spec,
    ordinalNet = ordinalNet_spec,
    glmnetcr = glmnetcr_spec,
    vgam = vgam_spec,
    rpart = rpart_spec,
    ctree = ctree_spec,
    ordfor = ordfor_spec,
    orf = orf_spec,
    cforest = cforest_spec
  )
) |>
  option_add(grid = polr_grid, id = "formula_polr") |>
  option_add(grid = vglm_grid, id = "formula_vglm") |>
  option_add(grid = lrm_grid, id = "formula_lrm") |>
  option_add(grid = orm_grid, id = "formula_orm") |>
  option_add(grid = ordinalNet_grid, id = "formula_ordinalNet") |>
  option_add(grid = glmnetcr_grid, id = "formula_glmnetcr") |>
  option_add(grid = vgam_grid, id = "formula_vgam") |>
  option_add(grid = rpart_grid, id = "formula_rpart") |>
  option_add(grid = ctree_grid, id = "formula_ctree") |>
  option_add(grid = ordfor_grid, id = "formula_ordfor") |>
  option_add(grid = orf_grid, id = "formula_orf") |>
  option_add(grid = cforest_grid, id = "formula_cforest") |>
  # for `fit_best()`
  option_add(control = control_grid(save_workflow = TRUE)) |>
  workflow_map(
    fn = "tune_grid",
    verbose = TRUE,
    resamples = house_resamples,
    # `brier_class` and `roc_auc` require `type = "prob"`
    metrics = metric_set(accuracy, kap)
  ) |>
  print() -> house_workflows

# rankings
house_workflows |>
  rank_results(rank_metric = "kap")
house_workflows |>
  autoplot(metric = c("accuracy", "kap"))

# best fit
house_workflows |>
  fit_best(metric = "kap") |>
  print() -> house_fit

# evaluations
predict(house_fit, house_test, type = "class") |>
  bind_cols(predict(house_fit, house_test, type = "prob")) |>
  bind_cols(dplyr::select(house_test, Sat)) |>
  print() -> house_pred
roc_auc(house_pred, truth = Sat, starts_with(".pred_"), -.pred_class)
brier_class(house_pred, truth = Sat, starts_with(".pred_"), -.pred_class)
kap(house_pred, truth = Sat, estimate = .pred_class)
# ERROR: Can't combine <ordered<3773d>> and <factor<3773d>>.
# FIXME: Have `yardstick::classification_cost()` preserve orderedness.
house_pred |>
  classification_cost(truth = Sat, starts_with(".pred_"), -.pred_class)
