library(devtools); library(tidymodels); load_all()

# reference example:
# https://workflowsets.tidymodels.org/articles/tuning-and-comparing-models.html

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

# ordinalNet: penalty (submodel via multi_predict) + mixture
ordinalNet_spec <-
  ordinal_reg() |>
  set_engine("ordinalNet") |>
  set_args(penalty = tune(), mixture = tune())

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

# rpartScore: split
rpart_spec <-
  decision_tree() |>
  set_engine("rpartScore") |>
  set_mode("classification") |>
  set_args(split = tune())

# --- rand_forest engines ---

# ordinalForest: trees
ordfor_spec <-
  rand_forest() |>
  set_engine("ordinalForest") |>
  set_mode("classification") |>
  set_args(trees = tune())

# orf: excluded due to excessive runtime and "subscript out of bounds"
# prediction warnings; see `orf` package prediction bugs referenced in
# `rand_forest-orf.R`

# --- tuning grids (all small to limit runtime) ---

polr_tune <- extract_parameter_set_dials(polr_spec)
( polr_grid <- grid_regular(polr_tune, levels = Inf) )

vglm_tune <- extract_parameter_set_dials(vglm_spec)
( vglm_grid <- grid_regular(vglm_tune, levels = 2) )

ordinalNet_tune <- extract_parameter_set_dials(ordinalNet_spec)
( ordinalNet_grid <- grid_regular(ordinalNet_tune, levels = 2) )

lrm_tune <- extract_parameter_set_dials(lrm_spec)
( lrm_grid <- grid_regular(lrm_tune, levels = 2) )

orm_tune <- extract_parameter_set_dials(orm_spec)
( orm_grid <- grid_regular(orm_tune, levels = c(Inf, 2)) )

glmnetcr_tune <- extract_parameter_set_dials(glmnetcr_spec)
( glmnetcr_grid <- grid_regular(glmnetcr_tune, levels = 2) )

vgam_tune <- extract_parameter_set_dials(vgam_spec)
( vgam_grid <- grid_regular(vgam_tune, levels = 2) )

rpart_tune <- extract_parameter_set_dials(rpart_spec)
( rpart_grid <- grid_regular(rpart_tune, levels = Inf) )

ordfor_tune <- extract_parameter_set_dials(ordfor_spec)
( ordfor_grid <- grid_regular(ordfor_tune, levels = 2L) )

# assemble and tune workflow set
workflow_set(
  preproc = list(formula = Sat ~ Infl + Type + Cont),
  models = list(
    polr = polr_spec,
    vglm = vglm_spec,
    ordinalNet = ordinalNet_spec,
    lrm = lrm_spec,
    orm = orm_spec,
    glmnetcr = glmnetcr_spec,
    vgam = vgam_spec,
    rpart = rpart_spec,
    ordfor = ordfor_spec
  )
) |>
  option_add(grid = polr_grid, id = "formula_polr") |>
  option_add(grid = vglm_grid, id = "formula_vglm") |>
  option_add(grid = ordinalNet_grid, id = "formula_ordinalNet") |>
  option_add(grid = lrm_grid, id = "formula_lrm") |>
  option_add(grid = orm_grid, id = "formula_orm") |>
  option_add(grid = glmnetcr_grid, id = "formula_glmnetcr") |>
  option_add(grid = vgam_grid, id = "formula_vgam") |>
  option_add(grid = rpart_grid, id = "formula_rpart") |>
  option_add(grid = ordfor_grid, id = "formula_ordfor") |>
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
house_workflows |>
  autoplot(metric = c("accuracy", "kap"), id = "formula_polr")

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
