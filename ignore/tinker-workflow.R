library(devtools); library(tidymodels); load_all()

# reference example:
# https://workflowsets.tidymodels.org/articles/tuning-and-comparing-models.html

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# models to compare
polr_spec <-
  ordinal_reg() |>
  set_engine("polr") |>
  set_args(ordinal_link = tune())
rpart_spec <-
  decision_tree() |>
  set_engine("rpartScore") |>
  set_mode("classification") |>
  set_args(split_func = tune())
ordfor_spec <-
  rand_forest() |>
  set_engine("ordinalForest") |>
  set_mode("classification") |>
  set_args(trees = tune())

# respective tuning grids
polr_tune <- extract_parameter_set_dials(polr_spec)
( polr_grid <- grid_regular(polr_tune, levels = Inf) )
rpart_tune <- extract_parameter_set_dials(rpart_spec)
( rpart_grid <- grid_regular(rpart_tune, levels = Inf) )
ordfor_tune <- extract_parameter_set_dials(ordfor_spec)
( ordfor_grid <- grid_regular(ordfor_tune, levels = 2L) )

# resampling method
( house_resamples <- vfold_cv(house_train, v = 3, strata = Sat) )

# assemble and tune workflow set
workflow_set(
  preproc = list(formula = Sat ~ Infl + Type + Cont),
  models = list(polr = polr_spec, rpart = rpart_spec, ordfor = ordfor_spec)
) |>
  # https://github.com/tidymodels/workflowsets/issues/37
  # FIXME: Allow to use `grid = Inf` and `grid = 2` instead.
  option_add(grid = polr_grid, id = "formula_polr") |>
  option_add(grid = rpart_grid, id = "formula_rpart") |>
  option_add(grid = ordfor_grid, id = "formula_ordfor") |>
  workflow_map(
    fn = "tune_grid",
    verbose = TRUE,
    resamples = house_resamples,
    # `brier_class` and `roc_auc` require `type = "prob"`
    metrics = metric_set(accuracy, kap)
  ) |>
  print() -> house_workflows
house_workflows

# rankings
house_workflows |>
  rank_results(rank_metric = "kap")
house_workflows |>
  autoplot(metric = c("accuracy", "kap"))
house_workflows |>
  autoplot(metric = c("accuracy", "kap"), id = "formula_polr")

# best fit
house_workflows |>
  extract_workflow("formula_polr") |>
  finalize_workflow(parameters = list(ordinal_link = "probit")) |>
  fit(data = house_train) |>
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
