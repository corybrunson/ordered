library(devtools); library(tidymodels); load_all()

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

house_train_vars <- model.matrix(
  Sat ~ Type + Infl + Cont + 0, data = house_train,
  contrasts.arg = lapply(house_train[, 2:4], contrasts, contrasts = FALSE)
)
house_test_vars <- model.matrix(
  Sat ~ Type + Infl + Cont + 0, data = house_test,
  contrasts.arg = lapply(house_test[, 2:4], contrasts, contrasts = FALSE)
)

# untidy model
house_mod <- ordinalNet::ordinalNet(
  # house_train[, -1L],
  house_train_vars,
  y = house_train$Sat,
  lambdaVals = .001
)
house_mod
predict(house_mod, newx = house_test_vars, type = "class") |>
  tibble::enframe(name = NULL, value = "Sat.pred") |>
  bind_cols(house_test) |>
  mutate(Sat.pred = ordered(levels(Sat)[Sat.pred], levels(Sat))) |>
  group_by(across(everything())) |> count() |> ungroup() |>
  print(n = 12)

# tunable model & analysis specification
# NB: The recipe must include categorical variable encoding because the tuners
# rely on `fit_xy()` rather than on `fit()` and therefore do not call
# `convert_form_to_xy_fit()`.
# house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train)
house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train) |>
  step_dummy(all_factor_predictors(), one_hot = TRUE)
house_spec <- ordinal_reg() |>
  set_engine("ordinalNet") |>
  set_args(ordinal_link = tune(), penalty = tune(), mixture = tune())
house_tune <- extract_parameter_set_dials(house_spec)
# (house_grid <- grid_regular(house_tune, levels = c(Inf, 3, 3)))
(house_grid <- grid_regular(house_tune, levels = c(3, 2, 2)))

# prepped recipe
house_prep <- prep(house_rec)
# baked training data
house_train_baked <- bake(house_prep, new_data = house_train)
# translation
house_trans <- translate(house_spec)
house_spec |>
  set_args(ordinal_link = "logistic", penalty = .001, mixture = .5) |>
  translate() |>
  print() -> house_trans_args
# fitted model
house_spec |>
  set_args(ordinal_link = "logistic", penalty = .001, mixture = .5) |>
  # fit(house_prep, data = house_train) |>
  # fit(formula(house_prep), data = house_train) |>
  fit(formula(house_prep), data = house_train_baked) |>
  print() -> house_fit
# prediction
predict(house_fit, new_data = head(house_train_baked), type = "class")
predict(house_fit, new_data = head(house_train_baked), type = "prob")
# prediction with specified penalty
house_fit |>
  predict(new_data = head(house_train_baked), type = "prob", penalty = .001)
house_fit |>
  predict(new_data = head(house_train_baked), type = "class", penalty = .01)
# prediction outside specified range
house_fit$fit$lambdaVals |> range()
house_fit |>
  predict(new_data = head(house_train_baked), type = "prob", penalty = 0)
house_fit |>
  predict(new_data = head(house_train_baked), type = "class", penalty = 1)

warning("Up to here, code should work with or without submodel support.")

# multiple model fitting
house_spec |>
  set_args(
    ordinal_link = "logistic",
    path_values = c(.01, .001),
    penalty = 1,
    mixture = .5
  ) |>
  # translate()
  fit(formula(house_prep), data = house_train_baked) |>
  print() -> house_multi_fit
# WARNING: `penalty` must be specified even though not used.
house_spec |>
  set_args(
    ordinal_link = "logistic",
    path_values = c(.01, .001),
    mixture = .5
  ) |>
  # translate()
  fit(formula(house_prep), data = house_train_baked) |>
  print() -> house_multi_fit

# multiple prediction
house_fit |>
  multi_predict(new_data = head(house_train_baked), type = "prob",
                penalty = c(.1, .01, .001))
house_fit |>
  multi_predict(new_data = head(house_train_baked), type = "class",
                penalty = c(.1, .01, .001))

# hyperparameter optimization
house_res <- tune_grid(
  house_spec,
  preprocessor = house_rec,
  resamples = vfold_cv(house_train, v = 3),
  grid = house_grid,
  metrics = metric_set(accuracy, roc_auc)
)
(house_best <- select_best(house_res, metric = "accuracy"))

# final fit
house_prep <- prep(house_rec)
house_final <- finalize_model(house_spec, house_best)
house_final_fit <- fit(
  house_final, formula(house_prep),
  data = house_train_baked
)
house_final_fit

# baked testing data
house_test_baked <- bake(house_prep, new_data = house_test)
# evaluation
house_pred_class <-
  predict(house_final_fit, new_data = house_test_baked, type = "class")
bind_cols(house_test, house_pred_class) |>
  accuracy(truth = Sat, estimate = .pred_class)
house_pred_prob <-
  predict(house_final_fit, new_data = house_test_baked, type = "prob")
bind_cols(house_test, house_pred_prob) |>
  roc_auc(truth = Sat, starts_with(".pred_"))
