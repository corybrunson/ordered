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

# untidy model (default lambda path to avoid convergence issues)
house_mod <- glmnetcr::glmnetcr(
  house_train_vars,
  y = house_train$Sat
)
house_mod
# predict.glmnetcr() returns a list (class, probs, BIC, etc.), not a factor
pred_all <- predict(house_mod, newx = house_test_vars)
pred_bic <- which.min(pred_all$BIC)
tibble::tibble(Sat.pred = pred_all$class[, pred_bic]) |>
  mutate(Sat.pred = ordered(Sat.pred, levels(house_train$Sat))) |>
  bind_cols(house_test) |>
  group_by(across(everything())) |> count() |> ungroup() |>
  print(n = 12)

# tunable model & analysis specification
# NB: The recipe must include categorical variable encoding because the tuners
# rely on `fit_xy()` rather than on `fit()` and therefore do not call
# `convert_form_to_xy_fit()`.
house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train) |>
  step_dummy(all_factor_predictors(), one_hot = TRUE)
house_spec <- ordinal_reg() |>
  set_engine("glmnetcr") |>
  set_args(penalty = tune(), mixture = tune())
house_tune <- extract_parameter_set_dials(house_spec)
(house_grid <- grid_regular(house_tune, levels = c(3, 3)))

# prepped recipe
house_prep <- prep(house_rec)
# baked training data
house_train_baked <- bake(house_prep, new_data = house_train)
# translation
house_trans <- translate(house_spec)
house_spec |>
  set_args(penalty = 1, mixture = .5) |>
  translate() |>
  print() -> house_trans_args
# fitted model
house_fit <- house_spec |>
  set_args(penalty = 1, mixture = .5) |>
  fit(formula(house_prep), data = house_train_baked) |>
  print()
# prediction
predict(house_fit, new_data = head(house_train_baked), type = "class")
predict(house_fit, new_data = head(house_train_baked), type = "prob")
# prediction with case weights
set.seed(1)
wts <- hardhat::importance_weights(runif(nrow(house_train_baked), 0.5, 2))
house_fit_wt <- house_spec |>
  set_args(penalty = 1, mixture = .5) |>
  fit(formula(house_prep), data = house_train_baked, case_weights = wts) |>
  print()
predict(house_fit_wt, new_data = head(house_train_baked), type = "class")

warning("Up to here, code should work with or without submodel support.")

# multi_predict ----------------------------------------------------------------

# class predictions at multiple penalty values
multi_class <- multi_predict(
  house_fit,
  new_data = head(house_train_baked),
  type = "class",
  penalty = c(0.5, 1, 5)
)
print(multi_class)
# prob predictions at multiple penalty values
multi_prob <- multi_predict(
  house_fit,
  new_data = head(house_train_baked),
  type = "prob",
  penalty = c(0.5, 1, 5)
)
print(multi_prob)

# hyperparameter optimization --------------------------------------------------

# use a moderate grid to avoid convergence issues at extreme penalties
house_mod_grid <- tidyr::crossing(
  penalty = c(0.01, 0.1, 1),
  mixture = c(0, 0.5, 1)
)
house_res <- tune_grid(
  house_spec,
  preprocessor = house_rec,
  resamples = vfold_cv(house_train, v = 3),
  grid = house_mod_grid,
  metrics = metric_set(accuracy, roc_auc)
)
(house_best <- select_best(house_res, metric = "accuracy"))

# final fit
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
