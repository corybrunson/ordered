library(devtools); library(tidymodels); load_all()

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# untidy model -- lrm
house_mod_lrm <- rms::lrm(
  formula = Sat ~ Infl + Type + Cont,
  data = house_train,
  penalty = .01
)
house_mod_lrm
predict(house_mod_lrm, newdata = house_test, type = "fitted.ind") |>
  tibble::as_tibble()

# untidy model -- orm
house_mod_orm <- rms::orm(
  formula = Sat ~ Infl + Type + Cont,
  data = house_train,
  family = "logistic",
  penalty = .01
)
house_mod_orm
predict(house_mod_orm, newdata = house_test, type = "fitted.ind") |>
  tibble::as_tibble()

# tunable model & analysis specification
house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train)

# lrm engine
house_spec_lrm <- ordinal_reg() |>
  set_engine("lrm") |>
  set_args(penalty = tune())
house_tune_lrm <- extract_parameter_set_dials(house_spec_lrm)
(house_grid_lrm <- grid_regular(house_tune_lrm, levels = 5))

# orm engine
house_spec_orm <- ordinal_reg() |>
  set_engine("orm") |>
  set_args(ordinal_link = tune(), penalty = tune())
house_tune_orm <- extract_parameter_set_dials(house_spec_orm)
(house_grid_orm <- grid_regular(house_tune_orm, levels = c(Inf, 3)))

# prepped recipe
house_prep <- prep(house_rec)

# fitted model -- lrm
house_spec_lrm |>
  set_args(penalty = .01) |>
  fit(formula(house_prep), data = house_train) -> house_fit_lrm

# fitted model -- orm
house_spec_orm |>
  set_args(ordinal_link = "logistic", penalty = .01) |>
  fit(formula(house_prep), data = house_train) -> house_fit_orm

# evaluation -- lrm
house_pred_class_lrm <- predict(
  house_fit_lrm, new_data = house_test, type = "class"
)
bind_cols(house_test, house_pred_class_lrm) |>
  accuracy(truth = Sat, estimate = .pred_class)
house_pred_prob_lrm <- predict(
  house_fit_lrm, new_data = house_test, type = "prob"
)
bind_cols(house_test, house_pred_prob_lrm) |>
  roc_auc(truth = Sat, starts_with(".pred_"))

# evaluation -- orm
house_pred_class_orm <- predict(
  house_fit_orm, new_data = house_test, type = "class"
)
bind_cols(house_test, house_pred_class_orm) |>
  accuracy(truth = Sat, estimate = .pred_class)
house_pred_prob_orm <- predict(
  house_fit_orm, new_data = house_test, type = "prob"
)
bind_cols(house_test, house_pred_prob_orm) |>
  roc_auc(truth = Sat, starts_with(".pred_"))

# hyperparameter optimization -- lrm
house_res_lrm <- tune_grid(
  house_spec_lrm,
  preprocessor = house_rec,
  resamples = vfold_cv(house_train, v = 3),
  grid = house_grid_lrm,
  metrics = metric_set(accuracy, roc_auc)
)
(house_best_lrm <- select_best(house_res_lrm, metric = "accuracy"))

# final fit -- lrm
house_final_lrm <- finalize_model(house_spec_lrm, house_best_lrm)
(house_fit_final_lrm <- fit(
  house_final_lrm, formula(house_prep), data = house_train
))

# final evaluation -- lrm
house_pred_class_final_lrm <- predict(
  house_fit_final_lrm, new_data = house_test, type = "class"
)
bind_cols(house_test, house_pred_class_final_lrm) |>
  accuracy(truth = Sat, estimate = .pred_class)

# hyperparameter optimization -- orm
house_res_orm <- tune_grid(
  house_spec_orm,
  preprocessor = house_rec,
  resamples = vfold_cv(house_train, v = 3),
  grid = house_grid_orm,
  metrics = metric_set(accuracy, roc_auc)
)
(house_best_orm <- select_best(house_res_orm, metric = "accuracy"))

# final fit -- orm
house_final_orm <- finalize_model(house_spec_orm, house_best_orm)
(house_fit_final_orm <- fit(
  house_final_orm, formula(house_prep), data = house_train
))

# final evaluation -- orm
house_pred_class_final_orm <- predict(
  house_fit_final_orm, new_data = house_test, type = "class"
)
bind_cols(house_test, house_pred_class_final_orm) |>
  accuracy(truth = Sat, estimate = .pred_class)
