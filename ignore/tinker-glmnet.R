# convert response variable to binary for `logistic_reg()`
house_alt <- function(x) {
  if (is.ordered(x) && all(levels(x) == c("Low", "Medium", "High")))
    x <- forcats::fct_recode(x, Medium = "Low")
  if (is.data.frame(x) && "Sat" %in% names(x))
    x$Sat <- house_alt(x$Sat)
  x
}

# untidy model
house_mod_alt <- glmnet::glmnet(
  # house_train[, -1L],
  house_train_vars,
  y = house_alt(house_train$Sat),
  family = "binomial",
  lambda = .001
)
house_mod_alt
predict(house_mod_alt, newx = house_test_vars, type = "class") |>
  as.vector() |>
  tibble::enframe(name = NULL, value = "Sat.pred") |>
  bind_cols(house_alt(house_test)) |>
  mutate(Sat.pred = ordered(Sat.pred, levels(Sat))) |>
  group_by(across(everything())) |> count() |> ungroup() |>
  print(n = 12)

# tunable model & analysis specification
# NB: The recipe must include categorical variable encoding because the tuners
# rely on `fit_xy()` rather than on `fit()` and therefore do not call
# `convert_form_to_xy_fit()`.
# house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train)
house_rec_alt <- recipe(Sat ~ Infl + Type + Cont, data = house_alt(house_train)) |>
  step_dummy(all_factor_predictors(), one_hot = TRUE)
house_spec_alt <- logistic_reg() |>
  set_engine("glmnet") |>
  set_args(penalty = tune(), mixture = tune())
house_tune_alt <- extract_parameter_set_dials(house_spec_alt)
(house_grid_alt <- grid_regular(house_tune_alt, levels = c(3, 3)))

# prepped recipe
house_prep_alt <- prep(house_rec_alt)
# baked training data
house_train_baked_alt <- bake(house_prep_alt, new_data = house_alt(house_train))
# translation
house_trans_alt <- translate(house_spec_alt)
# DIFF: no `lambda = tune()`
house_spec_alt |>
  set_args(penalty = .001, mixture = .5) |>
  translate() |>
  print() -> house_trans_args_alt
# fitted model
house_spec_alt |>
  set_args(penalty = .001, mixture = .5) |>
  # fit(house_prep, data = house_train) |>
  # fit(formula(house_prep), data = house_train) |>
  fit(formula(house_prep_alt), data = house_train_baked_alt) |>
  print() -> house_fit_alt
# prediction
predict(house_fit_alt, new_data = head(house_train_baked_alt), type = "class")
predict(house_fit_alt, new_data = head(house_train_baked_alt), type = "prob")
# prediction with specified penalty
house_fit_alt |>
  predict(new_data = head(house_train_baked_alt), type = "prob", penalty = .001)
house_fit_alt |>
  predict(new_data = head(house_train_baked_alt), type = "prob", penalty = .01)

# multiple model fitting
house_spec_alt |>
  set_args(
    path_values = c(.01, .001),
    penalty = 1,
    mixture = .5
  ) |>
  # translate() |>
  fit(formula(house_prep_alt), data = house_train_baked_alt) |>
  print() -> house_multi_fit_alt
# WARNING: `penalty` must be specified even though not used.
house_spec_alt |>
  set_args(
    path_values = c(.01, .001),
    mixture = .5
  ) |>
  # translate() |>
  fit(formula(house_prep_alt), data = house_train_baked_alt)

# multiple prediction
house_fit_alt |>
  multi_predict(new_data = head(house_train_baked_alt), type = "class",
                path_values = c(.1, .01, .001))
house_fit_alt |>
  multi_predict(new_data = head(house_train_baked_alt), type = "class",
                path_values = c(.1, .01, .001))
