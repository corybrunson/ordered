library(devtools); library(tidymodels); load_all()

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# untidy model
house_mod <- MASS::polr(
  formula = Sat ~ Infl + Type + Cont,
  data = house_train
)
house_mod
predict(house_mod, newdata = house_test) |>
  tibble::enframe(name = NULL, value = "Sat.pred") |>
  bind_cols(house_test) |>
  mutate(Sat.pred = ordered(levels(Sat)[Sat.pred], levels(Sat))) |>
  group_by(across(everything())) |> count() |> ungroup() |>
  print(n = 24)

# tunable model & analysis specification
house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train)
house_spec <- ordinal_reg() |>
  set_engine("polr") |>
  set_args(ordinal_link = tune())
house_tune <- extract_parameter_set_dials(house_spec)
(house_grid <- grid_regular(house_tune, levels = Inf))

# prepped recipe
house_prep <- prep(house_rec)
# fitted model
house_spec |>
  set_args(ordinal_link = "logistic") |>
  fit(formula(house_prep), data = house_train)

# weighted kappa metric
kap_quad <- metric_tweak("kap_quad", kap, weighting = "quadratic")
# hyperparameter optimization
house_res <- tune_grid(
  house_spec,
  preprocessor = house_rec,
  resamples = vfold_cv(house_train),
  grid = house_grid,
  metrics = metric_set(accuracy, kap_quad, roc_auc)
)
(house_best <- select_best(house_res, metric = "kap_quad"))

# final fit
house_prep <- prep(house_rec)
house_final <- finalize_model(house_spec, house_best)
(house_fit <- fit(house_final, formula(house_prep), data = house_train))

# evaluation
house_pred_class <- predict(house_fit, new_data = house_test, type = "class")
bind_cols(house_test, house_pred_class) |>
  accuracy(truth = Sat, estimate = .pred_class)
house_pred_prob <- predict(house_fit, new_data = house_test, type = "prob")
bind_cols(house_test, house_pred_prob) |>
  roc_auc(truth = Sat, starts_with(".pred_"))
