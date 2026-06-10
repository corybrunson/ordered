library(devtools); library(tidymodels); load_all()

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# untidy model
house_mod <- rpartScore::rpartScore(
  formula = Sat ~ Infl + Type + Cont,
  # NB: Must convert response variable to numeric.
  data = mutate(house_train, Sat = as.integer(Sat))
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
house_spec <- decision_tree() |>
  set_engine("rpartScore") |>
  set_mode("classification") |>
  set_args(split = tune(), prune = tune())

# tuning grid derived from matched dials
house_spec |>
  extract_parameter_set_dials() |>
  grid_regular(levels = Inf) |>
  print() -> house_grid

# translation with specific engine arguments
house_spec |>
  set_args(split = "quad", prune = "mr") |>
  translate()

# prepped recipe
house_prep <- prep(house_rec)
# fitted model
house_fit <- fit(
  set_args(house_spec, split = "quad", prune = "mr"),
  formula(house_prep),
  data = house_train
)
# validate specification
house_fit$fit$functions$split

# hyperparameter optimization
house_res <- tune_grid(
  house_spec,
  preprocessor = house_rec,
  resamples = vfold_cv(house_train, v = 6),
  grid = house_grid,
  metrics = metric_set(accuracy, kap)
)
( house_best <- select_best(house_res, metric = "accuracy") )

# final fit
house_final <- finalize_model(house_spec, house_best)
( house_fit <- fit(house_final, formula(house_prep), data = house_train) )

# evaluation
house_pred_class <- predict(house_fit, new_data = house_test, type = "class")
bind_cols(house_test, house_pred_class) |>
  accuracy(truth = Sat, estimate = .pred_class)
bind_cols(house_test, house_pred_class) |>
  kap(truth = Sat, estimate = .pred_class)
