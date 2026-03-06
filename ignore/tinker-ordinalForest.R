library(devtools); library(tidymodels); load_all()

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# untidy model
house_mod <- ordinalForest::ordfor(
  depvar = "Sat",
  data = house_train
)
house_mod
predict(house_mod, newdata = house_test) |>
  getElement("ypred") |>
  tibble::enframe(name = NULL, value = "Sat.pred") |>
  bind_cols(house_test) |>
  mutate(Sat.pred = ordered(levels(Sat)[Sat.pred], levels(Sat))) |>
  group_by(across(everything())) |> count() |> ungroup() |>
  print(n = 12)

# tunable model & analysis specification
house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train)
house_spec <- rand_forest() |>
  set_engine("ordinalForest") |>
  set_mode("classification") |>
  # FIXME: When this mistake is made, the error is uninformative:
  #>   Error in `grid_regular()`:
  #>   ! This argument must have class <param>: `NA`.
  #> Run `rlang::last_trace()` to see where the error occurred.
  # set_args(ntreefinal = tune(), nsets = tune(), ntreeperdiv = 20L)
  set_args(trees = tune(), nsets = tune(), ntreeperdiv = 20L)

# tuning grid derived from matched dials
house_spec |>
  extract_parameter_set_dials() |>
  grid_regular(levels = 2L) |>
  print() -> house_grid

# prepped recipe
house_prep <- prep(house_rec)
# fitted model
fit(
  set_args(house_spec, trees = 1000, nsets = 100),
  formula(house_prep),
  data = house_train
)

# hyperparameter optimization
house_res <- tune_grid(
  house_spec,
  preprocessor = house_rec,
  resamples = vfold_cv(house_train, v = 3),
  grid = house_grid,
  metrics = metric_set(accuracy, kap, roc_auc, brier_class),
  control = control_grid(verbose = TRUE)
)
( house_best <- select_best(house_res, metric = "kap") )

# final fit
house_final <- finalize_model(house_spec, house_best)
( house_fit <- fit(house_final, formula(house_prep), data = house_train) )

# evaluation
house_pred_class <- predict(house_fit, new_data = house_test, type = "class")
bind_cols(house_test, house_pred_class) |>
  kap(truth = Sat, estimate = .pred_class)
house_pred_prob <- predict(house_fit, new_data = house_test, type = "prob")
bind_cols(house_test, house_pred_prob) |>
  brier_class(truth = Sat, starts_with(".pred_"))
