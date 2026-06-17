library(devtools); library(tidymodels); load_all()

# disaggregated data & partition
# NOTE: subset to 200 rows to reduce active memory during tuning
set.seed(144688)
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_data <- house_data[sample(nrow(house_data), 200), ]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# untidy model — direct use of orf
X <- model.matrix(~ Infl + Type + Cont - 1, data = house_train)
Y <- as.numeric(house_train$Sat)
house_mod <- orf::orf(X, Y,
                      num.trees = 200, sample.fraction = 0.5,
                      honesty = FALSE)
house_mod

# class predictions — same X used for fit & prediction
predict(house_mod, newdata = X, type = "c") |>
  getElement("predictions") |>
  as.vector() |>
  factor(labels = levels(house_train$Sat)) |>
  table()

# tunable model & analysis specification
house_spec <- rand_forest() |>
  set_engine("orf") |>
  set_mode("classification") |>
  set_args(trees = tune(), sample.fraction = tune())

# tuning grid derived from matched dials (2 tunable params -> 4 grid pts)
house_spec |>
  extract_parameter_set_dials() |>
  grid_regular(levels = 2L) |>
  print() -> house_grid

# fitted model — formula interface; parsnip applies traditional dummy encoding
fit(
  set_args(house_spec, trees = 200, sample.fraction = 0.5, honesty = FALSE),
  Sat ~ Infl + Type + Cont,
  data = house_train
)

# hyperparameter optimization — 2 folds, 4 grid pts, 8 fits total
# NOTE: tune_grid with formula interface works because parsnip's
# `predictor_indicators = "traditional"` applies dummy encoding automatically.
# When using a recipe preprocessor instead, step_dummy() must be added manually.
house_res <- tune_grid(
  house_spec,
  preprocessor = Sat ~ Infl + Type + Cont,
  resamples = vfold_cv(house_train, v = 2),
  grid = house_grid,
  metrics = metric_set(accuracy, kap),
  control = control_grid(verbose = TRUE)
)
( house_best <- select_best(house_res, metric = "kap") )

# final fit
house_final <- finalize_model(house_spec, house_best)
( house_fit <- fit(house_final, Sat ~ Infl + Type + Cont, data = house_train) )

# evaluation
house_pred_class <- predict(house_fit, new_data = house_test, type = "class")
bind_cols(house_test, house_pred_class) |>
  kap(truth = Sat, estimate = .pred_class)
house_pred_prob <- predict(house_fit, new_data = house_test, type = "prob")
bind_cols(house_test, house_pred_prob) |>
  brier_class(truth = Sat, starts_with(".pred_"))
