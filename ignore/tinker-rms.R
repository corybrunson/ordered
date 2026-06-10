library(devtools); library(tidymodels); load_all()

# disaggregated data & partition
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_split <- initial_split(house_data, prop = .8)
house_train <- training(house_split)
house_test <- testing(house_split)

# untidy model
house_mod <- rms::lrm(
  formula = Sat ~ Infl + Type + Cont,
  data = house_train,
  penalty = .01
)
house_mod
predict(house_mod, newdata = house_test, type = "fitted.ind") |>
  tibble::as_tibble()

stop()

# tunable model & analysis specification
house_rec <- recipe(Sat ~ Infl + Type + Cont, data = house_train)
house_spec <- ordinal_reg() |>
  set_engine("polr") |>
  set_args(ordinal_link = tune())
house_tune <- extract_parameter_set_dials(house_spec)
(house_grid <- grid_regular(house_tune, levels = Inf))


