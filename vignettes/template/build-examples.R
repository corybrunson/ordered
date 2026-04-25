#' Source this script to generate the article vignette 'examples.Rmd'.

library(tidyverse)
library(ordered)

get_from_env("models") |>
  enframe(name = NULL, value = "model") |>
  mutate(dependency = map(model, get_dependency)) |>
  unnest(dependency) |>
  unnest(pkg) |>
  filter(pkg == "ordered") |>
  mutate(info = map(model, \(.x) get_from_env(paste0(.x, "_predict")))) |>
  nest(spec = c(model, engine, mode)) |>
  transmute(info = map2(info, spec, inner_join, join_by(engine, mode))) |>
  unnest(info) |>
  select(model, engine, type) |>
  print() -> ordered_preds

ordered_preds |>
  distinct(model, engine) |>
  print() -> ordered_specs

ordered_specs |>
  distinct(model) |>
  print() -> ordered_mods

# values must be character-ized
list(
  polr = list(ordinal_link = "\"probit\""),
  ordinalNet = list(penalty = ".001", mixture = ".5"),
  vglm = list(),
  vgam = list(),
  rpartScore = list(),
  ordinalForest = list(trees = "1000")
) |>
  enframe(name = "engine", value = "args") |>
  print() -> spec_args

# TODO: Put these in a `set_args()` call.
# values must be character-ized
list(
  polr = list(),
  ordinalNet = list(),
  vglm = list(),
  vgam = list(),
  rpartScore = list(split = "\"quad\""),
  ordinalForest = list(nsets = "100")
) |>
  enframe(name = "engine", value = "args") |>
  print() -> eng_args

type_phrases <- c(
  prob = "all class probabilities",
  class = "the most likely class",
  linear_pred = "the linear predictor"
)

oxford_c <- function(x) {
  res <- if (length(x) == 1L) {
    x[[1L]]
  } else if (length(x) == 2L) {
    paste0(x[[1L]], " or ", x[[2L]])
  } else {
    paste0(
      paste(x[seq(length(x) - 1L)], collapse = ", "),
      ", or ", x[[length(x)]]
    )
  }
  res
}

vignette_rmd <- here::here("vignettes/articles/examples.Rmd")

readLines("vignettes/template/_template-examples-front.Rmd") |>
  write(file = vignette_rmd, append = FALSE)

for (model_name in ordered_mods$model) {

  model_hyphen <- str_replace_all(model_name, "\\_", "-")
  model_abbr <- str_replace_all(model_name, "(^|\\_)([a-z])[a-z]+", "\\2")

  model_header <- glue::glue("## `{model}()` models", model = model_name)
  cat("\n", model_header, "\n", file = vignette_rmd, sep = "", append = TRUE)

  engine_names <- ordered_specs |> filter(model == model_name) |> pull(engine)

  for (engine_name in engine_names) {

    engine_hyphen <- str_replace_all(engine_name, "(\\_|\\.)", "-")

    engine_args <- spec_args |>
      filter(engine == engine_name) |>
      pull(args) |> unlist() |>
      enframe(name = "arg", value = "value") |>
      unite(pass, arg, value, sep = " = ") |>
      pull(pass) |> str_c(collapse = ", ")

    engine_types <-
      ordered_preds |>
      filter(model == model_name, engine == engine_name) |>
      pull(type)
    types_clause <- oxford_c(type_phrases[engine_types])
    type_lines <- map_chr(
      engine_types,
      function(s) glue::glue(
        "predict({abbr}_fit, house_test, type = \"{type}\")",
        abbr = model_abbr, type = s
      )
    )
    types_chunk <- str_c(type_lines, collapse = "\n")

    readLines("vignettes/template/_template-examples-engine.Rmd") |>
      gsub(pattern = "\\{model_name\\}", replacement = model_name) |>
      gsub(pattern = "\\{model_hyphen\\}", replacement = model_hyphen) |>
      gsub(pattern = "\\{model_abbr\\}", replacement = model_abbr) |>
      gsub(pattern = "\\{engine_name\\}", replacement = engine_name) |>
      gsub(pattern = "\\{engine_hyphen\\}", replacement = engine_hyphen) |>
      gsub(pattern = "\\{engine_args\\}", replacement = engine_args) |>
      gsub(pattern = "\\{types_clause\\}", replacement = types_clause) |>
      gsub(pattern = "\\{types_chunk\\}", replacement = types_chunk) |>
      write(file = vignette_rmd, append = TRUE)

  }

}
