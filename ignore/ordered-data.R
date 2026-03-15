library(tidyverse)

# working example with one package
data(package = "MASS") |>
  getElement("results") |>
  as_tibble() |>
  transmute(name = Item, title = Title) |>
  mutate(help = map(name, help, package = "MASS")) |>
  # pull(help) |> last() |> utils:::.getHelpFile()
  mutate(doc = map(help, utils:::.getHelpFile)) |>
  mutate(doc = map(doc, \(rd) str_c(as.character(rd), collapse = "\n"))) |>
  # pull(doc) |> first() -> test
  # ordinal data?
  filter(str_detect(doc, "ordered|ordinal")) |>
  print() -> mass_data

# function to tabulate relevant data from given packages
get_ord_data <- function(pkgs, detect = ".") {
  tibble(package = pkgs) |>
    mutate(data = map(package, \(pkg) as_tibble(data(package = pkg)$results))) |>
    unnest(data) |>
    transmute(package, name = Item, title = Title) |>
    # `?help`
    mutate(help = map2(name, package, \(x, y) help((x), (y)))) |>
    mutate(doc = map(help, \(h) try(utils:::.getHelpFile(h)))) |>
    filter(! map_lgl(doc, \(d) inherits(d, "try-error"))) |>
    mutate(doc = map(doc, \(rd) str_c(as.character(rd), collapse = "\n"))) |>
    # pull(doc) |> first() -> test
    # ordinal data?
    filter(str_detect(doc, detect))
}

# core & tidy packages
tidy_pkgs <- c("datasets", "MASS", "modeldata")
( core_ord_data <- get_ord_data(tidy_pkgs, detect = "ordered|ordinal") )
# ordered predictor
?datasets::VADeaths
?MASS::Insurance
?MASS::oats
# ordered identifier
?datasets::CO2
?datasets::ChickWeight
?datasets::DNase
?datasets::Indometh
?datasets::Loblolly
?datasets::Orange
?datasets::Theoph
# ordered response!
?MASS::housing # n >> p
?modeldata::hepatic_injury_qsar # n < p

( vgam_ord_data <-
    get_ord_data("VGAM", "ordered|ordinal|cumulative|acat|cratio|sratio") )
# ordered response!
?VGAM::backPain
# ?VGAM::backPain2
?VGAM::budworm
# ?VGAM::eyed # duplicative with {ordinalgmifs}
?VGAM::pneumo
# ?VGAM::wine # taken from {ordinal}
# unclear
?VGAM::car.all

ord_pkgs <- c(
  "ordinalNet", "rms", "glmpathcr",
  "ordinalgmifs", "ordinalbayes", "ordinal",
  "rpartScore", "ordinalForest", "orf"
)
( other_ord_data <- get_ord_data(ord_pkgs) )
# ordered response!
?glmpathcr::diabetes
?ordinalgmifs::eyedisease
?ordinalgmifs::hccframe
?ordinalbayes::cesc
?ordinalbayes::finalSet
?ordinalbayes::reducedSet
?ordinal::income
?ordinal::soup
?ordinal::wine
?ordinalForest::hearth
?orf::odata

# bound collection
semi_join(core_ord_data, tibble(name = c("housing", "hepatic_injury_qsar"))) |>
  bind_rows(anti_join(
    vgam_ord_data,
    tibble(name = c("eyed", "wine", "car.all"))
  )) |>
  bind_rows(other_ord_data) |>
  print() -> pkg_ord_data

