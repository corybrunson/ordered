# nocov start

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  make_ordinal_reg_polr()
  make_ordinal_reg_ordinalNet()
  make_decision_tree_rpartScore()
  make_rand_forest_ordinalForest()
}

# nocov end
