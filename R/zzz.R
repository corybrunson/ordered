# nocov start

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # `ordinal_reg`
  make_ordinal_reg_polr()
  make_ordinal_reg_ordinalNet()
  make_ordinal_reg_vglm()
  # `gen_additive_mod`
  make_gen_additive_mod_vgam()
  # `decision_tree`
  make_decision_tree_rpartScore()
  # `rand_forest`
  make_rand_forest_ordinalForest()
}

# nocov end
