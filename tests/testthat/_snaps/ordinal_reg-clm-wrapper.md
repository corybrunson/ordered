# clm_formulas() rejects a formula with no predictors

    Code
      clm_formulas(rating ~ 1, d)
    Condition
      Error:
      ! `parallel_reg = FALSE` needs at least one predictor to make non-parallel, but `formula` has none.

