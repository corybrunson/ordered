# next version

## bug fix

A bug in the prediction of `vglm` models, and associated tests, were patched.
Previously, `predict()` was used, which triggers S3 dispatch when **VGAM** is not attached but S4 dispatched when it is.
Now `predictvglm()` is used instead.

## additional ordinal regression and random forest engines

This version introduces source code and unit tests for new engines:
* `clm` from the **ordinal** package
  - cumulative link ordinal regression
  - additional `ordinal_link` dial values
  - dial for `threshold` argument
* `lrm` and `orm` from the **rms** package
  - regularized cumulative probability ordinal regression
  - shared prediction wrapper
* `glmnetcr` from the **glmnetcr** package
  - elastic net regularized continuation ratio models
  - penalty paths handled as with `ordinalNet`
  - prediction wrapper
* `orf` from the **orf** package
  - conditional probability ordered random forests
  - fit wrapper
  - dials for `sample.fraction`, `honesty`, and `honesty.fraction` arguments

Coordinated with [parsnip PR #1384](https://github.com/tidymodels/parsnip/pull/1384).

## linear prediction type

Linear predictions are enabled for the `clm`, `lrm`, `orm`, `vglm`, and `ordinalNet` ordinal regression engines and for the `vgam` generalized additive model engine.
They consistently return a single column of linear predictors (without threshold contributions).

Coordinated with [parsnip PR #1391](https://github.com/tidymodels/parsnip/pull/1391).

## threshold structure and parallel regression model arguments

The `threshold_structure` model argument for `ordinal_reg()` controls what constraints, if any, are imposed on the ordered thresholds. It can be used by the `clm` and `vglm` engines.

The `parallel_reg` model argument for `ordinal_reg()` provides a unified interface for controlling the parallel regression assumption. It accepts a logical value (applied to all terms), a formula with a logical LHS naming parallel or non-parallel terms, or a list combining both; the engines `clm`, `vglm`, and `ordinalNet` are compatible with different subsets of specifications:
* `clm_wrapper()` (new) translates the `parallel_reg` specification to a formula accepted by `ordinal::clm(nominal)`
* `VGAM_vglm_wrapper()` and `VGAM_vgam_wrapper()` accept a `parallel_reg` argument and translate it for the VGAM `parallel` argument
* `ordinalNet_wrapper()` accepts a `parallel_reg` argument and translates it for the `parallelTerms` and `nonparallelTerms` arguments

Coordinated with forthcoming PRs to parsnip and to dials.

# ordered 0.1.0

Initial CRAN submission.
