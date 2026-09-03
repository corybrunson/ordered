# next version

## maintenance

### bug fix

A bug in the prediction of `vglm` models, and associated tests, were patched.
Previously, `predict()` was used, which triggers S3 dispatch when **VGAM** is not attached but S4 dispatched when it is.
Now `predictvglm()` is used instead.

### refactor

`ordinal_reg()` argument value translation has been moved from the `translate()` method in parsnip to engine wrappers in ordered, with the exception of penalty path assembly for `ordinalNet` and `glmnetcr`, in coordination with [parsnip PR #1393](https://github.com/tidymodels/parsnip/pull/1393).

## new features

### additional ordinal regression and random forest engines

This version introduces source code and unit tests for new engines:
* `clm` from the **ordinal** package
  - cumulative link ordinal regression
  - fit wrapper to translate formulae
  - additional `ordinal_link` dial values
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

### linear prediction type

Linear predictions are enabled for the `clm`, `lrm`, `orm`, `vglm`, and `ordinalNet` ordinal regression engines and for the `vgam` generalized additive model engine.
They consistently return a single column of linear predictors (without threshold contributions).

Coordinated with [parsnip PR #1391](https://github.com/tidymodels/parsnip/pull/1391).

### threshold structure and parallel regression model arguments

The `threshold_structure` model argument for `ordinal_reg()` controls what constraints, if any, are imposed on the ordered thresholds.
It can be used by the `clm` and `vglm` engines.

The `parallel_reg` model argument for `ordinal_reg()` controls the parallel regression assumption with a logical value applied to all predictors.
It can be used by the `clm`, `vglm`, and `ordinalNet` engines.
Note that the default is to defer to the engine, and the `vglm` engine defaults to non-parallel terms.

The `gen_additive_mod()` `vgam` engine additionally registers the `Thresh` and `parallel` engine arguments.
These may be tuned using the `threshold_structure` and `parallel_reg` dials.

Coordinated with [parsnip PR #1393](https://github.com/tidymodels/parsnip/pull/1393) and [dials PR #462](https://github.com/tidymodels/dials/pull/462).

# ordered 0.1.0

Initial CRAN submission.
