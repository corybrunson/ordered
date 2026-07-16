# next version

## additional engines

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

Engine additions were coordinated with [parsnip PR #1384](https://github.com/tidymodels/parsnip/pull/1384).

## linear predictions

Linear predictions are enabled for the `clm`, `lrm`, `orm`, `vglm`, and `ordinalNet` ordinal regression engines.
They consistently return a single column of linear predictors (without threshold contributions).

## bug fix

A bug in the prediction of `vglm` models, and associated tests, were patched.
Previously, `predict()` was used, which triggers S3 dispatch when **VGAM** is not attached but S4 dispatched when it is.
Now `predictvglm()` is used instead.

# ordered 0.1.0

Initial CRAN submission.
