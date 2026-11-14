# Package index

## ‘ordered’ package

Tidymodels Engine Wrappers and Parameters for Ordinal Classification
Models

- [`ordered`](https://corybrunson.github.io/ordered/reference/ordered-package.md)
  [`ordered-package`](https://corybrunson.github.io/ordered/reference/ordered-package.md)
  : ordered: parsnip Engines for Ordinal Regression Models

## Wrappers

Wrappers for Ordinal Classification Engines

- [`clm_wrapper()`](https://corybrunson.github.io/ordered/reference/clm_wrapper.md)
  :

  Fit wrapper for
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html)

- [`predict_lrm_wrapper()`](https://corybrunson.github.io/ordered/reference/predict_lrm_wrapper.md)
  :

  Wrapper for `rms` predictions

- [`ordinalNet_wrapper()`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  [`predict_ordinalNet_wrapper()`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  [`predict(`*`<_ordinalNet>`*`)`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  [`multi_predict(`*`<_ordinalNet>`*`)`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  [`predict_raw(`*`<_ordinalNet>`*`)`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  [`predict_classprob(`*`<_ordinalNet>`*`)`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  [`predict_class(`*`<_ordinalNet>`*`)`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  :

  Fit and predict wrappers for `ordinalNet`

- [`predict_glmnetcr_wrapper()`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md)
  [`multi_predict_glmnetcr_wrapper()`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md)
  [`predict(`*`<_glmnetcr>`*`)`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md)
  [`predict_class(`*`<_glmnetcr>`*`)`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md)
  [`predict_classprob(`*`<_glmnetcr>`*`)`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md)
  [`multi_predict(`*`<_glmnetcr>`*`)`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md)
  :

  Predict wrappers for `glmnetcr`

- [`VGAM_vglm_wrapper()`](https://corybrunson.github.io/ordered/reference/VGAM_vglm_wrapper.md)
  [`VGAM_vgam_wrapper()`](https://corybrunson.github.io/ordered/reference/VGAM_vglm_wrapper.md)
  [`values_ordinal_link_VGAM`](https://corybrunson.github.io/ordered/reference/VGAM_vglm_wrapper.md)
  [`values_threshold_structure_VGAM`](https://corybrunson.github.io/ordered/reference/VGAM_vglm_wrapper.md)
  :

  Fit wrappers for `VGAM`

- [`rpartScore_wrapper()`](https://corybrunson.github.io/ordered/reference/rpartScore_wrapper.md)
  :

  Fit wrapper for `rpartScore`

- [`ordinalForest_wrapper()`](https://corybrunson.github.io/ordered/reference/ordinalForest_wrapper.md)
  :

  Fit wrapper for `ordinalForest`

- [`orf_wrapper()`](https://corybrunson.github.io/ordered/reference/orf_wrapper.md)
  :

  Fit wrapper for `orf`

## Parameters

Tunable Engine Parameters

- [`values_ordinal_link_clm`](https://corybrunson.github.io/ordered/reference/clm_parameters.md)
  :

  Dials for `ordinal` engine parameters

- [`parallel_penalty_factor()`](https://corybrunson.github.io/ordered/reference/ordinalNet_parameters.md)
  :

  Dials for `ordinalNet` engine parameters

- [`split_func()`](https://corybrunson.github.io/ordered/reference/rpartScore_parameters.md)
  [`prune_func()`](https://corybrunson.github.io/ordered/reference/rpartScore_parameters.md)
  :

  Dials for `rpartScore` engine parameters

- [`naive_scores()`](https://corybrunson.github.io/ordered/reference/ordinalForest_parameters.md)
  [`num_scores()`](https://corybrunson.github.io/ordered/reference/ordinalForest_parameters.md)
  [`num_score_perms()`](https://corybrunson.github.io/ordered/reference/ordinalForest_parameters.md)
  [`num_score_trees()`](https://corybrunson.github.io/ordered/reference/ordinalForest_parameters.md)
  [`num_scores_best()`](https://corybrunson.github.io/ordered/reference/ordinalForest_parameters.md)
  [`ord_metric()`](https://corybrunson.github.io/ordered/reference/ordinalForest_parameters.md)
  [`values_ord_metric`](https://corybrunson.github.io/ordered/reference/ordinalForest_parameters.md)
  :

  Dials for `ordinalForest` engine parameters

- [`sample_fraction()`](https://corybrunson.github.io/ordered/reference/orf_parameters.md)
  [`honesty()`](https://corybrunson.github.io/ordered/reference/orf_parameters.md)
  [`honesty_fraction()`](https://corybrunson.github.io/ordered/reference/orf_parameters.md)
  :

  Dials for `orf` engine parameters
