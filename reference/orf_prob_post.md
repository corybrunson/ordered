# Post-processing for `orf` probability predictions

Converts the `orf.prediction` object from
[`predict.orf()`](https://rdrr.io/pkg/orf/man/predict.orf.html) to a
tibble of class probabilities with columns named `.pred_{level}`.

## Usage

``` r
orf_prob_post(x, object)
```

## Arguments

- x:

  An `orf.prediction` object from
  [`predict.orf()`](https://rdrr.io/pkg/orf/man/predict.orf.html).

- object:

  A parsnip `model_fit` object.

## Value

A tibble of class probabilities.
