# Post-processing for `orf` class predictions

Converts the `orf.prediction` object from
[`predict.orf()`](https://rdrr.io/pkg/orf/man/predict.orf.html) to a
factor vector of predicted classes.

## Usage

``` r
orf_class_post(x, object)
```

## Arguments

- x:

  An `orf.prediction` object from
  [`predict.orf()`](https://rdrr.io/pkg/orf/man/predict.orf.html).

- object:

  A parsnip `model_fit` object.

## Value

A factor of predicted classes.
