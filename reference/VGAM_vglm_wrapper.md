# Fit wrappers for `VGAM`

These wrappers reorganize arguments into proper calls to
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
[`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html), which
specialize to several families of ordinal regression models. The
standardized argument values used by
[`parsnip::ordinal_reg()`](https://parsnip.tidymodels.org/reference/ordinal_reg.html)
and
[`parsnip::gen_additive_mod()`](https://parsnip.tidymodels.org/reference/gen_additive_mod.html)
are matched and converted to the values native to `VGAM` within the
wrappers.

## Usage

``` r
VGAM_vglm_wrapper(
  formula,
  data,
  family = "cumulative",
  link = "logitlink",
  parallel = FALSE,
  Thresh = NULL,
  ...,
  call = rlang::caller_env()
)

VGAM_vgam_wrapper(
  formula,
  data,
  family = "cumulative",
  link = "logitlink",
  parallel = FALSE,
  Thresh = NULL,
  ...,
  call = rlang::caller_env()
)

values_ordinal_link_VGAM

values_threshold_structure_VGAM
```

## Arguments

- formula:

  The formula to pass.

- data:

  The data frame to pass.

- family:

  The odds link function; either a standardized dial value
  (`"cumulative_link"`, `"adjacent_categories"`, `"continuation_ratio"`,
  `"stopping_ratio"`) or a `VGAM` native value (`"cumulative"`,
  `"acat"`, `"cratio"`, `"sratio"`).

- link:

  The ordinal link function; either a standardized dial value (e.g.
  `"logistic"`, `"cloglog"`) or a `VGAM` native value (e.g.
  `"logitlink"`, `"clogloglink"`).

- parallel:

  Logical; whether predictor effects are shared across thresholds. It
  corresponds to the standardized `parallel_reg` argument (see
  [`dials::parallel_reg()`](https://dials.tidymodels.org/reference/parallel_reg.html)).

- Thresh:

  Character; the threshold constraint pattern. It corresponds to the
  standardized `threshold_structure` argument (see
  [`dials::threshold_structure()`](https://dials.tidymodels.org/reference/threshold_structure.html)).

- ...:

  Additional arguments to pass.

## Value

An object of S3 parent class `VGAM` and primary classes `vglm` or `vgam`
as returned by [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html)
and [`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html).

## Details

The vector `values_ordinal_link_VGAM` extends the default `ordinal_link`
options encoded in
[`dials::values_ordinal_link`](https://dials.tidymodels.org/reference/ordinal_link.html)
to those accepted by
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
[`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html).

The vector `values_threshold_structure_VGAM` extends the default
`threshold_structure` options encoded in
[`dials::values_threshold_structure`](https://dials.tidymodels.org/reference/threshold_structure.html)
to those accepted by
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
[`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html).

NB: Prediction with `vgam` models under VGAM version 1.1-9 does not yet
support the use of smoothers `s()`.

Note that [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
[`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html) treat the rows
of `data` as the units of observation: Compressed `data` with one row
per predictor set will be understood as having a multinomial-valued
outcome; only expanded data with one row per case will be understood as
having a single-valued ordinal outcome. (This divide cannot be bridged
by passing a column of counts to the `weights` argument.) These wrappers
require a single ordinal outcome column and therefore do not accept the
convenient `cbind(y1, y2, ...) ~ x1 + x2 + ...` encoding commonly used
in `VGAM`.

## Examples

``` r
values_ordinal_link_VGAM
#>  [1] "logistic" "probit"   "loglog"   "cloglog"  "cauchit"  "foldsqrt"
#>  [7] "logc"     "gord"     "pord"     "nbord"   
dials::ordinal_link(values = values_ordinal_link_VGAM)
#> Ordinal Link (qualitative)
#> 10 possible values include:
#> 'logistic', 'probit', 'loglog', 'cloglog', 'cauchit', 'foldsqrt', 'logc',
#> 'gord', 'pord', and 'nbord'
values_threshold_structure_VGAM
#> [1] "flexible"         "symmetric_median" "symmetric_zero"   "equidistant"     
#> [5] "qnorm"           
dials::threshold_structure(values = values_threshold_structure_VGAM)
#> Threshold Structure (qualitative)
#> 5 possible values include:
#> 'flexible', 'symmetric_median', 'symmetric_zero', 'equidistant', and 'qnorm'

house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]

if (utils::packageVersion("VGAM") == package_version("1.1.9")) {#VGAM=1.1-9

# fit wrapper for linear model
( fit_orig <- VGAM::vglm(
  Sat ~ Type + Infl + Cont,
  family = VGAM::sratio(
    link = "probitlink", parallel = TRUE, threshold = "symmetric0"
  ),
  data = house_data
) )
( fit_wrap <- VGAM_vglm_wrapper(
  Sat ~ Type + Infl + Cont,
  family = "sratio",
  link = "probitlink", parallel = TRUE, Thresh = "symmetric_zero",
  data = house_data
) )
# fit wrapper for additive model
( fit_orig <- VGAM::vgam(
  Sat ~ Type + Infl + Cont,
  family = VGAM::cratio(
    link = "clogloglink", parallel = TRUE, threshold = "symmetric0",
  ),
  data = house_data
) )
( fit_wrap <- VGAM_vgam_wrapper(
  Sat ~ Type + Infl + Cont,
  family = "cratio",
  link = "cloglog", parallel = TRUE, Thresh = "symmetric0",
  data = house_data
) )

}#VGAM=1.1-9

if (utils::packageVersion("VGAM") > package_version("1.1.9")) {#VGAM>1.1-9

# fit wrapper for linear model
( fit_orig <- VGAM::vglm(
  Sat ~ Type + Infl + Cont,
  family = VGAM::sratio(
    link = "probitlink", parallel = TRUE, Thresh = "symm0"
  ),
  data = house_data
) )
( fit_wrap <- VGAM_vglm_wrapper(
  Sat ~ Type + Infl + Cont,
  family = "sratio",
  link = "probitlink", parallel = TRUE, Thresh = "symmetric_zero",
  data = house_data
) )
# fit wrapper for additive model
( fit_orig <- VGAM::vgam(
  Sat ~ Type + Infl + Cont,
  family = VGAM::cratio(
    link = "clogloglink", parallel = TRUE, Thresh = "symm0",
  ),
  data = house_data
) )
( fit_wrap <- VGAM_vgam_wrapper(
  Sat ~ Type + Infl + Cont,
  family = "cratio",
  link = "cloglog", parallel = TRUE, Thresh = "symm0",
  data = house_data
) )

}#VGAM>1.1-9
#> 
#> Call:
#> VGAM::vgam(formula = formula, family = VGAM::cratio(link = "clogloglink", 
#>     parallel = TRUE, Thresh = "symm0"), data = data)
#> 
#> 
#> Degrees of Freedom: 3362 Total; 3355 Residual
#> Residual deviance: 3490.264 
#> Log-likelihood: -1745.132 
```
