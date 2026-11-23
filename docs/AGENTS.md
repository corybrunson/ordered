# AGENTS.md — ordered

An R package providing ordinal classification engines for the
**tidymodels** ecosystem.

## Versions

- Focus primarily on the development version of {ordered} in the current
  directory (regardless of the current Git branch), not the installed
  version.
- For other packages, refer primarily to the locally installed version
  or else to the online development version (if specified in the
  description or CRAN page).
- Ignore the pkgdown website; it may reflect an older version of the
  package without more recent development changes.
- {parsnip} and {dials} should be loaded from the cloned development
  repositories at ‘../parsnip’ and ‘../dials’. These may contain
  un-released changes; prefer them over the CRAN release. Never request
  access to any other local directories.

## Key architecture

- Standard R package extending {parsnip} with new model engines.
- Engine registration happens at load time via `R/zzz.R` → `R/*-data.R`
  (`make_*()` functions) using
  [`parsnip::set_model_engine()`](https://parsnip.tidymodels.org/reference/set_new_model.html),
  [`parsnip::set_fit()`](https://parsnip.tidymodels.org/reference/set_new_model.html),
  [`parsnip::set_pred()`](https://parsnip.tidymodels.org/reference/set_new_model.html),
  etc.
- Actual model wrappers are in separate files (`R/*-wrapper.R`): these
  adapt the downstream package’s interface to tidymodels conventions.
- Custom tunable parameters live in `R/param-engine-*.R`.
- Internal validation helpers live in `R/parsnip-utils.R`:
  `check_pred_type()`, `check_spec_levels()`, `check_spec_pred_type()`,
  and `spec_has_pred_type()`.

## Models and engines

| model | engine | interface | fit calls | file key |  |  |
|----|----|----|----|----|----|----|
| `ordinal_reg` | `polr` | formula | [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) directly | `ordinal_reg-data.R` |  |  |
| `ordinal_reg` | `vglm` | formula | [`ordered::VGAM_vglm_wrapper`](https://corybrunson.github.io/ordered/reference/VGAM_vglm_wrapper.md) | `ordinal_reg-data.R` / `ordinal_reg-VGAM.R` |  |  |
| `ordinal_reg` | `ordinalNet` | matrix | [`ordered::ordinalNet_wrapper`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md) | `ordinal_reg-data.R` / `ordinal_reg-ordinalNet.R` |  |  |
| `ordinal_reg` | `clm` | formula | [`ordered::clm_wrapper`](https://corybrunson.github.io/ordered/reference/clm_wrapper.md) | `ordinal_reg-data.R` / `ordinal_reg-clm.R` |  |  |
| `ordinal_reg` | `lrm` | formula | [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html) directly | `ordinal_reg-data.R` / `ordinal_reg-rms.R` |  |  |
| `ordinal_reg` | `orm` | formula | [`rms::orm`](https://rdrr.io/pkg/rms/man/orm.html) directly | `ordinal_reg-data.R` / `ordinal_reg-rms.R` |  |  |
| `ordinal_reg` | `glmnetcr` | matrix | [`glmnetcr::glmnetcr`](https://rdrr.io/pkg/glmnetcr/man/glmnetcr.html) directly | `ordinal_reg-data.R` / `ordinal_reg-glmnetcr.R` |  |  |
| `gen_additive_mod` | `vgam` | formula | [`ordered::VGAM_vgam_wrapper`](https://corybrunson.github.io/ordered/reference/VGAM_vglm_wrapper.md) | `gen_additive_mod-data.R` / `ordinal_reg-VGAM.R` |  |  |
| `decision_tree` | `rpartScore` | formula | [`ordered::rpartScore_wrapper`](https://corybrunson.github.io/ordered/reference/rpartScore_wrapper.md) | `decision_tree-data.R` / `decision_tree-rpartScore.R` |  |  |
| `rand_forest` | `ordinalForest` | data.frame | [`ordered::ordinalForest_wrapper`](https://corybrunson.github.io/ordered/reference/ordinalForest_wrapper.md) | `rand_forest-data.R` / `rand_forest-ordinalForest.R` |  |  |
| `rand_forest` | `orf` | data.frame | [`ordered::orf_wrapper`](https://corybrunson.github.io/ordered/reference/orf_wrapper.md) | `rand_forest-data.R` / `rand_forest-orf.R` |  |  |

For idiosyncratic argument handling, append the fit wrapper’s
argument-value translation logic (matching standardized dials values to
native values) in `R/*-wrapper.R` through helpers like
`match_ordinal_family()`, `match_ordinal_link_VGAM()`, and
`match_ordinal_link_ordinalNet()`, rather than in
`translate.ordinal_reg()` in {parsnip}. Keep only penalty-path
construction (e.g. for `ordinalNet`/`glmnetcr`) in {parsnip}’s
`translate.ordinal_reg()`.

## Development commands

``` r

# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "ordinal_reg-polr")

# Regenerate documentation
roxygen2::roxygenise()

# Full R CMD check
devtools::check()

# Build pkgdown site
pkgdown::build_site()

# Code coverage
covr::package_coverage()
```

## Testing conventions

- testthat 3rd edition (`Config/testthat/edition: 3`).
- Tests use `skip_if_not_installed()` for Suggests packages (MASS, VGAM,
  ordinalNet, ordinal, rms, glmnetcr, orf, rpartScore, ordinalForest,
  QSARdata).
- Snapshot tests live in `tests/testthat/_snaps/` (one per engine).
- Test helper `tests/testthat/helper-data.R` creates shared datasets
  from [`MASS::housing`](https://rdrr.io/pkg/MASS/man/housing.html) (via
  `get_house()`) and
  [`QSARdata::caco`](https://rdrr.io/pkg/QSARdata/man/caco.html).
- Test files include `test-ordinal_reg-clm.R`,
  `test-ordinal_reg-glmnetcr.R`, `test-ordinal_reg-rms.R`,
  `test-rand_forest-orf.R`, and `test-dials.R`.
- Code in `R/*-data.R` and `R/zzz.R` is excluded from coverage via
  `# nocov start/end` (runs at load, can’t be test-executed).

## Prediction types

| model | engine | predict calls | class | prob |  |  |
|----|----|----|----|----|----|----|
| `decision_tree` | `rpartScore` | `predict` (S3) | ✔ | ✖ |  |  |
| `gen_additive_mod` | `vgam` | `predict` (VGAM) | ✔ | ✔ |  |  |
| `ordinal_reg` | `polr` | `predict` (S3) | ✔ | ✔ |  |  |
| `ordinal_reg` | `ordinalNet` | [`ordered::predict_ordinalNet_wrapper`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md) | ✔ | ✔ |  |  |
| `ordinal_reg` | `vglm` | [`VGAM::predictvglm`](https://rdrr.io/pkg/VGAM/man/predictvglm.html) (class/prob), `predict` (VGAM, link) | ✔ | ✔ |  |  |
| `ordinal_reg` | `clm` | `predict` (S3) | ✔ | ✔ |  |  |
| `ordinal_reg` | `lrm` | [`ordered::predict_lrm_wrapper`](https://corybrunson.github.io/ordered/reference/rms_orm_wrapper.md) | ✔ | ✔ |  |  |
| `ordinal_reg` | `orm` | [`ordered::predict_lrm_wrapper`](https://corybrunson.github.io/ordered/reference/rms_orm_wrapper.md) | ✔ | ✔ |  |  |
| `ordinal_reg` | `glmnetcr` | [`ordered::predict_glmnetcr_wrapper`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md) | ✔ | ✔ |  |  |
| `rand_forest` | `ordinalForest` | `predict` (S3) | ✔ | ✔ (only with `perffunction = "probability"`) |  |  |
| `rand_forest` | `orf` | `predict` (S3) | ✔ | ✔ |  |  |

## Notable quirks

- **`ordinalNet` penalty interpolation**: Supports submodel prediction
  at arbitrary penalty values via linear interpolation between fitted
  path points. Uses
  [`multi_predict._ordinalNet()`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md)
  and
  [`predict._ordinalNet()`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md).
- **`rpartScore` interface**: Wrapper converts outcome to integer
  ([`as.integer()`](https://rdrr.io/r/base/integer.html)) before calling
  [`rpartScore::rpartScore()`](https://rdrr.io/pkg/rpartScore/man/rpartScore.html).
- **VGAM `predictvglm` dispatch fix**: `vglm` pred functions use
  `predictvglm()` (not
  [`predict()`](https://rdrr.io/r/stats/predict.html)) to avoid mixed
  S3/S4 dispatch issues. `vgam` pred functions use standard `predict`
  (from VGAM) — no issue there. See NEWS.md.
- **`ordinalForest` defaults**: `num.threads = 1`,
  `perffunction = "probability"`. Prob predictions error unless
  `perffunction` is `"probability"`.
- **`values_ordinal_link_VGAM`**: Extends
  [`dials::values_ordinal_link`](https://dials.tidymodels.org/reference/ordinal_link.html)
  with VGAM-specific link functions `"foldsqrt"`, `"logc"`, `"gord"`,
  `"pord"`, `"nbord"`.
- **Value translation in wrappers**: Standardized dials values for
  `link`/`family`/`Thresh`/`parallel_reg` are matched to native values
  inside the `vglm`/`vgam`/`ordinalNet` fit wrappers (via
  `match_ordinal_link_VGAM()`, `match_ordinal_family()`,
  `match_threshold_structure_VGAM()`, `match_ordinal_link_ordinalNet()`)
  rather than in `parsnip::translate.ordinal_reg()`. Only penalty-path
  construction stays in {parsnip}. Native values (e.g. `"logitlink"`,
  `"sratio"`, `"logit"`) pass through unchanged.
- **`glmnetcr` penalty interpolation**: Supports submodel prediction at
  arbitrary penalty values via
  [`predict_glmnetcr_wrapper()`](https://corybrunson.github.io/ordered/reference/predict_glmnetcr_wrapper.md),
  which interpolates between probability matrices at neighboring path
  steps.
- **`orf` interface**: Wrapper converts data.frame + factor to numeric
  matrix `X` + numeric vector `Y` before calling
  [`orf::orf()`](https://rdrr.io/pkg/orf/man/orf.html).
- **`predict_lrm_wrapper`**: The `rms` package’s `predictrms()` dispatch
  does not accept tibble inputs. The wrapper converts tibbles to plain
  data frames before calling
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html).
- **`clm` prob predictions**: The prob prediction pre-processor strips
  the outcome column from `newdata` to prevent
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html) from
  expecting it.
- **`values_ordinal_link_clm`**: Extends
  [`dials::values_ordinal_link`](https://dials.tidymodels.org/reference/ordinal_link.html)
  with clm-specific link functions (see `param-engine-clm.R`).
- **`threshold_structure`**: A custom dial for
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html) threshold
  parameter (e.g., `"flexible"`, `"symmetric"`, `"equidistant"`).
  Defined in `param-engine-clm.R`.
- **Encodings vary by engine**: `traditional` for
  polr/vglm/vgam/clm/lrm/orm/orf, `one_hot` for ordinalNet/glmnetcr,
  `none` for rpartScore/ordinalForest.

## CI / PR workflows (GitHub Actions)

- `R-CMD-check.yaml`: multi-OS matrix including macOS, Windows, Ubuntu
  (devel & 2 oldrels).
- `R-CMD-check-no-suggests.yaml`: hard-deps only check (catches missing
  Suggests).
- `test-coverage.yaml`: runs covr, uploads to Codecov.
- `pkgdown.yaml`: builds site, deploys to `gh-pages` branch.
- `pr-commands.yaml`: `/document` (roxygenise + commit man/NAMESPACE)
  and `/style` (styler).

## R package development

This section is taken from the {usethis} package and was written for
claude.

### Key commands

``` r

# To run code
devtools::load_all(); code

# To run all tests
devtools::test()

# To run all tests for files starting with {name}
devtools::test(filter = '^{name}')

# To run all tests for R/{name}.R
devtools::test_active_file('R/{name}.R')

# To run a single test "blah" for R/{name}.R
devtools::test_active_file('R/{name}.R', desc = 'blah')

# To redocument the package
devtools::document()

# To check pkgdown documentation
pkgdown::check_pkgdown()

# To check the package with R CMD check
devtools::check()
```

You have two options to run R code:

- `Rscript --no-environ -e "code"`.

  Without `--no-environ`, every R call fails with
  `"Fatal error: cannot create 'R_TempDir'"` because the sandbox blocks
  reads of `~/.Renviron`, which R reads during startup before creating
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- If the mcp-repl tool is available, you can use it instead. Note that
  its default sandbox blocks network requests.

Other commands:

    # To format code
    air format .

### Coding

- Always run `air format .` after generating code
- Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`)
- Don’t use `_$x` or `_$[["x"]]` since this package must work on R 4.1.
- Use `\() ...` for single-line anonymous functions. For all other
  cases, use `function() {...}`
- Include a space between the bang prefix operators (`!`, `!!`, `!!!`)
  and their operands.
- Format code to fit within 80 characters.

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing
  tests.
- Strive to keep your tests minimal with few comments.
- Never put code in a `test-{name}.R` file outside of a `test_that()`
  block. Instead, use `tests/testthat/helper.R` or
  `tests/testthat/helper-{name}.R`.
- Avoid `expect_true()` and `expect_false()` in favour of a specific
  expectation which will give a better failure message. A few
  expectations in newer releases that you might not know about are
  `expect_all_true()`, `expect_all_equal()`, and `expect_r6_class()`.
- When testing errors and warnings, don’t use `expect_error()` or
  `expect_warning()`. Instead, use `expect_snapshot(error = TRUE)` for
  errors and `expect_snapshot()` for warnings because these allow the
  user to review the full text of the output.
- Avoid the `.package` argument to `local_mocked_bindings()`; this
  modifies the namespace of another package which is not good practice.
  Instead create a mockable version of the function in the current
  package. See `?local_mocked_bindings` for more details.

### Documentation

- Every user-facing function should be exported and have roxygen2
  documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Whenever you add a new (non-internal) documentation topic, also add
  the topic to `_pkgdown.yml`.
- Always re-document the package after changing a roxygen2 comment.
- Use
  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
  to check that all topics are included in the reference index.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`.
- Changes that shouldn’t get a bullet:
  - Small documentation changes.
  - Internal refactorings.
  - Fixes to bugs introduced in the current dev version.
- Each bullet should briefly describe the change to the end user and
  mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any
  new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function
  early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that
  don’t mention function names at the beginning.

### Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader
and editor with a deep understanding of clear, engaging, and
well-structured writing.

Work paragraph by paragraph, always starting by making a TODO list that
includes individual items for each top-level heading.

Fix spelling, grammar, and other minor problems without asking the user.
Label any unclear, confusing, or ambiguous sentences with a FIXME
comment.

Only report what you have changed.
