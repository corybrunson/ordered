## Checks and environments

* local OS X, R 4.2.3
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
  * `devtools::check(manual = TRUE, remote = TRUE)`
* local OS X, R 4.5.2
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
  * `devtools::check(manual = TRUE, remote = TRUE)`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`

## Check results

* The following words were flagged but are correctly spelled:
  Galimberti
  Hanlon
  Hornung
  Maso
  Rathouz
  Soffritti
  Wurm

* One URL was marked as possibly invalid due to not being in canonical form for CRAN packages:
  <https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf>
  (It is to a package vignette, not to the package.)

* Occasionally my computers could not verify the current time.

No ERRORs, WARNINGs, or other NOTEs obtained.

## Context

This package was developed in coordination with the following upgrades to other packages:
* {parsnip} 1.5.0
  * https://github.com/tidymodels/parsnip/issues/1298
  * https://github.com/tidymodels/parsnip/pull/1346
* {dials} 1.4.3
  * https://github.com/tidymodels/dials/pull/435
