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

* This is a new release so the NEWS is minimal.

* The following words were flagged but are correctly spelled:
  Galimberti (19:8)
  Hanlon (16:14)
  Hornung (22:8)
  Maso (19:38)
  Rathouz (16:26)
  Soffritti (19:20)
  Wurm (16:8)

* Occasionally my computers could not verify the current time.

No ERRORs, WARNINGs, or other NOTEs obtained.
