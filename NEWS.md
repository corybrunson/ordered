# next version

## additional engines

**TODO:** Coordinated additions to **parsnip** are required.

This version introduces source code and unit tests for new engines:
* `lrm` and `orm` from the **rms** package

## bug fix

A bug in the prediction of `vglm` models, and associated tests, were patched.
Previously, `predict()` was used, which triggers S3 dispatch when **VGAM** is not attached but S4 dispatched when it is.
Now `predictvglm()` is used instead.

# ordered 0.1.0

Initial CRAN submission.
