# next release

## bug fix

A bug in the prediction of `vglm` models, and associated tests, were patched.
Previously, `predict()` was used, which triggers S3 dispatch when **VGAM** is not attached but S4 dispatched when it is.
Now `predictvglm()` is used instead.

# ordered 0.1.0

Initial CRAN submission.
