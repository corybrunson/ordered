#' Fit wrapper for `ocf`
#'
#' The `ocf` package requires a predictor matrix `X` and an outcome vector `Y`
#' as separate arguments, with `Y` coded as sequential integers. This wrapper
#' converts the parsnip-standard matrix (`X`) and factor (`Y`) to the required
#' formats before calling [ocf::ocf()]. It also checks the balance parameter,
#' which `ocf::ocf()` validates against a fixed value rather than the supplied
#' one.
#'
#' NB: `ocf::ocf()` converts characters to factors and factors to integers, so
#' for consistency with tidymodels conventions, `ocf` models are registered for
#' one-hot encoding.
#'
#' @param X The predictor matrix.
#' @param Y The outcome factor.
#' @param ... Additional arguments to pass.
#' @keywords internal
#' @returns An object of S3 class `ocf` as returned by [ocf::ocf()].
#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("ocf")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' house_data <- house_data[sample(nrow(house_data), 50), ]
#' X_mat <- model.matrix(~ Infl + Type + Cont - 1, data = house_data)
#' # fit wrapper
#' ( ocf_wrapper(
#'   X = X_mat,
#'    Y = house_data$Sat,
#'    n.trees = 10
#'  ) )
#' @export
ocf_wrapper <- function(X, Y, ...) {
  rlang::check_installed("ocf")
  dots <- list(...)

  if ("alpha" %in% names(dots)) {
    alpha <- dots[["alpha"]]
    if (
      ! is.numeric(alpha) ||
        length(alpha) != 1L ||
        ! is.finite(alpha) ||
        alpha < 0 ||
        alpha > 0.5
    ) {
      cli::cli_abort(
        "The {.arg alpha} argument must be a number in [0, 0.5].",
        call = rlang::call2("fit")
      )
    }
  }

  X <- as.matrix(X)
  Y <- as.numeric(Y)

  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "ocf", .ns = "ocf",
    Y = Y, X = X, !!! dots
  )
  rlang::eval_tidy(cl)
}

ocf_class_post <- function(x, object) {
  tibble::tibble(
    .pred_class = factor(object$lvl[x$classification], levels = object$lvl)
  )
}

ocf_prob_post <- function(x, object) {
  x <- x$probabilities
  colnames(x) <- paste0(".pred_", object$lvl)
  tibble::as_tibble(x)
}
