#' @export
predict._clm <- function(object, new_data, type = NULL, opts = list(), ...) {
  if (! is.null(type) && type == "linear_pred") {
    return(predict_linear_pred(object, new_data = new_data, ...))
  }
  predict.model_fit(
    object, new_data = new_data, type = type, opts = opts, ...
  )
}
