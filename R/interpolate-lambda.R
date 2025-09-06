# `use_extreme` is a placeholder for a policy that we need to set; do we error
# when predicting outside of the observed penalty range or do something else
# (such as predicting at the closest value in the path)

adjacent_penalties <- function(object, penalty, use_extreme = TRUE) {
  ref <- object$lambdaVals
  in_rng <- penalty >= min(ref) & penalty <= max(ref)
  if (!in_rng) {
    cli::cli_abort("The penalty value of {format(penalty, digits = 3)} is
                    outside of the range of penalties contained in the model
                    object.", call = rlang::call2("predict"))
  }

  above <- which.min(ifelse(ref < penalty,  Inf, ref))
  below <- which.max(ifelse(ref > penalty, -Inf, ref))
  unique(sort(c(below, above)))
}

predict_ordinal_net <- function(object, new_data = NULL, penalty) {
  obs_penalties <- object$lambdaVals
  penalties_ind <- adjacent_penalties(object, penalty)
  penalties <- obs_penalties[penalties_ind]
  pred <- predict(object, newx = new_data, whichLambda = penalties_ind[1])
  if (length(penalties_ind) == 2) {
    pred_high <- predict(object, newx = new_data, whichLambda = penalties_ind[2])
    pred <- approx_prediction(pred, pred_high, penalties, penalty)
  }
  colnames(pred) <- paste(1:ncol(pred))
  dplyr::as_tibble(pred)
}

mulit_predict_ordinal_net <- function(object, new_data = NULL, penalty) {
  penalty <- sort(unique(penalty))
  if (length(penalty) < 2) {
    cli::cli_abort("There should be at least 2 penalty values for
                   {.fn multi_predict}; please use {.fn predict}) instead.",
                   call = rlang::call2("multi_predict"))
  }
  purrr::map_dfr(
    penalty,
    ~ predict_ordinal_net(object, new_data, penalty = .x) %>%
      parsnip::add_rowindex() %>%
      dplyr::mutate(penalty = .x))
}

approx_prediction <- function(low, high, adjacent, penalty) {
  res <- low * NA_real_
  num_cls <- ncol(low)
  both <- cbind(low, high)
  for (i in 1:num_cls) {
    tmp <- both[, c(i, i + num_cls)]
    res[, i] <- apply(tmp, 1, approx_prediction_row, adjacent, penalty)
  }
  res <- apply(res, 1, function(x) x / sum(x))
  t(res)
}

approx_prediction_row <- function(values, adjacent, penalty) {
  approx(adjacent, values, xout = penalty)$y
}
