make_gen_additive_mod_vgam <- function() {
  parsnip::set_model_engine("gen_additive_mod", "classification", "vgam")
  parsnip::set_dependency(
    "gen_additive_mod",
    eng = "vgam",
    pkg = "ordered",
    mode = "classification"
  )
  parsnip::set_dependency(
    "gen_additive_mod",
    eng = "vgam",
    pkg = "VGAM",
    mode = "classification"
  )

  parsnip::set_model_arg(
    model = "gen_additive_mod",
    eng = "vgam",
    parsnip = "link",
    original = "link",
    func = list(pkg = "dials", fun = "ordinal_link"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "gen_additive_mod",
    eng = "vgam",
    parsnip = "family",
    original = "family",
    func = list(pkg = "dials", fun = "odds_link"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "gen_additive_mod",
    eng = "vgam",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "ordered", fun = "VGAM_vgam_wrapper"),
      defaults = list(
        parallel = TRUE
      )
    )
  )

  parsnip::set_encoding(
    model = "gen_additive_mod",
    eng = "vgam",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "gen_additive_mod",
    eng = "vgam",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = predict_VGAM_class_post,
      func = c(fun = "predict", pkg = "VGAM"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
    )
  )

  parsnip::set_pred(
    model = "gen_additive_mod",
    eng = "vgam",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = predict_VGAM_prob_post,
      func = c(fun = "predict", pkg = "VGAM"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
    )
  )
}
