# interfaces agree

    Code
      translate(onet_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = TRUE
      
      Computational engine: vglm 
      
      Model fit template:
      ordered::VGAM_vglm_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), parallel = TRUE)

---

    Code
      onet_f_fit
    Output
      parsnip model object
      
      
      Call:
      VGAM::vglm(formula = formula, family = VGAM::cumulative(link = "logitlink", 
          parallel = TRUE, Thresh = NULL), data = data)
      
      
      Coefficients:
      (Intercept):1 (Intercept):2    mol_weight        volume         ClogP 
       -2.050864921  -0.398857082   0.012801094  -0.003472605  -0.006786418 
      
      Degrees of Freedom: 108 Total; 103 Residual
      Residual deviance: 106.903 
      Log-likelihood: -53.45152 

---

    Code
      onet_xy_fit
    Output
      parsnip model object
      
      
      Call:
      VGAM::vglm(formula = formula, family = VGAM::cumulative(link = "logitlink", 
          parallel = TRUE, Thresh = NULL), data = data)
      
      
      Coefficients:
      (Intercept):1 (Intercept):2    mol_weight        volume         ClogP 
       -2.050864921  -0.398857082   0.012801094  -0.003472605  -0.006786418 
      
      Degrees of Freedom: 108 Total; 103 Residual
      Residual deviance: 106.903 
      Log-likelihood: -53.45152 

# arguments agree

    Code
      translate(onet_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
        odds_link = stopping_ratio
        parallel_reg = TRUE
      
      Computational engine: vglm 
      
      Model fit template:
      ordered::VGAM_vglm_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), link = "cloglog", family = "stopping_ratio", 
          parallel = TRUE)

---

    Code
      set.seed(13)
      onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)

# standardized link, family, and threshold values are matched

    Code
      match_ordinal_link_VGAM("loglog")
    Condition
      Error:
      ! The VGAM engines do not support the log-log ordinal link.
      i See `?VGAM::Links` for provided link functions.

---

    Code
      match_ordinal_link_VGAM("logisitc")
    Condition
      Error:
      ! `ordinal_link` must be one of "logistic", "probit", "loglog", "cloglog", "cauchit", "foldsqrt", "logc", "gord", "pord", or "nbord", not "logisitc".
      i Did you mean "logistic"?

---

    Code
      match_ordinal_family("cumu")
    Condition
      Error:
      ! `odds_link` must be one of "cumulative_link", "adjacent_categories", "continuation_ratio", or "stopping_ratio", not "cumu".
      i Did you mean "cumulative_link"?

---

    Code
      match_threshold_structure_VGAM(c("flexible", "equidistant"))
    Condition
      Error:
      ! `threshold_structure` must be a single string, not a character vector.

# the adjacent categories family rejects incompatible links

    Code
      check_ordinal_link_family_VGAM(family = "acat", link = "logitlink")
    Condition
      Error:
      ! The "adjacent_categories" family is not compatible with the "logitlink" link function.
      i Use "cauchitlink" or "identitylink" instead.

