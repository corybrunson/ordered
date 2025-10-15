# interfaces agree

    Code
      onet_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
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
          parallel = TRUE), data = data)
      
      
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
          parallel = TRUE), data = data)
      
      
      Coefficients:
      (Intercept):1 (Intercept):2    mol_weight        volume         ClogP 
       -2.050864921  -0.398857082   0.012801094  -0.003472605  -0.006786418 
      
      Degrees of Freedom: 108 Total; 103 Residual
      Residual deviance: 106.903 
      Log-likelihood: -53.45152 

---

    Code
      onet_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: vgam 
      
      Model fit template:
      ordered::VGAM_vgam_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), parallel = TRUE)

---

    Code
      onet_f_fit
    Output
      parsnip model object
      
      
      Call:
      VGAM::vgam(formula = formula, family = VGAM::cumulative(link = "logitlink", 
          parallel = TRUE), data = data)
      
      
      Degrees of Freedom: 108 Total; 103 Residual
      Residual deviance: 106.903 
      Log-likelihood: -53.45152 

---

    Code
      onet_xy_fit
    Output
      parsnip model object
      
      
      Call:
      VGAM::vgam(formula = formula, family = VGAM::cumulative(link = "logitlink", 
          parallel = TRUE), data = data)
      
      
      Degrees of Freedom: 108 Total; 103 Residual
      Residual deviance: 106.903 
      Log-likelihood: -53.45152 

# arguments agree

    Code
      onet_arg_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
        odds_link = stopping
      
      Computational engine: vglm 
      
      Model fit template:
      ordered::VGAM_vglm_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), link = "cloglog", family = "stopping", 
          parallel = TRUE)

---

    Code
      set.seed(13)
      onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)

---

    Code
      onet_arg_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
        odds_link = stopping
      
      Computational engine: vgam 
      
      Model fit template:
      ordered::VGAM_vgam_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), link = "cloglog", family = "stopping", 
          parallel = TRUE)

---

    Code
      set.seed(13)
      onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)

