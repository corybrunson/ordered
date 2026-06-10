# interfaces agree

    Code
      onet_spec %>% translate()
    Output
      GAM Model Specification (classification)
      
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
      GAM Model Specification (classification)
      
      Engine-Specific Arguments:
        link = cloglog
        family = stopping
      
      Computational engine: vgam 
      
      Model fit template:
      ordered::VGAM_vgam_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), link = "cloglog", family = "stopping", 
          parallel = TRUE)

---

    Code
      set.seed(13)
      onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)

