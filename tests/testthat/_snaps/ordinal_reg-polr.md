# interfaces agree

    Code
      olpr_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: polr 
      
      Model fit template:
      MASS::polr(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

---

    Code
      olpr_f_fit
    Output
      parsnip model object
      
      Call:
      MASS::polr(formula = class ~ ., data = data)
      
      Coefficients:
        mol_weight       volume        ClogP 
      -0.012801682  0.003472769  0.006759441 
      
      Intercepts:
             L|M        M|H 
      -2.0510225 -0.3989855 
      
      Residual Deviance: 106.903 
      AIC: 116.903 

---

    Code
      olpr_xy_fit
    Output
      parsnip model object
      
      Call:
      MASS::polr(formula = ..y ~ ., data = data)
      
      Coefficients:
        mol_weight       volume        ClogP 
      -0.012801682  0.003472769  0.006759441 
      
      Intercepts:
             L|M        M|H 
      -2.0510225 -0.3989855 
      
      Residual Deviance: 106.903 
      AIC: 116.903 

# arguments agree

    Code
      olpr_arg_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
      
      Computational engine: polr 
      
      Model fit template:
      MASS::polr(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
          method = "cloglog")

---

    Code
      set.seed(13)
      olpr_arg_fit <- fit(olpr_arg_spec, class ~ ., data = caco_train)

