# interfaces agree

    Code
      onet_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.001
      
      Computational engine: ordinalNet 
      
      Model fit template:
      ordered::ordinal_net_wrapper(x = missing_arg(), y = missing_arg(), 
          weights = missing_arg(), lambdaVals = 0.001)

---

    Code
      onet_f_fit
    Output
      parsnip model object
      
      
      Summary of fit:
      
        lambdaVals nNonzero    loglik     devPct      aic     bic
      1      0.001        5 -53.46003 0.09886269 116.9201 126.865
      

---

    Code
      onet_xy_fit
    Output
      parsnip model object
      
      
      Summary of fit:
      
        lambdaVals nNonzero    loglik     devPct      aic     bic
      1      0.001        5 -53.46003 0.09886269 116.9201 126.865
      

# arguments agree

    Code
      onet_arg_spec %>% translate()
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
        odds_link = stopping
        penalty = 0.001
        mixture = 0.25
      
      Computational engine: ordinalNet 
      
      Model fit template:
      ordered::ordinal_net_wrapper(x = missing_arg(), y = missing_arg(), 
          weights = missing_arg(), link = "cloglog", family = "stopping", 
          lambdaVals = 0.001, alpha = 0.25)

---

    Code
      set.seed(13)
      onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)

