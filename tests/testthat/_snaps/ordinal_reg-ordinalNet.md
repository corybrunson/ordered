# interfaces agree

    Code
      translate(onet_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.01
      
      Computational engine: ordinalNet 
      
      Model fit template:
      ordered::ordinalNet_wrapper(x = missing_arg(), y = missing_arg(), 
          weights = missing_arg(), nLambda = 120L, lambdaMinRatio = 1e-08, 
          includeLambda0 = TRUE)

# arguments agree

    Code
      translate(onet_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
        odds_link = stopping
        penalty = 0.1
        mixture = 0.25
      
      Computational engine: ordinalNet 
      
      Model fit template:
      ordered::ordinalNet_wrapper(x = missing_arg(), y = missing_arg(), 
          weights = missing_arg(), link = "cloglog", family = "stopping", 
          lambdaVals = 10^seq(-6, -1), alpha = 0.25)

---

    Code
      set.seed(13)
      onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)

# parallel regression argument handles formulae

    Code
      fit(ordinal_reg(parallel_reg = TRUE ~ Infl, penalty = 0.01, engine = "ordinalNet"),
      Sat ~ Cont, data = house_sub)
    Condition
      Error in `parallel_reg_to_ordinalNet()`:
      ! The "ordinalNet" engine does not support partial parallelism.
      i Use engine "clm" or "vglm" for partial parallelism.

