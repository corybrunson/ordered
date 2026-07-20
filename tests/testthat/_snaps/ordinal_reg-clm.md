# interfaces agree

    Code
      translate(clm_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: clm 
      
      Model fit template:
      ordered::clm_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg())

---

    Code
      clm_f_fit
    Output
      parsnip model object
      
      formula: class ~ mol_weight + volume + ClogP
      data:    data
      
       link  threshold nobs logLik AIC    niter max.grad cond.H 
       logit flexible  54   -53.45 116.90 4(0)  3.60e-08 6.9e+07
      
      Coefficients:
      mol_weight     volume      ClogP 
       -0.012801   0.003473   0.006768 
      
      Threshold coefficients:
          L|M     M|H 
      -2.0509 -0.3989 

---

    Code
      clm_xy_fit
    Output
      parsnip model object
      
      formula: ..y ~ mol_weight + volume + ClogP
      data:    data
      
       link  threshold nobs logLik AIC    niter max.grad cond.H 
       logit flexible  54   -53.45 116.90 4(0)  3.60e-08 6.9e+07
      
      Coefficients:
      mol_weight     volume      ClogP 
       -0.012801   0.003473   0.006768 
      
      Threshold coefficients:
          L|M     M|H 
      -2.0509 -0.3989 

# arguments agree

    Code
      translate(clm_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = probit
      
      Computational engine: clm 
      
      Model fit template:
      ordered::clm_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), link = "probit")

# parallel regression argument handles lists

    Code
      fit(ordinal_reg(parallel_reg = list(FALSE ~ Infl, TRUE ~ Infl + Cont), engine = "clm"),
      Sat ~ Infl + Cont, data = house_sub)
    Condition
      Error in `list_to_clm_nominal()`:
      ! Variable "Infl" appears in both parallel and non-parallel specifications. The `clm` engine must treat each predictor as either parallel regression or category-specific (not both).

