# old {rms} version gates unsupported arguments (orm)

    Code
      fit(tidy_spec, Sat ~ Type + Cont, data = house_sub, case_weights = frequency_weights(
        rep(1L, nrow(house_sub))))
    Condition
      Error:
      ! The "orm" engine does not support case weights with rms "6.9.0".
      i Use rms >= 7.0-0 or omit `case_weights`.

---

    Code
      fit(set_mode(set_engine(ordinal_reg(penalty = 0.01), "orm"), "classification"),
      Sat ~ Type + Cont, data = house_sub)
    Condition
      Error:
      ! The "orm" engine does not support a nonzero `penalty` with rms "6.9.0".
      i Use rms >= 7.0-0 or omit `penalty`.

---

    Code
      zero_penalty_fit <- fit(set_mode(set_engine(ordinal_reg(penalty = 0), "orm"),
      "classification"), Sat ~ Type + Cont, data = house_sub)
    Condition
      Warning:
      The `penalty` argument was set to 0, which is not supported by rms "6.9.0" for the "orm" engine.
      i The argument will be omitted.

# interfaces agree (lrm)

    Code
      translate(lrm_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: lrm 
      
      Model fit template:
      rms::lrm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

# interfaces agree (orm)

    Code
      translate(orm_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: orm 
      
      Model fit template:
      ordered::rms_orm_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg())

# arguments agree (lrm)

    Code
      translate(lrm_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.1
      
      Computational engine: lrm 
      
      Model fit template:
      rms::lrm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
          penalty = 0.1)

# arguments agree (orm)

    Code
      translate(orm_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cauchit
        penalty = 0.1
      
      Computational engine: orm 
      
      Model fit template:
      ordered::rms_orm_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), family = "cauchit", penalty = 0.1)

