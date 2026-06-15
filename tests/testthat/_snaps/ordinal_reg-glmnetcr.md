# interfaces agree

    Code
      translate(gcr_spec)
    Condition
      Warning:
      x The glmnetcr engine ignores `penalty` in favor of a path that enables prediction at interpolated penalty values.
      ! `penalty` was passed 0 values.
      i Use `path_values` to override the default path.
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: glmnetcr 
      
      Model fit template:
      glmnetcr::glmnetcr(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          nlambda = 120L, lambda.min.ratio = 1e-08)

---

    Code
      translate(gcr_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 1
      
      Computational engine: glmnetcr 
      
      Model fit template:
      glmnetcr::glmnetcr(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          nlambda = 120L, lambda.min.ratio = 1e-08)

# arguments agree

    Code
      translate(gcr_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 1
        mixture = 0.25
      
      Engine-Specific Arguments:
        method = forward
      
      Computational engine: glmnetcr 
      
      Model fit template:
      glmnetcr::glmnetcr(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = pen_vec, alpha = 0.25, method = "forward")

