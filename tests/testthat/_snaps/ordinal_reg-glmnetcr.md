# interfaces agree

    Code
      translate(gcr_spec)
    Condition
      Error:
      x For the "glmnetcr" engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.
      i To override the default path, use `path_values`.

---

    Code
      translate(gcr_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 1
      
      Computational engine: glmnetcr 
      
      Model fit template:
      glmnetcr::glmnetcr(x = missing_arg(), y = missing_arg(), weights = missing_arg())

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

