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
          weights = missing_arg())

# arguments agree

    Code
      translate(onet_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
        odds_link = stopping_ratio
        penalty = 0.1
        mixture = 0.25
      
      Computational engine: ordinalNet 
      
      Model fit template:
      ordered::ordinalNet_wrapper(x = missing_arg(), y = missing_arg(), 
          weights = missing_arg(), link = "cloglog", family = "stopping_ratio", 
          lambdaVals = 10^seq(-6, -1), alpha = 0.25)

---

    Code
      set.seed(13)
      onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)

# standardized link and family values are matched

    Code
      match_ordinal_link_ordinalNet("logitlink")
    Condition
      Error:
      ! `ordinal_link` must be one of "logistic", "probit", "loglog", "cloglog", or "cauchit", not "logitlink".
      i Did you mean "logistic"?

---

    Code
      match_ordinal_link_ordinalNet("logisitc")
    Condition
      Error:
      ! `ordinal_link` must be one of "logistic", "probit", "loglog", "cloglog", or "cauchit", not "logisitc".
      i Did you mean "logistic"?

---

    Code
      match_ordinal_family("cumu")
    Condition
      Error:
      ! `odds_link` must be one of "cumulative_link", "adjacent_categories", "continuation_ratio", or "stopping_ratio", not "cumu".
      i Did you mean "cumulative_link"?

