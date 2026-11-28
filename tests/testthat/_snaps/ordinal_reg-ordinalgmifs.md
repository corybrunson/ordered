# unsupported odds links and reverse combinations error

    Code
      match_odds_link_ordinalgmifs("continuation_ratio")
    Condition
      Error:
      ! `odds_link` cannot be "continuation_ratio" for the "ordinalgmifs" engine.
      i Its native "ForwardCR" and "BackwardCR" models use stopping ratios.
      i Use `odds_link` = "stopping_ratio" with `reverse` = `FALSE` or `TRUE`.

---

    Code
      match_odds_link_ordinalgmifs("continuation_ratio", reverse = TRUE)
    Condition
      Error:
      ! `odds_link` cannot be "continuation_ratio" for the "ordinalgmifs" engine.
      i Its native "ForwardCR" and "BackwardCR" models use stopping ratios.
      i Use `odds_link` = "stopping_ratio" with `reverse` = `FALSE` or `TRUE`.

---

    Code
      match_odds_link_ordinalgmifs("cumulative_link", reverse = TRUE)
    Condition
      Error:
      ! `reverse` can only be used with `odds_link` = "stopping_ratio" or native `probability.model` values "ForwardCR" and "BackwardCR".
      i Set `reverse` = `FALSE` or omit it for other odds links.

---

    Code
      match_odds_link_ordinalgmifs("Stereotype", reverse = TRUE)
    Condition
      Error:
      ! `reverse` can only be used with `odds_link` = "stopping_ratio" or native `probability.model` values "ForwardCR" and "BackwardCR".
      i Set `reverse` = `FALSE` or omit it for other odds links.

---

    Code
      match_odds_link_ordinalgmifs("stopping_ratio", reverse = NA)
    Condition
      Error:
      ! `reverse` must be `TRUE` or `FALSE`.

# translation

    Code
      translate(spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = logistic
        odds_link = stereotype
      
      Engine-Specific Arguments:
        x = c("x1", "x2")
      
      Computational engine: ordinalgmifs 
      
      Model fit template:
      ordered::ordinalgmifs_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), link = "logistic", probability.model = "stereotype", 
          x = c("x1", "x2"))

---

    Code
      translate(reverse_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cloglog
        odds_link = stopping_ratio
      
      Engine-Specific Arguments:
        x = c("x1", "x2")
        reverse = TRUE
      
      Computational engine: ordinalgmifs 
      
      Model fit template:
      ordered::ordinalgmifs_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), link = "cloglog", probability.model = "stopping_ratio", 
          x = c("x1", "x2"), reverse = TRUE)

