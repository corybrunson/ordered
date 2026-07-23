# updating

    Code
      update(set_engine(ordinal_reg(ordinal_link = "cauchit"), "ordinalNet",
      standardize = FALSE), ordinal_link = tune(), standardize = tune())
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = tune()
      
      Engine-Specific Arguments:
        standardize = tune()
      
      Computational engine: ordinalNet 
      

# bad input

    Code
      res <- translate(set_engine(ordinal_reg(mode = "classification"), NULL))
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {polr, clm, lrm, orm, vglm, ordinalNet, glmnetcr}.

---

    Code
      ordinal_reg(mode = "regression")
    Condition
      Error in `ordinal_reg()`:
      ! `mode` should be 'classification'

---

    Code
      translate(set_engine(ordinal_reg(mode = "classification"), "wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `ordinal_reg()`.
      i See `show_engines("ordinal_reg")`.

# parallel_reg accepts logical input

    Code
      ordinal_reg(parallel_reg = TRUE)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = TRUE
      
      Computational engine: polr 
      

---

    Code
      ordinal_reg(parallel_reg = FALSE)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = FALSE
      
      Computational engine: polr 
      

# parallel_reg accepts formula input

    Code
      ordinal_reg(parallel_reg = TRUE ~ x)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = TRUE ~ x
      
      Computational engine: polr 
      

---

    Code
      ordinal_reg(parallel_reg = FALSE ~ y + z)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = FALSE ~ y + z
      
      Computational engine: polr 
      

# parallel_reg accepts list input

    Code
      ordinal_reg(parallel_reg = list(TRUE ~ x))
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = list(TRUE ~ x)
      
      Computational engine: polr 
      

---

    Code
      ordinal_reg(parallel_reg = list(FALSE ~ x, TRUE ~ y))
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = list(FALSE ~ x, TRUE ~ y)
      
      Computational engine: polr 
      

---

    Code
      ordinal_reg(parallel_reg = list(FALSE, TRUE ~ y))
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        parallel_reg = list(FALSE, TRUE ~ y)
      
      Computational engine: polr 
      

# parallel_reg rejects invalid inputs

    Code
      fit(set_engine(ordinal_reg(parallel_reg = "TRUE"), "clm"), Sat ~ Infl + Cont,
      data = house_sub)
    Condition
      Error in `fit()`:
      ! `parallel_reg` must be a single logical value, a formula with a logical LHS, or a list of at most two such elements.

---

    Code
      fit(set_engine(ordinal_reg(parallel_reg = Sat ~ Infl), "clm"), Sat ~ Infl +
      Cont, data = house_sub)
    Condition
      Error in `fit()`:
      ! The LHS of `parallel_reg` formula must be TRUE or FALSE.

---

    Code
      fit(set_engine(ordinal_reg(parallel_reg = list(TRUE ~ Infl, FALSE ~ Cont, TRUE)),
      "clm"), Sat ~ Infl + Cont, data = house_sub)
    Condition
      Error in `fit()`:
      ! `parallel_reg` list can have at most 2 elements.

