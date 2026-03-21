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
      ! Missing engine. Possible mode/engine combinations are: classification {polr, ordinalNet, vglm, vgam}.

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

