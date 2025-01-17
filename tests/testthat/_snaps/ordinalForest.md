# ordinal forests

    Code
      orf_spec %>% translate()
    Output
      Random Forest Model Specification (classification)
      
      Computational engine: ordinalForest 
      
      Model fit template:
      ordered::ordinal_forest_wrapper(x = missing_arg(), y = missing_arg(), 
          num.threads = 1, perffunction = "probability")

---

    Code
      orf_f_fit
    Output
      parsnip model object
      
      
      Ordinal forest 
      
      Number of observations: 54, number of covariates: 3 
      
      Classes of ordinal target variable: 
      "L" (n = 18), "M" (n = 18), "H" (n = 18) 
      
      Forest setup: 
      Number of trees in ordinal forest: 5000 
      Number of considered score sets in total: 1000 
      Number of best score sets used for approximating the optimal score set: 10 
      Number of trees per regression forests constructed in the optimization: 100 
      Performance function: "probability" 

---

    Code
      orf_xy_fit
    Output
      parsnip model object
      
      
      Ordinal forest 
      
      Number of observations: 54, number of covariates: 3 
      
      Classes of ordinal target variable: 
      "L" (n = 18), "M" (n = 18), "H" (n = 18) 
      
      Forest setup: 
      Number of trees in ordinal forest: 5000 
      Number of considered score sets in total: 1000 
      Number of best score sets used for approximating the optimal score set: 10 
      Number of trees per regression forests constructed in the optimization: 100 
      Performance function: "probability" 

---

    Code
      orf_arg_spec %>% translate()
    Output
      Random Forest Model Specification (classification)
      
      Main Arguments:
        mtry = 2
        trees = 100
        min_n = 11
      
      Computational engine: ordinalForest 
      
      Model fit template:
      ordered::ordinal_forest_wrapper(x = missing_arg(), y = missing_arg(), 
          mtry = min_cols(~2, x), ntreefinal = 100, min.node.size = min_rows(~11, 
              x), num.threads = 1, perffunction = "probability")

---

    Code
      set.seed(13)
      orf_arg_fit <- fit(orf_arg_spec, class ~ ., data = caco_train)
    Condition
      Warning in `ordinalForest::ordfor()`:
      'min.node.size' must not be smaller than 10, if perffunction = 'probability'. -> 'min.node.size.' set to 10.

