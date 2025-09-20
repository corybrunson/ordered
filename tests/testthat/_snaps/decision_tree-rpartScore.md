# interfaces agree

    Code
      odt_spec %>% translate()
    Output
      Decision Tree Model Specification (classification)
      
      Computational engine: rpartScore 
      
      Model fit template:
      ordered::rpart_score_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg())

---

    Code
      odt_f_fit
    Output
      parsnip model object
      
      n= 54 
      
      node), split, n, deviance, yval
            * denotes terminal node
      
       1) root 54 36 2  
         2) mol_weight< 552.1285 44 28 2  
           4) mol_weight< 404.9135 21 12 3  
             8) volume< 928.2365 8  5 2 *
             9) volume>=928.2365 13  3 3 *
           5) mol_weight>=404.9135 23 13 2  
            10) mol_weight< 494.5605 11  5 1 *
            11) mol_weight>=494.5605 12  5 2 *
         3) mol_weight>=552.1285 10  2 1 *

---

    Code
      odt_xy_fit
    Output
      parsnip model object
      
      n= 54 
      
      node), split, n, deviance, yval
            * denotes terminal node
      
       1) root 54 36 2  
         2) mol_weight< 552.1285 44 28 2  
           4) mol_weight< 404.9135 21 12 3  
             8) volume< 928.2365 8  5 2 *
             9) volume>=928.2365 13  3 3 *
           5) mol_weight>=404.9135 23 13 2  
            10) mol_weight< 494.5605 11  5 1 *
            11) mol_weight>=494.5605 12  5 2 *
         3) mol_weight>=552.1285 10  2 1 *

# arguments agree

    Code
      odt_def_spec %>% translate()
    Output
      Decision Tree Model Specification (classification)
      
      Computational engine: rpartScore 
      
      Model fit template:
      ordered::rpart_score_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg())

---

    Code
      odt_arg_spec %>% translate()
    Output
      Decision Tree Model Specification (classification)
      
      Main Arguments:
        cost_complexity = 0.01
        tree_depth = 3
        min_n = 7
        split_func = quad
        prune_func = mr
      
      Computational engine: rpartScore 
      
      Model fit template:
      ordered::rpart_score_wrapper(formula = missing_arg(), data = missing_arg(), 
          weights = missing_arg(), cp = 0.01, maxdepth = 3, minsplit = min_rows(7, 
              data), split = "quad", prune = "mr")

---

    Code
      set.seed(13)
      odt_def_fit <- fit(odt_def_spec, class ~ ., data = caco_train)

---

    Code
      set.seed(13)
      odt_arg_fit <- fit(odt_arg_spec, class ~ ., data = caco_train)

