# interfaces agree

    Code
      translate(orf_spec)
    Output
      Random Forest Model Specification (classification)
      
      Computational engine: orf 
      
      Model fit template:
      ordered::orf_wrapper(X = missing_arg(), Y = missing_arg())

---

    Code
      orf_f_fit
    Output
      parsnip model object
      
      Ordered Forest object of class orf 
      
      Number of Categories:             3 
      Sample Size:                      54 
      Number of Trees:                  1000 
      Build:                            Subsampling 
      Mtry:                             2 
      Minimum Node Size:                5 
      Honest Forest:                    TRUE 
      Weight-Based Inference:           FALSE 

---

    Code
      orf_xy_fit
    Output
      parsnip model object
      
      Ordered Forest object of class orf 
      
      Number of Categories:             3 
      Sample Size:                      54 
      Number of Trees:                  1000 
      Build:                            Subsampling 
      Mtry:                             2 
      Minimum Node Size:                5 
      Honest Forest:                    TRUE 
      Weight-Based Inference:           FALSE 

# arguments agree

    Code
      translate(orf_arg_spec)
    Output
      Random Forest Model Specification (classification)
      
      Main Arguments:
        mtry = 2
        trees = 100
        min_n = 11
      
      Engine-Specific Arguments:
        sample.fraction = 0.7
        honesty = TRUE
        honesty.fraction = 0.4
      
      Computational engine: orf 
      
      Model fit template:
      ordered::orf_wrapper(X = missing_arg(), Y = missing_arg(), mtry = min_cols(~2, 
          x), num.trees = 100, min.node.size = min_rows(~11, x), sample.fraction = 0.7, 
          honesty = TRUE, honesty.fraction = 0.4)

---

    Code
      set.seed(13)
      orf_arg_fit <- fit(orf_arg_spec, class ~ ., data = caco_train)

# engine arguments are registered

    Code
      print(unlist(tmp))
    Output
                    pkg               fun          argument 
              "ordered" "sample_fraction" "sample.fraction" 

---

    Code
      print(unlist(tmp))
    Output
            pkg       fun  argument 
      "ordered" "honesty" "honesty" 

---

    Code
      print(unlist(tmp))
    Output
                     pkg                fun           argument 
               "ordered" "honesty_fraction" "honesty.fraction" 

