# interfaces agree

    Code
      translate(ocf_spec)
    Output
      Random Forest Model Specification (classification)
      
      Computational engine: ocf 
      
      Model fit template:
      ordered::ocf_wrapper(X = missing_arg(), Y = missing_arg(), n.threads = 1)

# arguments agree

    Code
      translate(ocf_arg_spec)
    Output
      Random Forest Model Specification (classification)
      
      Main Arguments:
        mtry = 2
        trees = 20
        min_n = 11
      
      Engine-Specific Arguments:
        sample.fraction = 0.7
        honesty = TRUE
        replace = TRUE
        alpha = 0.3
        max.depth = 5
      
      Computational engine: ocf 
      
      Model fit template:
      ordered::ocf_wrapper(X = missing_arg(), Y = missing_arg(), mtry = min_cols(~2, 
          x), n.trees = 20, min.node.size = min_rows(~11, x), sample.fraction = 0.7, 
          honesty = TRUE, replace = TRUE, alpha = 0.3, max.depth = 5, 
          n.threads = 1)

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

---

    Code
      print(unlist(tmp))
    Output
                    pkg               fun          argument 
              "ordered" "replace_samples"         "replace" 

---

    Code
      print(unlist(tmp))
    Output
                  pkg             fun        argument 
            "ordered" "alpha_balance"         "alpha" 

---

    Code
      print(unlist(tmp))
    Output
               pkg          fun     argument 
           "dials" "tree_depth"  "max.depth" 

