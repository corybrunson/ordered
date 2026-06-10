# interfaces agree (lrm)

    Code
      translate(lrm_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: lrm 
      
      Model fit template:
      rms::lrm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

---

    Code
      lrm_f_fit
    Output
      parsnip model object
      
      Logistic Regression Model
      
      rms::lrm(formula = class ~ ., data = data)
      
                            Model Likelihood    Discrimination    Rank Discrim.    
                                  Ratio Test           Indexes          Indexes    
      Obs            54    LR chi2     11.75    R2       0.220    C       0.703    
       L             18    d.f.            3    R2(3,54) 0.150    Dxy     0.405    
       M             18    Pr(> chi2) 0.0083    R2(3,48) 0.167    gamma   0.405    
       H             18                         Brier    0.183    tau-a   0.275    
      max |deriv| 4e-08                                                            
      
                 Coef    S.E.   Wald Z Pr(>|Z|)
      y>=M        2.0509 1.0826  1.89  0.0582  
      y>=H        0.3989 1.0435  0.38  0.7023  
      mol_weight -0.0128 0.0072 -1.77  0.0774  
      volume      0.0035 0.0030  1.15  0.2491  
      ClogP       0.0068 0.1414  0.05  0.9618  
      

---

    Code
      lrm_xy_fit
    Output
      parsnip model object
      
      Logistic Regression Model
      
      rms::lrm(formula = ..y ~ ., data = data)
      
                            Model Likelihood    Discrimination    Rank Discrim.    
                                  Ratio Test           Indexes          Indexes    
      Obs            54    LR chi2     11.75    R2       0.220    C       0.703    
       L             18    d.f.            3    R2(3,54) 0.150    Dxy     0.405    
       M             18    Pr(> chi2) 0.0083    R2(3,48) 0.167    gamma   0.405    
       H             18                         Brier    0.183    tau-a   0.275    
      max |deriv| 4e-08                                                            
      
                 Coef    S.E.   Wald Z Pr(>|Z|)
      y>=M        2.0509 1.0826  1.89  0.0582  
      y>=H        0.3989 1.0435  0.38  0.7023  
      mol_weight -0.0128 0.0072 -1.77  0.0774  
      volume      0.0035 0.0030  1.15  0.2491  
      ClogP       0.0068 0.1414  0.05  0.9618  
      

# interfaces agree (orm)

    Code
      translate(orm_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Computational engine: orm 
      
      Model fit template:
      rms::orm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

---

    Code
      orm_f_fit
    Output
      parsnip model object
      
      Logistic (Proportional Odds) Ordinal Regression Model
      
      rms::orm(formula = class ~ ., data = data)
      
                            Model Likelihood               Discrimination    Rank Discrim.    
                                  Ratio Test                      Indexes          Indexes    
      Obs            54    LR chi2     11.75    R2                  0.220    rho     0.434    
       L             18    d.f.            3    R2(3,54)            0.150    Dxy     0.405    
       M             18    Pr(> chi2) 0.0083    R2(3,48)            0.167                     
       H             18    Score chi2  11.40    |Pr(Y>=median)-0.5| 0.234                     
      ESS            48    Pr(> chi2) 0.0098                                                  
      Distinct Y      3                                                                       
      Median Y        2                                                                       
      max |deriv| 2e-12                                                                       
      
                 Coef    S.E.   Wald Z Pr(>|Z|)
      y>=M        2.0509 1.0826  1.89  0.0582  
      y>=H        0.3989 1.0435  0.38  0.7023  
      mol_weight -0.0128 0.0072 -1.77  0.0774  
      volume      0.0035 0.0030  1.15  0.2491  
      ClogP       0.0068 0.1414  0.05  0.9618  
      

---

    Code
      orm_xy_fit
    Output
      parsnip model object
      
      Logistic (Proportional Odds) Ordinal Regression Model
      
      rms::orm(formula = ..y ~ ., data = data)
      
                            Model Likelihood               Discrimination    Rank Discrim.    
                                  Ratio Test                      Indexes          Indexes    
      Obs            54    LR chi2     11.75    R2                  0.220    rho     0.434    
       L             18    d.f.            3    R2(3,54)            0.150    Dxy     0.405    
       M             18    Pr(> chi2) 0.0083    R2(3,48)            0.167                     
       H             18    Score chi2  11.40    |Pr(Y>=median)-0.5| 0.234                     
      ESS            48    Pr(> chi2) 0.0098                                                  
      Distinct Y      3                                                                       
      Median Y        2                                                                       
      max |deriv| 2e-12                                                                       
      
                 Coef    S.E.   Wald Z Pr(>|Z|)
      y>=M        2.0509 1.0826  1.89  0.0582  
      y>=H        0.3989 1.0435  0.38  0.7023  
      mol_weight -0.0128 0.0072 -1.77  0.0774  
      volume      0.0035 0.0030  1.15  0.2491  
      ClogP       0.0068 0.1414  0.05  0.9618  
      

# arguments agree (lrm)

    Code
      translate(lrm_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.1
      
      Computational engine: lrm 
      
      Model fit template:
      rms::lrm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
          penalty = 0.1)

---

    Code
      set.seed(13)
      lrm_arg_fit <- fit(lrm_arg_spec, class ~ ., data = caco_train)

# arguments agree (orm)

    Code
      translate(orm_arg_spec)
    Output
      Ordinal Regression Model Specification (classification)
      
      Main Arguments:
        ordinal_link = cauchit
        penalty = 0.1
      
      Computational engine: orm 
      
      Model fit template:
      rms::orm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
          family = "cauchit", penalty = 0.1)

---

    Code
      set.seed(13)
      orm_arg_fit <- fit(orm_arg_spec, class ~ ., data = caco_train)

