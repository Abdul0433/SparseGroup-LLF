if(efficient.run){
  
  # limit number of repetitions for each simulation
  num.reps = 100 
  
  # Tuning parameters automatically tuned for random forests and LLF. 
  # Set to "all" for best performance. 
  # Select a subset (or "none", by default) for faster performance.
  tune = c("sample.fraction", "mtry", "min.node.size")
  ll.lambda = 0.1
  
  # limit XGB tuning 
  # note: HIGHLY recommended for code speed. Unfeasible to run many simulations with larger numbers.
  xgb_max = 100
  num_search_rounds = 5
  
  # limit BART tuning 
  ndpost = 200
} else {
  
  # increase number of repetitions 
  num.reps = 100
  
  # Tuning parameters automatically tuned for forests and LLF. 
  # Set to "all" for best performance. 
  # Set to "none" for more efficient performance.
  tune = "all" 
  ll.lambda = NULL
  
  if (full.xgb.tune) {
    # default R-learner cross-validation for XGB
    xgb_max = 100
    num_search_rounds = 1000
  } else {
    xgb_max = 100
    num_search_rounds = 5
  }

  # return to default cross-validation for BART
  ndpost = 1000
  
}
