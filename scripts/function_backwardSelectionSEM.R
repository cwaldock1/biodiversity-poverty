backward_selection <- function(test_model, verbose = TRUE, save_path = NULL){
  
  repeat {
    
    # # break function if model is significant
    # if(fisherC(test_model)[3] %>% as.numeric >= 0.05){
    #   if (verbose) message("All terms are significant. Stopping selection.")
    #   break
    #   }
  
  # Get full table of coefficients
  coef_table <- coefs(test_model)
  
  # Filter non-significant terms
  nonsig <- subset(coef_table, P.Value > 0.05)
  
  # break function if no improvement found
  if (nrow(nonsig) == 0) {
    if (verbose) message("All terms are significant. Stopping selection.")
    break
  }
  
  # Get the term with highest p-value
  worst <- nonsig[which.min(abs(nonsig$Std.Estimate)), ]
  response <- worst[,'Response']
  predictor <- worst[,'Predictor']
  
  # index and find model
  model_index <- NA
  for (z in seq_len(length(test_model) - 1)) {
    mod <- test_model[[z]]
    lhs <- all.vars(formula(mod))[1]  # left-hand side variable
    if (lhs == response) {
      model_index <- z
      break
    }
  }
  
  # add defense
  if (is.na(model_index)) {
    stop("Could not match model for response variable: ", response)
  }
  
  if (verbose) {
    message("Removing '", predictor, "' from model predicting '", response,
            "' (p = ", round(worst$P.Value, 4), ")", " Std.Eff = ", round(worst$Std.Estimate, 3), 
            '\n Model FisherC is ', fisherC(test_model, conserve = T)[,3])
  }
  
  # Update model by removing the predictor
  target_model <- test_model[[model_index]]
  new_formula <- update(formula(target_model), paste(". ~ . -", predictor))
  test_model[[model_index]] <- update(target_model, formula = new_formula)
  
  }
  
  if (!is.null(save_path)) {
    final_coefs <- coefs(test_model)
    tryCatch({
      write_csv(final_coefs, file = save_path)
      if (verbose) message("Coefficient table saved to: ", save_path)
    }, error = function(e) {
      warning("Failed to save coefficient table: ", e$message)
    })
  }
  
  
  return(test_model)

}


#plot(backward_selection(mammal_gd_sem, save_path = file.path('test.csv')), layout = 'tree')



