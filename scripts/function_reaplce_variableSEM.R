replace_variable_in_psem <- function(psem_obj, from, to, verbose = TRUE) {

  # Extract models (everything except the final data entry)
  models <- psem_obj[1:(length(psem_obj) - 1)]
  dat <- psem_obj$data
  
  # Replace the variable name in each formula and update the model
  new_models <- lapply(models, function(mod) {
    
    # get the forumla
    old_formula <- formula(mod)
    
    # Collapse the deparsed formula into a single string
    old_formula_str <- paste(deparse(old_formula), collapse = " ")
    
    # Check if the 'from' variable exists in the formula
    if (grepl(from, old_formula_str)) {
      
      new_formula_str <- gsub(paste0("\\b", from, "\\b"), to, old_formula_str)
      
      if (verbose) {
        
        message("🔁 Replacing in model: ", deparse(old_formula), " → ", new_formula_str)
        
      }
      
      # Use paste to combine into a single string formula and convert to formula object
      new_formula <- as.formula(paste(new_formula_str, collapse = " ")) 
  
  # Ensure that the 'data' argument is passed to update
  updated_mod <- update(mod, formula = new_formula, data = dat)
  
  return(updated_mod)
  } else {
    return(mod)  # If 'from' doesn't exist, return the model unchanged
  }})
  
  
  # Rebuild the psem object
  new_psem <- do.call(psem, c(new_models, list(data = dat)))
  
  return(new_psem)
}


# psem_obj = range_richness_sem
# from = 'range_richness'
# to = 'mammal_gd'
# replace_variable_in_psem(psem_obj = range_richness_sem,
#                          from = 'range_richness',
#                          to = 'mammal_gd')
