
########################
## Plotting functions ##
########################

GetQuantSimple <- function(calc_function, RepOut_list, minYear, maxYear, scale_factor = 1) {
  # Ensure the provided function is callable
  if (!is.function(calc_function)) {
    stop("The provided calc_function must be a valid function.")
  }
  
  # Calculate the quantity of interest using the provided function
  QuantInterest <- do.call(rbind, lapply(RepOut_list, calc_function))
  colnames(QuantInterest) <- minYear:maxYear
  
  # Melt to long format
  QuantInterest <- reshape2::melt(QuantInterest)
  colnames(QuantInterest) <- c("Scenario", "Year", "Quant")
  
  # Apply scaling
  QuantInterest$Quant <- QuantInterest$Quant * scale_factor
  
  # Rename scenarios
  QuantInterest[, 1] <- sub("^skj_", "", QuantInterest[, 1])
  
  return(QuantInterest)
}

