
#functin that runs a linear model and returns the slope and R2
#returns as a table
#can be run in data table

modout <- function(yvar, xvar1) {
  # Make the model
  model <- lm(yvar ~ xvar1 + 0)
  # Transpose the coef of the model and cast as data.table
  coefOut <- data.table(t(coef(model)))
  # extract r-squared from model
  rsqOut <- data.table(rsq(model))
  # label the column name for the rsqOut
  names(rsqOut)<-c("rsq")
  # Return combined columns
  out <- data.table(coefOut, rsqOut)
  names(out) <- c("Slope", "R2")
  return(out)
}
