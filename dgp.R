

# Athey Wager 2021 "Policy Learning With Observational Data." data generating code 
# https://github.com/grf-labs/policytree/blob/master/r-package/policytree/R/data.R

#setwd("Z:/skolarbete/gradu/R")


# Helper functions

transform_unbounded_interval_to_0_1 <- function(x){
  return(1/(1+exp(-2*x)))
}


replace_values_of_columns <- function(X,col_vector,replacing_fraction){
  # replaces a given fraction of values of given columns with values from N(0,1)
  replaceables <- X[,col_vector]
  replaceables[
    sample(
      1:length(replaceables),
      replacing_fraction*length(replaceables),
      replace = FALSE)] <- rnorm(replacing_fraction*length(replaceables),0,1)
  X[,col_vector] <- replaceables
  return(X)
}
gen_data <- function(r_error_fraction,m_error_fraction,tau_error_fraction,n = 1000,seed = 1){
  
  set.seed(seed)
  p <- 10
  r_independent_from_m <- TRUE
  
  X <- matrix(rnorm(n * p), n, p)
  
  r_variables <- c(3,4,7,8)
  
  W <- rbinom(n = n, size = 1, prob = 1/2)
  
  if (r_independent_from_m) {
    m_variables <- c(5,6)
  } else {
    m_variables <- c(3,5)
  }
  m <- 10 + round(pmax(0,rowSums(X[,m_variables])))*5 
  
  tau <- (abs(X[, 1]) + abs(X[, 2]) - 1) 
  
  r_unedited <- rowSums(X[,r_variables]) + 0.8  + W * tau 
  r <- transform_unbounded_interval_to_0_1(r_unedited)
  
  Y <- ifelse(r>0.5,0,1)
  
  # check/fix
  tau_prob <- transform_unbounded_interval_to_0_1(rowSums(X[,r_variables]) + 0.8  + tau)  - transform_unbounded_interval_to_0_1(rowSums(X[,r_variables]) + 0.8)
  
  X <- replace_values_of_columns(X,r_variables,r_error_fraction)
  X <- replace_values_of_columns(X,m_variables,m_error_fraction)
  X <- replace_values_of_columns(X,c(1,2),tau_error_fraction)
  
  list(
    r_error_fraction = r_error_fraction,
    m_error_fraction = m_error_fraction,
    w = W,
    x = X,
    y = Y,
    revenues = m,
    r = r,
    tau_prob = tau_prob
  )
}

