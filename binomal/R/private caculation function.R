#auxiliary function for bin_mean function
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

#auxiliary function for bin_variance function
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

#auxiliary function for bin_mode function
aux_mode <- function(trials, prob) {
  return(floor(trials * prob + prob))
}

#auxiliary function for bin_skewness function
aux_skewness <- function(trials, prob) {
  return((1-2*prob)/sqrt(trials*prob*(1-prob)))
}

#auxiliary function for bin_mkurtosis function
aux_kurtosis <- function(trials, prob) {
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}
