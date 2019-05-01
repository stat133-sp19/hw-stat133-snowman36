#' @importFrom graphics barplot lines plot

#' @title bin_choose
#' @description Caculate the combinations "n choose k"
#' @param n the total number of trials
#' @param k the total number of successes
#' @return the combinations "n choose k"
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)

bin_choose <- function(n, k) {
  if(max(k) > n) {
    stop("\nk cannot be greater than n")
  }
  number <- factorial(n)/factorial(k)/factorial(n-k)
  return(number)
}


#' @title bin_probability
#' @description To caculate the result of a binominal distribution
#' @param success the number of successes
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return the result of a binominal distribution.
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'
bin_probability <- function(success, trials, prob) {
  if(!check_trials(trials)) {
    stop("\ninvalid trials value")
  }
  if(!check_prob(prob)) {
    stop("\ninvaild prob value")
  }
  if(!check_success(success, trials)) {
    stop("\ninvalid success value")
  }

  probability <- bin_choose(trials, success) * prob^success * (1-prob)^(trials-success)

  return(probability)
}



#' @title bin_distribution
#' @description Create an object of class \code{"bindis"}
#' @param trials the total number of the trials
#' @param prob the probability of success every time
#' @return an object of class \code{"bindis"}
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)


bin_distribution <- function(trials, prob) {
  if(!check_trials(trials)) {
    stop("\ninvalid trials value")
  }
  if(!check_prob(prob)) {
    stop("\ninvaild prob value")
  }

  success <- 0:trials
  probability <- bin_probability(success = success, trials = trials, prob = prob)

  result <- list(success = success, probability = probability)
  class(result) <- "bindis"
  return(result)
}

#' @export
print.bindis <- function(x) {
  cat("\n")
  result <- data.frame(success = x$success, probability = x$probability)
  print(result)
  invisible(x)
}



#' @export

plot.bindis <- function(x) {
  barplot(x$probability,
          xlab = 'success',
          ylab = "probability",
          names.arg = c(0,1,2,3,4,5))
}




#' @title bin_cumulative
#' @description Create an object of class \code{"bincum"}
#' @param trials the total number of the trials
#' @param prob the probability of success every time
#' @return an object of class \code{"bindis"}
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)


bin_cumulative <- function(trials, prob) {
  if(!check_trials(trials)) {
    stop("\ninvalid trials value")
  }
  if(!check_prob(prob)) {
    stop("\ninvaild prob value")
  }

  success <- 0:trials
  probability <- bin_probability(success = success, trials = trials, prob = prob)

  result <- list(success = success, probability = probability, cumulative = cumsum(probability))
  class(result) <- "bincum"
  return(result)
}


#' @export
print.bincum <- function(x) {
  result <- data.frame(success = x$success, probability = x$probability, cumulative = x$cumulative)
  print(result)
  invisible(x)
}


#' @export
plot.bincum <- function(x) {
  plot(x$success,x$cumulative,
       xlab = "success",
       ylab = "probability")
  lines(x$success, x$cumulative)
}







#' @title bin_variable
#' @description Create an object of class \code{"binvar"}
#' @param trials the total number of the trials
#' @param prob the probability of success every time
#' @return an object of class \code{"binvar"}
#' @export
#' @examples
#' bin_variable(trials = 5, prob = 0.5)
bin_variable <- function(trials, prob){
  if(!check_trials(trials)) {
    stop("\ninvalid trials value")
  }
  if(!check_prob(prob)) {
    stop("\ninvaild prob value")
  }

  obj <- list(trials = trials, prob = prob)
  class(obj) <- "binvar"
  obj
}


#' @export
print.binvar <- function(x) {
  cat(sprintf("\n- number of trials: %s", x$trials))
  cat(sprintf("\n- prob of success: %s", x$prob))
  invisible(x)
}


#' @export
summary.binvar <- function(x) {
  trials <- x$trials
  prob <- x$prob
  obj <- list(trials = trials,
              prob = prob,
              mean = aux_mean(trials = trials, prob = prob),
              variance = aux_variance(trials = trials,prob = prob),
              mode = aux_mode(trials = trials,prob = prob),
              skewness = aux_skewness(trials = trials,prob = prob),
              kurtosis = aux_kurtosis(trials = trials,prob = prob))
  class(obj) <- "summary.binvar"
  obj
}


#' @export
print.summary.binvar <- function(x) {
  trials <- x$trials
  prob <- x$prob
  cat("\nSummary Binomial\n\n")
  cat("Paramaters")
  cat(sprintf("\n- number of trials: %s", trials))
  cat(sprintf("\n- prob of success: %s", prob))
  cat("\n\nMeasures")
  cat(sprintf("\n- mean\t: %s",aux_mean(trials = trials,prob = prob)))
  cat(sprintf("\n- variance\t: %s",aux_variance(trials = trials,prob = prob)))
  cat(sprintf("\n- mode\t: %s",aux_mode(trials = trials,prob = prob)))
  cat(sprintf("\n- skewness\t: %s",aux_skewness(trials = trials,prob = prob)))
  cat(sprintf("\n- kurtosis\t: %s",aux_kurtosis(trials = trials,prob = prob)))

}


#' @title bin_mean
#' @description caculate the mean of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_mean(trials = 5, prob = 0.5)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_mean(trials,prob)
}


#' @title bin_variance
#' @description caculate the variance of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_variance(trials = 5, prob = 0.5)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_variance(trials,prob)
}



#' @title bin_mode
#' @description caculate the mode of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_mode(trials = 5, prob = 0.5)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_mode(trials,prob)
}




#' @title bin_skewness
#' @description caculate the skewness of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_skewness(trials = 5, prob = 0.5)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_skewness(trials,prob)
}



#' @title bin_kurtosis
#' @description caculate the kurtosis of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_kurtosis(trials = 5, prob = 0.5)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_kurtosis(trials,prob)
}



