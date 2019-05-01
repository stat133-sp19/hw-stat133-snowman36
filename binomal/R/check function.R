
# private function to check probility
check_prob <- function(prob) {
  if (0 > prob | prob > 1) {
    stop("\np has to be a number between 0 and 1")
  }
  TRUE
}

# private function to check trials
check_trials <- function(trials) {
  if(trials <= 0) {
    stop("\ntrials has to be a number greater than 0")
  }
  TRUE
}

#private function to check success
check_success <- function(success, trials) {
  if(max(success) > trials) {
    stop("\nsuccess cannot be greater than trials")
  }
  if(min(success) < 0) {
    stop("\nsuccess cannot be less than 0")
  }
  TRUE
}
