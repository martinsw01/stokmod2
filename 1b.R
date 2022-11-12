lambda <-  5
mu <- 6

to_hours <- function(days) {
  return(24*days)
}

simulate <- function(days, lambda, mu) {

  time <- 0
  max_time <- to_hours(days=days)
  patients <- 0 # current number of patients
  states <- c(patients) # array of states (number of patients)
  time_in_state <- c(0) # time spent in a state at each time
  
  while (time < max_time) {
    if (patients == 0) { # no patients that can leave the UCC
      arrival_time <- rexp(1, lambda)
      patients <- patients + 1
      time <- time + arrival_time
    } else {
      time_until_state_change <- rexp(1, lambda+mu)
      time <- time + time_until_state_change
      
      new_patient <- sample(c(TRUE, FALSE), 1, prob=c(lambda, mu))
      if (new_patient) {
        patients <- patients + 1
      } else {
        patients <- patients - 1
      }
    }
    time_in_state <- append(time_in_state, time)
    states <- append(states, patients)
  }
  
  return(data.frame(
    time_in_state = time_in_state,
    states = states
  ))
}


avg_number_of_patients <- function(time_in_state, states) {
  len <- length(states)
  sum(diff(time_in_state)*states[2:len])/time_in_state[len]
}

expected_time <- function(time_in_state, states, lambda) {
  avg_number_of_patients(time_in_state, states)/lambda
}


sim <- simulate(50, lambda, mu)
time_in_state <- sim$time_in_state
states <- sim$states

expected_time(time_in_state, states, lambda)


mean_expected_time <- function(n, days, lambda, mu) {
  w <- rep(0, n)
  for (i in 1:n) {
    sim <- simulate(days, lambda, mu)
    w[i] <- expected_time(sim$time_in_state, sim$states, lambda)
  }
  
  CI <- qt(0.975, df=n-1)*sd(w)/sqrt(n)
  
  data.frame(
    w_avg = mean(w),
    CI = CI
  )
}

mean_expected_time(n=30, days=50, lambda=lambda, mu=mu)

# plot as discontinuous vertical lines
discrete <- function(x, y, ...) {
  len <- length(x)
  for (i in 2:len) {
    points(x[i+0:1], y[i-1]*c(1, 1), ...)
  }
}

plot_simulation <- function(time_in_state, states) {
  plot(x = NA, y = NA, pch = NA, 
       xlim = c(0, tail(time_in_state, 1)[1]), # max time
       ylim = c(0, max(states)),
       xlab = "Hours",
       ylab = "Patients")
  
  discrete(time_in_state, states, type="l", lwd=4)
}



sim <- simulate(0.5, lambda, mu)
time_in_state <- sim$time_in_state
states <- sim$states
plot_simulation(time_in_state, states)
