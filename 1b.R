lambda <-  5
mu <- 6

to_hours <- function(days) {
  return(24*days)
}

simulate <- function(days, lambda, mu) {

  time <- 0
  max_time <- to_hours(days=days)
  patients <- 0
  state <- c(patients)
  time_in_state <- c(0)
  
  while (time < max_time) {
    if (patients == 0) {
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
    state <- append(state, patients)
  }
  
  return(data.frame(
    time_in_state = time_in_state,
    state = state
  ))
}

plot_res <- function(time_in_state, state) {
  plot(x = NA, y = NA, pch = NA, 
       xlim = c(0, 50*24),#max_time), 
       ylim = c(0, max(state)),
       xlab = "Hours",
       ylab = "Patients")
  
  #for (i in 1:(length(state)-1)) {
  #  points(x=time_in_state[i + 0:1], y=state[c(i, i)], type="l")
  #}
  
  plot(time_in_state, state, type="l")
}


sim <- simulate(50, lambda, mu)
time_in_state <- sim$time_in_state
state <- sim$state

plot_res(time_in_state, state)



(state * time_in_state)[1:10]
time_in_states <- rep(0, max(state))

for (i in 1:length(time_in_states)) {
  time_in_states[i] <- sum((state == i) * time_in_state) / max_time
}

sum(state * time_in_state) / max_time
