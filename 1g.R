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

simulate_urgent_and_normal <- function(days, lambda, mu, p) {
  sim_total <- simulate(days, lambda, mu)
  total_patients <- sim_total$state
  
  urgent_patients <- rep(0, length(total_patients))
  
  for (i in 2:length(total_patients)) {
    new_patient = sim_total$state[i] - sim_total$state[i-1] == 1
    if (new_patient) {
      urgent = sample(c(TRUE, FALSE), 1, prob=c(p, 1-p))
      if (urgent) {
        urgent_patients[i] <- urgent_patients[i-1] + 1
      } else {
        urgent_patients[i] <- urgent_patients[i-1]
      }
    } else {
      urgent_patients[i] <- max(urgent_patients[i-1] - 1, 0)
    }
  }
  
  return(data.frame(
    time_in_state = sim_total$time_in_state,
    total_patients = total_patients,
    urgent_patients = urgent_patients,
    normal_patients = total_patients - urgent_patients
  ))
}

discrete <- function(x, y, ...) {
  len <- length(x)
  for (i in 2:len) {
    points(x[i+0:1], y[i-1]*c(1, 1), ...)
  }
}

plot_res <- function(time_in_state, total_patients, urgent_patients) {
  
  plot(x = NA, y = NA, pch = NA, 
       xlim = c(0, tail(time_in_state, 1)[1]),#max_time), 
       ylim = c(0, max(total_patients, urgent_patients)),
       xlab = "Hours",
       ylab = "Patients")

  discrete(time_in_state, total_patients, type="l", col="red", lwd=4)
  discrete(time_in_state, urgent_patients, type="l", col="blue", lwd=4)
  legend("topleft", c("Total", "Urgent"), fill=c("red", "blue"))
}


sim_urgent_and_normal <- simulate_urgent_and_normal(days=0.5, lambda=5, mu=6, p=0.8)

time_in_state <- sim_urgent_and_normal$time_in_state
total_patients <- sim_urgent_and_normal$total_patients
urgent_patients <- sim_urgent_and_normal$urgent_patients
normal_patients <- sim_urgent_and_normal$normal_patients

plot_res(time_in_state, urgent_patients, normal_patients)#, urgent_patients)


long_term_mean_time <- function(days, lambda, mu, p) {
  sim <- simulate_urgent_and_normal(days, lambda, mu, p)
  len <- length(sim$urgent_patients)
  average_number_of_patients <- sum(diff(sim$time_in_state) * sim$urgent_patients[2:len])/sim$time_in_state[len]
  return(average_number_of_patients/(p*lambda))
}

w = rep(0, 30)
for (i in 1:30) {
  w[i] <- long_term_mean_time(days=50, lambda=5, mu=6, p=0.8)
}

CI <- qt(0.975, df=30-1)*sd(w)/sqrt(30)

plot_time <- function(w, CI) {
  barplot(w)#, ylim = c(min(w) - 0.1, max(w) + 0.1))
  abline(h=mean(w) + CI*c(1,-1))
}

plot_time(w, CI)


plot_res(time_in_state, total_patients-0.02, urgent_patients+0.02)
