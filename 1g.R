simulate_urgent_and_normal <- function(days, lambda, mu, p) {
  sim_total <- simulate(days, lambda, mu)
  total_patients <- sim_total$states
  
  urgent_patients <- rep(0, length(total_patients))
  
  for (i in 2:length(total_patients)) {
    new_patient = (total_patients[i] - total_patients[i-1]) == 1
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






# simulate long term mean time an urgent patients spends in the UCC
long_term_mean_time <- function(days, lambda, mu, p) {
  sim <- simulate_urgent_and_normal(days, lambda, mu, p)
  len <- length(sim$urgent_patients)
  average_number_of_patients <- sum(diff(sim$time_in_state) * sim$urgent_patients[2:len])/sim$time_in_state[len]
  return(average_number_of_patients/(p*lambda))
}


# 30 realizations of long term mean time urgent patients spends in the UCC
w = rep(0, 30)
for (i in 1:30) {
  w[i] <- long_term_mean_time(days=50, lambda=5, mu=6, p=0.8)
}

CI <- qt(0.975, df=30-1)*sd(w)/sqrt(30)



plot_total_and_urgent_patients <- function(time_in_state, total_patients, urgent_patients) {
  
  plot(x = NA, y = NA, pch = NA, 
       xlim = c(0, tail(time_in_state, 1)[1]),#max_time), 
       ylim = c(0, max(total_patients, urgent_patients)),
       xlab = "Hours",
       ylab = "Patients")
  
  discrete(time_in_state, total_patients, type="l", col="red", lwd=4)
  discrete(time_in_state, urgent_patients, type="l", col="blue", lwd=4)
  legend("topleft", c("Total", "Urgent"), fill=c("red", "blue"))
}

sim <- simulate_urgent_and_normal(0.5, lambda, mu, 0.8)
plot_total_and_urgent_patients(sim$time_in_state, sim$total_patients,
                               sim$urgent_patients)
