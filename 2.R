# array of thetas from 0.25 to 0.5 in steps of 0.005
theta <- seq(0.25, 0.5, length.out = 51)

theta_0 <- c(0.3, 0.35, 0.39, 0.41, 0.45)
x_0 <- c(0.5, 0.32, 0.4, 0.35, 0.6)
var <- 0.5**2
# make array of 0.5 same length as theta
e_theta <- rep(0.5, length(theta))

# define function of variance
variance <- function(theta_1, theta_2) {
    return((1 + 15 * abs(theta_1 - theta_2)) * exp(-15 * abs(theta_1 - theta_2))/var)
}

# calculate variance matrix for theta
var_matrix_theta <- outer(theta, theta, variance)
var_matrix_theta_0 <- outer(theta_0, theta_0, variance)
var_matrix_com <- outer(theta, theta_0, variance)

expected <- e_theta + var_matrix_com %*% solve(var_matrix_theta_0) %*% (x_0 - rep(0.5, length(theta_0)))
var_exp <- var_matrix_theta - var_matrix_com %*% solve(var_matrix_theta_0) %*% t(var_matrix_com)


# plot
png(file = "figures/2a.png")
plot(theta, expected, col = "red", type = "l", xlab = "theta", ylab = "Expected value", ylim = c(0, 1))
lines(theta, expected + qnorm(0.95)*sqrt(diag(var_exp)), col = "blue")
lines(theta, expected - qnorm(0.95)*sqrt(diag(var_exp)), col = "blue")
points(theta_0, x_0)
dev.off()
# 2 b)
# calculate the probablity of it being less than 0.3 at each point
prob <- pnorm(0.3, expected, sqrt(diag(var_exp)))
prob[11] <- 0
png(file = "figures/2b.png")
plot(theta, prob, type = "l", xlab = "theta", ylab = "Probability of being less than 0.3")
dev.off()

# 2 c)
# add (0.33, 0.4) to the data
theta_1 <- c(theta_0, 0.33)
x_1 <- c(x_0, 0.4)
var_matrix_theta_1 <- outer(theta_1, theta_1, variance)
var_matrix_com_1 <- outer(theta, theta_1, variance)

expected_1 <- e_theta + var_matrix_com_1 %*% solve(var_matrix_theta_1) %*% (x_1 - rep(0.5, length(theta_1)))
var_exp_1 <- var_matrix_theta - var_matrix_com_1 %*% solve(var_matrix_theta_1) %*% t(var_matrix_com_1)


sqr <- sqrt(diag(var_exp_1))
# Change the 11th element to 0 as it was -1e-15, assuming this is a rounding error
sqr[11] <- 0
# plot
png(file = "figures/2c_1.png")

plot(theta, expected_1, col = "red", type = "l", xlab = "theta", ylab = "Expected value", ylim = c(0, 1))
lines(theta, expected_1 + qnorm(0.95)*sqr, col = "blue")
lines(theta, expected_1 - qnorm(0.95)*sqr, col = "blue")
points(theta_1, x_1)
dev.off()

# Probablity of being less than 0.3 at each point
prob_1 <- pnorm(0.3, expected_1, sqr)
png(file = "figures/2c_2.png")

plot(theta, prob_1, type = "l", xlab = "theta", ylab = "Probability of being less than 0.3")
dev.off()
print(theta[which.max(prob_1)])
