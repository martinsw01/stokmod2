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
plot(theta, expected, col = "red", type = "l", xlab = "theta", ylab = "Expected value", ylim = c(0, 1))
lines(theta, expected + qnorm(0.95)*sqrt(diag(var_exp)), col = "blue")
lines(theta, expected - qnorm(0.95)*sqrt(diag(var_exp)), col = "blue")
points(theta_0, x_0)
