lambda <- 5
mu <- 1 / 10 * 60

# array of values between 0 and 1
p <- seq(0, 1, length.out = 100)

# define function of probability p
w_u <- function(p) {
  return(1 / (mu - p * lambda))
}

w_n <- function(p) {
  return(mu / ((mu - lambda) * (mu - p * lambda)))
}
png(file = "figures/1f.png")
# plot
plot(p, w_u(p), col = "red", type = "l", xlab = "p"
, ylab = "Expected waiting time", ylim = c(0, 6))
lines(p, w_n(p), col = "blue")

# add legend
legend("topleft", legend = c("W_u", "W_n"), col = c("red", "blue"), lty = 1)

dev.off()
