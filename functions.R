bf <- function(d, n, v0) {
  a <- 1 / sqrt(n * (v0 + 1))
  b <- n^2 * d^2
  c <- 2 * (n + 1 / v0)
  return(a * exp(b / c))
}

updatePar <- function(rho0, v0, N, ybar) {
  prec <- (N + 1 / v0)
  v1 <- 1 / prec
  c <- N * ybar
  mu1 <- v1 * c
  w <- bf(ybar, N, v0)
  rho1 <- rho0 / (rho0 + (1 - rho0) * w)
  return(c("ph0" = rho1, "mu" = mu1, "sd" = sqrt(v1)))
}
