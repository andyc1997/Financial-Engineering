# Parameter Estimates
r <- 0.02
sigma <- 0.4
K <- 100
T <- 0.5
s <- seq(from = 70, to = 200, length.out = 50)

# Pricing functions
payoff.call <- function(s, K){
  return(max(s - K, 0))
}

price.call <- function(s, t, K){
  d1 <- (log(s/K) + (r + (1/2)*sigma^2)*(T-t)) / (sigma*sqrt(T-t))
  d2 <- d1 - sigma*sqrt(T - t)
  return(s*pnorm(d1) - K*exp(-r*(T - t))*pnorm(d2))
}

price.cash.or.nothing.call <- function(s, t, K){
  d2 <- (log(s/K) + (r - (1/2)*sigma^2)*(T - t)) / (sigma*sqrt(T-t))
  return(exp(-r*(T - t))*pnorm(d2))
}

price.up.and.out <- function(s, t, L){
  # Reiner and Rubinstein (1991) Analytical solution to single barrier option
  if (s >= L){
    price <- 0
  } else {
    const <- (L/s)^(2*(r - 1/2*sigma^2)/sigma^2)
    if (L > K){
      price <- price.call(s, t, K) - price.call(s, t, L) - (L - K)*price.cash.or.nothing.call(s, t, L)
      price <- price - const*(price.call(L^2/s, t, K) - price.call(L^2/s, t, L) - (L - K)*price.cash.or.nothing.call(L^2/s, t, L))
    } else {
      price <- price.call(s, t, K) - const*price.call(L^2/s, t, K)
    }
  }
  return(price)
}

# Graph
# Investigate how price of barrier option changes over different barrier, L.
price.cal.up.and.out <- sapply(s, price.up.and.out, t = 0, L = 190)

plot(s, price.cal.up.and.out, type = 'l', col = 'blue',
     ylab = 'Up-and-out barrier options',
     xlab = 'Stock price at maturity if s < L',
     main = 'Up-and-out barrier options at different barriers')
for (L in seq(120, 180, 10)){
  price.cal.up.and.out <- sapply(s, price.up.and.out, t = 0, L = L)
  lines(s, price.cal.up.and.out, type = 'l', col = 'blue')
}


