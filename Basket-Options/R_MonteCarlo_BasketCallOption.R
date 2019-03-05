# FINA 4354 Pricing basket call option
library(quantmod)
library(dplyr)
date.begin <- '2018-03-05'
date.end <- '2019-03-05'

getSymbols('AAPL', from = date.begin, to = date.end)
getSymbols('GOOGL', from = date.begin, to = date.end)
rm(date.begin, date.end)

# Adjusted closed price
AAPL.p <- AAPL$AAPL.Adjusted
GOOG.p <- GOOGL$GOOGL.Adjusted

# Log return
AAPL.r <- AAPL.p %>% log %>% diff %>% na.omit
GOOG.r <- GOOG.p %>% log %>% diff %>% na.omit

# Standard deviation/Volatility
AAPL.vol <- sd(AAPL.r)/sqrt(1/252)
GOOG.vol <- sd(GOOG.r)/sqrt(1/252)
cov.mat <- cov(data.frame(AAPL.r, GOOG.r))*252

# Cholesky decomposition & Simulation
C <- cov.mat %>% chol %>% t
d <- 10^6 # Simulation trials
std.normal <- matrix(rnorm(d*2), nrow = 2)
corr.normal <- C %*% std.normal

# Parameters in option pricing model
K <- 400
r <- 0.05
t <- 1
s1 <- rep(0, 2) -> s2
f <- rep(0, d)
s1[1] <- AAPL.p %>% tail(1)
s2[1] <- GOOG.p %>% tail(1)

# Code optimization
# Using for loop, performance: 13.22s
system.time(
for(j in 1:d){
  r1 <- (r-1/2*AAPL.vol^2)*t + sqrt(t)*corr.normal[1, j]
  r2 <- (r-1/2*GOOG.vol^2)*t + sqrt(t)*corr.normal[2, j]
  s1[2] <- s1[1]*exp(r1)
  s2[2] <- s2[1]*exp(r2)
  f[j] <- exp(-r*t)*max(mean(c(s1[2], s2[2])) - K, 0) # Call payoff
}
)
cat("Option Price Estimate:", f %>% mean %>% round(4), "\n")

# Vectorization, performance: 0.36s
call.price <- function(){
  r1 <- (r-1/2*AAPL.vol^2)*t + sqrt(t)*corr.normal[1, 1:d]
  r2 <- (r-1/2*GOOG.vol^2)*t + sqrt(t)*corr.normal[2, 1:d]
  s1.c <- s1[1]*exp(r1)
  s2.c <- s2[1]*exp(r2)
  return(exp(-r*t)*pmax(rowMeans(matrix(c(s1.c, s2.c), ncol = 2)) - K, 0)) # Call payoff
}
system.time(
cat("Option Price Estimate:", call.price() %>% mean %>% round(4), "\n")
)