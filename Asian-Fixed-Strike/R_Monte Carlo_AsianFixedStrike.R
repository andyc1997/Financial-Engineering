# Preparing data
setwd('C:\\Users\\user\\Documents\\HKU_Note\\FINA_4354\\R')
data <- read.csv('GOOG_AAPL.csv')

GOOG <- data[, 2]
GOOG.r <- diff(log(GOOG)) # Log return
GOOG.vol <- sd(GOOG.r)/sqrt(1/252) # Annualized volatility

# Heston model: Stochastic volatility
# Asian fixed strike call option
# Speed up computation using Rcpp
library(Rcpp)
sourceCpp("Cpp_MonteCarlo_AsianFloatingStrike.cpp")

d <- 10^6 # Size of simulation
r <- 0.02 # Risk-free rate
K <- 500 # Strike
t <- 1 # Time to maturity
# Parameters for Heston models:
theta <- GOOG.vol^2 
kappa <- 3
xi <- 0.5

p <- priceCpp(GOOG[length(GOOG)], GOOG.vol,  r,  t , K, theta, kappa, xi, d)
cat('Estimated option price:', p, '\n')
