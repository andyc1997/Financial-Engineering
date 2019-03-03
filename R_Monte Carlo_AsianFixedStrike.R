setwd('C:\\Users\\user\\Documents\\HKU_Note\\FINA_4354\\R')
data <- read.csv('GOOG_AAPL.csv')

GOOG <- data[, 2]
GOOG.r <- diff(log(GOOG))
GOOG.vol <- sd(GOOG.r)/sqrt(1/252)

library(Rcpp)
sourceCpp("Cpp_MonteCarlo_AsianFloatingStrike.cpp")

d <- 10^4
r <- 0.02
K <- 500
theta <- GOOG.vol^2
kappa <- 3
xi <- 0.5

p <- priceCpp(GOOG[length(GOOG)], GOOG.vol,  0.02,  1.0 , K, theta, kappa, xi, d)
cat('Estimated option price:', p, '\n')
