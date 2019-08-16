# MODEL
# Purpose: Simulate stochastic interest rate for European style options based on a well-known model.
# Theory: Vasicek, O. (1977). An equilibrium characterization of the term structure.
# Calibration: van den Berg, T. (2011). Calibrating the ornstein-uhlenbeck (vasicek) model.

# The following code demonstrate the clibration of parameters of Vasicek model (One factor short rate model)
# HIBOR data from HSB
setwd('C:\\Users\\user\\Documents\\Programming\\Note on R\\Vasicek Model_Calibration')
interest.rate.data <- read.csv('hibor.csv')
# head(interest.rate.data, 10) # Show data
sapply(interest.rate.data, class) # Check data types

interest.rate.data$Date <- as.Date(unlist(interest.rate.data$Date), '%d/%m/%Y')
interest.rate.data <- interest.rate.data[order(interest.rate.data$Date),]

# Core code
vasicek.calibration <- function(r, method) {
  n <- length(r)
  dt <- 1 / 252
  cat('Parameters estimation of Vasicek model\n')

  if (method == 'ols') {
    # 1. Ordinary Least Square (OLS) Estimation
    # Vasicek model SDE can be discretized as an AR(1) Process in finite time
    # One can also use arima(interest.rate.data$HIBOR, c(1, 0, 0)) for simplicity.
    r.lag <- c(NA, r[1:n - 1])

    reg.lm <- lm(r ~ r.lag)
    summary(reg.lm) # Show regression output

    a <- as.numeric(reg.lm$coefficients[2])
    b <- as.numeric(reg.lm$coefficients[1])
    res.var <- var(reg.lm$residuals)

    # SDE Parameters calibrated by OLS
    mu <- b / (1 - a)
    lambda <- (1 - a) / dt
    sigma <- sqrt(res.var / dt)

    cat('Estimation method: Ordinary Least Square')
    cat('Estimated mu: ', mu, '\n')
    cat('Estimated lambda: ', lambda, '\n')
    cat('Estimated sigma: ', sigma, '\n')

    list.parameters <- list(mu, lambda, sigma)
    names(list.parameters) <- c('mu', 'lambda', 'sigma')
    return(list.parameters)

  } else if (method == 'mle') {
    # 2. Maximum Likelihood Estimation (MLE)
    r.x <- r[1:n - 1]
    r.y <- r[2:n]

    sum.r.x <- mean(r.x) * (n - 1)
    sum.r.y <- mean(r.y) * (n - 1)
    sum.sq.r.x <- var(r.x) * (n - 2)
    sum.sq.r.y <- var(r.y) * (n - 2)
    sum.r.x.y <- sum(r.x * r.y)

    # SDE Parameters calibrated by MLE
    # Optimized with vectorization
    mu <- (sum.r.y * sum.sq.r.x - sum.r.x * sum.r.x.y) / (n * (sum.sq.r.x - sum.r.x.y) - (sum.r.x^2 - sum.r.x * sum.r.y))
    lambda <- sum((r.y - r.x) * (mu - r.x)) / dt / sum((r.x - mu) ^ 2)
    sigma <- sqrt(sum((r.y - (r.x + lambda * (mu - r.x) * dt)) ^ 2) / n / dt)

    cat('Estimation method: Maximum likelihood')
    cat('Estimated mu: ', mu, '\n')
    cat('Estimated lambda: ', lambda, '\n')
    cat('Estimated sigma: ', sigma, '\n')

    list.parameters <- list(mu, lambda, sigma)
    names(list.parameters) <- c('mu', 'lambda', 'sigma')
    return(list.parameters)
  }
}

par <- vasicek.calibration(interest.rate.data$HIBOR, method = 'mle')

# Plotting Code
plot(interest.rate.data$HIBOR,
     type = 'l', lwd = 2,
     ylab = 'HIBOR Rate',
     xlab = 'Date')

# Simulation Code
trial <- nrow(interest.rate.data) # Length of paths to be simulated
sim.count <- 15 # Number of paths to be simulated
dt <- 1/252 # Time step

for (j in 1:sim.count){
  r <- rep(0, trial)
  r[1] <- interest.rate.data$HIBOR[1]

  for (i in 2:trial) {
    # Ornstein-Uhlenbeck process SDE
    dr <- par$lambda*(par$mu - r[i - 1])*dt + (par$sigma)*sqrt(dt)*rnorm(1)
    r[i] <- r[i - 1] + dr
  }
  lines(r, type = 'l', col = runif(1, 0, 20))
}



