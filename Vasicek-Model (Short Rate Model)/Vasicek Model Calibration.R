# The following code demonstrate the clibration of parameters of Vasicek model (One factor short rate model)
# True parameters in simulation
dt <- 0.25 # Time steps
lambda <- 3 # Mean reversion speed
mu <- 1 # Long term mean level (annualized)
sigma <- 0.5 # Volatility of interest rate (annualized)
trial <- 10^5 # Number of Trial

r <- rep(0, trial) 
r[1] <- 3 # Assumption
for (i in 2:trial) {
  # Ornstein-Uhlenbeck process SDE
  dr <- lambda*(mu - r[i - 1])*dt + sigma*sqrt(dt)*rnorm(1)
  r[i] <- r[i-1] + dr
}

# Ordinary Least Square (OLS) Estimation
# Vasicek model SDE can be discretized as an AR(1) Process in finite time
lm <- lm(r[2:trial] ~ r[1:trial - 1])
summary(lm)

a <- lm$coefficients[2]
b <- lm$coefficients[1]
res.var <- var(lm$residuals)

# SDE Parameters calibrated by OLS
calibrated.mu <- b/(1 - a)
calibrated.lambda <- (1 - a)/dt
calibrated.sigma <- sqrt(res.var/dt)
cat('Estimated mu', calibrated.mu, '\n')
cat('Estimated lambda', calibrated.lambda, '\n')
cat('Estimated sigma', calibrated.sigma, '\n')
