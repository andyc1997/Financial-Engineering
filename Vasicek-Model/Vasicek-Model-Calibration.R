# DRAFT MODEL
# The following code demonstrate the clibration of parameters of Vasicek model (One factor short rate model)
# HIBOR data from HSB
setwd('C:\\Users\\user\\Documents\\Programming\\Note on R\\Vasicek Model_Calibration')
interest.rate.data <- read.csv('hibor.csv')
head(interest.rate.data, 10) # Show data
sapply(interest.rate.data, class) # Check data types

interest.rate.data$Date <- as.Date(unlist(interest.rate.data$Date), '%d/%m/%Y')

# Ordinary Least Square (OLS) Estimation
# Vasicek model SDE can be discretized as an AR(1) Process in finite time
row.count <- nrow(interest.rate.data)
interest.rate.data$HIBOR.lag <- c(NA, interest.rate.data$HIBOR[1:row.count - 1])

reg.lm <- lm(HIBOR ~ HIBOR.lag, data = interest.rate.data)
summary(reg.lm) # Show regression output

a <- reg.lm$coefficients[2]
b <- reg.lm$coefficients[1]
res.var <- var(reg.lm$residuals)
dt <- 1/252

# SDE Parameters calibrated by OLS
mu <- b/(1 - a)
lambda <- (1 - a)/dt
sigma <- sqrt(res.var/dt)

cat('Estimated mu: ', mu, '\n')
cat('Estimated lambda: ', lambda, '\n')
cat('Estimated sigma: ', sigma, '\n')

plot(interest.rate.data$HIBOR,
     type = 'l', lwd = 2,
     ylab = 'HIBOR Rate',
     xlab = 'Date')

# Simulation
trial <- row.count # Length of paths to be simulated
sim.count <- 15 # Number of paths to be simulated

for (j in 1:sim.count){
  r <- rep(0, trial)
  r[1] <- interest.rate.data$HIBOR[1]

  for (i in 2:trial) {
    # Ornstein-Uhlenbeck process SDE
    dr <- lambda*(mu - r[i - 1])*dt + sigma*sqrt(dt)*rnorm(1)
    r[i] <- r[i-1] + dr
  }
  lines(r, type = 'l', col = runif(1, 0, 20))
}





