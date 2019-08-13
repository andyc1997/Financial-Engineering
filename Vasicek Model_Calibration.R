# Vasicek model/Ornstein-Uhlenbeck Process
dt <- 0.25
lambda <- 3
mu <- 1
sigma <- 0.5
trial <- 10^5

r <- rep(0, trial)
r[1] <- 3
for (i in 2:trial) {
  dr <- lambda*(mu - r[i-1])*dt + sigma*sqrt(dt)*rnorm(1)
  r[i] <- r[i-1] + dr
}

lm <- lm(r[2:trial] ~ r[1:trial - 1])
summary(lm)

a <- lm$coefficients[2]
b <- lm$coefficients[1]
res.var <- var(lm$residuals)

cat('Estimated mu', b/(1 - a), '\n')
cat('Estimated lambda', (1 - a)/dt, '\n')
cat('Estimated sigma', sqrt(res.var/dt), '\n')