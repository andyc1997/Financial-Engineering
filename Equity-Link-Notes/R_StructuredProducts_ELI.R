# FINA 4354 Structured Products -- Equity Linked Instruments (ELI)
library(quantmod)
library(dplyr)
date.begin <- '2018-03-05'
date.end <- '2019-03-05'

getSymbols('AAPL', from = date.begin, to = date.end)
rm(date.begin, date.end)

AAPL.p <- AAPL$AAPL.Adjusted
AAPL.r <- AAPL.p %>% log %>% diff %>% na.omit
AAPL.vol <- sd(AAPL.r)/sqrt(1/252)

K <- 195
r <- 2.4/100
n <- 10
t <- 1
d <- 10^6 # Trials

library(ggplot2)

# BULL ELI ----------------------------------------------------------------

bull.eli.payoff <- function(n, K, S){
  return(min(n*S, n*K))
}

plot.S <- 0:500
plot.payoff <- sapply(plot.S, bull.eli.payoff, n=n, K=K)

ggplot(data = data.frame(plot.S, plot.payoff), aes(x = plot.S, y = plot.payoff)) +
  geom_line(color = 'red', linetype = 'dashed') +
  xlab('Stock price of AAPL') +
  ylab('Payoff of Bull ELI') +
  ggtitle('BULL ELI PAYOFF DIAGRAM')

S <- AAPL.p %>% tail(1) %>% as.numeric

put.price <- function(S, sigma, r, K, t, d){
  r.1 <- (1/2*r*sigma^2)*t + sqrt(t)*rnorm(1:d)
  s.1 <- S*exp(r.1)
  return(exp(-r*t)*pmax(K - s.1, 0)) # Return a numeric vector
}

set.seed(123)
system.time(
  bull.eli.price <- exp(-r*t)*n*K - n*mean(put.price(S, AAPL.vol, r, K, t, d))
) # Performance: 0.15s
cat('Estimated BULL ELI price: ', bull.eli.price, '\n')


# BEAR ELI ----------------------------------------------------------------

bear.eli.payoff <- function(n, K, S){
  P <- K*n
  return(max(min(2*P - S*n, P), 0))
}

plot.S <- 0:500
plot.payoff <- sapply(plot.S, bear.eli.payoff, n=n, K=K)

ggplot(data = data.frame(plot.S, plot.payoff), aes(x = plot.S, y = plot.payoff)) +
  geom_line(color = 'blue', linetype = 'dashed') +
  xlab('Stock price of AAPL') +
  ylab('Payoff of Bear ELI') +
  ggtitle('BEAR ELI PAYOFF DIAGRAM')

call.price <- function(S, sigma, r, K, t, d){
  r.1 <- (1/2*r*sigma^2)*t + sqrt(t)*rnorm(1:d)
  s.1 <- S*exp(r.1)
  return(exp(-r*t)*pmax(s.1 - K, 0)) # Return a numeric vector
}

set.seed(123)
system.time(
  bear.eli.price <- n*mean(call.price(S, AAPL.vol, r, K, t, d)) -
    n*mean(call.price(S, AAPL.vol, r, 2*K, t, d)) + exp(-r*t)*n*K
)
cat('Estimated BEAR ELI price: ', bear.eli.price, '\n')
