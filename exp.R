# The theoretical mean and std. deviation of exponential distributions
# are both 1/lambda.

lambda <- .2
n <- 40

x <- rexp(10000, lambda)
mean(x) # 4.92
sd(x) # 4.98
1/lambda # 5


