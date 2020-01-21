pnorm(.10, .15, .020616)

n <- 300
p <- 0.15
mu <- p
sigma <- sqrt(p*(1 - p)/n)

#Find Probability(p-hat < 10%)
pnorm(.10, mu, sigma) * 100


#Find Probability(p-hat > 18%)
1 - pnorm(.18, mu, sigma) * 100

#Find Probability(12% < p-hat < 16%)
(pnorm(.16, mu, sigma) - pnorm(.12, mu, sigma)) * 100





#increasing sample size
pnorm(.10, .15, .020616)

n <- 500
p <- 0.15
mu <- p
sigma <- sqrt(p*(1 - p)/n)

#Find Probability(p-hat < 10%)
pnorm(.10, mu, sigma) * 100


#Find Probability(p-hat > 18%)
1 - pnorm(.18, mu, sigma) * 100

#Find Probability(12% < p-hat < 16%)
(pnorm(.16, mu, sigma) - pnorm(.12, mu, sigma)) * 100



#example problem, men's  height
mu.x <- 69.3
sigma.x <- 3.1
n <- 10

mu.xbar <- mu.x
sigma.xbar <- sigma.x/sqrt(n)

#probabilty that xbar is > 72
1 - pnorm(72, mu.xbar, sigma.xbar)

#probability that xnar is < 67
pnorm(67, mu.xbar, sigma.xbar)






#example problem 2, waiting time for bus
mu.X <- 7.25
sigma.X <- 6.5
n <- 50

mu.xxbar <- mu.X
sigma.xxbar <- sigma.X/sqrt(n)

#probabilty that xbar is > 8
1 - pnorm(8, mu.xxbar, sigma.xxbar)

#probability that 7 < xbar < 8
pnorm(8, mu.xxbar, sigma.xxbar) - pnorm(7, mu.xxbar, sigma.xxbar)




#percentiles
# X ~ N(mu = 9, sigma = 2.5)
#Find the 85th percentile of x

qnorm(.85, 9, 2.5)


#example problem 2, waiting time for bus
mu.y <- 10
sigma.y <- 0.6222539674
n <- 50

mu.yybar <- mu.y
sigma.yybar <- sigma.y/sqrt(n)

#probabilty that xbar is > 8
pnorm(50, mu.yybar, sigma.yybar)

#probability that 7 < xbar < 8
pnorm(8, mu.yybar, sigma.yybar) - pnorm(7, mu.yybar, sigma.yybar)



