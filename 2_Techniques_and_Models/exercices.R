# Week 1

## Monte Carlo estimation

set.seed(54)

m = 10000
a = 2.0 # shape
b = 1./3 # rate

theta = rgamma(n=m,shape=a,rate=b) # generate m random value

hist(theta,freq=F) # freq: density rather than count
curve(dgamma(x,shape=a,rate=b),col="blue",add=T)

mean(theta) # monte carlo approximation
a/b # true mean of the gamma distribution

var(theta) # monte carlo approximation
a/(b**2) # true variance

ind = theta<5
mean(ind) # MC approx of P(theta>5)
pgamma(5,shape=a,rate=b) # true probability

quantile(theta,0.9) # MC approx of p(theta<q)=0.9
qgamma(0.9,shape=a,rate=b) # True theorical value

se = sd(theta) / sqrt(m) # MC standard error of our approximation

mean(theta) - 2*se # lower edge of 99% confidence interval
mean(theta) + 2*se # upper

ind = theta<5
mean(ind)

se = sd(ind) / sqrt(m) # MC std error of our approx
mean(ind) - 2*se
mean(ind) + 2*se

m = 1e5
y = numeric(m)
phi = numeric(m)
for (i in 1:m){ # MC Loop
  phi[i] = rbeta(1,shape1=2,shape2=2)
  y[i] = rbinom(1,size=10,prob=phi[i])
}

phi = rbeta(m,shape1=2,shape2=2) # Vectorize, far faster than loop
y = rbinom(m,size=10,prob=phi)

table(y)/m
plot(table(y)/m) # MC approximation of y
mean(y)

## Test

m =  2e5
a = 5
b = 3
hist(theta,freq=F)

odds = theta/(1-theta)
summary(odds)
odds.ind = odds>1
mean(odds.ind)

y = rnorm(1e6,mean=0,sd=1)
hist(y)
quantile(y,probs=0.3)

sqrt(5.2/5000)
