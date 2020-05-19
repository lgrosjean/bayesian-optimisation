x = seq(0,1,0.01)

a = 1
b = 20

lambda = dgamma(x,a,b)

y = c(12,15,8,13.5,25)

a_1 = a+length(y)
b_1 = b+sum(y)

a_1/b_1

lambda_1 = dgamma(x,a_1,b_1)

plot(x,lambda)
lines(x,lambda_1)

pgamma(1/10,a_1,b_1)
pgamma(1/10,a,b)

#########
# Earthquake
#########

a = 1
b = 30
y = c(16,8,114,60,4,23,30,105)

a_1 = a+length(y)
b_1 = b+sum(y)

qgamma(0.975,a_1,b_1)

poster = function(y){
  return(b_1^a_1*a_1/((b_1+y)^(a_1+1)))
}

yy = seq(0,120,1)

plot(yy,poster(yy))

#### Thermometer

sigma = 0.25
m = 100
s = 0.25

x = seq(90,110,0.1)

theta = dnorm(x, m, s)
plot(x,theta)

y = c(94.6,95.4,96.2,94.9,95.9)

n = length(y)
ybar = mean(y)

denom = (n/sigma+1/s)
m_post = (n*ybar/sigma+m/s)/denom
s_post = 1/denom

qnorm(0.975,m_post,sd=sqrt(s_post))
theta_post = dnorm(x, m_post, s_post)

plot(x,theta)
lines(x,theta_post)

# rnorm = random distribution

a=3
b=200
z = rgamma(n=300,shape=a,rate=b)
x = 1/z
mean(x)

z <- rgamma(1000,shape=16.5,rate=6022.9)
sig2 <- 1/z
mu <- rnorm(1000,mean=609.3,sd=sqrt(sig2/27.1))

quantile(x=mu, probs=c(0.025,0.975))

a = 2+18
b = 200+29/2*403.1+0.1*30/(2*(0.1+30))*((622.8-500)^2)
m = (30*622.8+0.1*500)/(0.1+30)

zz <- rgamma(1000,shape=a,rate=b)
sigg2 <- 1/zz
muu <- rnorm(1000,mean=m,sd=sqrt(sigg2/30.1))

mean(muu>mu)

# Unproper

x = seq(0,1,0.01)
p = dbeta(x,0.5,0.5)
plot(x,p)

# Linear regression

height = read.table("http://www.randomservices.org/random/data/Challenger2.txt", header=T)
head(height)
names(height)
attach(height)
plot(T,I)

height.lm = lm(I~T)
summary(height.lm)

lines(T, fitted(height.lm))
# 95% probability
height.lm$coefficients

## Regression
library(data.table)
library(ggplot2)

dat = fread("http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat")

names(dat)[1] = "AvgDist"
names(dat)[2] = "Acc"
names(dat)[3] = "FM"
head(dat)

dat$FM <- as.factor(dat$FM)

ggplot(dat, aes(x=AvgDist,y=Acc,color=FM))+
  geom_point()

datF <- dat[FM=="1",]
attach(datF)

datF.lm <- lm(Acc~AvgDist)
summary(datF.lm)

new <- data.frame(AvgDist=c(260))
predict(datF.lm, new, interval = "prediction")

dat = fread("http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat")
names(dat) = c("AvgDist", "Acc", "FM")
dat[FM==c(1,2),(FM):=c(0,1)]

a = c(1,2)
m = matrix(c(3,5,8,4),nrow=2,ncol=2,byrow=T)
