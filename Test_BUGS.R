install.packages("R2WinBUGS")

library(R2WinBUGS)
rm(list=ls())
schools<-read.csv("schools.csv")

J <- nrow(schools)
y <-schools$estimate
sigma.y <- schools$sd
data <- list("J","y","sigma.y")
inits <- function()
  list(theta=rnorm(J,0,1000), mu.theta=rnorm(1,0,100),sigma.theta=runif(1,0,100))
parameters<-c("theta","mu.theta","sigma.theta")

schools.sim <- bugs(data,inits,parameters,"test_1.txt",n.chains=2,n.iter=3000)
schools.sim$summary

y.rep <- array(NA, c(schools.sim$n.sims, J))
for (sim in 1:schools.sim$n.sims)
{
  y.rep[sim,]<- rnorm(J,theta[sim,],sigma.y)
}

par (mfrow=c(5,4), mar=c(3,3,2,2))
hist(y,xlab="",main="y")
for (sim in 2:20)
  hist(y.rep[sim,],xlab="",main=paste("y.rep",sim))

# Now creating a fair dice and then testing whether these values come about as a result of chance
