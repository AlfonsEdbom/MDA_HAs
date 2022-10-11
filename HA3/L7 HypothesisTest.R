#--------------------#
# Hypothesis testing #
#--------------------#

#----------#
# T^2 test #
#----------#

data <- read.table("http://pages.stat.wisc.edu/~rich/JWMULT06dat/T5-1.DAT")

names(data) <- c("Sweat rate", "Sodium", "Potassium")

head(data)

pairs(data)

#library(Hotelling)

sm <- t(apply(data,2,mean))

scov <- cov(data)

mu0 <- t(c(4,50,10))

n <- dim(data)[1]; p <- dim(data)[2]

T2 <- n * (sm-mu0) %*% solve(scov) %*% t((sm-mu0))

T2 > (n-1)*p/(n-p)*qf(0.90, p, n-p) # we can reject the null hypothesis.

#-------------------#
# Confidence region #
#-------------------#

closed <- read.table("http://pages.stat.wisc.edu/~rich/JWMULT06dat/T4-1.DAT")
opened <- read.table("http://pages.stat.wisc.edu/~rich/JWMULT06dat/T4-5.DAT")

Rad.data <- cbind(closed, opened)
names(Rad.data) <- c("closed", "opened")

head(Rad.data)

# The quality-control department of a manufacturer of microwave ovens is required by the 
# federal government to monitor the amount of radiation emitted when the doors of the ovens
# are closed and open.

plot(Rad.data)

plot((Rad.data)^{1/4})

(sm <- colMeans((Rad.data)^{1/4}))
(scov <- cov((Rad.data)^{1/4}))
library(ellipse)
dist <- sqrt(((41*2)/(40*42))*qf(0.95, 2, 40))

plot(ellipse(x=scov,centre=sm,t=dist),type="l",asp=1,xlim=c(0.5,0.62),ylim=c(0.55,0.66))

points(sm[1], sm[2])

# one at a time

x1_bound <- c(sm[1] - qt(0.975,41)*sqrt(scov[1,1]/42), sm[1] + qt(0.975,41)*sqrt(scov[1,1]/42))
x2_bound <- c(sm[2] - qt(0.975,41)*sqrt(scov[2,2]/42), sm[2] + qt(0.975,41)*sqrt(scov[2,2]/42))

segments(x1_bound[1], x2_bound[1], x1_bound[2], x2_bound[1])
segments(x1_bound[1], x2_bound[1], x1_bound[1], x2_bound[2])
segments(x1_bound[2], x2_bound[1], x1_bound[2], x2_bound[2])
segments(x1_bound[1], x2_bound[2], x1_bound[2], x2_bound[2])

# T square interval

dist <- sqrt(((41*2)/(40))*qf(0.95, 2, 40))
x1_bound <- c(sm[1] - dist*sqrt(scov[1,1]/42), sm[1] + dist*sqrt(scov[1,1]/42))
x2_bound <- c(sm[2] - dist*sqrt(scov[2,2]/42), sm[2] + dist*sqrt(scov[2,2]/42))

segments(x1_bound[1], x2_bound[1], x1_bound[2], x2_bound[1],lty=2)
segments(x1_bound[1], x2_bound[1], x1_bound[1], x2_bound[2],lty=2)
segments(x1_bound[2], x2_bound[1], x1_bound[2], x2_bound[2],lty=2)
segments(x1_bound[1], x2_bound[2], x1_bound[2], x2_bound[2],lty=2)

# title("One at a time V.S. Hotelling T square")

x1_bound <- c(sm[1] - qt(0.9875,41)*sqrt(scov[1,1]/42), sm[1] + qt(0.9875,41)*sqrt(scov[1,1]/42))
x2_bound <- c(sm[2] - qt(0.9875,41)*sqrt(scov[2,2]/42), sm[2] + qt(0.9875,41)*sqrt(scov[2,2]/42))

segments(x1_bound[1], x2_bound[1], x1_bound[2], x2_bound[1],col="red")
segments(x1_bound[1], x2_bound[1], x1_bound[1], x2_bound[2],col="red")
segments(x1_bound[2], x2_bound[1], x1_bound[2], x2_bound[2],col="red")
segments(x1_bound[1], x2_bound[2], x1_bound[2], x2_bound[2],col="red")

#-----------------------------------------------------------------------------#

# Example 6.1, wastewater data
# Aim: Reliability of self monitoring of municipal wasterwater treatment plants.
# Sample: One half of each sample was sent to the Wisconsin State lab and one half was sent to 
# private commercial lab
# Two variables are measured from sample: biochemical oxygen demand (BOD) and suspended solids (SS).

data <- read.table("http://pages.stat.wisc.edu/~rich/JWMULT06dat/T6-1.dat")
names(data) <- c("BOD1","SS1","BOD2","SS2")
data

(D <- cbind(data[,1]-data[,3], data[,2]-data[,4]))

# C = cbind(diag(2), -diag(2))
# X = as.matrix(data)
# X%*%t(C)

D <- data.frame(D)
colnames(D) <- c("Bod1_diff", "SS1_diff")

n <- dim(D)[1]; p <- dim(D)[2]

plot(D[,1],D[,2],asp=1)
lines(c(min(D[,1]),max(D[,1])),c(0,0),lty=2) ; lines(c(0,0),c(min(D[,2]),max(D[,2])),lty=2)
points(mean(D[,1]),mean(D[,2]),pch=20,col="red")
title("Example 6.1 Effluent data")

# Hotellings T2-test
delta0 <- c(0,0)
(T2<-n*t(colMeans(D))%*%solve(cov(D))%*%colMeans(D))
(cv <- ((n-1)*p)/(n-p)*qf(0.95, p, n-p))

# Check T² confidence interval
c(colMeans(D)[1] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n), 
  colMeans(D)[1] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n))
c(colMeans(D)[2] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n), 
  colMeans(D)[2] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n))
# Bonferroni correction CI
c(colMeans(D)[1] - qt(0.9875, n-1)*sqrt(cov(D)[1,1]/n), 
  colMeans(D)[1] + qt(0.9875, n-1)*sqrt(cov(D)[1,1]/n))
c(colMeans(D)[2] - qt(0.9875, n-1)*sqrt(cov(D)[2,2]/n), 
  colMeans(D)[2] + qt(0.9875, n-1)*sqrt(cov(D)[2,2]/n))

#-----------------------------------------------------------------------------#
D <- D[-8,]
#-----------------------------------------------------------------------------#
#library(MVN)
#-----------------------------------------------------------------------------#

# Example 6.2: Test of anesthetics.

# 19 dogs were initially given the drug pentobarbitol.
# Each dog was then administered carbon dioxide (CO2) at each of two pressure levels (low, high).
# This was done with and without substance halothane (H) (abscent, present).
# The time between heartbeats (milliseconds) was measured at each of the four treatment combinations.

# Treatment 1: High CO2 + without H
# Treatment 2: Low CO2 + without H
# Treatment 3: High CO2 + with H
# Treatment 4: Low CO2 + with H
# Effect of halothane : (mu_3 + mu_4) - (mu_1 + mu_2)
# Effect of CO2: (mu_1 + mu_3) - (mu_2 + mu_4)
# Interaction between H and CO2: (mu_1 + mu_4) - (mu_2 + mu_3)

data <- read.table("http://pages.stat.wisc.edu/~rich/JWMULT06dat/T6-2.dat")

names(data) <- c("T1","T2","T3","T4")
(data)

colMeans(data)

#Contrast matrix:
(C <- matrix(c(-1,1,1,-1,-1,-1,1,1,-1,1,-1,1), 3, 4))

C%*%colMeans(data)

#[1,] 209.31579 #Halothane-effect
#[2,] -60.05263 #Carbone dioxide-effect
#[3,] -12.78947 #Interaction-effect

(n <- dim(data)[1])

(q <- dim(data)[2])

(T2 <- n*t(colMeans(data))%*%t(C)%*%solve(C%*%cov(data)%*%t(C))%*%C%*%colMeans(data))

(cv <- qf(0.95, (q-1), (n-q+1)) * (n-1)*(q-1)/(n-q+1))

T2 > cv # Reject the null hypothesis

