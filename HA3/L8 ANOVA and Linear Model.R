# One way ANOVA vs Linear Model #

rm(list = ls())

# data: PlantGrowth #

data = PlantGrowth
data
class(data)
class(data$group)
levels(data$group)
attach(data)

#-----------------------------------#
#--- Variance decomposition view ---#
#-----------------------------------#

# calculate the F statistics by coding
(N = dim(data)[1]) # total sample size
(n = table(group)[1]); # sample size in each group
(g = length(table(group))) # number of groups

(m_t = mean(weight)) # overall mean
(m_g = ave(weight,group)) # group mean

(SST = var(weight)*(N-1)) # total variation
(SSB = sum((m_g - m_t)^2)) # variation between groups

SSW = 0
for(i in unique(group)){
  SSW = SSW + var(data[which(group == i), 1])*(n-1)
}
SSW # variation within groups

SSW+SSB; 
SST

(F.anova <- (SSB/(g-1))/(SSW/(N-g)))
qf(0.95, g-1, N-g) # cv

# Build in function "aov"
m = aov(weight~group)
summary(m)

#-----------------------#
#--- Regression view ---#
#-----------------------#

model <- lm(weight ~ group)
summary(model)

#--- alternative way of present factor variable ---#
Z <- rbind(matrix(rep(c(0,0),10),10,2,byrow = T),
           matrix(rep(c(1,0),10),10,2,byrow = T),
           matrix(rep(c(0,1),10),10,2,byrow = T))

data_1 <- data.frame(cbind(weight, Z))
names(data_1) <- c("weight","z1","z2")
data_1
summary(lm(weight~., data = data_1))
#--------------------------------------------------#


# Likelihood ratio test  #
######################

(N <- dim(data)[1]) # total sample size
(n <- table(group)[1]); # sample size in each group
(g <- length(table(group))) # number of groups

M_F <- lm(weight ~ group)
M_R <- lm(weight ~ 1)

resid_F <- M_F$residuals
(SSR_F <- sum((resid_F)^2)) # compare with SSW

resid_R <- M_R$residuals
(SSR_R <- sum((resid_R)^2)) # compare with SST

(Ftest <- ((SSR_R-SSR_F)/(g-1))/(SSR_F/(N-g)))
qf(0.95, (3-1), (N-3))

(LRtest <- -2*log( ( SSR_R / SSR_F )^{-N/2} )) # chisq df = 3-1
qchisq(0.95, 2)

#-------------------------------------------------------------------------------------#

# MANOVA

#-------------------------------------------------------------------------------------#
# Example 6.13 plastic film #

data <- read.table("http://pages.stat.wisc.edu/~rich/JWMULT06dat/T6-4.dat")
names(data) <- c("F1", "F2", "TR", "G", "O")
data$F1 <- as.factor(c(rep("L",10),rep("H",10)))
data$F2 <- as.factor(c(rep("L",5),rep("H",5),rep("L",5),rep("H",5)))
data

#-----------------------------------------------------------------------#
# Mesurement on plastic film: TR--tear resistance, G--gloss, O--opacity #
# F1: change in rate of extrusion: Low or High                          #
# F2: Amount of additive: Low or High                                   #
#-----------------------------------------------------------------------#

# 1 way MANOVA #

m1 <- manova(cbind(TR,G,O) ~ F1, data = data)
summary(m1, test="Wilks")

#-------------------------------------------#
#--- Calculate the lambda test statistic ---#
#---------- Based on the LRT idea ----------#
#-------------------------------------------#

data
m1$fitted.values
m1$residuals
F_det <- det(cov(m1$residuals)) # generalized variance of residuals for the Full model

m2 <- manova(cbind(TR,G,O) ~ 1, data = data) # under the null hypothesis, Restricted model
m2$fitted.values
m2$residuals
R_det <- det(cov(m2$residuals)) # generalized variance
(Lambda <- F_det/R_det) #|W|/|W+B|
summary(m1,test="Wilks")
p <- 3; k <- 2; n1 <- n2 <- 10
appF <- ((n1+n2-p-1)/p)*((1-Lambda)/Lambda)

#------------------------------------------------------------------------------------------#
# 2 ways with interaction effect MANOVA
res <- manova(cbind(TR,G,O) ~ F1 * F2, data = data)
summary(res, test="Wilks")

#-------------------------------------------#
#--- Calculate the lambda test statistic ---#
#---------- Based on the LRT idea ----------#
#-------------------------------------------#

m_R <- manova(cbind(TR,G,O) ~ F1 + F2, data = data) # Restricted model
m_F <- manova(cbind(TR,G,O) ~ F1 * F2, data = data) # Full model
R_det <- det(cov(m_R$residuals)) 
F_det <- det(cov(m_F$residuals)) 
F_det/R_det 
summary(m_F, test="Wilks")

#------------------------------------------------------------------------------------------#
# 2 ways MANOVA
res <- manova(cbind(TR,G,O) ~ F1 + F2, data = data)
summary(res, test="Wilks")

#-------------------------------------------#
#--- Calculate the lambda test statistic ---#
#---------- Based on the LRT idea ----------#
#-------------------------------------------#

# F1
m_R <- manova(cbind(TR,G,O) ~ F2, data = data)
m_F <- manova(cbind(TR,G,O) ~ F1+F2, data = data)
R_det <- det(cov(m_R$residuals))
F_det <- det(cov(m_F$residuals))
(Lambda = F_det/R_det)
summary(m_F,test="Wilks")
(appF = ((n1+n2-p-2)/p)*((1-Lambda)/Lambda))

# F2
m_R <- manova(cbind(TR,G,O) ~ F1, data = data)
m_F <- manova(cbind(TR,G,O) ~ F1+F2, data = data)
R_det <- det(cov(m_R$residuals))
F_det <- det(cov(m_F$residuals))
(Lambda = F_det/R_det)
summary(m_F,test="Wilks")
(appF = ((n1+n2-p-2)/p)*((1-Lambda)/Lambda))

