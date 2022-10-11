library(readxl)

data = read_excel("Knee.xlsx")


# Get the knee strength data for the different groups
dat1 = data[which(data[, 5]==1),51:58]
dat2 = data[which(data[, 5]==2),51:58]
dat3 = data[which(data[, 5]==3),51:58]

# Remove rows containing missing data
knee_no_m1 <- subset(dat1, I_Q_conNm_weight != "M" & C_Q_conNm_weight != "M"
                    & I_Q_eccNm_weight != "M" & C_Q_eccNm_weight != "M"
                    & I_H_conNm_weight != "M" & C_H_conNm_weight != "M"
                    & I_H_eccNm_weight != "M" & C_H_eccNm_weight != "M")

knee_no_m2 <- subset(dat2, I_Q_conNm_weight != "M" & C_Q_conNm_weight != "M"
                     & I_Q_eccNm_weight != "M" & C_Q_eccNm_weight != "M"
                     & I_H_conNm_weight != "M" & C_H_conNm_weight != "M"
                     & I_H_eccNm_weight != "M" & C_H_eccNm_weight != "M")

knee_no_m3 <- subset(dat3, I_Q_conNm_weight != "M" & C_Q_conNm_weight != "M"
                     & I_Q_eccNm_weight != "M" & C_Q_eccNm_weight != "M"
                     & I_H_conNm_weight != "M" & C_H_conNm_weight != "M"
                     & I_H_eccNm_weight != "M" & C_H_eccNm_weight != "M")


# Convert into numerical matrix
df1 = as.matrix(knee_no_m1)
df1 = matrix(as.numeric(df1), ncol = ncol(df1))

df2 = as.matrix(knee_no_m2)
df2 = matrix(as.numeric(df2), ncol = ncol(df2))

df3 = as.matrix(knee_no_m3)
df3 = matrix(as.numeric(df3), ncol = ncol(df3))

# Get the difference between contrl and injured knee 
D1 = cbind(df1[,1] - df1[,2], df1[,3] - df1[,4], df1[,5]-df1[,6], df1[,7]-df1[,8])
D2 = cbind(df2[,1] - df2[,2], df2[,3] - df2[,4], df2[,5]-df2[,6], df2[,7]-df2[,8])
D3 = cbind(df3[,1] - df3[,2], df3[,3] - df3[,4], df3[,5]-df3[,6], df3[,7]-df3[,8])

# Do Hotteling T^2
n1 = length(D1[, 1])
p1 = length(D1[1, ])
(T2_1<-n1*t1(colMeans(D1))%*%solve(cov(D1))%*%colMeans(D1))
(cv_1 <- ((n1-1)*p1)/(n1-p1)*qf(0.95, p1, n1-p1))

n2 = length(D2[, 1])
p2 = length(D2[1, ])
(T2_2<-n2*t2(colMeans(D2))%*%solve(cov(D2))%*%colMeans(D2))
(cv_1 <- ((n2-1)*p2)/(n2-p2)*qf(0.95, p2, n2-p2))


# first group
## T^2 
(first1 = c(colMeans(D1)[1] - sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D1)[1,1]/n1), 
  colMeans(D1)[1] + sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D1)[1,1]/n1)))

(second2 = c(colMeans(D1)[2] - sqrt(p*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p)*cov(D1)[2,2]/n1), 
           colMeans(D1)[2] + sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D1)[2,2]/n1)))

(third1 = c(colMeans(D1)[3] - sqrt(p*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p)*cov(D1)[3,3]/n1), 
           colMeans(D1)[3] + sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D1)[3,3]/n1)))


(fourth1 = c(colMeans(D1)[4] - sqrt(p*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p)*cov(D1)[4,4]/n1), 
           colMeans(D1)[4] + sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D1)[4,4]/n1)))

## Bonferroni
(bon1_1 = c(colMeans(D1)[1] - qt(0.99375, n1-1)*sqrt(cov(D1)[1,1]/n1), 
  colMeans(D1)[1] + qt(0.99375, n1-1)*sqrt(cov(D1)[1,1]/n1)))

(bon2_1 = c(colMeans(D1)[2] - qt(0.99375, n1-1)*sqrt(cov(D1)[2,2]/n1), 
  colMeans(D1)[2] + qt(0.99375, n1-1)*sqrt(cov(D1)[2,2]/n1)))

(bon3_1 = c(colMeans(D1)[3] - qt(0.99375, n1-1)*sqrt(cov(D1)[3,3]/n1), 
           colMeans(D1)[3] + qt(0.99375, n1-1)*sqrt(cov(D1)[3,3]/n1)))

(bon4_1 = c(colMeans(D1)[4] - qt(0.99375, n1-1)*sqrt(cov(D1)[4,4]/n1), 
           colMeans(D1)[4] + qt(0.99375, n1-1)*sqrt(cov(D1)[4,4]/n1)))

## T-test
(ttest1_1 = c(colMeans(D1)[1] - qt(0.975, n1-1)*sqrt(cov(D1)[1,1]/n1), 
            colMeans(D1)[1] + qt(0.9375, n1-1)*sqrt(cov(D1)[1,1]/n1)))

(ttest2_1 = c(colMeans(D1)[2] - qt(0.975, n1-1)*sqrt(cov(D1)[2,2]/n1), 
            colMeans(D1)[2] + qt(0.975, n1-1)*sqrt(cov(D1)[2,2]/n1)))

(ttest3_1 = c(colMeans(D1)[3] - qt(0.975, n1-1)*sqrt(cov(D1)[3,3]/n1), 
            colMeans(D1)[3] + qt(0.975, n1-1)*sqrt(cov(D1)[3,3]/n1)))

(ttest4_1 = c(colMeans(D1)[4] - qt(0.975, n1-1)*sqrt(cov(D1)[4,4]/n1), 
            colMeans(D1)[4] + qt(0.975, n1-1)*sqrt(cov(D1)[4,4]/n1)))


# second group
## T^2 
(first2 = c(colMeans(D2)[1] - sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[1,1]/n2), 
            colMeans(D2)[1] + sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[1,1]/n2)))

(second2 = c(colMeans(D2)[2] - sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[2,2]/n2), 
             colMeans(D2)[2] + sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[2,2]/n2)))

(third2 = c(colMeans(D2)[3] - sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[3,3]/n2), 
            colMeans(D2)[3] + sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[3,3]/n2)))


(fourth2 = c(colMeans(D2)[4] - sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[4,4]/n2), 
             colMeans(D2)[4] + sqrt(p2*(n2-1)/(n2-p2)*qf(0.95, p2, n2-p2)*cov(D2)[4,4]/n2)))

## Bonferroni
(bon1_2 = c(colMeans(D2)[1] - qt(0.99375, n2-1)*sqrt(cov(D2)[1,1]/n2), 
            colMeans(D2)[1] + qt(0.99375, n2-1)*sqrt(cov(D2)[1,1]/n2)))

(bon2_2 = c(colMeans(D2)[2] - qt(0.99375, n2-1)*sqrt(cov(D2)[2,2]/n2), 
            colMeans(D2)[2] + qt(0.99375, n2-1)*sqrt(cov(D2)[2,2]/n2)))

(bon3_2 = c(colMeans(D2)[3] - qt(0.99375, n2-1)*sqrt(cov(D2)[3,3]/n2), 
            colMeans(D2)[3] + qt(0.99375, n2-1)*sqrt(cov(D2)[3,3]/n2)))

(bon4_2 = c(colMeans(D2)[4] - qt(0.99375, n2-1)*sqrt(cov(D2)[4,4]/n2), 
            colMeans(D2)[4] + qt(0.99375, n2-1)*sqrt(cov(D2)[4,4]/n2)))

## T-test
(ttest1_2 = c(colMeans(D2)[1] - qt(0.975, n2-1)*sqrt(cov(D2)[1,1]/n2), 
              colMeans(D2)[1] + qt(0.9375, n2-1)*sqrt(cov(D2)[1,1]/n2)))

(ttest2_2 = c(colMeans(D2)[2] - qt(0.975, n2-1)*sqrt(cov(D2)[2,2]/n2), 
              colMeans(D2)[2] + qt(0.975, n2-1)*sqrt(cov(D2)[2,2]/n2)))

(ttest3_2 = c(colMeans(D2)[3] - qt(0.975, n2-1)*sqrt(cov(D2)[3,3]/n2), 
              colMeans(D2)[3] + qt(0.975, n2-1)*sqrt(cov(D2)[3,3]/n2)))

(ttest4_2 = c(colMeans(D2)[4] - qt(0.975, n2-1)*sqrt(cov(D2)[4,4]/n2), 
              colMeans(D2)[4] + qt(0.975, n2-1)*sqrt(cov(D2)[4,4]/n2)))




# task 2 repeated measures - contrast matrix 

# task 3 comparison between 2 populations read book

# task 4 manova