library(readxl)

data = read_excel("Knee.xlsx")


# Get the knee strength data for the different groups
group_1 = data[which(data[, 5]==1),c(51,53,55,57)]
group_2 = data[which(data[, 5]==2),c(51,53,55,57)]


# Remove rows containing missing data
group_1 <- subset(group_1, I_Q_conNm_weight != "M" & I_Q_eccNm_weight != "M"
                  & I_H_conNm_weight != "M" & I_H_eccNm_weight != "M")

group_2 <- subset(group_2, I_Q_conNm_weight != "M" & I_Q_eccNm_weight != "M"
                  & I_H_conNm_weight != "M" & I_H_eccNm_weight != "M")

# Convert into numerical matrix
group_1 = as.matrix(group_1)
group_1 = matrix(as.numeric(group_1), ncol = ncol(group_1))

group_2 = as.matrix(group_2)
group_2 = matrix(as.numeric(group_2), ncol = ncol(group_2))


# get statistics for each group
(p = dim(group_1)[2])
(mu1 = colMeans(group_1))
(S1 = cov(group_1))
(n1 = dim(group_1)[1])

(mu2 = colMeans(group_2))
(S2 = cov(group_2))
(n2 = dim(group_2)[1])


# Pooled variance
(Sp = ((n1 -1) / (n1 + n2 -2))*S1 + ((n2 -1) / (n1 + n2 -2))*S2)


# See result 6.2 / ex. page 287
(T2 <- t((mu1 - mu2)) %*% solve(((1/n1)+(1/n2)*Sp)) %*% t(t(mu1-mu2)))
(cv <- ((p*(n1+n2-2))/(n1+n2-p-1))* qf(0.95, p,n1+n2-p-1))

# No significant difference??



# Dont do this, normal Hotelling T2 - test
(T2 <- n*t(colMeans(data))%*%t(C)%*%solve(C%*%cov(data)%*%t(C))%*%C%*%colMeans(data))