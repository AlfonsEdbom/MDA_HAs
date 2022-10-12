library(readxl)

data = read_excel("Knee.xlsx")

data = data[,18:20]
data = subset(data, C_D_Length1 != "M" & C_D_Length2 != "M" & C_D_Length3 != "M")

data = as.matrix(data)
data = matrix(as.numeric(data), ncol = ncol(data))

# Contrast matrix
(C = matrix(cbind(c(1,1), -diag(2)), 2, 3))

# Difference matrix
D = data %*% t(C)
C%*%colMeans(data) # diff in means(grp1-grp2, grp1-grp3)


(n <- dim(data)[1]) # num observations
(p <- dim(data)[2]) # num variables

# Hotellings T2-test
(T2 <- n*t(colMeans(data))%*%t(C)%*%solve(C%*%cov(data)%*%t(C))%*%C%*%colMeans(data)) # T^2
(cv <- qf(0.95, (p-1), (n-p+1)) * (n-1)*(p-1)/(n-p+1))


# t^2 interval
(first1 = c(colMeans(D)[1] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n), 
            colMeans(D)[1] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n)))

(second2 = c(colMeans(D)[2] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n), 
             colMeans(D)[2] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n)))

# Both significant
# conclude there is difference between 1st and 2nd and 1st and 3rd
# Do not need to continue doing more liberal tests