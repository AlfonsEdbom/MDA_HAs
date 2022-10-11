library(readxl)

data = read_excel("Knee.xlsx")

data = data[,18:20]
data = subset(data, C_D_Length1 != "M" & C_D_Length2 != "M" & C_D_Length3 != "M")

data = as.matrix(data)
data = matrix(as.numeric(data), ncol = ncol(data))

(C = matrix(cbind(c(1,1), -diag(2)), 2, 3))

D = data %*% t(C)
C%*%colMeans(data)

(n <- dim(data)[1])

(q <- dim(data)[2])

(T2 <- n*t(colMeans(data))%*%t(C)%*%solve(C%*%cov(data)%*%t(C))%*%C%*%colMeans(data))

(cv <- qf(0.95, (q-1), (n-q+1)) * (n-1)*(q-1)/(n-q+1))


n1 = length(D[, 1])
p1 = length(D[1, ])
(first1 = c(colMeans(D)[1] - sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D)[1,1]/n1), 
            colMeans(D)[1] + sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D)[1,1]/n1)))

(second2 = c(colMeans(D)[2] - sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D)[2,2]/n1), 
             colMeans(D)[2] + sqrt(p1*(n1-1)/(n1-p1)*qf(0.95, p1, n1-p1)*cov(D)[2,2]/n1)))

