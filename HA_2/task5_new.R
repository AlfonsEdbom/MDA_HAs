rm(list = ls())

# Read data
data = read.table(gzfile("zip.train"))
data <- as.matrix(data)


Five_Six <- data[which(data[,1]==5 | data[,1] == 6),2:257]
Five_Six_w_labels <- data[which(data[,1]==5 | data[,1] == 6),1:257]


n = dim(Five_Six)[1] # sample size
set.seed(2022)
id = sample(1:n, floor(0.8*n)) 

train_data = Five_Six[id, ] # training set
test_data = Five_Six[-id, ] # testing set


S = cov(train_data) # training covariance matrix
mu = colMeans(train_data) # mean vector

# Eigen decomposition
eigen_dec = eigen(S) 
eigen_val = eigen_dec$values # eigen values
eigen_vec = eigen_dec$vectors # eigen vectors

# Get the first 2 principal components for train data
PC1 = numeric(length(train_data[, 1]))
PC2 = numeric(length(train_data[, 1]))

# get principal component for each observation
for (i in 1:length(train_data[, 1])){
  z1 = t(train_data[i, ] - mu) %*% eigen_vec[, 1]
  z2 = t(train_data[i, ] - mu) %*% eigen_vec[, 2]
  
  PC1[i] = z1
  PC2[i] = z2
}


# create new dataframe with labels and principal components for train data
train_labels = Five_Six_w_labels[id, 1]
train_PCs = data.frame(train_labels, PC1, PC2)

# separate 5s and 6s
train_fives = train_PCs[which(train_PCs[,1]==5),2:3]
train_sixs = train_PCs[which(train_PCs[,1]==6),2:3]

# Get statistics for 5 and 6s
mu5 = colMeans(train_fives)
S5 = cov(train_fives)

mu6 = colMeans(train_sixs)
S6 = cov(train_sixs)

# get test data

test_PC1 = numeric(length(test_data[, 1]))
test_PC2 = numeric(length(test_data[, 1]))

for (i in 1:length(test_data[, 1])) {
  z1 = t(test_data[i, ]- mu) %*% eigen_vec[, 1]
  z2 = t(test_data[i, ]- mu) %*% eigen_vec[, 2]
  
  test_PC1[i] = z1
  test_PC2[i] = z2
}

test_labels = Five_Six_w_labels[-id, 1]
test_PCs = data.frame(test_labels, test_PC1, test_PC2)


exponent = function(my_vector, row_observation, cov_matrix){
  x = as.vector(row_observation) - as.vector(my_vector)
  result = exp(-t(t(x)) %*% solve(cov_matrix) %*% t(x)/2)
  return(result)
}


base = function(cov_matrix, p){
  result <- 1/(((2*pi)^(p/2))*sqrt(det(cov_matrix)))
  return(result)
}

classifier <- function(row_observation, five_prob, six_prob){
  five <- base(S5, length(row_observation)) * exponent(mu5, row_observation, S5) * five_prob
  six <- base(S6, length(row_observation)) * exponent(mu6, row_observation, S6) * six_prob

  print(five, six)
  if (six > five){
    prediction <- 6
  } else{
    prediction <- 5
  }
  return(prediction)
}

five_prob = length(train_fives[,1]) / length(train_data[, 1]) 
six_prob = length(train_sixs[,1]) / length(train_data[, 1])


five <- base(S5, length(test_PCs[165, 2:3])) * exponent(mu5, test_PCs[165, 2:3], S5) * five_prob
six <- base(S6, length(test_PCs[165, 2:3])) * exponent(mu6, test_PCs[165, 2:3], S6) * six_prob


guess <- classifier(test_PCs[1, 2:3], five_prob, six_prob) # return 5 or 6


num_five = 0
num_six = 0
pred_col = numeric(length(test_PCs))
for (i in 1:nrow(test_PCs)) {
  guess <- classifier(test_PCs[i, 2:3], five_prob, six_prob) # return 5 or 6
  if(guess == 5){ 
    num_five = num_five + 1
    pred_col[i] =  5
  }else{ # guess is 0=benign
    num_six = num_six + 1
    pred_col[i] =  6
  }
}

num_five
num_six

cor_pred <- 0
for (i in 1:length(pred_col)){
  if(pred_col[i]==5 & test_PCs[i,1]==5){ # if predict = 1 and diagnosis = "M"
    cor_pred <- cor_pred + 1
  }
  if(pred_col[i]==6 & test_PCs[i,1]==6){ # if predict = 0 and diagnosis = "B"
    cor_pred <- cor_pred + 1
  }
}

accuracy = cor_pred / length(pred_col)
accuracy
