################################################################
# Create train set, test set
################################################################

# Load required packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

# Download the dataset from my Dropbox and unzip it
url <- "https://www.dropbox.com/s/7yzdmv2im26hauh/fashionmnist.zip?raw=1"
download(url, dest="fashionmnist.zip", mode="wb") 
unzip ("fashionmnist.zip", exdir = ".")

# Create the data set for model building and verification
verification = read.csv("fashion-mnist_test.csv", header = TRUE)
modelling = read.csv("fashion-mnist_train.csv", header = TRUE)

# Seperate labels and images for the modelling and the verification set
x <- modelling[,2:ncol(modelling)]
y <- factor(modelling[,1])
x_verification <- verification[,2:ncol(verification)]
y_verification <- factor(verification[,1])

# Splitting the modelling set into mod and test set (not using the verification for model building)
mod <- modelling[1:50000,]
test <- modelling[50001:60000,]

# Seperating pictures and labels
x_mod <- mod[,2:785]
y_mod <- as.factor(mod[,1])
x_test <- test[,2:785]
y_test <- as.factor(test[,1])


################################################################
# knn optimization on the training set
################################################################

# PCA for the training model 
pca_mod <- prcomp(x_mod)
col_means <- colMeans(x_test)

# Prepare the test set to conduct the same components as the training set
x_test_knn3 <- sweep(as.matrix(x_test), 2, col_means) %*% pca_mod$rotation


# Vector with components used
i <- seq(10, 100, 10)

# Sapply for all the values in the vector i
accuracy_knn <- sapply(i, function(k){
  
  # Reduce the training and test set
  x_train <- pca_mod$x[,1:k]
  x_test_knn3 <- x_test_knn3[,1:k]
  
  # Train the model with knn3
  fit_knn3 <- knn3(x_train, y_mod)
  
  # Predict for the test set
  y_hat_knn3 <- predict(fit_knn3, x_test_knn3, type = "class")
  
  # Print the overall accuracy and the differences by class
  return(confusionMatrix(y_hat_knn3, factor(y_test))$overall["Accuracy"])
}
)


################################################################
# rf optimization on the training set
################################################################

# Optimizing the number of components used for the random forest model
# From 10 to 100 in steps of 10
i <- seq(10, 100, 10)

# Sapply for the vector i
accuracy_rf_k <- sapply(i, function(k){
  
  # Create the training set
  x_train <- pca_mod$x[,1:k]
  x_train <- data.frame(y_mod, x_train)
  
  # Create the test set
  x_test_rpart <- sweep(as.matrix(x_test), 2, col_means) %*% pca_mod$rotation
  x_test_rpart <- data.frame(x_test_rpart[,1:k])
  
  # Model building
  rf <- randomForest(
    y_mod ~ .,
    data = x_train,
    ntree = 50
  )
  
  # Prediction on the test set
  y_hat_rf <- predict(rf, data.frame(x_test_rpart), type = "class")
  
  # Return the accuracy
  return(confusionMatrix(y_hat_rf, y_test)$overall["Accuracy"])
}
)

# Optimizing the number of trees used for the random forest model
# Vector with optimization values
j <- seq(20, 200, 20)

# Take the best number of components from the previous optimization
k <- i[which.max(accuracy_rf_k)]

# Sapply for the vector j
accuracy_rf_ntree <- sapply(j, function(n){
  
  # Create the training set
  x_train <- pca_mod$x[,1:k]
  x_train <- data.frame(y_mod, x_train)
  
  # Create the test set
  x_test_rpart <- sweep(as.matrix(x_test), 2, col_means) %*% pca_mod$rotation
  x_test_rpart <- data.frame(x_test_rpart[,1:k])
  
  # Model building
  rf <- randomForest(
    y_mod ~ .,
    data = x_train,
    ntree = n
  )
  
  # Prediction on the test set
  y_hat_rf <- predict(rf, data.frame(x_test_rpart), type = "class")
  
  # Return the accuracy
  return(confusionMatrix(y_hat_rf, y_test)$overall["Accuracy"])
}
)

################################################################
# Compute the final accuracy of knn on the verification set
################################################################

# Principal component analysis on the full set
pca <- prcomp(x)
# Mean values for the columns of the verification set
col_means <- colMeans(x_verification)

# Take the best k value from the previous excerise
k <- i[which.max(accuracy_knn)]

# Build the train set
x_train <- pca$x[,1:k]

# Prepare the test set to conduct the same components as the training set
x_ver_knn3 <- sweep(as.matrix(x_verification), 2, col_means) %*% pca$rotation
x_ver_knn3 <- x_ver_knn3[,1:k]

# Train the model with knn3
fit_knn3 <- knn3(x_train, y)

# Predict for the test set
y_hat_knn3 <- predict(fit_knn3, x_ver_knn3, type = "class")


################################################################
# Compute the final accuracy of rf on the verification set
################################################################

# Take the best parameters from the model building chapter
k <- i[which.max(accuracy_rf_k)]
n <- j[which.max(accuracy_rf_ntree)]

# Create the training set
x_train <- pca$x[,1:k]
x_train <- data.frame(y, x_train)

# Create the test set
x_ver_rpart <- sweep(as.matrix(x_verification), 2, col_means) %*% pca$rotation
x_ver_rpart <- data.frame(x_ver_rpart[,1:k])

# Model building
rf <- randomForest(
  y ~ .,
  data = x_train,
  ntree = n
)

# Prediction on the test set
y_hat_rf <- predict(rf, data.frame(x_ver_rpart), type = "class")

# Print the accuracy
sprintf("Accuracy knn: %s", confusionMatrix(y_hat_knn3, factor(y_verification))$overall["Accuracy"])
sprintf("Accuracy rf: %s", confusionMatrix(y_hat_rf, y_verification)$overall["Accuracy"])