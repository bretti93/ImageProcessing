################################
# Create train set, test set
################################

# Load required packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")

# Download the dataset from my Dropbox and unzip it
url <- "https://www.dropbox.com/s/7yzdmv2im26hauh/fashionmnist.zip?raw=1"
download(url, dest="fashionmnist.zip", mode="wb") 
unzip ("fashionmnist.zip", exdir = ".")

# Create the data set for model building and verification
verification = read.csv("fashion-mnist_test.csv", header = TRUE)
modelling = read.csv("fashion-mnist_train.csv", header = TRUE)

# Seperate labels and images
x <- modelling[,2:ncol(modelling)]
y <- factor(modelling[,1])
x_verification <- verification[,2:ncol(verification)]
y_verification <- factor(verification[,1])

x_images <- array(as.numeric(unlist(x)), dim=c(60000, 28, 28))
x_verification_images <- array(as.numeric(unlist(x_verification)), dim=c(10000, 28, 28))

x_mod <- modelling[1:50000,]
x_test <- modelling[50001:60000,]
x_sub <- x_images[1:50000,,]
y_sub <- y[1:50000]
x_test_sub <- x_images[50001:60000,,]
y_test_sub <- y[50001:60000]

fashion_cat = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')  
