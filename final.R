setwd("/Users/eavy/Downloads/7390/Final")
# The CPU version of MXNet R package can be installed in R like other packages
cran <- getOption("repos")
cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
options(repos = cran)
#install.packages("mxnet")
#install.packages("mlbench")

library(mlbench)
library(mxnet)
library(ggplot2)
library(caret)
library(lattice)
require(mlbench)
require(mxnet)

mydata <- read.csv("semeion.csv")

coordName <- NULL 
for(i in c(1:16)){
  coord <- paste(i, paste("-", c(1:16), sep = ""), sep = "")
  coordName <- c(coordName, coord)
}

coordName #Creating header names
colnames(mydata) <- c(coordName, c(0:9)) #Assigning column names to each column

colnames(mydata)

for(i in c(1:16)){
  mydata[,i] <- as.integer(mydata[,i])
}
head(mydata)


# Split dataset into train and test dataset
numOfindex <- as.integer(nrow(mydata) * 0.2)
set.seed(100)
index <- sample(c(1:nrow(mydata)), numOfindex)
train.data <- mydata[-index,]
test.data <-  mydata[index,]

train.data #80% data
test.data


# Split datasets into X(input) and Y(output)
train.x <- train.data[,-(257:266)]
train.y <- train.data[,(257:266)]

text.x <- test.data[,-(257:266)]
text.y <- test.data[,(257:266)]

str(train.x) #256 Variables
str(train.y) #10 Variables

str(text.x)
str(text.y)

# reshape the matrices into arrays:
# Set up train and test datasets
train_x <- t(train.x)
train_array <- train_x
dim(train_array) <- c(16, 16, 1, ncol(train_x))
train_y <- train.y

train_y

text_x <- t(text.x)
test_array <- text_x
dim(test_array) <- c(16, 16, 1, ncol(test_array))
test_y <- text.y

# Then let’s prepare the dataframe for each image and then using a for loop 
# we will resize it and set it to greyscale. 
# We also set names: First columns are the labels and the other columns are the pixels.
# Convert output vector to one number “label” and set “label” as Integer
train_y[,"Label"] <- NULL
for(i in c(1:nrow(train_y))) {
  if(train_y[i,"0"]==1){
    train_y[i,"Label"] <- 0
  }else if(train_y[i,"1"]==1){
    train_y[i,"Label"] <- 1
  }else if(train_y[i,"2"]==1){
    train_y[i,"Label"] <- 2
  }else if(train_y[i,"3"]==1){
    train_y[i,"Label"] <- 3
  }else if(train_y[i,"4"]==1){
    train_y[i,"Label"] <- 4
  }else if(train_y[i,"5"]==1){
    train_y[i,"Label"] <- 5
  }else if(train_y[i,"6"]==1){
    train_y[i,"Label"] <- 6
  }else if(train_y[i,"7"]==1){
    train_y[i,"Label"] <- 7
  }else if(train_y[i,"8"]==1){
    train_y[i,"Label"] <- 8
  }else if(train_y[i,"9"]==1){
    train_y[i,"Label"] <- 9
  }
}
train_y[,"Label"] <- as.integer(train_y[,"Label"])
train_output <- train_y[,"Label"]





test_y[,"Label"] <- NULL
for(i in c(1:nrow(test_y))) {
  if(test_y[i,"0"]==1){
    test_y[i,"Label"] <- 0
  }else if(test_y[i,"1"]==1){
    test_y[i,"Label"] <- 1
  }else if(test_y[i,"2"]==1){
    test_y[i,"Label"] <- 2
  }else if(test_y[i,"3"]==1){
    test_y[i,"Label"] <- 3
  }else if(test_y[i,"4"]==1){
    test_y[i,"Label"] <- 4
  }else if(test_y[i,"5"]==1){
    test_y[i,"Label"] <- 5
  }else if(test_y[i,"6"]==1){
    test_y[i,"Label"] <- 6
  }else if(test_y[i,"7"]==1){
    test_y[i,"Label"] <- 7
  }else if(test_y[i,"8"]==1){
    test_y[i,"Label"] <- 8
  }else if(test_y[i,"9"]==1){
    test_y[i,"Label"] <- 9
  }
}
test_y[,"Label"] <- as.integer(test_y[,"Label"])
test_output <- test_y[,"Label"]









# Set up the symbolic model
# Create a symbolic variable
data <- mx.symbol.Variable('data')


#-------------------------------------------------------------------------------
# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")

# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 10)

# Output. Softmax output since we'd like to get some probabilities.
NN_model <- mx.symbol.SoftmaxOutput(data = fc_2)



#-------------------------------------------------------------------------------
# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(4, 4), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(4, 4), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")

# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 40)

# Output. Softmax output since we'd like to get some probabilities.
NN_model2 <- mx.symbol.SoftmaxOutput(data = fc_2)



#-------------------------------------------------------------------------------
# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(3, 3), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(3, 3), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")

# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 40)

# Output. Softmax output since we'd like to get some probabilities.
NN_model3 <- mx.symbol.SoftmaxOutput(data = fc_2)


# Pre-training set up
#-------------------------------------------------------------------------------

# Set seed for reproducibility
mx.set.seed(100)

# Device used. CPU in my case.
devices <- mx.cpu()

# Training
#-------------------------------------------------------------------------------

# Train the model
model <- mx.model.FeedForward.create(NN_model,
                                     X = train_array,
                                     y = train_output,
                                     ctx = devices,
                                     num.round = 510,
                                     array.batch.size = 40,
                                     learning.rate = 0.01,
                                     momentum = 0.9,
                                     eval.metric = mx.metric.accuracy)





model_2 <- mx.model.FeedForward.create(NN_model2,
                                     X = train_array,
                                     y = train_output,
                                     ctx = devices,
                                     num.round = 510,
                                     array.batch.size = 40,
                                     learning.rate = 0.01,
                                     momentum = 0.9,
                                     eval.metric = mx.metric.accuracy)


model_3 <- mx.model.FeedForward.create(NN_model3,
                                       X = train_array,
                                       y = train_output,
                                       ctx = devices,
                                       num.round = 510,
                                       array.batch.size = 40,
                                       learning.rate = 0.01,
                                       momentum = 0.9,
                                       eval.metric = mx.metric.accuracy)







# Testing
#-------------------------------------------------------------------------------
# Predict labels
predicted <- predict(model, test_array)
# Assign labels
predicted_labels <- max.col(t(predicted)) - 1

conf.mat <- confusionMatrix(predicted_labels, test_output)
conf.mat



# Predict labels
predicted <- predict(model_2, test_array)
# Assign labels
predicted_labels <- max.col(t(predicted)) - 1

conf.mat <- confusionMatrix(predicted_labels, test_output)
conf.mat



# Predict labels
predicted <- predict(model_3, test_array)
# Assign labels
predicted_labels <- max.col(t(predicted)) - 1

conf.mat <- confusionMatrix(predicted_labels, test_output)
conf.mat












