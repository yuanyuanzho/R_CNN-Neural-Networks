# cran <- getOption("repos")
# cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
# options(repos = cran)
# install.packages("mxnet",dependencies = T)
library("mxnet")
library("caret")

HandwrittenData <- read.csv("semeion.csv")
coordName <- NULL
for(i in c(1:16)){
  coord <- paste(i, paste("-", c(1:16), sep = ""), sep = "")
  coordName <- c(coordName, coord)
}
colnames(HandwrittenData) <- c(coordName, c(0:9))
for(i in c(1:16)){
  HandwrittenData[,i] <- as.integer(HandwrittenData[,i])
}
head(HandwrittenData)


numOfindex <- as.integer(nrow(HandwrittenData) * 0.2)
set.seed(200)
sampleIndex <- sample(c(1:nrow(HandwrittenData)), numOfindex)
train.data <- HandwrittenData[-sampleIndex,]
test.data <-  HandwrittenData[sampleIndex,]
head(train.data)

train.x <- train.data[,-(257:266)]
train.y <- train.data[,(257:266)]
text.x <- test.data[,-(257:266)]
text.y <- test.data[,(257:266)]
head(train.x)
head(train.y)

train_x <- t(train.x)
train_array <- train_x
dim(train_array) <- c(16, 16, 1, ncol(train_x))
train_y <- train.y
text_x <- t(text.x)
test_array <- text_x
dim(test_array) <- c(16, 16, 1, ncol(test_array))
test_y <- text.y


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
train_y
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
test_y

test_output <- test_y[,"Label"]


data <- mx.symbol.Variable('data')
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
# Output. Softmax output since we'd like to get some probabilities.
NN_model <- mx.symbol.SoftmaxOutput(data = fc_1)



# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 3nd convolutional layer
conv_3 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 80)
tanh_3 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_3 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_3)
fc_2 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
# Output. Softmax output since we'd like to get some probabilities.
NN_model_2 <- mx.symbol.SoftmaxOutput(data = fc_2)






# 3nd convolutional layer
conv_3 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 80)
tanh_3 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_3 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_3)
fc_3_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
tanh_fc_1 <- mx.symbol.Activation(data=fc_3_1, act_type="tanh")
fc_3_2 <- mx.symbol.FullyConnected(data=tanh_fc_1, num_hidden=10)
# Output. Softmax output since we'd like to get some probabilities.
NN_model_3 <- mx.symbol.SoftmaxOutput(data = fc_3_2)



# Set seed for reproducibility
mx.set.seed(100)
# Device used. CPU in this case.
devices <- mx.cpu()



model <- mx.model.FeedForward.create(NN_model,
                                     X = train_array,
                                     y = train_output,
                                     ctx = devices,
                                     num.round = 480,
                                     array.batch.size = 40,
                                     learning.rate = 0.01,
                                     momentum = 0.9,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))


model_2 <- mx.model.FeedForward.create(NN_model_2,
                                       X = train_array,
                                       y = train_output,
                                       ctx = devices,
                                       num.round = 480,
                                       array.batch.size = 40,
                                       learning.rate = 0.01,
                                       momentum = 0.9,
                                       eval.metric = mx.metric.accuracy)


model_3 <- mx.model.FeedForward.create(NN_model_3,
                                       X = train_array,
                                       y = train_output,
                                       ctx = devices,
                                       num.round = 480,
                                       array.batch.size = 40,
                                       learning.rate = 0.01,
                                       momentum = 0.9,
                                       eval.metric = mx.metric.accuracy)


# Predict labels
predicted <- predict(model, test_array)
# Assign labels
predicted_labels <- max.col(t(predicted)) - 1



# Predict labels
predicted_2 <- predict(model_2, test_array)
# Assign labels
predicted_labels_2 <- max.col(t(predicted_2)) - 1


# Predict labels
predicted_3 <- predict(model_3, test_array)
# Assign labels
predicted_labels_3 <- max.col(t(predicted_3)) - 1




library(lattice)
library(ggplot2)
library(caret)
conf.mat <- confusionMatrix(predicted_labels, test_output)
conf.mat





