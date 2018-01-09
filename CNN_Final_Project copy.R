
                                 #DEEP CONVOLUTIONAL NEURAL NETWORK ON SEMEION DATASET
  
#Description:
  
#1593 handwritten digits (rows) and 256 attributes (columns).
#16x16 in a gray scale of 256 values [resolution of 256 grays scale]
#Each pixel of each image was scaled into a boolean (1/0) value using a fixed threshold
#Each image stretched and scaled between 0 and 1 (setting to 0 every pixel whose value was under tha value 127 
#of the grey scale (127 included) and setting to 1 each pixel whose original value in the grey scale was over 127). 
#Each binary image was scaled again into a 16x16 square box (the final 256 binary attributes).

-------------------------------------------------------------------

install.packages("mxnet") #Provide CNN
install.packages("mlbench")
install.packages("lattice")

library(mlbench)
library(mxnet)
library(ggplot2)
library(caret)
library(lattice)
require(mlbench)
require(mxnet) 

-------------------------------------------------------------------

                #STEP 1: UPLOADING DATA

mydata <- read.csv("desktop/semeion.csv")#Loading the data
str(mydata) #1592 obs. of  266 variables

head_name <- NULL 
for(i in c(1:16)){ #Since we are building a 16x16 matrix
  coord <- paste(i, paste("-", c(1:16), sep = ""), sep = "")
  head_name <- c(head_name, coord)
}

head_name #Displaying created header names
colnames(mydata) <- c(head_name, c(0:9)) #Assigning header names to each column

colnames(mydata)

for(i in c(1:16)){
  mydata[,i] <- as.integer(mydata[,i])
}
head(mydata)

-------------------------------------------------------------------


                #STEP 2: DIVIDING THE DATASET

# Split dataset into train and test dataset
numOfindex <- as.integer(nrow(mydata) * 0.2)
set.seed(200) #Randomly selecting the records for data division
sampleIndex <- sample(c(1:nrow(mydata)), numOfindex)
train.data <- mydata[-sampleIndex,]
test.data <-  mydata[sampleIndex,]

str(train.data) #80% data (1274 rows)
str(test.data) #20% data (318 rows)

# Split datasets into X(input) and Y(output)
train.x <- train.data[,-(257:266)]
train.y <- train.data[,(257:266)]

text.x <- test.data[,-(257:266)]
text.y <- test.data[,(257:266)]

str(train.x) #256 Variables and 1274 rows
str(train.y) #10 Variables and 1274 Rows

str(text.x) #256 Variables and 318 Rows
str(text.y) #10 Variables and 318 Rows

#Reshape the matrices into arrays:
#Set up train and test datasets
#Using MXNet the shape of the data
train_x <- t(train.x) #transposes a matrix or data frame
train_x

train_array <- train_x
#dim shows that data is made of ncol(train_x) of shape 16x16
#1 signifies that the image is greyscale (1 channel) [If RGB, enter 3]
dim(train_array) <- c(16, 16, 1, ncol(train_x)) 
train_y <- train.y
train_y

text_x <- t(text.x) #transposes a matrix or data frame
test_array <- text_x
dim(test_array) <- c(16, 16, 1, ncol(test_array))
test_y <- text.y
test_y

-------------------------------------------------------------------
                
  
                #STEP 3: CREATING LABELS

#Convert output vector to one number “label” and set “label” as Integer
#Assigning each vector to a number from 0-9 to set it as label 
train_y[,"Label"] <- NULL  #Creating an array to store the labels
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
train_y[,"Label"] <- as.integer(train_y[,"Label"]) #tests for type "integer"
train_output <- train_y[,"Label"]
train_output


#Similarly for test dataset
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
test_output

#-------------------------------------------------------------------------------

                #STEP 4: BUILDING THE THREE MODELS 

#Symbolic model
model_data <- mx.symbol.Variable('data')
model_data

#Model Structure:
#2 Convolutional layer: Uses 5x5 kernel
#2 Non-linear activation layer
#2 Pooling (downsampling) Layer: max pooling, act_type is kept as tanh [others can be signma and ReLu]
#2 Fully Connected Layers: Outputs the classification labels
#Softmax Output

  #MODEL 1:

# 1st convolutional layer
convolution_1 <- mx.symbol.Convolution(data = model_data, kernel = c(5, 5), num_filter = 20)
activation_1 <- mx.symbol.Activation(data = convolution_1, act_type = "tanh")
pooling_1 <- mx.symbol.Pooling(data = activation_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 2nd convolutional layer
convolution_2 <- mx.symbol.Convolution(data = pooling_1, kernel = c(5, 5), num_filter = 50)
activation_2 <- mx.symbol.Activation(data = convolution_2, act_type = "tanh")
pooling_2 <- mx.symbol.Pooling(data=activation_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pooling_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
activation_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")

# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = activation_3, num_hidden = 10)

#Using softmax, generate the output:
NN_model <- mx.symbol.SoftmaxOutput(data = fc_2)


  #MODEL 2:

#1st convolutional layer
convolution_1 <- mx.symbol.Convolution(data = model_data, kernel = c(4, 4), num_filter = 20)
activation_1 <- mx.symbol.Activation(data = convolution_1, act_type = "tanh")
pooling_1 <- mx.symbol.Pooling(data = activation_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

#2nd convolutional layer
convolution_2 <- mx.symbol.Convolution(data = pooling_1, kernel = c(4, 4), num_filter = 50)
activation_2 <- mx.symbol.Activation(data = convolution_2, act_type = "tanh")
pooling_2 <- mx.symbol.Pooling(data=activation_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

#1st fully connected layer
flatten <- mx.symbol.Flatten(data = pooling_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
activation_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")

#2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = activation_3, num_hidden = 40)

#Using softmax, generate the output:
NN_model2 <- mx.symbol.SoftmaxOutput(data = fc_2)


  #MODEL 3:

#1st convolutional layer
convolution_1 <- mx.symbol.Convolution(data = model_data, kernel = c(3, 3), num_filter = 20)
activation_1 <- mx.symbol.Activation(data = convolution_1, act_type = "tanh")
pooling_1 <- mx.symbol.Pooling(data = activation_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

#2nd convolutional layer:
convolution_2 <- mx.symbol.Convolution(data = pooling_1, kernel = c(3, 3), num_filter = 50)
activation_2 <- mx.symbol.Activation(data = convolution_2, act_type = "tanh")
pooling_2 <- mx.symbol.Pooling(data=activation_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

#1st fully connected layer:
flatten <- mx.symbol.Flatten(data = pooling_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
activation_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")

#2nd fully connected layerL
fc_2 <- mx.symbol.FullyConnected(data = activation_3, num_hidden = 40)

#Using softmax, generate the output:
NN_model3 <- mx.symbol.SoftmaxOutput(data = fc_2)


-------------------------------------------------------------------
  
                #STEP 5: PRE-TRAINING SETUP


#Set seed for reproducibility:
mx.set.seed(100)

#Device used. CPU:
dev <- mx.cpu()


-------------------------------------------------------------------

                #STEP 6: TRAINING THE MODEL

#Hyperparameter Tuning:
#1) Learning Rate: controls how much to update the weight in the optimization algorithm
#2) Epochs: the number of times the entire training set pass through the neural network
#3) Batch Size: defines number of samples that going to be propagated through the network
#4) Activation Function: defines the output of that node given an input or set of inputs
#5) Number of hidden layers/units
#6) Num.round: Number of times the data is trained for accuracy 


  #MODEL 1
model <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_output, ctx = dev, num.round = 130, 
                                     array.batch.size = 40, learning.rate = 0.01, momentum = 0.9, eval.metric = mx.metric.accuracy)

  #MODEL 2
model_2 <- mx.model.FeedForward.create(NN_model2, X = train_array, y = train_output, ctx = dev, num.round = 130, 
                                       array.batch.size = 40, learning.rate = 0.01, momentum = 0.9, eval.metric = mx.metric.accuracy)

  #MODEL 3
model_3 <- mx.model.FeedForward.create(NN_model3, X = train_array, y = train_output, ctx = dev, num.round = 130,
                                       array.batch.size = 40, learning.rate = 0.01, momentum = 0.9, eval.metric = mx.metric.accuracy)


-------------------------------------------------------------------

                #STEP 7: TESTING DIFFERENT MODELS

  #MODEL 1
# Predict labels
predicted <- predict(model, test_array)
# Assign labels
predicted_labels <- max.col(t(predicted)) - 1
#Create confusion matrix
confusion_mat <- confusionMatrix(predicted_labels, test_output)
confusion_mat


  #MODEL 2
#Predict labels
predicted <- predict(model_2, test_array)
#Assign labels
predicted_labels <- max.col(t(predicted)) - 1
#Create confusion matrix
confusion_mat <- confusionMatrix(predicted_labels, test_output)
confusion_mat


  #MODEL 3
#Predict labels
predicted <- predict(model_3, test_array)
#Assign labels
predicted_labels <- max.col(t(predicted)) - 1
#Create confusion matrix
confusion_mat <- confusionMatrix(predicted_labels, test_output)
confusion_mat



