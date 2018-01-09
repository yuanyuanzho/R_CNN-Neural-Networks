#Data Cleaning Part------------------------------------------------------------------------------------------

#The data cleanning part is copied from my midterm project,so this part may be similar with my teammates'.
#Read the csv data
Prudential_Data <-read.csv("/Users/eavy/Downloads/7390/Final/Cleaned_Data.csv", header = T)
summary(Prudential_Data)
str(Prudential_Data)

# Finding the the percentage of null values in each column
colMeans(is.na(Prudential_Data)*100)


#Removing the columns that have more than or equal 70% of null values; and removing unnecessary Id column
Prudential_Data <- subset(Prudential_Data, select = -c(Id,Medical_History_15, Medical_History_24 ,Medical_History_32, Medical_History_10,Family_Hist_5) )


# Replacing the null values in each column with the computed mean of that column
Prudential_Data$Family_Hist_2 <- ifelse(is.na(Prudential_Data$Family_Hist_2), ave(Prudential_Data$Family_Hist_2, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Family_Hist_2)

Prudential_Data$Family_Hist_3 <- ifelse(is.na(Prudential_Data$Family_Hist_3), ave(Prudential_Data$Family_Hist_3, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Family_Hist_3)

Prudential_Data$Family_Hist_4 <- ifelse(is.na(Prudential_Data$Family_Hist_4), ave(Prudential_Data$Family_Hist_4, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Family_Hist_4)

Prudential_Data$Insurance_History_5 <- ifelse(is.na(Prudential_Data$Insurance_History_5), ave(Prudential_Data$Insurance_History_5, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Insurance_History_5)

Prudential_Data$Medical_History_1 <- ifelse(is.na(Prudential_Data$Medical_History_1), ave(Prudential_Data$Medical_History_1, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Medical_History_1)

Prudential_Data$Employment_Info_1 <- ifelse(is.na(Prudential_Data$Employment_Info_1), ave(Prudential_Data$Employment_Info_1, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Employment_Info_1)

Prudential_Data$Employment_Info_4 <- ifelse(is.na(Prudential_Data$Employment_Info_4), ave(Prudential_Data$Employment_Info_4, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Employment_Info_4)

Prudential_Data$Employment_Info_6 <- ifelse(is.na(Prudential_Data$Employment_Info_6), ave(Prudential_Data$Employment_Info_6, FUN=function(x) mean(x, na.rm = TRUE)), Prudential_Data$Employment_Info_6)

# Checking the percentage of null values again
colMeans(is.na(Prudential_Data)*100)

#The sum of null values for a specific column should be zero after filling null values with the computed mean value
sum(is.na(Prudential_Data$Family_Hist_2)) # sum is zero as no more null values


#Setting categorical columns to as a factor to get factors with n levels
Prudential_Data$Product_Info_1 <- as.factor(Prudential_Data$Product_Info_1)
Prudential_Data$Product_Info_2 <- as.factor(Prudential_Data$Product_Info_2)
Prudential_Data$Product_Info_5 <- as.factor(Prudential_Data$Product_Info_5)
Prudential_Data$Product_Info_6 <- as.factor(Prudential_Data$Product_Info_6)
Prudential_Data$Product_Info_7 <- as.factor(Prudential_Data$Product_Info_7)

Prudential_Data$Employment_Info_3 <- as.factor(Prudential_Data$Employment_Info_3)
Prudential_Data$Employment_Info_5 <- as.factor(Prudential_Data$Employment_Info_5)

Prudential_Data$InsuredInfo_1 <- as.factor(Prudential_Data$InsuredInfo_1)
Prudential_Data$InsuredInfo_2 <- as.factor(Prudential_Data$InsuredInfo_2)
Prudential_Data$InsuredInfo_3 <- as.factor(Prudential_Data$InsuredInfo_3)
Prudential_Data$InsuredInfo_4 <- as.factor(Prudential_Data$InsuredInfo_4)
Prudential_Data$InsuredInfo_5 <- as.factor(Prudential_Data$InsuredInfo_5)
Prudential_Data$InsuredInfo_6 <- as.factor(Prudential_Data$InsuredInfo_6)
Prudential_Data$InsuredInfo_7 <- as.factor(Prudential_Data$InsuredInfo_7)

Prudential_Data$Insurance_History_1 <- as.factor(Prudential_Data$Insurance_History_1)
Prudential_Data$Insurance_History_2 <- as.factor(Prudential_Data$Insurance_History_2)
Prudential_Data$Insurance_History_3 <- as.factor(Prudential_Data$Insurance_History_3)
Prudential_Data$Insurance_History_4 <- as.factor(Prudential_Data$Insurance_History_4)
Prudential_Data$Insurance_History_7 <- as.factor(Prudential_Data$Insurance_History_7)
Prudential_Data$Insurance_History_8 <- as.factor(Prudential_Data$Insurance_History_8)
Prudential_Data$Insurance_History_9 <- as.factor(Prudential_Data$Insurance_History_9)

Prudential_Data$Family_Hist_1 <- as.factor(Prudential_Data$Family_Hist_1)

Prudential_Data$Medical_History_3 <- as.factor(Prudential_Data$Medical_History_3)
Prudential_Data$Medical_History_4 <- as.factor(Prudential_Data$Medical_History_4)
Prudential_Data$Medical_History_5 <- as.factor(Prudential_Data$Medical_History_5)
Prudential_Data$Medical_History_6 <- as.factor(Prudential_Data$Medical_History_6)
Prudential_Data$Medical_History_7 <- as.factor(Prudential_Data$Medical_History_7)
Prudential_Data$Medical_History_8 <- as.factor(Prudential_Data$Medical_History_8)
Prudential_Data$Medical_History_9 <- as.factor(Prudential_Data$Medical_History_9)
Prudential_Data$Medical_History_11 <- as.factor(Prudential_Data$Medical_History_11)
Prudential_Data$Medical_History_12 <- as.factor(Prudential_Data$Medical_History_12)
Prudential_Data$Medical_History_13 <- as.factor(Prudential_Data$Medical_History_13)
Prudential_Data$Medical_History_14 <- as.factor(Prudential_Data$Medical_History_14)
Prudential_Data$Medical_History_16 <- as.factor(Prudential_Data$Medical_History_16)
Prudential_Data$Medical_History_17 <- as.factor(Prudential_Data$Medical_History_17)
Prudential_Data$Medical_History_18 <- as.factor(Prudential_Data$Medical_History_18)
Prudential_Data$Medical_History_19 <- as.factor(Prudential_Data$Medical_History_19)
Prudential_Data$Medical_History_20 <- as.factor(Prudential_Data$Medical_History_20)
Prudential_Data$Medical_History_21 <- as.factor(Prudential_Data$Medical_History_21)
Prudential_Data$Medical_History_22 <- as.factor(Prudential_Data$Medical_History_22)
Prudential_Data$Medical_History_23 <- as.factor(Prudential_Data$Medical_History_23)
Prudential_Data$Medical_History_25 <- as.factor(Prudential_Data$Medical_History_25)
Prudential_Data$Medical_History_26 <- as.factor(Prudential_Data$Medical_History_26)
Prudential_Data$Medical_History_27 <- as.factor(Prudential_Data$Medical_History_27)
Prudential_Data$Medical_History_28 <- as.factor(Prudential_Data$Medical_History_28)
Prudential_Data$Medical_History_29 <- as.factor(Prudential_Data$Medical_History_29)
Prudential_Data$Medical_History_30 <- as.factor(Prudential_Data$Medical_History_30)
Prudential_Data$Medical_History_31 <- as.factor(Prudential_Data$Medical_History_31)
Prudential_Data$Medical_History_33 <- as.factor(Prudential_Data$Medical_History_33)
Prudential_Data$Medical_History_34 <- as.factor(Prudential_Data$Medical_History_34)
Prudential_Data$Medical_History_35 <- as.factor(Prudential_Data$Medical_History_35)
Prudential_Data$Medical_History_36 <- as.factor(Prudential_Data$Medical_History_36)
Prudential_Data$Medical_History_37 <- as.factor(Prudential_Data$Medical_History_37)
Prudential_Data$Medical_History_38 <- as.factor(Prudential_Data$Medical_History_38)
Prudential_Data$Medical_History_39 <- as.factor(Prudential_Data$Medical_History_39)
Prudential_Data$Medical_History_40 <- as.factor(Prudential_Data$Medical_History_40)
Prudential_Data$Medical_History_41 <- as.factor(Prudential_Data$Medical_History_41)

# Making sure the data types of the categorical columns have changed to Factor with n levels
str(Prudential_Data)

# Creating a function to convert each level of every categorical column to a separate column
convert.fun <- function(Prudential_Data, Attribute){
  for(level in unique(Prudential_Data[[Attribute]])){
    Prudential_Data[paste(Attribute,seq = "_",level)]<- ifelse(Prudential_Data[[Attribute]] == level,1,0)
  }
  return(subset(Prudential_Data,select = -get(Attribute)))
}

# Calling the function for all categorical columns; If the categorical column has a lot of values, then we deal with it as a discrete column (e.g. Product_Info_3, Employment_Info_2, Medical_History_2)
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_1")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_2")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_5")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_6")
Prudential_Data <- convert.fun(Prudential_Data, "Product_Info_7")

Prudential_Data <- convert.fun(Prudential_Data, "Employment_Info_3")
Prudential_Data <- convert.fun(Prudential_Data, "Employment_Info_5")

Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_1")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_2")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_3")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_4")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_5")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_6")
Prudential_Data <- convert.fun(Prudential_Data, "InsuredInfo_7")

Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_1")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_2")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_3")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_4")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_7")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_8")
Prudential_Data <- convert.fun(Prudential_Data, "Insurance_History_9")
Prudential_Data <- convert.fun(Prudential_Data, "Family_Hist_1")

Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_3")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_4")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_5")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_6")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_7")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_8")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_9")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_11")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_12")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_13")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_14")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_16")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_17")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_18")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_19")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_20")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_21")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_22")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_23")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_25")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_26")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_27")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_28")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_29")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_30")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_31")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_33")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_34")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_35")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_36")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_37")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_38")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_39")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_40")
Prudential_Data <- convert.fun(Prudential_Data, "Medical_History_41")

# Writing the cleaned data to a new file
Cleaned_Data <- Prudential_Data
write.csv(Cleaned_Data, file = "C:/Users/h/Desktop/Cleaned_Data.csv", row.names = FALSE)
str(Cleaned_Data)

library(stats)
Cleaned_Data_Pca <- prcomp(Cleaned_Data, center = TRUE, scale = TRUE) 
summary(Cleaned_Data_Pca)
biplot(Cleaned_Data_Pca)

Cleaned_Data_Pca$x


#Fitting the linear regression model
fit <-lm(Cleaned_Data$Response ~. ,Cleaned_Data)
summary(fit)

#Taking only variables/columns with p-value less than or equal 0.05
Final_data <- data.frame(summary(fit)$coef[summary(fit)$coef[,4] <= .0000001, 4])

#Checking the number of variables/columns left after using p-value
length(summary(fit)$coef[summary(fit)$coef[,4] <= .0000001, 4])

#Writing the complete final data with all 24 columns to a new file
Prudential_final_Data <- subset(Cleaned_Data, select = c(Product_Info_4,Ins_Age,Ht,Wt,Family_Hist_4,Medical_History_1,Medical_Keyword_3,
                                                         Medical_Keyword_9,Medical_Keyword_25,Medical_Keyword_33,
                                                         Medical_Keyword_34,Medical_Keyword_38,Medical_Keyword_41,
                                                         Medical_Keyword_45,`Employment_Info_3 _ 1`,`InsuredInfo_2 _ 2`,
                                                         `InsuredInfo_5 _ 1`,`InsuredInfo_6 _ 2`,`InsuredInfo_7 _ 1`,
                                                         `Medical_History_4 _ 1`,`Medical_History_11 _ 3`,`Medical_History_11 _ 1`,
                                                         `Medical_History_35 _ 1`,`Medical_History_39 _ 3`,Response) )
#Making sure all 24 variable with Response column are included 
str(Prudential_final_Data)
#------write.csv(Prudential_final_Data, file = "/Users/ouyoshimisatoshi/Desktop/Data Science/MidtermProject/data/Prudential_final_Data.csv", row.names = FALSE)

#Making sure all of the column names are acceptable model inputs
colnames(Prudential_final_Data)
colnames(Prudential_final_Data)[1]<-"ProductInfo4"
colnames(Prudential_final_Data)[2]<-"InsAge"
colnames(Prudential_final_Data)[5]<-"FamilyHist4"
colnames(Prudential_final_Data)[6]<-"MedicalHistory1"
colnames(Prudential_final_Data)[7]<-"MedicalKeyword3"
colnames(Prudential_final_Data)[8]<-"MedicalKeyword9"
colnames(Prudential_final_Data)[9]<-"MedicalKeyword25"
colnames(Prudential_final_Data)[10]<-"MedicalKeyword33"
colnames(Prudential_final_Data)[11]<-"MedicalKeyword34"
colnames(Prudential_final_Data)[12]<-"MedicalKeyword38"
colnames(Prudential_final_Data)[13]<-"MedicalKeyword41"
colnames(Prudential_final_Data)[14]<-"MedicalKeyword45"
colnames(Prudential_final_Data)[15]<-"EmploymentInfo31"
colnames(Prudential_final_Data)[16]<-"InsuredInfo22"
colnames(Prudential_final_Data)[17]<-"InsuredInfo51"
colnames(Prudential_final_Data)[18]<-"InsuredInfo62"
colnames(Prudential_final_Data)[19]<-"InsuredInfo71"
colnames(Prudential_final_Data)[20]<-"MedicalHistory41"
colnames(Prudential_final_Data)[21]<-"MedicalHistory113"
colnames(Prudential_final_Data)[22]<-"MedicalHistory111"
colnames(Prudential_final_Data)[23]<-"MedicalHistory351"
colnames(Prudential_final_Data)[24]<-"MedicalHistory393"


colnames(Prudential_final_Data)

#Scaling(normalize) the data
maxs <- apply(Prudential_final_Data, 2, max) 
mins <- apply(Prudential_final_Data, 2, min)
scaled_Prudential_final_Data <- as.data.frame(scale(Prudential_final_Data, center = mins, scale = maxs - mins))
head(scaled_Prudential_final_Data)

#Splitting dataset into train and test datasets
set.seed(2) # we set the seed to make sure that the train and test data will not change every time we divide them by running the sample function

sample_index = sample(1:nrow(scaled_Prudential_final_Data), nrow(scaled_Prudential_final_Data)*0.8) #length(sample_index) should be %80 of the dataset
Prudential_train = scaled_Prudential_final_Data[sample_index,] #80% of dataset is train data
Prudential_test= scaled_Prudential_final_Data[-sample_index,] #20% of dataset is test data
head(Prudential_train)
str(Prudential_train)
head(Prudential_test)
str(Prudential_test)
#Runing neural networks part-----------------------------------------------------------------------------

# Taking 1000 rows randomly to fit the SVM model because the original dataset is so huge
set.seed(2)
sample_index2 = sample(1:nrow(scaled_Prudential_final_Data), size=1000) 
Prudential_data2 = scaled_Prudential_final_Data[sample_index2,] 

#Split the new dataset Prudential_data2 to two parts,one part for training, one for testing
sample_index3 = sample(1:nrow(Prudential_data2), size=800) 
Prudential_train2 = Prudential_data2[sample_index3,] #80% of Prudential_data2 is new train data
Prudential_test2= Prudential_data2[-sample_index3,]  
library(neuralnet)
n <- names(Prudential_train2)
n
n2<-paste("Response ~", paste(n[!n %in% "Response"], collapse = " + "))
f <- as.formula(n2)
nn <- neuralnet(f,data=Prudential_train2,hidden=10,linear.output=T,stepmax=1e6)
#MSE.nn=3.023956971
nn <- neuralnet(f,data=Prudential_train2,hidden=11,linear.output=T,stepmax=1e6)
#1.968221674
nn <- neuralnet(f,data=Prudential_train2,hidden=c(9,5),linear.output=T,stepmax=1e7)
#2.87103463
nn <- neuralnet(f,data=Prudential_train2,hidden=c(10,5),linear.output=T,stepmax=1e6)
#3.890517195
nn <- neuralnet(f,data=Prudential_train2,hidden=c(12,5),linear.output=T,stepmax=1e6)
#3.077473047
nn <- neuralnet(f,data=Prudential_train2,hidden=c(12,6),linear.output=T,stepmax=1e6)
#2.634855857

plot(nn)

#making prediction                
pr.nn <- compute(nn,Prudential_test2[,1:24])

#scale it back in order to make a meaningful comparison 
pr.nn_ <- pr.nn$net.result*(max(Prudential_final_Data$Response)
                            -min(Prudential_final_Data$Response))
                            +min(Prudential_final_Data$Response)
#true value
test.r <- (Prudential_test2$Response)*(max(Prudential_final_Data$Response)
                                       -min(Prudential_final_Data$Response))
                                       +min(Prudential_final_Data$Response)

#analysis the prediction
#Analytical method 1
error.df<-data.frame(test.r,pr.nn_)
head(error.df)
library(ggplot2)
ggplot(error.df,aes(x=test.r,y=pr.nn_))+geom_point()+stat_smooth()

plot(test.r,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

#Analytical method 2
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(Prudential_test2)
MSE.nn