
############################ Handwritten Digit Recognition #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to to develop a model that can correctly identify the hand written digit (between 0-9).
# It should correctly classify the digits from image based on the pixel values given as features.



#####################################################################################

# 2. Data Understanding: 
# MNIST data which is a large database of handwritten digits 
# where we have pixel values of each digit along with its label. 
# Number of Instances in Training Data: 60,000
# Number of Instances in Test Data: 10,000
# Number of Attributes: 784 (28 x 28). It contains the converted gray scale pixel value of image.
# It can be observed: the pixels in the corners will be mostly blank, hence likely to be 0
# the first column of both the dataset contains the digit to be predicted.

######################################################################################

#3. Data Preparation: 

# Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(caTools)
library(ggplot2)
library(gridExtra)

# Loading Data

setwd("C:/Users/Pritha/Desktop/DS C7 UPGRAD IIITB COURSE/Course 4")
getwd()
mnist_train <- read.csv("mnist_train.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)
mnist_test <- read.csv("mnist_test.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)

# Understanding Dimensions

dim(mnist_train) # [1] 60000   785
dim(mnist_test) # [1] 10000   785

# View the dataset
View(mnist_train)
View(mnist_test)

#Structure of the dataset

str(mnist_train) # 60000 obs. of  785 variables (int)
str(mnist_test) # 10000 obs. of  785 variables (int)


# No labels are provided in the dataset.
# As, we observed, the first column denotes the digit
# the next 784 (28 X 28) columns provide the pixel value.
# Adding labels to first column (for our clear understanding) :

colnames(mnist_train)[1] <- "digit"
colnames(mnist_test)[1] <- "digit"

# this is what we will have to predict

# checking for missing value

sapply(mnist_train, function(x) sum(is.na(x)))
sapply(mnist_test, function(x) sum(is.na(x)))

# 0. No NA

# checking for duplicates

sum(duplicated(mnist_train)) 
sum(duplicated(mnist_test)) 

#0 No duplicates

# Converting output variable to factor

mnist_train$digit <-factor(mnist_train$digit)
mnist_test$digit <-factor(mnist_test$digit)

# checking for the digits column in both the dataset

summary(mnist_train$digit)
#  0    1    2    3    4    5    6    7    8    9 
# 5923 6742 5958 6131 5842 5421 5918 6265 5851 5949 

summary(mnist_test$digit)
#  0    1    2    3    4    5    6    7    8    9 
# 980 1135 1032 1010  982  892  958 1028  974 1009 

# Hence, it can be concluded that both the dataset have 0~9 digits only.


# checking for Pixel value: 0 - 255 

which(sapply(mnist_train, function(x) x>255 | x<0))
# 0 (NOTE: Warning Msg due to 1st column "Digit" is converted into factor)
which(sapply(mnist_test, function(x) x>255 | x<0))
# 0 (NOTE: Warning Msg due to 1st column "Digit" is converted into factor)

# Please note that no columns are deleted because, each data represents pixel value. 

# Data Visualisation: 

#Looking at one digit (Image)
digit_image <- matrix(as.numeric(mnist_train[1, -1]), nrow = 28)
image(digit_image, col = grey.colors(255))

#Looking at distribution of digit in train and test data

G01<- ggplot(mnist_train) + geom_bar(aes(x= digit, fill = "red")) +
      theme(legend.position="none") + 
      labs(title = "Train_Distribution", x = "Digit", y = "count")
G01 # Train Dataset

G02<- ggplot(mnist_test) + geom_bar(aes(x= digit, fill = "red")) +
  theme(legend.position="none") + 
  labs(title = "Test_Distribution", x = "Digit", y = "count")
G02 # Test Dataset

# Hence, more or less, even distribution of digits can be observed in both the datasets.

#taking only 15 % of the training data for constructing model

set.seed(100)
mnist_train <- mnist_train[sample(1:nrow(mnist_train), 0.15*nrow(mnist_train)),]

View(mnist_train)
View(mnist_test)
# No scaling is done as it is a digit recognition dataset

###############################################################################################


# 4. Model Building on mnist_train

########################### 4.1 Using Linear Kernel ############################

Model_linear1 <- ksvm(digit~ ., data = mnist_train, scale = FALSE, kernel = "vanilladot")
Eval_linear1<- predict(Model_linear1, mnist_test)
Model_linear1
# parameter : cost C = 1
# Number of Support Vectors : 2545

# Warning message due to not scaling can be ignored

#confusion matrix - Linear Kernel1
confusionMatrix(Eval_linear1,mnist_test$digit)

#Prediction    0    1    2    3    4    5    6    7    8    9
#         0  961    0   12    6    1   14   18    1    8   10
#         1    0 1118    8    7    0    7    2   10    8    8
#         2    1    3  940   19   10    3   11   24   13    5
#         3    2    1   19  911    2   54    1    8   51   11
#         4    2    2    9    3  942    6   10    9    9   47
#         5    6    2    5   26    0  775   16    1   42    6
#         6    4    3    9    0    3    8  898    0    7    0
#         7    1    0    6   10    3    2    0  932    6   29
#         8    3    6   23   23    1   17    2    3  822   10
#         9    0    0    1    5   20    6    0   40    8  883

# overall Accuracy : 0.9182 
# P-Value [Acc > NIR] : < 2.2e-16

########################### 4.2 Using Non- Linear Kernel ############################

#Using RBF Kernel

Model_RBF <- ksvm(digit~ ., data = mnist_train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnist_test)
Model_RBF
 
# parameter : cost C = 1, Hyperparameter : sigma =  1.63158080573796e-07 
# Number of Support Vectors : 3543

# Warning message due to not scaling can be ignored

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnist_test$digit)



#Prediction    0    1    2    3    4    5    6    7    8    9
#         0  970    0   11    1    2    9    9    0    4    5
#         1    0 1124    0    0    0    1    3   14    1    6
#         2    2    3  981    9    7    4    1   18    4    1
#         3    0    2    6  968    0   18    0    5   12    8
#         4    0    0    8    0  946    4    5    9    9   31
#         5    4    1    0   10    0  839   10    0   12    7
#         6    2    3    4    1    3    8  928    0   10    1
#         7    1    0    6    7    2    2    0  956    6   12
#         8    1    2   15   11    2    5    2    3  912    7
#         9    0    0    1    3   20    2    0   23    4  931

# Overall Accuracy : 0.9555  
# Hence, it can be seen that accuray improves with non-linearity


############   5. Hyperparameter tuning and Cross Validation #####################

# Using the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(100)
lgrid<- expand.grid(C=seq(1,10, by=1))
lin.svm <- train(digit~.,data=mnist_train, method="svmLinear", metric=metric,
                 tuneGrid=lgrid, trControl=trainControl)

print(lin.svm)

# Support Vector Machines with Linear Kernel 

# 9000 samples
# 784 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 7199, 7202, 7199, 7201, 7199 
# Resampling results across tuning parameters:
  
#  C   Accuracy   Kappa    
#  1  0.9077769  0.897504
#  2  0.9077769  0.897504
#  3  0.9077769  0.897504
#  4  0.9077769  0.897504
#  5  0.9077769  0.897504
#  6  0.9077769  0.897504
#  7  0.9077769  0.897504
#  8  0.9077769  0.897504
#  9  0.9077769  0.897504
# 10  0.9077769  0.897504

# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 1.

# Plotting the model result
plot(lin.svm)
# we observe that accuracy is similar for any C(1 to 10)

# Evaluating the model on test
evaluate_lin_test <- predict(lin.svm, mnist_test)
confusionMatrix(evaluate_lin_test,mnist_test$digit)

# Prediction    0    1    2    3    4    5    6    7    8    9
#          0  961    0   12    6    1   14   18    1    8   10
#          1    0 1118    8    7    0    7    2   10    8    8
#          2    1    3  940   19   10    3   11   24   13    5
#          3    2    1   19  911    2   54    1    8   51   11
#          4    2    2    9    3  942    6   10    9    9   47
#          5    6    2    5   26    0  775   16    1   42    6
#          6    4    3    9    0    3    8  898    0    7    0
#          7    1    0    6   10    3    2    0  932    6   29
#          8    3    6   23   23    1   17    2    3  822   10
#          9    0    0    1    5   20    6    0   40    8  883

#Overall Statistics

# Accuracy : 0.9182          
# 95% CI : (0.9127, 0.9235)
# No Information Rate : 0.1135          
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.9091  

# It can be considered as a pretty good model with 91.82% accuracy
# Let's see, if non-linearity can improve accuracy

#####################################################################################

# RBF Kernel

# As we found value of sigma =  1.63158080573796e-07 for RBF model.
# So we will do cross Validation on RBF model with 
# range of Sigma =0.63e-07 ~ 2.63e-07

grid <- expand.grid(.sigma=seq(0.63e-07, 2.63e-07, by= 0.0000001), .C=seq(1, 5, by=1))


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(digit~.,data=mnist_train, method="svmRadial", metric=metric,
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

# Support Vector Machines with Radial Basis Function Kernel 

#9000 samples
#784 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 7200, 7201, 7201, 7199, 7199 
#Resampling results across tuning parameters:
  
# sigma     C  Accuracy   Kappa    
# 6.30e-08  1  0.9360004  0.9288769
# 6.30e-08  2  0.9440000  0.9377661
# 6.30e-08  3  0.9450007  0.9388780
# 6.30e-08  4  0.9487791  0.9430772
# 6.30e-08  5  0.9507792  0.9453000
# 1.63e-07  1  0.9551109  0.9501146
# 1.63e-07  2  0.9618886  0.9576467
# 1.63e-07  3  0.9628888  0.9587582
# 1.63e-07  4  0.9636668  0.9596227
# 1.63e-07  5  0.9643331  0.9603630
# 2.63e-07  1  0.9629998  0.9588820
# 2.63e-07  2  0.9672213  0.9635733
# 2.63e-07  3  0.9674434  0.9638201
# 2.63e-07  4  0.9676657  0.9640670
# 2.63e-07  5  0.9675544  0.9639435

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 2.63e-07 and C = 4.


plot(fit.svm)
# from the plot it is clear that the best model is having sigma = 2.63e-07 and C = 4

# Evaluating the model on test
evaluate_fit_test <- predict(fit.svm, mnist_test)
confusionMatrix(evaluate_fit_test,mnist_test$digit)

# Prediction    0    1    2    3    4    5    6    7    8    9
#          0  970    0   10    1    1    5    7    0    3    5
#          1    0 1124    0    0    0    0    2   10    0    7
#          2    1    3  999    5    5    0    1   15    4    2
#          3    0    1    2  983    0   16    0    3   11    8
#          4    0    1    1    0  958    1    6    6    6   20
#          5    3    1    0    5    0  854    9    0   10    6
#          6    4    3    3    0    2    5  930    0    5    0
#          7    1    0    6    6    0    1    0  979    4    6
#          8    1    2   11    8    1    8    3    1  928    6
#          9    0    0    1    2   15    2    0   14    3  949

# Overall Statistics

# Accuracy : 0.9674  
# 95% CI : (0.9637, 0.9708)
# No Information Rate : 0.1135
# P-Value [Acc > NIR] : < 2.2e-16
# Kappa : 0.9638

# We observe better performance with 96.74% accuracy as compared to linear model

########################### Final Model #########################################

# Non-linearity is available to a very small extent as sigma is of 1e-7 order. 
# However, accuracy improves from 91.82% to 96.74% with this non-linearity.
# Overfitting may or may not be the case. Cannot conclude
# With non linearity, Specificity & sensitivity across different levels is also good.
# after performing cross validation for rbfdot kernel, 
# the final selected model with maxmum accuracy is with the following hyperparameter:
# sigma = 2.63e-07
# C = 4


############################################################################################
#                                          End
############################################################################################


