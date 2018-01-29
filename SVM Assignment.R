# Comment next line as that will just create problem
setwd("C:/Users/putripat/Downloads/SVM Dataset")

###### Loading Neccessary libraries ######
library(kernlab)
library(readr)
library(caret)
library(e1071)

###### Loading Full MNIST Data and performing sanity checks on data ######
Data <- read_csv("mnist_train.csv", col_names=FALSE)
# Reading full test MNIST dataset
testMNIST <- read_csv("mnist_test.csv", col_names=FALSE)

######## Data Preparation and EDA #########
# Checking Duplicates 
which(duplicated(Data) | duplicated(Data[nrow(Data):1, ])[nrow(Data):1])
# integer(0)
# There are no duplicates in data

#Understanding Dimensions
dim(Data)

#Structure of the dataset
str(Data)
# All data is numeric & X1 is our Digit / Label Column

#printing first few rows
head(Data)

#Exploring the data
summary(Data)

#checking missing value in the dataset
sapply(Data, function(x) sum(is.na(x)))
#No columns found with NA(missing) values.

# converting target column to factor
Data$digit <- factor(Data$X1)
# checking distribution of digits
summary(Data$digit)
# 0    1    2    3    4    5    6    7    8    9 
# 5923 6742 5958 6131 5842 5421 5918 6265 5851 5949 

# removing the older label
Data <- Data[,-1]

# We will not remove any column as it is a digit recognition dataset

###### Sample Selection #######
# Creating a sample of 10% data for model creation
# We will use this data from here on to work this problem
set.seed(100)
Data2 = Data[sample(1:nrow(Data), 0.1*nrow(Data)),]

# checking distribution of digits in 10% sample
summary(Data2$digit)
# 0   1   2   3   4   5   6   7   8   9 
# 594 649 626 612 579 555 594 657 557 577 
# Both Full dataset and 10% sample have same distribution

# Checking Duplicates in Data2 
which(duplicated(Data2) | duplicated(Data2[nrow(Data2):1, ])[nrow(Data2):1])
# integer(0) -> there are no duplicates 

# Keepin  a 10% sample of test too
test <- testMNIST[sample(1:nrow(testMNIST), 0.1*nrow(testMNIST)),]

# Scaling Data

Data2[,1:784] = (as.matrix(Data2[,1:784]/255))
test[,2:ncol(test)] = (as.matrix(test[,2:ncol(test)]/255))

####### Model Creation #######
#Constructing Individual Model
#Using Linear Kernel using vanilladot kernel
Model_linear <- ksvm(digit ~ ., data = Data2, scale = FALSE, kernel = "vanilladot")

# Evaluating the Linear SVM model on 30% test data
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$X1)

# Confusion Matrix and Statistics
#           Reference
# Prediction   0   1   2   3   4   5   6   7   8   9
#          0 100   0   1   1   0   1   4   0   2   0
#          1   0 112   2   1   0   0   0   2   1   1
#          2   0   0  99   2   0   0   1   1   1   0
#          3   0   0   0  96   0   4   0   2   4   1
#          4   0   0   1   0  77   1   0   1   0   8
#          5   1   1   0   3   0  82   2   0   2   0
#          6   1   1   4   0   1   1  97   0   1   0
#          7   0   0   2   1   0   0   0  93   1   3
#          8   0   0   1   3   0   0   0   0  86   3
#          9   0   0   0   1   1   1   0   2   0  78
# 
# Overall Statistics
#                                           
#                Accuracy : 0.92            
#                  95% CI : (0.9014, 0.9361)
#     No Information Rate : 0.114           
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                           
#                   Kappa : 0.911           
#  Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9804   0.9825   0.9000   0.8889   0.9747   0.9111   0.9327   0.9208   0.8776   0.8298
# Specificity            0.9900   0.9921   0.9944   0.9877   0.9881   0.9901   0.9900   0.9922   0.9922   0.9945
# Pos Pred Value         0.9174   0.9412   0.9519   0.8972   0.8750   0.9011   0.9151   0.9300   0.9247   0.9398
# Neg Pred Value         0.9978   0.9977   0.9877   0.9866   0.9978   0.9912   0.9922   0.9911   0.9868   0.9826
# Prevalence             0.1020   0.1140   0.1100   0.1080   0.0790   0.0900   0.1040   0.1010   0.0980   0.0940
# Detection Rate         0.1000   0.1120   0.0990   0.0960   0.0770   0.0820   0.0970   0.0930   0.0860   0.0780
# Detection Prevalence   0.1090   0.1190   0.1040   0.1070   0.0880   0.0910   0.1060   0.1000   0.0930   0.0830
# Balanced Accuracy      0.9852   0.9873   0.9472   0.9383   0.9814   0.9506   0.9613   0.9565   0.9349   0.9121
# Accuracy of Linear Model - 92%

# Tuning the model

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(10)

#train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

## Trying linear model
grid <- expand.grid(C = c(0, 0.05, 0.1, 0.5, 1, 1.5, 2, 5))

# Trying SVM Linear model here with C from 1 to 5
# Performing 5-fold cross validation
fit.svm <- train(digit ~ ., data=Data2, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# Checking the accuracy and other coeffs of the model
print(fit.svm)
# Accuracy is coming out to be 92.13% on 5-Fold Cross Validation 
# And the accuracy is best at C = 0.05

# Support Vector Machines with Linear Kernel 
# 
# 6000 samples
#  784 predictor
#   10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 4800, 4801, 4800, 4798, 4801 
# Resampling results across tuning parameters:
# 
#   C     Accuracy   Kappa    
#   0.00        NaN        NaN
#   0.05  0.9213392  0.9125818
#   0.10  0.9196740  0.9107299
#   0.50  0.9108373  0.9009087
#   1.00  0.9070042  0.8966487
#   1.50  0.9061721  0.8957237
#   2.00  0.9061721  0.8957237
#   5.00  0.9061721  0.8957237
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 0.05.

# Plotting model results
plot(fit.svm)

# Evaluating the model on test 
evaluate_linear_test<- predict(fit.svm, test)

# Checking the Confusion Matrix to see the accuracy of our SVM model
confusionMatrix(evaluate_linear_test, test$X1)
# Over All Accuracy of of Linear Model - 92.9%
# This is slightly better than the training data where the accuracy was 92.13%

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1   2   3   4   5   6   7   8   9
#          0 102   0   1   1   0   1   1   0   1   0
#          1   0 112   0   0   1   0   0   2   1   1
#          2   0   0 101   4   0   0   0   1   0   0
#          3   0   0   0  98   0   4   0   3   6   2
#          4   0   0   1   0  76   1   1   1   0   5
#          5   0   1   0   2   0  81   4   0   3   0
#          6   0   1   4   0   1   1  98   0   1   0
#          7   0   0   2   1   0   0   0  92   1   2
#          8   0   0   1   1   0   1   0   0  85   0
#          9   0   0   0   1   1   1   0   2   0  84
# 
# Overall Statistics
#                                           
#                Accuracy : 0.929           
#                  95% CI : (0.9113, 0.9441)
#     No Information Rate : 0.114           
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                           
#                   Kappa : 0.921           
#  Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            1.0000   0.9825   0.9182   0.9074   0.9620   0.9000   0.9423   0.9109   0.8673   0.8936
# Specificity            0.9944   0.9944   0.9944   0.9832   0.9902   0.9890   0.9911   0.9933   0.9967   0.9945
# Pos Pred Value         0.9533   0.9573   0.9528   0.8673   0.8941   0.8901   0.9245   0.9388   0.9659   0.9438
# Neg Pred Value         1.0000   0.9977   0.9899   0.9887   0.9967   0.9901   0.9933   0.9900   0.9857   0.9890
# Prevalence             0.1020   0.1140   0.1100   0.1080   0.0790   0.0900   0.1040   0.1010   0.0980   0.0940
# Detection Rate         0.1020   0.1120   0.1010   0.0980   0.0760   0.0810   0.0980   0.0920   0.0850   0.0840
# Detection Prevalence   0.1070   0.1170   0.1060   0.1130   0.0850   0.0910   0.1060   0.0980   0.0880   0.0890
# Balanced Accuracy      0.9972   0.9884   0.9563   0.9453   0.9761   0.9445   0.9667   0.9521   0.9320   0.9440

# We will stick with the linear model as this is a performing really very well on the test data
# Better than it did while in training phase of the model
# the accuracy is increasing as we move from training data to test data




#########################################################################
#Using RBF Kernel
Model_RBF <- ksvm(digit ~ ., data = Data2, scale = FALSE, kernel = "rbfdot")

# Evaluating the RBF SVM model on test data
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF, test$X1)

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1   2   3   4   5   6   7   8   9
#          0 101   0   0   0   0   1   0   0   1   0
#          1   0 113   0   0   0   0   0   2   1   1
#          2   0   0 101   0   0   0   0   2   0   0
#          3   0   0   1 105   0   1   0   2   2   1
#          4   0   0   1   0  76   0   0   0   0   4
#          5   0   0   0   1   0  86   1   0   0   0
#          6   1   1   3   0   1   2 103   0   1   0
#          7   0   0   3   2   1   0   0  93   1   2
#          8   0   0   1   0   0   0   0   0  92   1
#          9   0   0   0   0   1   0   0   2   0  85
# 
# Overall Statistics
#                                          
#                Accuracy : 0.955          
#                  95% CI : (0.9402, 0.967)
#     No Information Rate : 0.114          
#     P-Value [Acc > NIR] : < 2.2e-16      
#                                          
#                   Kappa : 0.9499         
#  Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9902   0.9912   0.9182   0.9722   0.9620   0.9556   0.9904   0.9208   0.9388   0.9043
# Specificity            0.9978   0.9955   0.9978   0.9922   0.9946   0.9978   0.9900   0.9900   0.9978   0.9967
# Pos Pred Value         0.9806   0.9658   0.9806   0.9375   0.9383   0.9773   0.9196   0.9118   0.9787   0.9659
# Neg Pred Value         0.9989   0.9989   0.9900   0.9966   0.9967   0.9956   0.9989   0.9911   0.9934   0.9901
# Prevalence             0.1020   0.1140   0.1100   0.1080   0.0790   0.0900   0.1040   0.1010   0.0980   0.0940
# Detection Rate         0.1010   0.1130   0.1010   0.1050   0.0760   0.0860   0.1030   0.0930   0.0920   0.0850
# Detection Prevalence   0.1030   0.1170   0.1030   0.1120   0.0810   0.0880   0.1120   0.1020   0.0940   0.0880
# Balanced Accuracy      0.9940   0.9934   0.9580   0.9822   0.9783   0.9767   0.9902   0.9554   0.9683   0.9505

# Accuracy of Overall Model - 95.5% 

# Tuning Radial SVM model

set.seed(35)
# Creating new grid for CV

grid <- expand.grid(.sigma=c(10^(-1:2)), .C=c(.05,0.1,2))
#
#
#
##train function takes Target ~ Prediction, Data, Method = Algorithm
##Metric = Type of metric, tuneGrid = Grid of Parameters,
## trcontrol = Our traincontrol method.
#
fit.svm_radial <- train(digit ~ ., data=Data2, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

print(fit.svm_radial)

# Though RBF is performing better but it is -
# 1. A complex model 
# 2. And an accuracy of 95% in training means high bias and is prone to overfitting
# which means there is a good chance it might be a bad choice for data that it has not seen yet.
# We will go with a Simpler model, Linear SVM Model
