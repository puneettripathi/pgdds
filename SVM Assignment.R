# Comment next line as that will just create problem
# setwd("D:\\pgdds\\SVM")

###### Loading Neccessary libraries ######
library(kernlab)
library(readr)
library(caret)
library(e1071)

###### Loading Full MNIST Data and performing sanity checks on data ######
Data <- read_csv("mnist_train.csv", col_names=FALSE)
# Reading full test MNIST dataset
testMNIST <- read_csv("mnist_test.csv", col_names=FALSE)


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

# Creating a sample of 10% data for model creation
# We will use this data from here on to work this problem
set.seed(1)
Data2 = Data[sample(1:nrow(Data), 0.1*nrow(Data)),]

# checking distribution of digits in 10% sample
summary(Data2$digit)
# 0   1   2   3   4   5   6   7   8   9 
# 623 719 578 594 592 516 569 613 575 621 

# Checking Duplicates in Data2 
which(duplicated(Data2) | duplicated(Data2[nrow(Data2):1, ])[nrow(Data2):1])
# integer(0) -> there are no duplicates 

# Both Full dataset and 10% sample have same distribution

# Split the data into train and test set
# Creating new train data to test the individual models, we will have a 70/30 split
# we will create model on 70% of sample data and test it on 30%
train.indices = sample(1:nrow(Data2), 0.7*nrow(Data2))

# Creating Training dataset
train = Data2[train.indices, ]

# Creating test dataset
test = Data2[-train.indices, ]

#Constructing Individual Model
#Using Linear Kernel using vanilladot kernel
Model_linear <- ksvm(digit ~ ., data = train, scale = FALSE, kernel = "vanilladot")

# Evaluating the Linear SVM model on 30% test data
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1   2   3   4   5   6   7   8   9
#          0 194   0   1   0   0   2   3   0   1   1
#          1   0 224   1   0   1   1   1   1   3   2
#          2   1   0 145   5   2   1   2   1   7   0
#          3   0   1   6 153   0  10   0   1   4   1
#          4   0   1   2   0 152   3   0   3   0   8
#          5   1   0   1   7   0 125   2   2   3   2
#          6   2   0   2   0   3   0 177   0   4   0
#          7   0   0   8   2   2   0   0 166   0  12
#          8   0   2   3   4   1   3   1   0 144   0
#          9   0   1   0   2  11   2   0   7   1 152
# 
# Overall Statistics
#                                           
#                Accuracy : 0.9067          
#                  95% CI : (0.8923, 0.9197)
#     No Information Rate : 0.1272          
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                           
#                   Kappa : 0.8961          
#  Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7
# Sensitivity            0.9798   0.9782  0.85799  0.88439  0.88372  0.85034  0.95161  0.91713
# Specificity            0.9950   0.9936  0.98835  0.98586  0.98956  0.98911  0.99318  0.98518
# Pos Pred Value         0.9604   0.9573  0.88415  0.86932  0.89941  0.87413  0.94149  0.87368
# Neg Pred Value         0.9975   0.9968  0.98533  0.98768  0.98774  0.98672  0.99442  0.99068
# Prevalence             0.1100   0.1272  0.09389  0.09611  0.09556  0.08167  0.10333  0.10056
# Detection Rate         0.1078   0.1244  0.08056  0.08500  0.08444  0.06944  0.09833  0.09222
# Detection Prevalence   0.1122   0.1300  0.09111  0.09778  0.09389  0.07944  0.10444  0.10556
# Balanced Accuracy      0.9874   0.9859  0.92317  0.93513  0.93664  0.91973  0.97240  0.95115
#                      Class: 8 Class: 9
# Sensitivity           0.86228  0.85393
# Specificity           0.99143  0.98520
# Pos Pred Value        0.91139  0.86364
# Neg Pred Value        0.98599  0.98399
# Prevalence            0.09278  0.09889
# Detection Rate        0.08000  0.08444
# Detection Prevalence  0.08778  0.09778
# Balanced Accuracy     0.92685  0.91957

# Accuracy of Linear Model - 90.67%

#Using RBF Kernel
Model_RBF <- ksvm(digit ~ ., data = train, scale = FALSE, kernel = "rbfdot")

# Evaluating the RBF SVM model on 30% test data
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF, test$digit)

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1   2   3   4   5   6   7   8   9
#          0 195   0   1   0   0   0   1   0   0   0
#          1   0 224   0   0   1   0   0   0   2   3
#          2   0   0 155   1   0   0   0   1   4   0
#          3   0   0   3 163   0   7   0   0   2   1
#          4   0   1   2   0 161   1   0   0   0   2
#          5   1   3   0   4   0 138   3   1   2   1
#          6   1   0   1   0   0   0 180   0   3   0
#          7   0   0   5   2   0   0   0 176   0   6
#          8   1   1   2   1   1   1   2   0 154   1
#          9   0   0   0   2   9   0   0   3   0 164
# 
# Overall Statistics
#                                           
#                Accuracy : 0.95            
#                  95% CI : (0.9389, 0.9596)
#     No Information Rate : 0.1272          
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                           
#                   Kappa : 0.9444          
#  Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9848   0.9782  0.91716  0.94220  0.93605  0.93878   0.9677  0.97238  0.92216  0.92135
# Specificity            0.9988   0.9962  0.99632  0.99201  0.99631  0.99093   0.9969  0.99197  0.99388  0.99137
# Pos Pred Value         0.9898   0.9739  0.96273  0.92614  0.96407  0.90196   0.9730  0.93122  0.93902  0.92135
# Neg Pred Value         0.9981   0.9968  0.99146  0.99384  0.99326  0.99454   0.9963  0.99690  0.99205  0.99137
# Prevalence             0.1100   0.1272  0.09389  0.09611  0.09556  0.08167   0.1033  0.10056  0.09278  0.09889
# Detection Rate         0.1083   0.1244  0.08611  0.09056  0.08944  0.07667   0.1000  0.09778  0.08556  0.09111
# Detection Prevalence   0.1094   0.1278  0.08944  0.09778  0.09278  0.08500   0.1028  0.10500  0.09111  0.09889
# Balanced Accuracy      0.9918   0.9872  0.95674  0.96710  0.96618  0.96485   0.9823  0.98217  0.95802  0.95636

# Accuracy of Overall Model - 95% 

# Though RBF is performing better but it is -
# 1. A complex model 
# 2. And an accuracy of 95% in training means high bias and is prone to overfitting
# which means there is a good chance it might be a bad choice for data that it has not seen yet.
# We will go with a Simpler model, Linear SVM Model
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
set.seed(7)

#train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

## Trying linear model
grid <- expand.grid(C = c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 5))

# Trying SVM Linear model here with C from 1 to 5
# Performing 5-fold cross validation
fit.svm <- train(digit ~ ., data=Data2, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# Checking the accuracy and other coeffs of the model
print(fit.svm)
# Accuracy is coming out to be 90.48% on 5-Fold Cross Validation 
# And the accuracy is same on all the values of C

# Support Vector Machines with Linear Kernel 
# 
# 6000 samples
#  784 predictor
#   10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 4799, 4800, 4800, 4801, 4800 
# Resampling results across tuning parameters:
# 
#   C  Accuracy   Kappa    
#   1  0.9048318  0.8941608
#   2  0.9048318  0.8941608
#   3  0.9048318  0.8941608
#   4  0.9048318  0.8941608
#   5  0.9048318  0.8941608
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 1.

# Plotting model results
plot(fit.svm)

# Evaluating the model on the MNIST test dataset 
evaluate_linear_test<- predict(fit.svm, testMNIST)

# Checking the Confusion Matrix to see the accuracy of our SVM model
confusionMatrix(evaluate_linear_test, testMNIST$X1)
# Over All Accuracy of of Linear Model - 91.39%
# This is better than the training data where the accuracy was 90.2%
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    0    1    2    3    4    5    6    7    8    9
#          0  962    0   14    3    1   12   11    2    9    8
#          1    0 1122   10   10    4    9    4   16   13    6
#          2    0    1  939   37    6    7   11   20   17    1
#          3    2    2   15  892    1   50    1   15   35    8
#          4    0    0   14    3  914   13   10   10    7   53
#          5    6    1    2   32    1  758   10    0   28   14
#          6    7    3    6    1    4   13  908    0   12    0
#          7    3    1   12    6    5    2    1  928    8   32
#          8    0    5   19   19    3   20    2    4  838    9
#          9    0    0    1    7   43    8    0   33    7  878
# 
# Overall Statistics
#                                           
#                Accuracy : 0.9139          
#                  95% CI : (0.9082, 0.9193)
#     No Information Rate : 0.1135          
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                           
#                   Kappa : 0.9043          
#  Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9816   0.9885   0.9099   0.8832   0.9308   0.8498   0.9478   0.9027   0.8604   0.8702
# Specificity            0.9933   0.9919   0.9888   0.9857   0.9878   0.9897   0.9949   0.9922   0.9910   0.9890
# Pos Pred Value         0.9413   0.9397   0.9038   0.8737   0.8926   0.8897   0.9518   0.9299   0.9119   0.8987
# Neg Pred Value         0.9980   0.9985   0.9896   0.9869   0.9924   0.9854   0.9945   0.9889   0.9850   0.9855
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0962   0.1122   0.0939   0.0892   0.0914   0.0758   0.0908   0.0928   0.0838   0.0878
# Detection Prevalence   0.1022   0.1194   0.1039   0.1021   0.1024   0.0852   0.0954   0.0998   0.0919   0.0977
# Balanced Accuracy      0.9875   0.9902   0.9494   0.9344   0.9593   0.9197   0.9714   0.9475   0.9257   0.9296

# We will stick with the linear model as this is a performing really very well on the test data
# Better than it did while in training phase of the model
# the accuracy is increasing as we move from training data to test data
