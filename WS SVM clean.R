library(kernlab)
library(e1071)
library(ggplot2)
library(tidyverse)
library(caret)
library(ROSE)
theme_set(theme_minimal())

# Load the data
team_data <- read.csv("Team Data (1974-2018) PCs.csv", header = T, stringsAsFactors = F)

# Change WS.Win. to factor
team_data[,5] <- as.factor(team_data[,5])

# Change levels from 0 and 1 to 'Loss' and 'Win'
levels(team_data[,5]) <- c("Loss", "Win")

# Set random number generator seed so that the results are reproducible
set.seed(1)


#### ---------------- Split data into training and test sets ----------------


# Take 70% selection for training
training_data_split <- sample(nrow(team_data), size = floor(nrow(team_data) * 0.7))
train_data <- team_data[training_data_split, ] # Training data set

# Use remaining 30% for testing
test_data <- team_data[-training_data_split, ] # Testing data set

# Create set of teams in training set who won the World Series
train_WSwinners <- vector()
count <- 0
for (i in 1:nrow(train_data)) {
  if (train_data[i,5] == 'Win') {
    train_WSwinners[count] = train_data[i,1]
    count = count + 1
  } else {
    next
  }
}

# Create x and y training and testing variables
x_train <- as.matrix(train_data[,2:4])
y_train <- as.factor(train_data[,5])

x_test <- as.matrix(test_data[,2:4])
y_test <- as.factor(test_data[,5])


### -------------------- RBF Model w/ caret  ---------------------------

# Set up control model for cross validation
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10,
                     repeats = 5, 
                     summaryFunction = twoClassSummary, 
                     classProbs = T, 
                     savePred=T)


# Tune model (First Pass w/ tunelength)
rbf_ws_model <- train(x_train, y_train,
                      method = 'svmRadialWeights',
                      metric = "ROC",
                      trControl = ctrl,
                      preProcess = c("center", "scale"),
                      sigma = 10,
                      tuneLength = 3)

# best tuning parameter for sigma/C/weight = 0.4596594/128/1
# 0.4973595/0.25/9
rbf_ws_model$bestTune

# Create grid of parameters to test
param_grid <- expand.grid(sigma = c(.01, .02, .03, .04, .05),
                          Weight = 3:10,
                          C = 2^c(-2:3))

# Tune model using tuneGrid
rbf_ws_model <- train(x_train, y_train,
                      method = 'svmRadialWeights',
                      metric = "ROC",
                      trControl = ctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = param_grid)
rbf_ws_model$bestTune
# predicting the test data 
pred_rbf <- predict(rbf_ws_model, x_test) 
pred_rbf

# percent correctly classified 
sum(pred_rbf == test_data[,5]) / nrow(test_data) 

# Confusion matrix
confusionMatrix(pred_rbf, test_data[,5], positive = "Win")
t1 <- table(pred_rbf, test_data[,5]) # Just the contingency table
spec <- t1[2,2]/(t1[2,1]+t1[2,2]) # Specificity - Correctly classified Wins/Total actual Wins
sens <- t1[1,1]/(t1[1,1]+t1[1,2]) # Sensitivity - Correctly classified Losses/Total actual Losses
ppv <- t1[1,1]/(t1[1,1]+t1[2,1]) # Positive Predictive Value - Correctly classified Losses/Total predicted Losses
npv <- t1[2,2]/(t1[1,2]+t1[2,2]) # Negative Predictive value - Correctly classified Wins/Total predicted Wins
# We want to favor the model that give the best balance of spec and npv
# In the above code, positive -> Loss and negative -> Win (despite the added parameter to the confusion matrix)








