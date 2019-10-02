library(kernlab)
library(caret)

# Load the data
team_data <- read.csv("Team Data (1974-2018) PCs.csv", header = T, stringsAsFactors = F)

set.seed(2019)

# Take 70% selection for training

training_data_split <- sample(nrow(team_data), size = floor(nrow(team_data) * 0.7))
train_data <- team_data[training_data_split, ] # Training data set

# Use remaining 30% for testing

test_data <- team_data[-training_data_split, ] # Testing data set


# Create vector to store teams in training dataset who won the World Series
train_ws_winners <- vector() 

# Loop through training data and separate list of WS winners
count <- 1
for (i in 1:nrow(train_data)) {
  if (train_data[i,5] == 1) {
    train_ws_winners[count] = train_data[i,1] 
    count = count + 1
  } else {
    next
  }
}

# Check that team names were entered correctly
train_ws_winners


# ctrl
ctrl <- trainControl(method = "cv", savePred=T)

# SVM with rbf kernel 
rbf_ws_model <- train(x, y,
                      method = 'svmRadial',
                      trControl = ctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)

# Best tuning parameter for sigma and C (0.420638 & 0.25)
rbf_ws_model$bestTune


# Check predictions against training data 
pred_rbf <- predict(rbf_ws_model, train_data[,1:3]) 

# Percent correctly classified 
sum(pred_rbf == train_data[,4]) / nrow(train_data) 


# Create vector to store predicted World Series winners
rbf_pred_winners <- vector()

# Loop through predictions and separate list of predicted WS winners
count <- 1
for (i in 1:length(pred_rbf)) {
  if (pred_rbf[i] == 1) {
    rbf_pred_winners[count] = train_data[i,1]
    count = count + 1
  } else {
    next
  }
}

# Take intersection of winners classified by training model and actual winners

correctly_classified_winners <- intersect(train_ws_winners, rbf_pred_winners)

# Compute percentage of actual WS winners that were identified by training model

rbf_pred_percent <- length(correctly_classified_winners) / length(train_ws_winners) * 100


# Plots 
ggplot(rbf_ws_model)

plot(rbf_ws_model$trainingData)

ws_data$WS.Win <- factor(ws_data$WS.Win)

ggplot(ws_data, aes(x = V1, y = V2)) + 
  geom_point(aes(col = WS.Win), size = 2.75) + 
  scale_color_manual(values = c("#FF0000", "#0066CC")) 