library(kernlab)
library(ggplot2)

# Load the data

team_data <- read.csv("Team Data (1974-2018) PCs.csv", header = T, stringsAsFactors = F)

# Set random number generator seed so that our results are reproducible

set.seed(1)


# --------- Split data into training and test sets ---------


# Take 70% selection for training

training_data_split <- sample(nrow(team_data), size = floor(nrow(team_data) * 0.7))
train_set <- team_data[training_data_split, ] # Training data set

# Use remaining 30% for testing

test_set <- team_data[-training_data_split, ] # Testing data set

# Create set of teams in training set who won the World Series

train_WSwinners <- vector()
count <- 0
for (i in 1:nrow(train_set)) {
  if (train_set[i,5] == 1) {
    train_WSwinners[count] = train_set[i,1]
    count = count + 1
  } else {
    next
  }
}


# -------------------- Train SVM models --------------------


# Margins to be tested

margins <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)

# Store SVM prediction accuracy to pick the best of 10

train_pred_accuracy <- rep(0, length(margins))

train_WS_accuracy <- rep(0, length(margins))

# Create training variables x, y

x_train <- as.matrix(train_set[,2:4])
y_train <- as.factor(train_set[,5])

# Set class weights for unbalanced data

classWeights = c("0" = 1, "1" = 10) # Tried: 

# Loop through each parameter (margins)

for (i in 1:length(margins)) {

  # Fit SVM models using training set

  scaled_model <- ksvm(x_train, y_train,
                       type = "C-svc",
                       kernel = "rbfdot",
                       C = margins[i],
                       kpar = "automatic", # "uses the heuristics in 'sigest' to calculate a good sigma"
                       scaled = T,
                       class.weights = classWeights,
                       cross = 10) # 10-fold cross-validation

  # Use cross-validated (?) training model to make WS predictions, using x_train

  train_pred <- predict(scaled_model, x_train)
  
  # Store the total accuracy used of these binary predictions according to margin used
  
  train_pred_accuracy[i] <- sum(train_pred == train_set$WS.Win.) / nrow(train_set)
  #print(train_pred_accuracy[i])
  
  # Create vector to store predicted winners

  train_winners <- vector()

  # Loop through predictions and separate list of predicted WS winners

  count = 1

  for (i in 1:length(train_pred)) {

    if (train_pred[i] == 1) {
      train_winners[count] = train_set[i,1]
      # cat("Predicted World Series winner: ", train_set[i,1], "\n")
      count = count + 1

    } else {
      next
    }
  }

  # Take intersection of winners classified by training model and actual winners

  correctly_classified_winners <- intersect(train_WSwinners, train_winners)

  # Compute percentage of actual WS winners that were identified by training model

  win_pred_percent <- length(correctly_classified_winners) / length(train_WSwinners) * 100
  print(win_pred_percent)
  
  # Store percentage of winners predicted according to margin used

  train_WS_accuracy[i] <- win_pred_percent
  print(list(train_WS_accuracy))
  
}
train_pred_accuracy
win_pred_percent








# ----------------- Choose the best parameters -----------------


### Best Parameter(s): ?

# Print list of total prediction accuracy by margin 

train_pred_accuracy

### Find percentage of actual winners found by SVM
# Create vector to store predicted winners

train_winners <- vector()











# -------------------- Test SVM model --------------------

### Test Model

# Create variable x, y for testing

x_test <- as.matrix(test_set[,2:4])
y_test <- as.factor(test_set[,5])

# Testing set viz
ggplot(data = test_set, aes(x = V1, y = V2, color = y_test, shape = y_test)) + 
  geom_point(size = 1.5) +
  scale_color_manual(values=c("#A9A9A9", "#228b22")) 

# Build SVM with test set using parameters discovered from training set
SVM_test <- ksvm(y_test ~ x_test,
                  type = "C-svc", 
                  kernel = kern, # Taken from training model
                  kpar = list(sigma = sig), # Taken from training model
                  C = c, # Taken from training model
                  scaled = T,
                  class.weights = classWeights)

# View predictions on test set
test_pred <- predict(SVM_test, test_set[, 2:4])
test_pred

# See % of accurate predictions from test model
cat("Percentage of test cases found: ", sum(test_pred == test_set[,5]) / nrow(test_set) * 100)


# -------------------- Visualizations --------------------


# Training set viz
ggplot(data = train_set, aes(x = V1, y = V2, color = y_train, shape = y_train)) + 
  geom_point(size = 1.5) +
  scale_color_manual(values=c("#A9A9A9", "#228b22")) 
# Classification viz
V1 = train_set[, 2]
V2 = train_set[, 3]
V3 = train_set[, 4]
d = data.frame(PC1 = V1, PC2 = V2, PC3 = V3, WSwin = WS)
model.ksvm = ksvm(y_train ~ PC1 + PC2, 
                  data = d, 
                  type="C-svc",
                  kernel = kern, 
                  kpar = list(sigma = sig), 
                  C = c, 
                  scaled = T,
                  class.weights = classWeights,
                  cross = 3)
plot(model.ksvm, data = d)


############### OLD CODE THROUGH END ###############

# Set class weights
classWeights = c("0" = 1, "1" = 7) # Found 8.7 through trial-and-error

# Build SVM on training data with class weights
kern <- "rbfdot" 
sig <- .5
c <- .3
SVM_model <- ksvm(y_train ~ x_train,
                  type = "C-svc", 
                  kernel = kern, 
                  kpar = list(sigma = sig), 
                  C = c, 
                  scaled = T,
                  cross = 10,
                  class.weights = classWeights)


# See binary predictions made by model on training data
train_pred <- predict(SVM_model, train_set[,2:4])
train_pred # See predictions


# See who's being classified as winners
count = 0
for (i in 1:length(train_pred)) {
  if (train_pred[i] == 1) {
    cat("Predicted World Series winner: ", train_set[i,1], "\n")
    count = count + 1
  } else {
    next
  }
}

# Return number of predicted WS winners
count 

# Return percentage of winners predicted in training set
percent_winners = count / nrow(train_set)
percent_winners * 100


# See % of accurate predictions from training model
cat("Percentage of training cases found: ", sum(train_pred == train_set[,5]) / nrow(train_set) * 100, "\n")
