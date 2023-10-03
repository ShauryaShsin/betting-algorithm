###########################################
# Author: Shaurya Singh
# Topic: EPL Sports Betting Algorithm
# Course: Empirical Economics
# Term: 2022 Winter
###########################################

# Relevant Analysis Libraries

set.seed(1)

library(caret)
library(glmnet)
library(forecast)
library(readxl)
library(tidyverse)

# Model 1: LPM Few Variables (LPM1) ---------------------------------------------------------------

data_m1 <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
data_m1$result <- ifelse(data_m1$home_team_goal_count > data_m1$away_team_goal_count, 1, 0)

# View(data_m1)


## Step 1.1: Initial Check --------------------------------------------------------------------------


head(data_m1)
dim(data_m1)
summary(data_m1)
n <- dim(data_m1)[1]

plot(dist(data_m1$result, method = "binary"))
heatmap(cor(data_m1), Rowv = NA, Colv = NA, scale = "none")

#
## Step 1.2: Add/Remove Columns ---------------------------------------------------------------------
#

# Removing redundant columns
data_m1 <- data_m1[,-c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                 33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,55,56,57,58,59,
                 60,61,62,63,66,67,68,69,70,73)]

# Adding interaction & Higher Power Terms

spi1_expend <- as.vector.data.frame(data_m1$spi1 * data_m1$transfer_expenditure_h)
spi2_expend <- as.vector.data.frame(data_m1$spi2 * data_m1$transfer_expenditure_h)
expend2 <- as.vector.data.frame(data_m1$transfer_expenditure_h^2)
lattendance <- as.vector.data.frame(log(data_m1$attendance))

data_m1 <- cbind(data_m1[1:13], spi1_expend)
data_m1 <- cbind(data_m1[1:14], spi2_expend)
data_m1 <- cbind(data_m1[1:15], expend2)
data_m1 <- cbind(data_m1[1:16], lattendance)


## Step 1.3: Training Algorithm ---------------------------------------------------------------------

# Data Partitioning
train_ind <- sample(1:n, n * 0.7)

train_data_LPM1 <- data_m1[train_ind,]
test_data_LPM1 <- data_m1[-train_ind,]

n_p <- n - length(train_ind)

#
# The Following includes a slight manipulation where variables are added from the dataset,
# and partition according to the partitions set before. They are then removed before we 
# train the model, but are re-added when calculating the amount of money earned and lost
#


#### A Little Manipulation
# 
odds <- data_m1$odds_ft_home_team_win
odds_train <- train_data_LPM1$odds_ft_home_team_win
odds_test <- test_data_LPM1$odds_ft_home_team_win    

data_m1 <- data_m1[,-4]
train_data_LPM1 <- train_data_LPM1[,-4]
test_data_LPM1 <- test_data_LPM1[,-4]
# 
#### End of Manipulation

reg_LPM1 <- lm(result ~ ., data = data_m1, subset = (train_ind))
summary(reg_LPM1)

pred_LPM1 <- predict(reg_LPM1, newdata = test_data_LPM1)

naive_b_LPM1 <- rep(mean(train_data_LPM1$result), n_p)

accuracy(naive_b_LPM1, test_data_LPM1$result)
accuracy(pred_LPM1, test_data_LPM1$result)


# Classifying Results (Fitted Value => 0.5 == 1, else 0)

class_LPM1 <- as.matrix(round(pred_LPM1, digits = 0))

results_comparison_LPM1 <- data.frame(test_data_LPM1$result, class_LPM1)
names(results_comparison_LPM1) <- c("Actual", "Predicted")

# View(results_comparison_LPM1)

Home_Win <- test_data_LPM1$result # Naming the Variable so Confusion Matrix is Prettier


## Step 1.4: Analysis of Errors ---------------------------------------------------------------------

results_LPM1 <- data.frame(rbind(accuracy(naive_b_LPM1, test_data_LPM1$result), 
                                 accuracy(pred_LPM1, test_data_LPM1$result)),
                           row.names = c("NB", "LPM1"))

# View(results_LPM1)


# Confusion Matrix

C_1 <- table(Home_Win, class_LPM1)

# Sensitivity

C_1[2,2]/sum(C_1[,2]) # How many Wins Do we Predict Correctly

# Specificity

C_1[1,1] / sum(C_1[,1]) # How many Tie/Losses Do We Predict Correctly


## Step Earnings Calculations from Odds (LPM1)


test_data_LPM1 <- cbind(test_data_LPM1[1:16], odds_test)
test_data_LPM1 <- cbind(test_data_LPM1[1:17], class_LPM1)
test_data_LPM1 <- test_data_LPM1[,-c(1:11)]
test_data_LPM1 <- test_data_LPM1[,-c(2:5)]

# View(test_data_LPM1)

test_data_LPM1$winnings <- ifelse(test_data_LPM1$class_LPM1 == 1 & test_data_LPM1$result == 1, 
                                  ((100*odds_test)-100), 
                                  ifelse(test_data_LPM1$class_LPM1 == 1 & 
                                           test_data_LPM1$result == 0, -100 , 0))

test_data_LPM1$won <- ifelse(test_data_LPM1$winnings > 0, test_data_LPM1$winnings, 0)
test_data_LPM1$loss <- ifelse(test_data_LPM1$winnings < 0, test_data_LPM1$winnings, 0)

# View(test_data_LPM1)

sum(test_data_LPM1$winnings)
sum(test_data_LPM1$won)
sum(test_data_LPM1$loss)

# Model 2: LPM All Variables (LPM2) ---------------------------------------------------------------

data_m1 <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
data_m1$result <- ifelse(data_m1$home_team_goal_count > data_m1$away_team_goal_count, 1, 0)

# View(data_m1)


## Step 2.1: Initial Check --------------------------------------------------------------------------


head(data_m1)
dim(data_m1)
summary(data_m1)
n <- dim(data_m1)[1]

plot(dist(data_m1$result, method = "binary"))
heatmap(cor(data_m1), Rowv = NA, Colv = NA, scale = "none")

#
## Step 2.2: Add/Remove Columns ---------------------------------------------------------------------
#

# Removing redundant columns
data_m1 <- data_m1[,-c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                       33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,55,56,57,58,59,
                       60,61,62,63,66,67,68,69,70,73)]

# Adding interaction & Higher Power Terms

spi1_expend <- as.vector.data.frame(data_m1$spi1 * data_m1$transfer_expenditure_h)
spi2_expend <- as.vector.data.frame(data_m1$spi2 * data_m1$transfer_expenditure_h)
expend2 <- as.vector.data.frame(data_m1$transfer_expenditure_h^2)
lattendance <- as.vector.data.frame(log(data_m1$attendance))

data_m1 <- cbind(data_m1[1:13], spi1_expend)
data_m1 <- cbind(data_m1[1:14], spi2_expend)
data_m1 <- cbind(data_m1[1:15], expend2)
data_m1 <- cbind(data_m1[1:16], lattendance)


## Step 2.3: Training Algorithm ---------------------------------------------------------------------

# Data Partitioning
train_ind <- sample(1:n, n * 0.7)

train_data_LPM1 <- data_m1[train_ind,]
test_data_LPM1 <- data_m1[-train_ind,]

n_p <- n - length(train_ind)

#
# The Following includes a slight manipulation where variables are added from the dataset,
# and partition according to the partitions set before. They are then removed before we 
# train the model, but are re-added when calculating the amount of money earned and lost
#


#### A Little Manipulation
# 
odds <- data_m1$odds_ft_home_team_win
odds_train <- train_data_LPM1$odds_ft_home_team_win
odds_test <- test_data_LPM1$odds_ft_home_team_win    

data_m1 <- data_m1[,-4]
train_data_LPM1 <- train_data_LPM1[,-4]
test_data_LPM1 <- test_data_LPM1[,-4]
# 
#### End of Manipulation

reg_LPM1 <- lm(result ~ ., data = data_m1, subset = (train_ind))
summary(reg_LPM1)

pred_LPM1 <- predict(reg_LPM1, newdata = test_data_LPM1)

naive_b_LPM1 <- rep(mean(train_data_LPM1$result), n_p)

accuracy(naive_b_LPM1, test_data_LPM1$result)
accuracy(pred_LPM1, test_data_LPM1$result)


# Classifying Results (Fitted Value => 0.5 == 1, else 0)

class_LPM1 <- as.matrix(round(pred_LPM1, digits = 0))

results_comparison_LPM1 <- data.frame(test_data_LPM1$result, class_LPM1)
names(results_comparison_LPM1) <- c("Actual", "Predicted")

# View(results_comparison_LPM1)

Home_Win <- test_data_LPM1$result # Naming the Variable so Confusion Matrix is Prettier


## Step 2.4: Analysis of Errors ---------------------------------------------------------------------

results_LPM1 <- data.frame(rbind(accuracy(naive_b_LPM1, test_data_LPM1$result), 
                                 accuracy(pred_LPM1, test_data_LPM1$result)),
                           row.names = c("NB", "LPM1"))

# View(results_LPM1)


# Confusion Matrix

C_1 <- table(Home_Win, class_LPM1)

# Sensitivity

C_1[2,2]/sum(C_1[,2]) # How many Wins Do we Predict Correctly

# Specificity

C_1[1,1] / sum(C_1[,1]) # How many Tie/Losses Do We Predict Correctly


## Step Earnings Calculations from Odds (LPM1)


test_data_LPM1 <- cbind(test_data_LPM1[1:16], odds_test)
test_data_LPM1 <- cbind(test_data_LPM1[1:17], class_LPM1)
test_data_LPM1 <- test_data_LPM1[,-c(1:11)]
test_data_LPM1 <- test_data_LPM1[,-c(2:5)]

# View(test_data_LPM1)

test_data_LPM1$winnings <- ifelse(test_data_LPM1$class_LPM1 == 1 & test_data_LPM1$result == 1, 
                                  ((100*odds_test)-100), 
                                  ifelse(test_data_LPM1$class_LPM1 == 1 & 
                                           test_data_LPM1$result == 0, -100 , 0))

test_data_LPM1$won <- ifelse(test_data_LPM1$winnings > 0, test_data_LPM1$winnings, 0)
test_data_LPM1$loss <- ifelse(test_data_LPM1$winnings < 0, test_data_LPM1$winnings, 0)

# View(test_data_LPM1)

sum(test_data_LPM1$winnings)
sum(test_data_LPM1$won)
sum(test_data_LPM1$loss)

# Model 3: GLM Few Variables (GLM1) ---------------------------------------------------------------

data_m1 <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
data_m1$result <- ifelse(data_m1$home_team_goal_count > data_m1$away_team_goal_count, 1, 0)

# View(data_m1)


## Step 3.1: Initial Check --------------------------------------------------------------------------


head(data_m1)
dim(data_m1)
summary(data_m1)
n <- dim(data_m1)[1]

plot(dist(data_m1$result, method = "binary"))
heatmap(cor(data_m1), Rowv = NA, Colv = NA, scale = "none")

#
## Step 3.2: Add/Remove Columns ---------------------------------------------------------------------
#

# Removing redundant columns
data_m1 <- data_m1[,-c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                       33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,55,56,57,58,59,
                       60,61,62,63,66,67,68,69,70,73)]

# Adding interaction & Higher Power Terms

spi1_expend <- as.vector.data.frame(data_m1$spi1 * data_m1$transfer_expenditure_h)
spi2_expend <- as.vector.data.frame(data_m1$spi2 * data_m1$transfer_expenditure_h)
expend2 <- as.vector.data.frame(data_m1$transfer_expenditure_h^2)
lattendance <- as.vector.data.frame(log(data_m1$attendance))

data_m1 <- cbind(data_m1[1:13], spi1_expend)
data_m1 <- cbind(data_m1[1:14], spi2_expend)
data_m1 <- cbind(data_m1[1:15], expend2)
data_m1 <- cbind(data_m1[1:16], lattendance)


## Step 3.3: Training Algorithm ---------------------------------------------------------------------

# Data Partitioning
train_ind <- sample(1:n, n * 0.7)

train_data_LPM1 <- data_m1[train_ind,]
test_data_LPM1 <- data_m1[-train_ind,]

n_p <- n - length(train_ind)

#
# The Following includes a slight manipulation where variables are added from the dataset,
# and partition according to the partitions set before. They are then removed before we 
# train the model, but are re-added when calculating the amount of money earned and lost
#


#### A Little Manipulation
# 
odds <- data_m1$odds_ft_home_team_win
odds_train <- train_data_LPM1$odds_ft_home_team_win
odds_test <- test_data_LPM1$odds_ft_home_team_win    

data_m1 <- data_m1[,-4]
train_data_LPM1 <- train_data_LPM1[,-4]
test_data_LPM1 <- test_data_LPM1[,-4]
# 
#### End of Manipulation

reg_LPM1 <- lm(result ~ ., data = data_m1, subset = (train_ind))
summary(reg_LPM1)

pred_LPM1 <- predict(reg_LPM1, newdata = test_data_LPM1)

naive_b_LPM1 <- rep(mean(train_data_LPM1$result), n_p)

accuracy(naive_b_LPM1, test_data_LPM1$result)
accuracy(pred_LPM1, test_data_LPM1$result)


# Classifying Results (Fitted Value => 0.5 == 1, else 0)

class_LPM1 <- as.matrix(round(pred_LPM1, digits = 0))

results_comparison_LPM1 <- data.frame(test_data_LPM1$result, class_LPM1)
names(results_comparison_LPM1) <- c("Actual", "Predicted")

# View(results_comparison_LPM1)

Home_Win <- test_data_LPM1$result # Naming the Variable so Confusion Matrix is Prettier


## Step 3.4: Analysis of Errors ---------------------------------------------------------------------

results_LPM1 <- data.frame(rbind(accuracy(naive_b_LPM1, test_data_LPM1$result), 
                                 accuracy(pred_LPM1, test_data_LPM1$result)),
                           row.names = c("NB", "LPM1"))

# View(results_LPM1)


# Confusion Matrix

C_1 <- table(Home_Win, class_LPM1)

# Sensitivity

C_1[2,2]/sum(C_1[,2]) # How many Wins Do we Predict Correctly

# Specificity

C_1[1,1] / sum(C_1[,1]) # How many Tie/Losses Do We Predict Correctly


## Step Earnings Calculations from Odds (LPM1)


test_data_LPM1 <- cbind(test_data_LPM1[1:16], odds_test)
test_data_LPM1 <- cbind(test_data_LPM1[1:17], class_LPM1)
test_data_LPM1 <- test_data_LPM1[,-c(1:11)]
test_data_LPM1 <- test_data_LPM1[,-c(2:5)]

# View(test_data_LPM1)

test_data_LPM1$winnings <- ifelse(test_data_LPM1$class_LPM1 == 1 & test_data_LPM1$result == 1, 
                                  ((100*odds_test)-100), 
                                  ifelse(test_data_LPM1$class_LPM1 == 1 & 
                                           test_data_LPM1$result == 0, -100 , 0))

test_data_LPM1$won <- ifelse(test_data_LPM1$winnings > 0, test_data_LPM1$winnings, 0)
test_data_LPM1$loss <- ifelse(test_data_LPM1$winnings < 0, test_data_LPM1$winnings, 0)

# View(test_data_LPM1)

sum(test_data_LPM1$winnings)
sum(test_data_LPM1$won)
sum(test_data_LPM1$loss)




# Model 4: GLM All Variables (GLM2) ---------------------------------------------------------------

data_m1 <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
data_m1$result <- ifelse(data_m1$home_team_goal_count > data_m1$away_team_goal_count, 1, 0)

# View(data_m1)


## Step 4.1: Initial Check --------------------------------------------------------------------------


head(data_m1)
dim(data_m1)
summary(data_m1)
n <- dim(data_m1)[1]

plot(dist(data_m1$result, method = "binary"))
heatmap(cor(data_m1), Rowv = NA, Colv = NA, scale = "none")

#
## Step 4.2: Add/Remove Columns ---------------------------------------------------------------------
#

# Removing redundant columns
data_m1 <- data_m1[,-c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                       33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,55,56,57,58,59,
                       60,61,62,63,66,67,68,69,70,73)]

# Adding interaction & Higher Power Terms

spi1_expend <- as.vector.data.frame(data_m1$spi1 * data_m1$transfer_expenditure_h)
spi2_expend <- as.vector.data.frame(data_m1$spi2 * data_m1$transfer_expenditure_h)
expend2 <- as.vector.data.frame(data_m1$transfer_expenditure_h^2)
lattendance <- as.vector.data.frame(log(data_m1$attendance))

data_m1 <- cbind(data_m1[1:13], spi1_expend)
data_m1 <- cbind(data_m1[1:14], spi2_expend)
data_m1 <- cbind(data_m1[1:15], expend2)
data_m1 <- cbind(data_m1[1:16], lattendance)


## Step 4.3: Training Algorithm ---------------------------------------------------------------------

# Data Partitioning
train_ind <- sample(1:n, n * 0.7)

train_data_LPM1 <- data_m1[train_ind,]
test_data_LPM1 <- data_m1[-train_ind,]

n_p <- n - length(train_ind)

#
# The Following includes a slight manipulation where variables are added from the dataset,
# and partition according to the partitions set before. They are then removed before we 
# train the model, but are re-added when calculating the amount of money earned and lost
#


#### A Little Manipulation
# 
odds <- data_m1$odds_ft_home_team_win
odds_train <- train_data_LPM1$odds_ft_home_team_win
odds_test <- test_data_LPM1$odds_ft_home_team_win    

data_m1 <- data_m1[,-4]
train_data_LPM1 <- train_data_LPM1[,-4]
test_data_LPM1 <- test_data_LPM1[,-4]
# 
#### End of Manipulation

reg_LPM1 <- lm(result ~ ., data = data_m1, subset = (train_ind))
summary(reg_LPM1)

pred_LPM1 <- predict(reg_LPM1, newdata = test_data_LPM1)

naive_b_LPM1 <- rep(mean(train_data_LPM1$result), n_p)

accuracy(naive_b_LPM1, test_data_LPM1$result)
accuracy(pred_LPM1, test_data_LPM1$result)


# Classifying Results (Fitted Value => 0.5 == 1, else 0)

class_LPM1 <- as.matrix(round(pred_LPM1, digits = 0))

results_comparison_LPM1 <- data.frame(test_data_LPM1$result, class_LPM1)
names(results_comparison_LPM1) <- c("Actual", "Predicted")

# View(results_comparison_LPM1)

Home_Win <- test_data_LPM1$result # Naming the Variable so Confusion Matrix is Prettier


## Step 4.4: Analysis of Errors ---------------------------------------------------------------------

results_LPM1 <- data.frame(rbind(accuracy(naive_b_LPM1, test_data_LPM1$result), 
                                 accuracy(pred_LPM1, test_data_LPM1$result)),
                           row.names = c("NB", "LPM1"))

# View(results_LPM1)


# Confusion Matrix

C_1 <- table(Home_Win, class_LPM1)

# Sensitivity

C_1[2,2]/sum(C_1[,2]) # How many Wins Do we Predict Correctly

# Specificity

C_1[1,1] / sum(C_1[,1]) # How many Tie/Losses Do We Predict Correctly


## Step Earnings Calculations from Odds (LPM1)


test_data_LPM1 <- cbind(test_data_LPM1[1:16], odds_test)
test_data_LPM1 <- cbind(test_data_LPM1[1:17], class_LPM1)
test_data_LPM1 <- test_data_LPM1[,-c(1:11)]
test_data_LPM1 <- test_data_LPM1[,-c(2:5)]

# View(test_data_LPM1)

test_data_LPM1$winnings <- ifelse(test_data_LPM1$class_LPM1 == 1 & test_data_LPM1$result == 1, 
                                  ((100*odds_test)-100), 
                                  ifelse(test_data_LPM1$class_LPM1 == 1 & 
                                           test_data_LPM1$result == 0, -100 , 0))

test_data_LPM1$won <- ifelse(test_data_LPM1$winnings > 0, test_data_LPM1$winnings, 0)
test_data_LPM1$loss <- ifelse(test_data_LPM1$winnings < 0, test_data_LPM1$winnings, 0)

# View(test_data_LPM1)

sum(test_data_LPM1$winnings)
sum(test_data_LPM1$won)
sum(test_data_LPM1$loss)




