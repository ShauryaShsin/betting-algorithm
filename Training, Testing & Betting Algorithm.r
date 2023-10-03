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

# Model 1: LPM Many Variables (LPM1) ---------------------------------------------------------------

data_m1 <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
data_m1$result <- ifelse(data_m1$home_team_goal_count > data_m1$away_team_goal_count, 1, 0)

# View(data_m1)


## Step 1.1: Initial Check --------------------------------------------------------------------------


head(data_m1)
dim(data_m1)
summary(data_m1)
n <- dim(data_m1)[1]

## Step 1.2: Add/Remove Columns ---------------------------------------------------------------------
#

# Removing redundant columns
data_m1 <- data_m1[,-c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                 33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,55,56,57,58,59,
                 60,61,62,63,66,67,68,69,70,73)]

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

# class_LPM1 <- as.matrix(round(pred_LPM1, digits = 0))
class_LPM1 <- as.matrix(ifelse(pred_LPM1 > 0.8, 1, 0))

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



# Model 2: LPM Few Variables (LPM2) ---------------------------------------------------------------

data_m2 <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
data_m2$result <- ifelse(data_m2$home_team_goal_count > data_m2$away_team_goal_count, 1, 0)

# View(data_m2)


## Step 2.1: Initial Check --------------------------------------------------------------------------


head(data_m2)
dim(data_m2)
summary(data_m2)
n <- dim(data_m2)[1]

## Step 2.2: Add/Remove Columns ---------------------------------------------------------------------

# Removing redundant columns
data_m2 <- data_m2[,-c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                       33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,55,56,57,58,59,
                       60,61,62,63,66,67,68,69,70,73)]

data_m2 <- data_m2[,-c(1,5:12)]


## Step 2.3: Training Algorithm ---------------------------------------------------------------------

# Data Partitioning
train_ind <- sample(1:n, n * 0.7)

train_data_LPM2 <- data_m2[train_ind,]
test_data_LPM2 <- data_m2[-train_ind,]

n_p <- n - length(train_ind)

#
# The Following includes a slight manipulation where variables are added from the dataset,
# and partition according to the partitions set before. They are then removed before we 
# train the model, but are re-added when calculating the amount of money earned and lost
#


#### A Little Manipulation
# 
odds <- data_m2$odds_ft_home_team_win
odds_train <- train_data_LPM2$odds_ft_home_team_win
odds_test <- test_data_LPM2$odds_ft_home_team_win    

data_m2 <- data_m2[,-3]
train_data_LPM2 <- train_data_LPM2[,-3]
test_data_LPM2 <- test_data_LPM2[,-3]
 
#### End of Manipulation

reg_LPM2 <- lm(result ~ ., data = data_m2, subset = (train_ind))
summary(reg_LPM2)

pred_LPM2 <- predict(reg_LPM2, newdata = test_data_LPM2)

naive_b_LPM2 <- rep(mean(train_data_LPM2$result), n_p)

accuracy(naive_b_LPM2, test_data_LPM2$result)
accuracy(pred_LPM2, test_data_LPM2$result)


# Classifying Results (Fitted Value => 0.5 == 1, else 0)

class_LPM2 <- as.matrix(ifelse(pred_LPM2 > 0.8, 1, 0))

results_comparison_LPM2 <- data.frame(test_data_LPM2$result, class_LPM2)
names(results_comparison_LPM2) <- c("Actual", "Predicted")

# View(results_comparison_LPM1)

Home_Win <- test_data_LPM2$result # Naming the Variable so Confusion Matrix is Prettier


## Step 2.4: Analysis of Errors ---------------------------------------------------------------------

results_LPM2 <- data.frame(rbind(accuracy(naive_b_LPM2, test_data_LPM2$result), 
                                 accuracy(pred_LPM2, test_data_LPM2$result)),
                           row.names = c("NB", "LPM2"))

# View(results_LPM2)


# Confusion Matrix

C_2 <- table(Home_Win, class_LPM2)

# Sensitivity

C_2[2,2]/sum(C_2[,2]) # How many Wins Do we Predict Correctly

# Specificity

C_2[1,1] / sum(C_2[,1]) # How many Tie/Losses Do We Predict Correctly


## Step Earnings Calculations from Odds (LPM1)


test_data_LPM2 <- cbind(test_data_LPM2[1:3], odds_test)
test_data_LPM2 <- cbind(test_data_LPM2[1:4], class_LPM2)
test_data_LPM2 <- test_data_LPM2[,-c(1:2)]

# View(test_data_LPM2)

test_data_LPM2$winnings <- ifelse(test_data_LPM2$class_LPM2 == 1 & test_data_LPM2$result == 1, 
                                  ((100*odds_test)-100), 
                                  ifelse(test_data_LPM2$class_LPM2 == 1 & 
                                           test_data_LPM2$result == 0, -100 , 0))

test_data_LPM2$won <- ifelse(test_data_LPM2$winnings > 0, test_data_LPM2$winnings, 0)
test_data_LPM2$loss <- ifelse(test_data_LPM2$winnings < 0, test_data_LPM2$winnings, 0)

# View(test_data_LPM1)

sum(test_data_LPM2$winnings)
sum(test_data_LPM2$won)
sum(test_data_LPM2$loss)




# Model 3: GLM Many Variables (GLM) ---------------------------------------------------------------

data_m3 <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
data_m3$result <- ifelse(data_m3$home_team_goal_count > data_m3$away_team_goal_count, 1, 0)

# View(data_m3)


## Step 1.1: Initial Check --------------------------------------------------------------------------


head(data_m3)
dim(data_m3)
summary(data_m3)
n <- dim(data_m3)[1]

## Step 1.2: Add/Remove Columns ---------------------------------------------------------------------
#

# Removing redundant columns
data_m3 <- data_m3[,-c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                       33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,55,56,57,58,59,
                       60,61,62,63,66,67,68,69,70,73)]

spi1_expend <- as.vector.data.frame(data_m3$spi1 * data_m3$transfer_expenditure_h)
spi2_expend <- as.vector.data.frame(data_m3$spi2 * data_m3$transfer_expenditure_h)
expend2 <- as.vector.data.frame(data_m3$transfer_expenditure_h^2)
lattendance <- as.vector.data.frame(log(data_m3$attendance))

data_m3 <- cbind(data_m3[1:13], spi1_expend)
data_m3 <- cbind(data_m3[1:14], spi2_expend)
data_m3 <- cbind(data_m3[1:15], expend2)
data_m3 <- cbind(data_m3[1:16], lattendance)

## Step 1.3: Training Algorithm ---------------------------------------------------------------------

# Data Partitioning
train_ind <- sample(1:n, n * 0.7)

train_data_GLM1 <- data_m3[train_ind,]
test_data_GLM1 <- data_m3[-train_ind,]

n_p <- n - length(train_ind)

#
# The Following includes a slight manipulation where variables are added from the dataset,
# and partition according to the partitions set before. They are then removed before we 
# train the model, but are re-added when calculating the amount of money earned and lost
#


#### A Little Manipulation
# 
odds <- data_m3$odds_ft_home_team_win
odds_train <- train_data_GLM1$odds_ft_home_team_win
odds_test <- test_data_GLM1$odds_ft_home_team_win    

data_m3 <- data_m3[,-4]
train_data_LPM1 <- train_data_GLM1[,-4]
test_data_LPM1 <- test_data_GLM1[,-4]
# 
#### End of Manipulation

reg_GLM1 <- glm(result ~ ., data = data_m3, subset = train_ind, family = binomial)
summary(reg_GLM1)

pred_GLM1 <- predict(reg_GLM1, newdata = test_data_GLM1)

naive_b_GLM1 <- rep(mean(train_data_GLM1$result), n_p)

accuracy(naive_b_GLM1, test_data_GLM1$result)
accuracy(pred_GLM1, test_data_GLM1$result)


# Classifying Results (Fitted Value => 0.5 == 1, else 0)

class_GLM1 <- as.matrix(ifelse(pred_GLM1 > 0.8, 1, 0))

results_comparison_GLM1 <- data.frame(test_data_GLM1$result, class_GLM1)
names(results_comparison_GLM1) <- c("Actual", "Predicted")

# View(results_comparison_LPM1)

Home_Win <- test_data_GLM1$result # Naming the Variable so Confusion Matrix is Prettier


## Step 1.4: Analysis of Errors ---------------------------------------------------------------------

results_GLM1 <- data.frame(rbind(accuracy(naive_b_GLM1, test_data_GLM1$result), 
                                 accuracy(pred_GLM1, test_data_GLM1$result)),
                           row.names = c("NB", "GLM1"))

# View(results_GLM1)


# Confusion Matrix

C_3 <- table(Home_Win, class_GLM1)

# Sensitivity

C_3[2,2]/sum(C_3[,2]) # How many Wins Do we Predict Correctly

# Specificity

C_3[1,1] / sum(C_3[,1]) # How many Tie/Losses Do We Predict Correctly


## Step Earnings Calculations from Odds (LPM1)


test_data_GLM1 <- cbind(test_data_GLM1[1:16], odds_test)
test_data_GLM1 <- cbind(test_data_GLM1[1:17], class_GLM1)
test_data_GLM1 <- test_data_GLM1[,-c(1:11)]
test_data_GLM1 <- test_data_GLM1[,-c(2:5)]

# View(test_data_LPM1)

test_data_GLM1$winnings <- ifelse(test_data_GLM1$class_GLM1 == 1 & test_data_LPM1$result == 1, 
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




