###########################################################################
# Author: Shaurya Singh
# Topic: EPL Sports Betting Algorithm
# Course: Empirical Economics
# Term: 2022 Winter
###########################################################################

library(readxl)

# Load Data ---------------------------------------------------------------

data <- read_excel("~/Desktop/EPL-2018-2019-2021-All-Data.xlsx")
View(data)

data$result <- ifelse(data$total_goal_count > data$total)


# Initial Analysis ---------------------------------------------------

dim(data)
summary(data)

dist(data$)


# Cleansing ---------------------------------------------------------------

data$stadium_name <- as.factor(data$stadium_name)




# Training Algorithm ------------------------------------------------------






# Predictive Performance & Errors -----------------------------------------






# Betting Algorithm -------------------------------------------------------


