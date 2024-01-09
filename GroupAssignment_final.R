library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(tidyverse)
library(modeldata)
?geom_boxplot

sys_data <- read.csv("online_edu_sys_review_clean.csv")


#UNIVARIATE ANALYSIS
#Numerical Variable
#Insight with Histogram
hist(sys_data$age_years, col = "skyblue", border = "black", main = "Histogram of Age Distribution", xlab = "Age", ylab = "Frequency")
hist(sys_data$study_time, col = "skyblue", border = "black", main = "Study Time Distribution", xlab = "Study Time (Hours)", ylab = "Frequency")
hist(sys_data$social_media_time, col = "skyblue", border = "black", main = "Social Media Time Distribution", xlab = "Social Media Time(Hours)", ylab = "Frequency")
hist(sys_data$internet_facility, col = "skyblue", border = "black", main = "Internet Facility Distribution", xlab = "Internet facility", ylab = "Frequency")


#Categorical Variable
#Insight with Barplot

barplot(table(sys_data$economic_status),
        main = "Economic Status Distribution",
        xlab = "Economic Status",
        ylab = "Frequency",
        col = "skyblue",
        border = "black"
)
barplot(table(sys_data$home_location),
        main = "Home Location Distribution",
        xlab = "Home Location",
        ylab = "Frequency",
        col = "skyblue",
        border = "black"
)
barplot(table(sys_data$devices_used),
        main = "Used Device Distribution",
        xlab = "Devices Used",
        ylab = "Frequency",
        col = "skyblue",
        border = "black"
)
barplot(table(sys_data$sport_engagement),
        main = "Sports Engagement Distribution",
        xlab = "Sports Engagement",
        ylab = "Frequency",
        col = "skyblue",
        border = "black"
)

#MULTIVARIANTE ANALYSIS
#MULTIVARIANTE ANALYSIS WITH 2 VARIABLES
#INSIGHT WITH BOXPLOT
#Comparing the Distribution of Study Time Across Different Levels of Education
ggplot(sys_data, aes(x = level_of_education, y = study_time, fill = level_of_education)) +
  geom_boxplot() +
  labs(title = "Study Time by Level of Education",
       x = "Level of Education",
       y = "Study Time") +
  theme_minimal()
#Comparing Average Mark Distribution across Social Media Time
ggplot(sys_data, aes(x = average_mark_score, y = social_media_time, fill = average_mark_score)) +
  geom_boxplot() +
  labs(title = "Average Mark Score by Social Media Time",
       x = "Average Mark Score",
       y = "Social Media Time") +
  theme_minimal()
#MULTIVARIATE ANALYSIS WITH 3 VARIABLES
#INSIGHT WITH BOXPLOT
#Comparing Study Time, Level of Education, and Satisfaction Level
ggplot(sys_data, aes(x = level_of_education, y = study_time, fill = satisfaction_level)) +
  geom_boxplot() +
  labs(title = "Study Time by Level of Education and Satisfaction",
       x = "Level of Education",
       y = "Study Time",
       fill = "Satisfaction Level") +
  theme_minimal()
#Comparing Average Mark Score by Age and Gaming Interest
ggplot(sys_data, aes(x = gender, y = age_years, fill = gaming_interest)) +
  geom_boxplot() +
  labs(title = "Average Mark Score by Gender and Gaming Interest",
       x = "Gender",
       y = "Average Mark Score",
       fill = "Gaming Interest") +
  theme_minimal()

# Create a table for performance level, internet facility, and separate room study
performance_internet_room <- table(sys_data$performance_level, sys_data$internet_facility, sys_data$separate_room_study)

# Convert to data frame
performance_internet_room_df <- as.data.frame.table(performance_internet_room)

# Plot grouped bar chart
ggplot(performance_internet_room_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Var3, scales = "free_x") +
  labs(x = "Performance Level", y = "Frequency", fill = "Variables") +
  ggtitle("Performance Level by Internet Facility and Separate Room Study")

# Create a table for performance level, sport engagement, and elder supervising
performance_sport_elder <- table(sys_data$performance_level, sys_data$sport_engagement, sys_data$elder_supervising)

# Convert to data frame
performance_sport_elder_df <- as.data.frame.table(performance_sport_elder)

# Plot grouped bar chart
ggplot(performance_sport_elder_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Var3, scales = "free_x") +
  labs(x = "Performance Level", y = "Frequency", fill = "Variables") +
  ggtitle("Performance Level by Sport Engagement and Elder Supervising")

performance_interest <- sys_data %>%
  select(performance_level, interested) %>%
  gather(key = "Interest_Type", value = "Interest", -performance_level) %>%
  group_by(performance_level, Interest_Type, Interest) %>%
  summarise(Frequency = n())

ggplot(performance_interest, aes(x = factor(performance_level), y = Frequency, fill = Interest, group = Interest_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Performance Level", y = "Frequency", fill = "Interest") +
  ggtitle("Performance Level by Interest in Practical, Theory, or Both")

ggplot(sys_data, aes(x = study_time, y = average_mark_score, color = gender)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Study Time", y = "Average MArk") + ggtitle("Scatter Plot of Study Time and Average Mark Score by Gender and Education") + facet_wrap(~level_of_education)
ggplot(sys_data, aes(x = sleep_time, y = average_mark_score, color = gender)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Sleep Time", y = "Average MArk") + ggtitle("Scatter Plot of Sleep Time and Average Mark Score by Gender and Education") + facet_wrap(~level_of_education)

ggplot(sys_data, aes(x = factor(study_time), y = average_mark_score, group = factor(level_of_education), color = factor(gender))) +
  geom_line() +
  facet_wrap(~level_of_education) +
  labs(x = "Study Time", y = "Average Mark Score", color = "Gender") +
  ggtitle("Trend of Average Mark Score by Study Time, Education Level, and Gender")

# Convert categorical variables to factors

sys_data <- sys_data %>%
  mutate(
    gender = as.factor(gender),
    home_location = as.factor(home_location),
    level_of_education = as.factor(level_of_education),
    devices_used = as.factor(devices_used),
    economic_status = as.factor(economic_status),
    sport_engagement = as.factor(sport_engagement),
    elder_supervising = as.factor(elder_supervising),
    gaming_interest = as.factor(gaming_interest),
    separate_room_study = as.factor(separate_room_study),
    group_study_engagement = as.factor(group_study_engagement),
    average_mark_score = as.factor(average_mark_score),
    interested = as.factor(interested),
    satisfaction_level = as.factor(satisfaction_level)
  )

# Scale numerical variables

scale_numeric <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Select numerical columns to scale/normalize

numeric_cols <- c("age_years", "number_of_subjects", "family_size", "internet_facility",
                  "study_time", "sleep_time", "social_media_time", "online_mode_interaction",
                  "performance_level")

# Apply scaling to numeric columns

sys_data[numeric_cols] <- lapply(sys_data[numeric_cols], scale_numeric)

# Remove the first column X

sys_data <- sys_data[, -1]


# Data Split

set.seed(123)
train_indices <- sample(1:nrow(sys_data), 0.7 * nrow(sys_data))
train_data <- sys_data[train_indices, ]
test_data <- sys_data[-train_indices, ]

# Define target and predictors

target_variable <- "satisfaction_level"
predictors <- setdiff(names(train_data), target_variable)

# Construct the formula

formula_rf <- as.formula(paste(target_variable, "~ ."))

# Train the random forest model

rf_model <- randomForest(formula_rf, data = train_data, ntree = 100, mtry = 5)

# Train Decision Tree

dt_model <- rpart(as.formula(paste(target_variable, "~ .")), data = train_data, method = "class")

# Evaluate models

rf_predictions <- predict(rf_model, newdata = test_data)
dt_predictions <- predict(dt_model, newdata = test_data, type = "class")

# Confusion matrix for RF

confusion_rf <- table(rf_predictions, test_data$satisfaction_level)
print("Confusion Matrix for Random Forest:")
print(confusion_rf)

# Confusion matrix for DT

confusion_dt <- table(dt_predictions, test_data$satisfaction_level)
print("Confusion Matrix for Decision Tree:")
print(confusion_dt)

# Calculate accuracy, precision, recall, and F1-score for RF

accuracy_rf <- sum(diag(confusion_rf)) / sum(confusion_rf)
precision_rf <- confusion_rf[2, 2] / sum(confusion_rf[, 2])
recall_rf <- confusion_rf[2, 2] / sum(confusion_rf[2, ])
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

# Calculate accuracy, precision, recall, and F1-score for DT

accuracy_dt <- sum(diag(confusion_dt)) / sum(confusion_dt)
precision_dt <- confusion_dt[2, 2] / sum(confusion_dt[, 2])
recall_dt <- confusion_dt[2, 2] / sum(confusion_dt[2, ])
f1_score_dt <- 2 * (precision_dt * recall_dt) / (precision_dt + recall_dt)

# Print the metrics for RF and DT

print("Random Forest Metrics:")
print(paste("Accuracy:", accuracy_rf))
print(paste("Precision:", precision_rf))
print(paste("Recall:", recall_rf))
print(paste("F1-Score:", f1_score_rf))

print("Decision Tree Metrics:")
print(paste("Accuracy:", accuracy_dt))
print(paste("Precision:", precision_dt))
print(paste("Recall:", recall_dt))
print(paste("F1-Score:", f1_score_dt))


# Interpret models

importance(rf_model)

dt_importance <- dt_model$variable.importance
print(dt_importance)

# Creating a dataframe for metrics comparison

comparison_data <- data.frame(
  Model = c("Random Forest", "Decision Tree"),
  Accuracy = c(accuracy_rf, accuracy_dt),
  Precision = c(precision_rf, precision_dt),
  Recall = c(recall_rf, recall_dt),
  F1_Score = c(f1_score_rf, f1_score_dt)
)

# Reshape data using pivot_longer

comparison_data_long <- comparison_data %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# Create a grouped bar plot for metrics comparison

comparison_plot <- ggplot(comparison_data_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance: RF vs. DT", y = "Metric Value", fill = "Metric") +
  theme_minimal() +
  facet_wrap(~Metric, scales = "free_y")

print(comparison_plot)


# Assuming 'rf_model' is your trained Random Forest model object
saveRDS(rf_model, file = "C:/Users/Rayan/Documents/GroupAssignmentPDS/GroupApp/data/rf_model.rds")
