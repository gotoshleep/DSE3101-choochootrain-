library(tidyverse)       # for data manipulation
library(xgboost)
library(Matrix)          # to construct matrices for XGBoost and Random Forest
library(readxl)
library(caret)           # to split data into test and training via random sampling
library(randomForest)
library(broom)           # used for tidy() function
library(e1071)           # for naiveBayes()


########################
## Prepare model data ##
########################
# load in stations masterlist
stations <- read.csv("raw_data/stations.csv") %>% 
  group_by(stations) %>%
  mutate(n_lines = n()) %>%
  mutate(stations, cost_per_km = ifelse(line_code == "CGL", 0.2391608, cost_per_km)) 
# Set Changi Airport Line cost_per_km to be equal to East West Line (remove later)

vul_data <- read.csv("raw_data/vul_data_dynamic.csv")

model_data <- vul_data %>%
  rename(n_riders = AVG_RIDERS, id = X.1) %>%
  mutate(cost_per_km = cost / length) %>%
  select(-X, -join_station, -YEAR_MONTH) %>%
  
  mutate(n_lines = ifelse(is_interchange, 2, 1),
         n_lines = ifelse(stations %in% c("Dhoby Ghaut", "Marina Bay", "Outram Park"), 3, n_lines))

# Check for any incomplete rows and filter them out
model_data %>% filter(!complete.cases(model_data))
model_data <- model_data %>% filter(complete.cases(model_data))

###############################################
## Prepare sample dataset to fit predictions ##
###############################################

rainfall_buckets <- model_data %>%
  group_by(stations, station_code) %>%
  summarise(fair = min(rain_fall.mm.),
            moderate = median(rain_fall.mm.[rain_fall.mm. > 0], na.rm = TRUE),
            heavy = max(rain_fall.mm.), 
            .groups = "drop") %>% 
  mutate(moderate = ifelse(is.na(moderate), 0, moderate)) %>%
  pivot_longer(c(fair, moderate, heavy), names_to = "weather_condition", values_to = "rain_fall.mm.")

mean_5day_buckets <- model_data %>%
  group_by(stations, station_code) %>%
  summarise(fair = min(mean_last_5_days),
            moderate = median(mean_last_5_days[mean_last_5_days > 0], na.rm = TRUE),
            heavy = max(mean_last_5_days), 
            .groups = "drop") %>% 
  mutate(moderate = ifelse(is.na(moderate), 0, moderate)) %>%
  pivot_longer(c(fair, moderate, heavy), names_to = "mean_weather_condition", values_to = "mean_last_5_days")

ridership <- read.csv("raw_data/ridership_by_stations_6months.csv") %>%
  group_by(station_code, day_type, is_peak) %>%
  mutate(n_riders = mean(AVG_RIDERS)) %>% # taking average across 6 months
  mutate(day_type = ifelse(day_type == "WEEKENDS/HOLIDAY", "WEEKEND/HOLIDAY", day_type)) %>%
  select(station_code, stations, day_type, is_peak, n_riders) %>%
  distinct_all()

fit_data <- stations %>%
  #left_join(rainfall_buckets) %>%
  left_join(mean_5day_buckets) %>%
  left_join(ridership) %>%
  mutate(date = Sys.Date(),
         line_age = as.numeric(difftime(date, commencement, units = "days")) %/% 365.25,
         cost_per_km = cost / length,
         yr = year(date)) %>%
  rename(id = X) %>%
  na.omit()

###############################
## Exploratory Data Analysis ##
###############################

# 1. Checking for correlation between ridership volume and rainfall
df_wday_peak <- model_data %>% filter(day_type == "WEEKDAY" & is_peak == 1)
df_wday_npeak <- model_data %>% filter(day_type == "WEEKDAY" & is_peak == 0)
df_wend_peak <- model_data %>% filter(day_type != "WEEKDAY" & is_peak == 1)
df_wend_npeak <- model_data %>% filter(day_type != "WEEKDAY" & is_peak == 0)

x_rr_wday_peak <- model.matrix(~ n_riders + rain_fall.mm., 
                               data = df_wday_peak)[, -1]
x_rr_wday_npeak <- model.matrix(~ n_riders + rain_fall.mm., 
                                data = df_wday_npeak)[, -1]
x_rr_wend_peak <- model.matrix(~ n_riders + rain_fall.mm., 
                               data = df_wend_peak)[, -1]
x_rr_wend_npeak <- model.matrix(~ n_riders + rain_fall.mm., 
                                data = df_wend_npeak)[, -1]

pairs(x_rr_wday_peak, main = "Weekday Peak Hours")
pairs(x_rr_wday_npeak, main = "Weekday Off-Peak Hours")
pairs(x_rr_wend_peak, main = "Weekend/Holiday Peak Hours")
pairs(x_rr_wend_npeak, main = "Weekend/Holiday Odd-Peak Hours")

cor(x_rr_wday_peak, method = c("pearson", "kendall", "spearman"))
cor(x_rr_wday_npeak, method = c("pearson", "kendall", "spearman"))
cor(x_rr_wend_peak, method = c("pearson", "kendall", "spearman"))
cor(x_rr_wend_npeak, method = c("pearson", "kendall", "spearman"))

x_m5_wday_peak <- model.matrix(~ n_riders + mean_last_5_days, 
                               data = df_wday_peak)[, -1]
x_m5_wday_npeak <- model.matrix(~ n_riders + mean_last_5_days, 
                                data = df_wday_npeak)[, -1]
x_m5_wend_peak <- model.matrix(~ n_riders + mean_last_5_days, 
                               data = df_wend_peak)[, -1]
x_m5_wend_npeak <- model.matrix(~ n_riders + mean_last_5_days, 
                                data = df_wend_npeak)[, -1]

pairs(x_m5_wday_peak, main = "Weekday Peak Hours (Avg rainfall across last 5 days)")
pairs(x_m5_wday_npeak, main = "Weekday Off-Peak Hours (Avg rainfall across last 5 days)")
pairs(x_m5_wend_peak, main = "Weekend/Holiday Peak Hours (Avg rainfall across last 5 days)")
pairs(x_m5_wend_npeak, main = "Weekend/Holiday Odd-Peak Hours (Avg rainfall across last 5 days)")

cor(x_m5_wday_peak, method = c("pearson", "kendall", "spearman"))
cor(x_m5_wday_npeak, method = c("pearson", "kendall", "spearman"))
cor(x_m5_wend_peak, method = c("pearson", "kendall", "spearman"))
cor(x_m5_wend_npeak, method = c("pearson", "kendall", "spearman"))

# 2. Checking for correlation between numeric variables
x_numeric <- model.matrix(~ is_interchange + is_above_ground + n_stations + n_lines + cost_per_km + line_age + 
                            n_riders + rain_fall.mm. + mean_last_5_days + is_peak,
                          data = model_data)[, -1]

abs(cor(x_numeric, method = c("pearson", "kendall", "spearman"))) > 0.3
cor(x_numeric, method = c("pearson", "kendall", "spearman"))

## Uncomment to view pairs plot
#pairs(x_numeric)

# 3. Observe breakdown distribution in data
table(model_data$breakdown)

#################################
## Create train and test split ##
#################################

set.seed(1000)
# Stratified sampling: 80% train, 20% test
test_df <- model_data %>% group_by(breakdown) %>% sample_frac(0.2)
train_df <- subset(model_data, !(id %in% test_df$id))

train_df %>% filter(breakdown > 0) %>% count()
train_df %>% filter(breakdown == 0) %>% count() # 0.550 %
test_df %>% filter(breakdown > 0) %>% count()
test_df %>% filter(breakdown == 0) %>% count() # 0.552 %

variables <- ~ line_code + 
  is_interchange + 
  is_above_ground +
  #is_above_ground*rain_fall.mm. +
  is_above_ground*mean_last_5_days +
  line_age + 
  operator + 
  n_stations + 
  cost_per_km + 
  n_riders + 
  n_lines + 
  day_type +
  is_peak +
  mean_last_5_days -1  # -1 to drop intercept, avoids collinearity.
  #rain_fall.mm. -1

x <- sparse.model.matrix(variables, data = model_data)

x_train <- sparse.model.matrix(variables, data = train_df)
x_train_mat <- as.matrix(x_train) # for linear regression and logit
y_train <- train_df$breakdown

x_test <- sparse.model.matrix(variables, data = test_df)
x_test_mat <- as.matrix(x_test) # for linear regression and logit
y_test <- test_df$breakdown

#######################################################
## Run XGBoost on training data and make predictions ##
#######################################################

xgb_model <- xgboost(data = x_train, label = y_train, 
                     nrounds = 100, objective = "reg:squarederror",
                     verbose = 0)
xgb_pred <- predict(xgb_model, x_test)

#######################
## Model Performance ##
#######################

# 1. Precision and Recall: maximise recall
xgb_pred_df <- data.frame(xgb_pred)
y_pred <- cbind(xgb_pred_df, y_test)
threshold <- y_pred %>% filter(y_test > 0) %>% min() 
# Threshold is the lowest prediction score that correctly identifies a breakdown station

TP <- y_pred %>% filter(xgb_pred >= threshold & y_test > 0) %>% count() %>% pull()
FN <- y_pred %>% filter(xgb_pred < threshold & y_test > 0) %>% count() %>% pull()
FP <- y_pred %>% filter(xgb_pred >= threshold & y_test == 0) %>% count() %>% pull()
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F1 <- 2*precision*recall / (precision + recall)

# 2. Compare RMSE with baseline (average) model
rmse_xgb <- sqrt(mean((y_test - xgb_pred)^2))
baseline_rmse <- sqrt(mean((y_test - mean(y_train))^2))

#######################
## Feature selection ##
#######################

importance <- xgb.importance(model = xgb_model)
selected_features <- importance[Gain > 0.01, Feature]
xgb.plot.importance(importance_matrix = importance, top_n = 10) # plot feature importance

######################
## Cross-validation ##
######################

xgb_cv <- xgb.cv(data = x_train,label = y_train,
                 nrounds = 100, nfold = 5, objective = "reg:squarederror",
                 verbose = 0)

best_iter <- which.min(xgb_cv$evaluation_log$test_rmse_mean)

xgb_cv$evaluation_log[best_iter, c("train_rmse_mean", "train_rmse_std", "test_rmse_mean", "test_rmse_std")]

# Plot RMSE learning curve
ggplot(xgb_cv$evaluation_log, aes(x = iter)) +
  geom_line(aes(y = train_rmse_mean, color = "Train")) +
  geom_line(aes(y = test_rmse_mean, color = "Test")) +
  labs(title = "Learning Curve", x = "Iteration", y = "RMSE") +
  theme_minimal()

###################################################
## Fit model on sample data and finalise scoring ##
###################################################

x_fit <- sparse.model.matrix(variables, data = fit_data)
fit_data$xgb_score <- predict(xgb_model, x_fit) # run prediction on new dataset

# Scale xgb_score from 0 to 1 for easier comparison
min_score = min(fit_data$xgb_score)
max_score = max(fit_data$xgb_score)
fit_data <- fit_data %>% mutate(xgb_scaled = (xgb_score - min_score) / (max_score - min_score))

# Create quantile-based scoring buckets
fit_data$vul_category <- cut(fit_data$xgb_scaled,
                             breaks = quantile(fit_data$xgb_scaled, probs = seq(0, 1, 0.2), na.rm = TRUE),
                             labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                             include.lowest = TRUE)

# Visualise results
ggplot(fit_data, aes(x = xgb_scaled, fill = vul_category)) +
  geom_histogram(bins = 30) +
  scale_fill_manual(values = c("Very Low" = "#094A25",
                               "Low" = "#4C7B00",
                               "Medium" = "#F8B324",
                               "High" = "#EB580C",
                               "Very High" = "#BC2023")) +
  labs(title = "Vulnerability Score Distribution (Dynamic)",
       x = "Scaled Vulnerability Score (0-1)",
       y = "Number of Stations") +
  theme_minimal()

#############################################
## Consolidate final results for front-end ##
#############################################

vul_scores <- fit_data %>% 
  dplyr::select(id, station_code, stations, line_code, day_type, is_peak,
                line_age, 
                n_riders, 
                #rain_fall.mm., weather_condition, 
                mean_last_5_days, mean_weather_condition,
                xgb_score, xgb_scaled, vul_category) %>%
  rename(rain_fall.mm. = mean_last_5_days, weather_condition = mean_weather_condition) %>%
  arrange(desc(xgb_score))

## Uncomment to save CSV
#write.csv(vul_scores, file = "vul_scores_dynamic.csv")

##########################
## Testing other models ##
##########################

# 1. Linear Regression
lm_model <- lm(y_train ~ x_train_mat - 1)  # - 1 removes intercept
lm_score <- predict(lm_model, newdata = as.data.frame(x_test_mat))

# Scale xgb_score from 0 to 1 for easier comparison
lm_scaled <- (lm_score - min(lm_score)) / (max(lm_score) - min(lm_score))

summary(lm_model)  # View coefficients and statistics

rmse_lm <- sqrt(mean((y_test - lm_score)^2)) # calculate RMSE

plot(lm_model)

# Feature importance #run twice to get result 
# tidy(lm_model) %>%
#   mutate(term = reorder(term, abs(estimate))) %>%
#   arrange(desc(abs(estimate))) %>%
#   head(10) %>%
#   ggplot(aes(x = abs(estimate), y = term)) +
#   geom_col() +
#   labs(title = "Feature Importance")


# 2. Logistic Regression
logit_model <- glm(y_train ~ x_train_mat - 1, family = binomial()) # - 1 removes intercept
prob <- predict(logit_model, newdata = as.data.frame(x_test_mat), type = "response")

# Scale xgb_score from 0 to 1 for easier comparison
prob_scaled <- (prob - min(prob)) / (max(prob) - min(prob))

summary(logit_model)  # View coefficients and statistics

rmse_logit <- sqrt(mean((y_test - prob)^2)) # calculate RMSE

plot(logit_model)

# Feature importance #run twice to get result 
# tidy(logit_model) %>%
#   mutate(term = reorder(term, abs(estimate))) %>%
#   arrange(desc(abs(estimate))) %>%
#   head(10) %>%
#   ggplot(aes(x = abs(estimate), y = term)) +
#   geom_col() +
#   labs(title = "Feature Importance")


# 3. Naive Bayes
y_train_factor <- as.factor(y_train)
y_test_factor <- as.factor(y_test)

nb_model <- naiveBayes(x_train_mat, y_train_factor)
nb_pred <- predict(nb_model, x_test_mat)

# Confusion matrix to observe performance (precision and recall)
confusionMatrix(nb_pred, y_test_factor)


# 4. Random Forest
x_train_dense <- as.data.frame(as.matrix(x_train))
x_test_dense <- as.data.frame(as.matrix(x_test))

rf_ctrl <- trainControl(method = "cv", 
                        number = 5,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)

rf_model <- train(x = x_train_dense,
                  y = factor(y_train, levels = c(0, 1), labels = c("Class0", "Class1")),
                  method = "rf",
                  metric = "ROC",
                  trControl = rf_ctrl,
                  tuneLength = 5)

rf_model$finalModel$votes
rf_pred <- as.data.frame(predict(rf_model, newdata = x_test, "prob"))
rf_pred[rf_pred$class1 > 0]


