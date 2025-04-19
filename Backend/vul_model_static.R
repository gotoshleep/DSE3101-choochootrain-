library(tidyverse)       # for data manipulation
library(xgboost)
library(Matrix)          # to construct matrices for XGBoost and Random Forest
library(readxl)
library(caret)           # to split data into test and training via random sampling

########################
## Prepare model data ##
########################

vul_data <- read.csv("raw_data/vul_data_static.csv")

model_data_rmrr <- vul_data %>%
  mutate(id = row_number(),
         stations = coalesce(stations.x, stations.y),
         cost_per_km = cost / length) %>%
  mutate(day_type = ifelse(day_type == "WEEKENDS/HOLIDAY", "WEEKEND/HOLIDAY", day_type)) %>%
  rename(yr = year) %>%
  select(-stations.x, -stations.y) %>%
  select(-n_riders, -rain_fall.mm.) %>% # remove dynamic variables
  
  mutate(n_lines = ifelse(is_interchange, 2, 1),
         n_lines = ifelse(stations %in% c("Dhoby Ghaut", "Marina Bay", "Outram Park"), 3, n_lines))

# Check for any incomplete rows and filter them out
model_data_rmrr %>% filter(!complete.cases(model_data_rmrr))
model_data_rmrr <- model_data_rmrr %>% filter(complete.cases(model_data_rmrr))

###############################################
## Prepare sample dataset to fit predictions ##
###############################################

stations <- read.csv("raw_data/stations.csv") %>%
  group_by(stations) %>%
  mutate(n_lines = n())

fit_rmrr <- stations %>%
  expand_grid(is_peak = c(0, 1),
              day_type = c("WEEKDAY", "WEEKEND/HOLIDAY")) %>%
  mutate(date = Sys.Date(),
         line_age = as.numeric(difftime(date, commencement, units = "days")) %/% 365.25,
         cost_per_km = cost / length,
         yr = year(date),
         id = row_number()) %>%
  select(-X)

###############################
## Exploratory Data Analysis ##
###############################

# 1. Checking for correlation between numeric variables
x_numeric_rmrr <- model.matrix(~ is_interchange + is_above_ground + n_stations + n_lines + 
                                 cost_per_km + line_age + is_peak + yr,
                               data = model_data_rmrr)[, -1]

abs(cor(x_numeric_rmrr, method = c("pearson", "kendall", "spearman"))) > 0.3
cor(x_numeric_rmrr, method = c("pearson", "kendall", "spearman"))

## Uncomment to view pairs plot
#pairs(x_numeric_rmrr)

# 2. Observe breakdown distribution in data
table(model_data_rmrr$breakdown)

#################################
## Create train and test split ##
#################################

set.seed(1000)
test_df_rmrr <- model_data_rmrr %>% group_by(breakdown) %>% sample_frac(0.2)
train_df_rmrr <- subset(model_data_rmrr, !(id %in% test_df_rmrr$id))

variables_rmrr <- ~ line_code + 
  is_interchange + 
  is_above_ground +
  line_age + 
  operator + 
  n_stations + 
  cost_per_km + 
  n_lines + 
  day_type +
  is_peak +
  yr -1   # account for variations over time (Instrument Variable)

x_train_rmrr <- sparse.model.matrix(variables_rmrr, data = train_df_rmrr)
y_train_rmrr <- train_df_rmrr$breakdown

x_test_rmrr <- sparse.model.matrix(variables_rmrr, data = test_df_rmrr)
y_test_rmrr <- test_df_rmrr$breakdown

#######################################################
## Run XGBoost on training data and make predictions ##
#######################################################

xgb_model_rmrr <- xgboost(data = x_train_rmrr, label = y_train_rmrr, 
                          nrounds = 100, objective = "reg:squarederror",
                          verbose = 0)
xgb_pred_rmrr <- predict(xgb_model_rmrr, x_test_rmrr)

#######################
## Model Performance ##
#######################

# 1. Precision and Recall: maximise recall
xgb_pred_rmrr_df <- data.frame(xgb_pred_rmrr)
y_pred_rmrr <- cbind(xgb_pred_rmrr_df, y_test_rmrr)
threshold_rmrr <- y_pred_rmrr %>% filter(y_test_rmrr > 0) %>% min()  
# Threshold is the lowest prediction score that correctly identifies a breakdown station

TP_rmrr <- y_pred_rmrr %>% filter(xgb_pred_rmrr >= threshold_rmrr & y_test_rmrr > 0) %>% count() %>% pull()
FN_rmrr <- y_pred_rmrr %>% filter(xgb_pred_rmrr < threshold_rmrr & y_test_rmrr > 0) %>% count() %>% pull()
FP_rmrr <- y_pred_rmrr %>% filter(xgb_pred_rmrr >= threshold_rmrr & y_test_rmrr == 0) %>% count() %>% pull()
precision_rmrr <- TP_rmrr / (TP_rmrr + FP_rmrr)
recall_rmrr <- TP_rmrr / (TP_rmrr + FN_rmrr)
F1_rmrr <- 2*precision_rmrr*recall_rmrr / (precision_rmrr + recall_rmrr)

# 2. Compare RMSE with baseline (average) model
rmse_xgb_rmrr <- sqrt(mean((y_test_rmrr - xgb_pred_rmrr)^2))
baseline_rmse_rmrr <- sqrt(mean((y_test_rmrr - mean(y_train_rmrr))^2))

#######################
## Feature selection ##
#######################

importance_rmrr <- xgb.importance(model = xgb_model_rmrr)
selected_features_rmrr <- importance_rmrr[Gain > 0.01, Feature]
xgb.plot.importance(importance_matrix = importance_rmrr, top_n = 10) # plot feature importance

######################
## Cross-validation ##
######################

xgb_cv_rmrr <- xgb.cv(data = x_train_rmrr,label = y_train_rmrr,
                      nrounds = 100, nfold = 5, objective = "reg:squarederror",
                      verbose = 0)

best_iter_rmrr <- which.min(xgb_cv_rmrr$evaluation_log$test_rmse_mean)

xgb_cv_rmrr$evaluation_log[best_iter_rmrr, c("train_rmse_mean", "train_rmse_std", "test_rmse_mean", "test_rmse_std")]

###################################################
## Fit model on sample data and finalise scoring ##
###################################################

x_fit_rmrr <- sparse.model.matrix(variables_rmrr, data = fit_rmrr)
fit_rmrr$xgb_score <- predict(xgb_model_rmrr, x_fit_rmrr) # run prediction on new dataset

# Scale xgb_score from 0 to 1 for easier comparison
min_score_rmrr = min(fit_rmrr$xgb_score)
max_score_rmrr = max(fit_rmrr$xgb_score)
fit_rmrr <- fit_rmrr %>% mutate(xgb_scaled = (xgb_score - min_score_rmrr) / (max_score_rmrr - min_score_rmrr))

# Create quantile-based scoring buckets
fit_rmrr$vul_category <- cut(fit_rmrr$xgb_scaled,
                             breaks = quantile(fit_rmrr$xgb_scaled, probs = seq(0, 1, 0.2), na.rm = TRUE),
                             labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                             include.lowest = TRUE)

# Visualise results
ggplot(fit_rmrr, aes(x = xgb_scaled, fill = vul_category)) +
  geom_histogram(bins = 30) +
  scale_fill_manual(values = c("Very Low" = "#094A25",
                               "Low" = "#4C7B00",
                               "Medium" = "#F8B324",
                               "High" = "#EB580C",
                               "Very High" = "#BC2023")) +
  labs(title = "Vulnerability Score Distribution (Static)",
       x = "Scaled Vulnerability Score (0-1)",
       y = "Number of Stations") +
  theme_minimal()

#############################################
## Consolidate final results for front-end ##
#############################################

vul_scores_rmrr <- fit_rmrr %>% 
  dplyr::select(station_code, stations, line_code, line_age, yr,
                day_type, is_peak, is_above_ground, is_interchange, 
                xgb_score, xgb_scaled, vul_category) %>%
  arrange(desc(xgb_score))

# Uncomment to save CSV
#write.csv(vul_scores_rmrr, file = "vul_scores_static.csv")
