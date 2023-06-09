library(readxl) # readxl package used to import excel files
library(dplyr)
library(neuralnet)
library(Metrics) # For MAE evaluation

uow_consumptions <- read_excel("D:\\Coding Area\\University Projects\\Courseworks\\R-Machine-Learning-MNN---MLCW-IIT\\uow_consumption.xlsx")

# Function to normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Function to un-normalize data
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

# MAPE calculator
calculate_mape <- function(actual, predicted) {
  return (mean(abs((actual-predicted)/actual)) * 100)
}

# RMSE calculator
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# ----------------- Data Pre-processing -----------------

# Get hourly data separately
uow_consumptions_inputs_18th <- uow_consumptions[-c(1, 3, 4)]
uow_consumptions_inputs_19th <- uow_consumptions[-c(1, 2, 4)]
uow_consumptions_inputs_20th <- uow_consumptions[-c(1: 3)]

# Normalize data sets
uow_consumptions_inputs_18th_norm <- as.data.frame(lapply(uow_consumptions_inputs_18th, normalize))
uow_consumptions_inputs_19th_norm <- as.data.frame(lapply(uow_consumptions_inputs_19th, normalize))
uow_consumptions_inputs_20th_norm <- as.data.frame(lapply(uow_consumptions_inputs_20th, normalize))

# Not normalized 20th hour data sets (For evaluations)
original_train_data_18th_hour <- uow_consumptions_inputs_18th[1: 380,]
original_test_data_18th_hour <- uow_consumptions_inputs_18th[381: 470,]

original_data_18th_hour_min <- min(uow_consumptions_inputs_18th)
original_data_18th_hour_max <- max(uow_consumptions_inputs_18th)

original_train_data_19th_hour <- uow_consumptions_inputs_19th[1: 380,]
original_test_data_19th_hour <- uow_consumptions_inputs_19th[381: 470,]

original_data_19th_hour_min <- min(uow_consumptions_inputs_19th)
original_data_19th_hour_max <- max(uow_consumptions_inputs_19th)

# Creating lags for the data sets
lag_18_1 = lag(uow_consumptions_inputs_18th_norm, 1)
lag_18_2 = lag(uow_consumptions_inputs_18th_norm, 2)
lag_18_3 = lag(uow_consumptions_inputs_18th_norm, 3)
lag_18_4 = lag(uow_consumptions_inputs_18th_norm, 4)
lag_18_7 = lag(uow_consumptions_inputs_18th_norm, 7)

lag_19_1 = lag(uow_consumptions_inputs_19th_norm, 1)
lag_19_2 = lag(uow_consumptions_inputs_19th_norm, 2)
lag_19_3 = lag(uow_consumptions_inputs_19th_norm, 3)
lag_19_4 = lag(uow_consumptions_inputs_19th_norm, 4)
lag_19_7 = lag(uow_consumptions_inputs_19th_norm, 7)

lag_20_1 = lag(uow_consumptions_inputs_20th_norm, 1)
lag_20_2 = lag(uow_consumptions_inputs_20th_norm, 2)
lag_20_3 = lag(uow_consumptions_inputs_20th_norm, 3)
lag_20_4 = lag(uow_consumptions_inputs_20th_norm, 4)
lag_20_7 = lag(uow_consumptions_inputs_20th_norm, 7)

# Create time-delayed I/O metrics for the data set
uow_consumptions_inputs_norm_io_18_1 <- cbind(uow_consumptions_inputs_18th_norm, lag_18_1, lag_18_2, lag_18_3, lag_18_4, lag_18_7)
colnames(uow_consumptions_inputs_norm_io_18_1) <- c("original", "t18_1", "t18_2", "t18_3", "t18_4", "t18_7")

uow_consumptions_inputs_norm_io_19_1 <- cbind(uow_consumptions_inputs_19th_norm, lag_19_1, lag_19_2, lag_19_3, lag_19_4, lag_19_7)
colnames(uow_consumptions_inputs_norm_io_19_1) <- c("original", "t19_1", "t19_2", "t19_3", "t19_4", "t19_7")

uow_consumptions_inputs_norm_io_20_1 <- cbind(uow_consumptions_inputs_20th_norm, lag_20_1, lag_20_2, lag_20_3, lag_20_4, lag_20_7)
colnames(uow_consumptions_inputs_norm_io_20_1) <- c("original", "t20_1", "t20_2", "t20_3", "t20_4", "t20_7")

uow_consumptions_inputs_norm_io_20_2 <- cbind(uow_consumptions_inputs_20th_norm, lag_20_1, lag_20_4, lag_20_7)
colnames(uow_consumptions_inputs_norm_io_20_2) <- c("original", "t20_1", "t20_4", "t20_7")

# Formatting time-delayed I/O metrics training data set removing N/As
uow_consumptions_inputs_norm_io_18_1 <- uow_consumptions_inputs_norm_io_18_1[complete.cases(uow_consumptions_inputs_norm_io_18_1),]
uow_consumptions_inputs_norm_io_19_1 <- uow_consumptions_inputs_norm_io_19_1[complete.cases(uow_consumptions_inputs_norm_io_19_1),]
uow_consumptions_inputs_norm_io_20_1 <- uow_consumptions_inputs_norm_io_20_1[complete.cases(uow_consumptions_inputs_norm_io_20_1),]
uow_consumptions_inputs_norm_io_20_2 <- uow_consumptions_inputs_norm_io_20_2[complete.cases(uow_consumptions_inputs_norm_io_20_2),]

# Divide testing and training data sets
uow_consumptions_inputs_norm_io_18_1_train <- uow_consumptions_inputs_norm_io_18_1[1: 373, ]
uow_consumptions_inputs_norm_io_18_1_test <- uow_consumptions_inputs_norm_io_18_1[-(1: 373), ]

uow_consumptions_inputs_norm_io_19_1_train <- uow_consumptions_inputs_norm_io_19_1[1: 373, ]
uow_consumptions_inputs_norm_io_19_1_test <- uow_consumptions_inputs_norm_io_19_1[-(1: 373), ]

uow_consumptions_inputs_norm_io_20_1_train <- uow_consumptions_inputs_norm_io_20_1[1: 373, ]
uow_consumptions_inputs_norm_io_20_1_test <- uow_consumptions_inputs_norm_io_20_1[-(1: 373), ]

uow_consumptions_inputs_norm_io_20_2_train <- uow_consumptions_inputs_norm_io_20_2[1: 373, ]
uow_consumptions_inputs_norm_io_20_2_test <- uow_consumptions_inputs_norm_io_20_2[-(1: 373), ]

# ----------------- NN Implementation -----------------

# ---- Train NN models ----
# Create the comparison table
columns = c("Model Name", "RMSE", "MAE", "MAPE", "sMAPE", "Training data set","Hidden layers", "Activation function", "Linear", "Algorithm") 
comparison_table <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(comparison_table) <- columns

get_predicted_data_from_nn_model <- function(model, testing_data, data_min, data_max) {
  # Test NN model and un-normalize predicted data
  model_result <- neuralnet::compute(model, testing_data)
  predicted_data <- unnormalize(model_result$net.result, data_min, data_max)
  
  return(predicted_data)
}

# Update comparison table
insert_comparison_table_row <- function(model, model_name_str, training_data_set, hidden_layer_count, act_func, isLinear, algorithm, testing_data, actual_data, data_min, data_max) {
  predicted_data <- get_predicted_data_from_nn_model(model, testing_data, data_min, data_max)
  
  # RMSE evaluation https://www.r-bloggers.com/2021/07/how-to-calculate-root-mean-square-error-rmse-in-r/
  rmse_value = calculate_rmse(data.matrix(actual_data), predicted_data)
  
  # MAE evaluation https://www.r-bloggers.com/2021/07/how-to-calculate-mean-absolute-error-in-r/
  mae_value = mae(data.matrix(actual_data), predicted_data)
  
  # MAPE evaluation https://www.r-bloggers.com/2021/08/how-to-calculate-mean-absolute-percentage-error-mape-in-r/
  mape_value = calculate_mape(data.matrix(actual_data), predicted_data)
  
  # sMAPE evaluation https://www.r-bloggers.com/2021/08/how-to-calculate-smape-in-r/
  smape_value = smape(data.matrix(actual_data), predicted_data) * 100
  
  comparison_table[nrow(comparison_table) + 1,] <<- c(model_name_str, rmse_value, mae_value, mape_value, smape_value, training_data_set, hidden_layer_count, act_func, isLinear, algorithm)
}

# NN model - 1 (18th hour data set)
uow_consumptions_inputs_norm_io_18_1_train_model_1 <- neuralnet("original ~ t18_1 + t18_2 + t18_3 + t18_4 + t18_7", hidden = c(4, 2), data = uow_consumptions_inputs_norm_io_18_1_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_18_1_train_model_1, "uow_consumptions_inputs_norm_io_18_1_train_model_1", "Set 1", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_norm_io_18_1_test, original_test_data_18th_hour, original_data_18th_hour_min, original_data_18th_hour_max)
predicted_data_18th_hour <- get_predicted_data_from_nn_model(uow_consumptions_inputs_norm_io_18_1_train_model_1, uow_consumptions_inputs_norm_io_18_1_test, original_data_18th_hour_min, original_data_18th_hour_max)

# 2nd data set of 19th hour data
uow_consumptions_inputs_norm_io_19_1 <- cbind(uow_consumptions_inputs_19th_norm, lag_19_1, lag_19_2, lag_19_3, lag_19_4, lag_19_7)
colnames(uow_consumptions_inputs_norm_io_19_1) <- c("original", "t19_1", "t19_2", "t19_3", "t19_4", "t19_7")

# NN model - 2 (19th hour data set)
uow_consumptions_inputs_norm_io_19_1_train_model_1 <- neuralnet("original ~ t19_1 + t19_2 + t19_3 + t19_4 + t19_7", hidden = c(4, 2), data = uow_consumptions_inputs_norm_io_19_1_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_19_1_train_model_1, "uow_consumptions_inputs_norm_io_19_1_train_model_1", "Set 1", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_norm_io_19_1_test, original_test_data_19th_hour, original_data_19th_hour_min, original_data_19th_hour_max)
View(comparison_table)

# NN model - 2 (19th hour data set)
uow_consumptions_inputs_norm_io_19_1_train_model_1 <- neuralnet("original ~ t19_1 + t19_2 + t19_3 + t19_4 + t19_7", hidden = c(4, 2), data = uow_consumptions_inputs_norm_io_19_1_train, linear.output = TRUE)

# NN model - 2 (1st train set)
uow_consumptions_inputs_norm_io_1_train_model_2 <- neuralnet("original ~ t120 + t220 + t320 + t420 + t720 + t19 + t18", hidden = c(5, 3), data = uow_consumptions_inputs_norm_io_1_train, act.fct = "logistic", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_1_train_model_2, "uow_consumptions_inputs_norm_io_1_train_model_2", "Set 1", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_norm_io_1_test, original_test_data)

# NN model - 3 (2nd train set)
uow_consumptions_inputs_norm_io_2_train_model_1 <- neuralnet("original ~ t120 + t420 + t720 + t19 + t18", hidden = c(4, 2), data = uow_consumptions_inputs_norm_io_2_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_2_train_model_1, "uow_consumptions_inputs_norm_io_2_train_model_1", "Set 2", 2, "None", TRUE, "Default", uow_consumptions_inputs_norm_io_2_test, original_test_data)

# NN model - 4 (2nd train set)
uow_consumptions_inputs_norm_io_2_train_model_2 <- neuralnet("original ~ t120 + t420 + t720 + t19 + t18", hidden = c(5), data = uow_consumptions_inputs_norm_io_2_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_2_train_model_2, "uow_consumptions_inputs_norm_io_2_train_model_2", "Set 2", 1, "tanh", FALSE, "Default", uow_consumptions_inputs_norm_io_2_test, original_test_data)

# NN model - 5 (3rd train set)
uow_consumptions_inputs_norm_io_3_train_model_1 <- neuralnet("original ~ t120 + t720 + t19 + t18", hidden = c(3, 2), data = uow_consumptions_inputs_norm_io_3_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_3_train_model_1, "uow_consumptions_inputs_norm_io_3_train_model_1", "Set 3", 2, "None", TRUE, "Default", uow_consumptions_inputs_norm_io_3_test, original_test_data)

# NN model - 6 (3rd train set)
uow_consumptions_inputs_norm_io_3_train_model_2 <- neuralnet("original ~ t120 + t720 + t19 + t18", hidden = c(5), data = uow_consumptions_inputs_norm_io_3_train, act.fct = "logistic", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_3_train_model_2, "uow_consumptions_inputs_norm_io_3_train_model_2", "Set 3", 1, "logistic", FALSE, "Default", uow_consumptions_inputs_norm_io_3_test, original_test_data)

View(comparison_table)

# Graphical representation - Best performed NN model
par(mfrow=c(1,1))
plot(data.matrix(original_test_data), get_predicted_data_from_nn_model(uow_consumptions_inputs_norm_io_1_train_model_1, uow_consumptions_inputs_norm_io_1_test), col='red', main='Real vs predicted NN', pch = 18, cex = 0.7)
abline(a=0, b=1, h=90, v=90)

plot(uow_consumptions_inputs_norm_io_2_train_model_1)
