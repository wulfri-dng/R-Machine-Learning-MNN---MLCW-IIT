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

# Not normalized 20th hour data sets (For un-normalize step)
original_train_data <- uow_consumptions_inputs_20th[1: 380,]
original_test_data <- uow_consumptions_inputs_20th[381: 470,]

original_train_data_min <- min(original_train_data)
original_train_data_max <- max(original_train_data)

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
uow_consumptions_inputs_norm_io_1 <- cbind(uow_consumptions_inputs_20th_norm, lag_20_1, lag_20_2, lag_20_3, lag_20_4, lag_20_7, lag_19_1, lag_19_2, lag_19_3, lag_19_4, lag_19_7, lag_18_1, lag_18_2, lag_18_3, lag_18_4, lag_18_7)
colnames(uow_consumptions_inputs_norm_io_1) <- c("original", "t120", "t220", "t320", "t420", "t720", "t119", "t219", "t319", "t419", "t719", "t118", "t218", "t318", "t418", "t718")

uow_consumptions_inputs_norm_io_2 <- cbind(uow_consumptions_inputs_20th_norm, lag_20_1, lag_20_4, lag_20_7, lag_19_1, lag_19_4, lag_19_7, lag_18_1, lag_18_4, lag_18_7)
colnames(uow_consumptions_inputs_norm_io_2) <- c("original", "t120", "t420","t720", "t119", "t419","t719", "t118", "t418","t718")

uow_consumptions_inputs_norm_io_3 <- cbind(uow_consumptions_inputs_20th_norm, lag_20_1, lag_20_2, lag_20_3, lag_20_4, lag_20_7, lag_19_1, lag_19_7, lag_18_1, lag_18_7)
colnames(uow_consumptions_inputs_norm_io_3) <- c("original", "t120", "t220", "t320", "t420", "t720", "t119", "t719", "t118", "t718")

# Formatting time-delayed I/O metrics training data set removing N/As
uow_consumptions_inputs_norm_io_1 <- uow_consumptions_inputs_norm_io_1[complete.cases(uow_consumptions_inputs_norm_io_1),]
uow_consumptions_inputs_norm_io_2 <- uow_consumptions_inputs_norm_io_2[complete.cases(uow_consumptions_inputs_norm_io_2),]
uow_consumptions_inputs_norm_io_3 <- uow_consumptions_inputs_norm_io_3[complete.cases(uow_consumptions_inputs_norm_io_3),]

# Divide testing and training data sets
uow_consumptions_inputs_norm_io_1_train <- uow_consumptions_inputs_norm_io_1[1: 373, ]
uow_consumptions_inputs_norm_io_1_test <- uow_consumptions_inputs_norm_io_1[-(1: 373), ]

uow_consumptions_inputs_norm_io_2_train <- uow_consumptions_inputs_norm_io_2[1: 373, ]
uow_consumptions_inputs_norm_io_2_test <- uow_consumptions_inputs_norm_io_2[-(1: 373), ]

uow_consumptions_inputs_norm_io_3_train <- uow_consumptions_inputs_norm_io_3[1: 373, ]
uow_consumptions_inputs_norm_io_3_test <- uow_consumptions_inputs_norm_io_3[-(1: 373), ]

# ----------------- NN Implementation -----------------

# ---- Train NN models ----
# Create the comparison table
columns = c("Model Name", "RMSE", "MAE", "MAPE", "sMAPE", "Training data set","Hidden layers", "Activation function", "Linear", "Algorithm") 
comparison_table <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(comparison_table) <- columns

get_predicted_data_from_nn_model <- function(model, testing_data) {
  # Test NN model and un-normalize predicted data
  model_result <- neuralnet::compute(model, testing_data)
  predicted_data <- unnormalize(model_result$net.result, original_train_data_min, original_train_data_max)
  
  return(predicted_data)
}

# Update comparison table
insert_comparison_table_row <- function(model, model_name_str, training_data_set, hidden_layer_count, act_func, isLinear, algorithm, testing_data, actual_data) {
  predicted_data <- get_predicted_data_from_nn_model(model, testing_data)
  
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

# NN model - 1 (1st train set)
uow_consumptions_inputs_norm_io_1_train_model_1 <- neuralnet("original ~ t120 + t220 + t320 + t420 + t720 + t119 + t219 + t319 + t419 + t719 + t118 + t218 + t318 + t418 + t718", hidden = c(7, 4, 3, 2), data = uow_consumptions_inputs_norm_io_1_train, linear.output = TRUE)
# Insert data into comparison table
insert_comparison_table_row(uow_consumptions_inputs_norm_io_1_train_model_1, "uow_consumptions_inputs_norm_io_1_train_model_1", "Set 1", 4, "None", TRUE, "Default", uow_consumptions_inputs_norm_io_1_test, original_test_data)

# NN model - 2 (1st train set)
uow_consumptions_inputs_norm_io_1_train_model_2 <- neuralnet("original ~ t120 + t220 + t320 + t420 + t720 + t119 + t219 + t319 + t419 + t719 + t118 + t218 + t318 + t418 + t718", hidden = c(10, 6), data = uow_consumptions_inputs_norm_io_1_train, act.fct = "logistic", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_1_train_model_2, "uow_consumptions_inputs_norm_io_1_train_model_2", "Set 1", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_norm_io_1_test, original_test_data)

# NN model - 3 (2nd train set)
uow_consumptions_inputs_norm_io_2_train_model_1 <- neuralnet("original ~ t120 + t420 + t720 + t119 + t419 + t719 + t118 + t418 + t718", hidden = c(5, 3, 2), data = uow_consumptions_inputs_norm_io_2_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_2_train_model_1, "uow_consumptions_inputs_norm_io_2_train_model_1", "Set 2", 3, "None", TRUE, "Default", uow_consumptions_inputs_norm_io_2_test, original_test_data)

# NN model - 4 (2nd train set)
uow_consumptions_inputs_norm_io_2_train_model_2 <- neuralnet("original ~ t120 + t420 + t720 + t119 + t419 + t719 + t118 + t418 + t718", hidden = c(7, 3), data = uow_consumptions_inputs_norm_io_2_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_2_train_model_2, "uow_consumptions_inputs_norm_io_2_train_model_2", "Set 2", 2, "tanh", FALSE, "Default", uow_consumptions_inputs_norm_io_2_test, original_test_data)

# NN model - 5 (3rd train set)
uow_consumptions_inputs_norm_io_3_train_model_1 <- neuralnet("original ~ t120 + t220 + t320 + t420 + t720 + t119 + t719 + t118 + t718", hidden = c(5, 3, 2), data = uow_consumptions_inputs_norm_io_3_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_3_train_model_1, "uow_consumptions_inputs_norm_io_3_train_model_1", "Set 3", 3, "None", TRUE, "Default", uow_consumptions_inputs_norm_io_3_test, original_test_data)

# NN model - 6 (3rd train set)
uow_consumptions_inputs_norm_io_3_train_model_2 <- neuralnet("original ~ t120 + t220 + t320 + t420 + t720 + t119 + t719 + t118 + t718", hidden = c(6, 4), data = uow_consumptions_inputs_norm_io_3_train, act.fct = "logistic", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_norm_io_3_train_model_2, "uow_consumptions_inputs_norm_io_3_train_model_2", "Set 3", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_norm_io_3_test, original_test_data)

View(comparison_table)

# Graphical representation - Best performed NN model
par(mfrow=c(1,1))
plot(data.matrix(original_test_data), get_predicted_data_from_nn_model(uow_consumptions_inputs_norm_io_2_train_model_1, uow_consumptions_inputs_norm_io_2_test), col='red', main='Real vs predicted NN', pch = 18, cex = 0.7)
abline(a=0, b=1, h=90, v=90)

plot(uow_consumptions_inputs_norm_io_2_train_model_1)
