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

# Only keep the data from 20th hour column
uow_consumptions_inputs_20th <- uow_consumptions[-c(1: 3)]

# Normalize the full data set
uow_consumptions_inputs_20th_norm <- as.data.frame(lapply(uow_consumptions_inputs_20th, normalize))

# Not normalized data sets (For un-normalize step)
original_train_data <- uow_consumptions_inputs_20th[1: 380,]
original_test_data <- uow_consumptions_inputs_20th[381: 470,]

original_train_data_min <- min(original_train_data)
original_train_data_max <- max(original_train_data)

# Creating lags for the data set
lag_1 = lag(uow_consumptions_inputs_20th_norm,1)
lag_2 = lag(uow_consumptions_inputs_20th_norm,2)
lag_3 = lag(uow_consumptions_inputs_20th_norm,3)
lag_4 = lag(uow_consumptions_inputs_20th_norm,4)
lag_7 = lag(uow_consumptions_inputs_20th_norm,7)

# Create time-delayed I/O metrics for the data set
uow_consumptions_inputs_20th_norm_io_1 <- cbind(uow_consumptions_inputs_20th_norm, lag_1, lag_2, lag_3, lag_4, lag_7)
colnames(uow_consumptions_inputs_20th_norm_io_1) <- c("original", "t1", "t2", "t3", "t4", "t7")

uow_consumptions_inputs_20th_norm_io_2 <- cbind(uow_consumptions_inputs_20th_norm, lag_2, lag_3, lag_4, lag_7)
colnames(uow_consumptions_inputs_20th_norm_io_2) <- c("original", "t2", "t3", "t4", "t7")

uow_consumptions_inputs_20th_norm_io_3 <- cbind(uow_consumptions_inputs_20th_norm, lag_1, lag_2, lag_3, lag_4)
colnames(uow_consumptions_inputs_20th_norm_io_3) <- c("original", "t1", "t2","t3","t4")

uow_consumptions_inputs_20th_norm_io_4 <- cbind(uow_consumptions_inputs_20th_norm, lag_1, lag_2, lag_3)
colnames(uow_consumptions_inputs_20th_norm_io_4) <- c("original", "t1", "t2","t3")

# Formatting time-delayed I/O metrics training data set removing N/As
uow_consumptions_inputs_20th_norm_io_1 <- uow_consumptions_inputs_20th_norm_io_1[complete.cases(uow_consumptions_inputs_20th_norm_io_1),]
uow_consumptions_inputs_20th_norm_io_2 <- uow_consumptions_inputs_20th_norm_io_2[complete.cases(uow_consumptions_inputs_20th_norm_io_2),]
uow_consumptions_inputs_20th_norm_io_3 <- uow_consumptions_inputs_20th_norm_io_3[complete.cases(uow_consumptions_inputs_20th_norm_io_3),]
uow_consumptions_inputs_20th_norm_io_4 <- uow_consumptions_inputs_20th_norm_io_4[complete.cases(uow_consumptions_inputs_20th_norm_io_4),]

# Divide testing and training data sets
uow_consumptions_inputs_20th_norm_io_1_train <- uow_consumptions_inputs_20th_norm_io_1[1: 373, ]
uow_consumptions_inputs_20th_norm_io_1_test <- uow_consumptions_inputs_20th_norm_io_1[-(1: 373), ]

uow_consumptions_inputs_20th_norm_io_2_train <- uow_consumptions_inputs_20th_norm_io_2[1: 373, ]
uow_consumptions_inputs_20th_norm_io_2_test <- uow_consumptions_inputs_20th_norm_io_2[-(1: 373), ]

uow_consumptions_inputs_20th_norm_io_3_train <- uow_consumptions_inputs_20th_norm_io_3[1: 376, ]
uow_consumptions_inputs_20th_norm_io_3_test <- uow_consumptions_inputs_20th_norm_io_3[-(1: 376), ]

uow_consumptions_inputs_20th_norm_io_4_train <- uow_consumptions_inputs_20th_norm_io_4[1: 377, ]
uow_consumptions_inputs_20th_norm_io_4_test <- uow_consumptions_inputs_20th_norm_io_4[-(1: 377), ]

# ----------------- NN Implementation -----------------

# ---- Train NN models ----

# https://www.rdocumentation.org/packages/neuralnet/versions/1.44.2/topics/neuralnet
# https://towardsdatascience.com/how-to-choose-the-right-activation-function-for-neural-networks-3941ff0e6f9c

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
  smape_value = smape(data.matrix(actual_data), predicted_data)
  
  comparison_table[nrow(comparison_table) + 1,] <<- c(model_name_str, rmse_value, mae_value, mape_value, smape_value, training_data_set, hidden_layer_count, act_func, isLinear, algorithm)
}

# NN model - 1 (1st train set)
uow_consumptions_inputs_20th_norm_io_1_train_model_1 <- neuralnet(original ~ t1 + t2 + t3 + t4 + t7, hidden = c(5), data = uow_consumptions_inputs_20th_norm_io_1_train, linear.output = TRUE)
# Insert data to comparison table
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_1_train_model_1, "uow_consumptions_inputs_20th_norm_io_1_train_model_1", "Set 1", 1, "None", TRUE, "Default", uow_consumptions_inputs_20th_norm_io_1_test, original_test_data)

# NN model - 2 (1st train set)
uow_consumptions_inputs_20th_norm_io_1_train_model_2 <- neuralnet(original ~ t1 + t2 + t3 + t4 + t7, hidden = c(4, 2), data = uow_consumptions_inputs_20th_norm_io_1_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_1_train_model_2, "uow_consumptions_inputs_20th_norm_io_1_train_model_2", "Set 1", 2, "None", TRUE, "Default", uow_consumptions_inputs_20th_norm_io_1_test, original_test_data)

# NN model - 3 (1st train set)
uow_consumptions_inputs_20th_norm_io_1_train_model_3 <- neuralnet(original ~ t1 + t2 + t3 + t4 + t7, hidden = c(6, 4, 2), data = uow_consumptions_inputs_20th_norm_io_1_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_1_train_model_3, "uow_consumptions_inputs_20th_norm_io_1_train_model_3", "Set 1", 3, "tanh", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_1_test, original_test_data)

# NN model - 4 (1st train set)
uow_consumptions_inputs_20th_norm_io_1_train_model_4 <- neuralnet(original ~ t1 + t2 + t3 + t4 + t7, hidden = c(4, 2), data = uow_consumptions_inputs_20th_norm_io_1_train, act.fct = "logistic", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_1_train_model_4, "uow_consumptions_inputs_20th_norm_io_1_train_model_4", "Set 1", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_1_test, original_test_data)

# NN model - 5 (1st train set)
uow_consumptions_inputs_20th_norm_io_1_train_model_5 <- neuralnet(original ~ t1 + t2 + t3 + t4 + t7, hidden = c(4, 2), data = uow_consumptions_inputs_20th_norm_io_1_train, act.fct = "tanh", stepmax=1e7, algorithm = "backprop", learningrate=0.001, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_1_train_model_5, "uow_consumptions_inputs_20th_norm_io_1_train_model_5", "Set 1", 2, "tanh", FALSE, "backprop", uow_consumptions_inputs_20th_norm_io_1_test, original_test_data)

# NN model - 6 (2nd train set)
uow_consumptions_inputs_20th_norm_io_2_train_model_1 <- neuralnet(original ~ t2 + t3 + t4 + t7, hidden = c(4), data = uow_consumptions_inputs_20th_norm_io_2_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_2_train_model_1, "uow_consumptions_inputs_20th_norm_io_2_train_model_1", "Set 2", 1, "None", TRUE, "Default", uow_consumptions_inputs_20th_norm_io_2_test, original_test_data)

# NN model - 7 (2nd train set)
uow_consumptions_inputs_20th_norm_io_2_train_model_2 <- neuralnet(original ~ t2 + t3 + t4 + t7, hidden = c(3, 2), data = uow_consumptions_inputs_20th_norm_io_2_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_2_train_model_2, "uow_consumptions_inputs_20th_norm_io_2_train_model_2", "Set 2", 2, "tanh", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_2_test, original_test_data)

# NN model - 8 (2nd train set)
uow_consumptions_inputs_20th_norm_io_2_train_model_3 <- neuralnet(original ~ t2 + t3 + t4 + t7, hidden = c(4, 3), data = uow_consumptions_inputs_20th_norm_io_2_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_2_train_model_3, "uow_consumptions_inputs_20th_norm_io_2_train_model_3", "Set 2", 2, "tanh", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_2_test, original_test_data)

# NN model - 9 (2nd train set)
uow_consumptions_inputs_20th_norm_io_2_train_model_4 <- neuralnet(original ~ t2 + t3 + t4 + t7, hidden = c(3, 2), data = uow_consumptions_inputs_20th_norm_io_2_train, act.fct = "logistic", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_2_train_model_4, "uow_consumptions_inputs_20th_norm_io_2_train_model_4", "Set 2", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_2_test, original_test_data)

# NN model - 10 (3rd train set)
uow_consumptions_inputs_20th_norm_io_3_train_model_1 <- neuralnet(original ~ t1 + t2 + t3 + t4, hidden = c(4), data = uow_consumptions_inputs_20th_norm_io_3_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_3_train_model_1, "uow_consumptions_inputs_20th_norm_io_3_train_model_1", "Set 3", 1, "None", TRUE, "Default", uow_consumptions_inputs_20th_norm_io_3_test, original_test_data)

# NN model - 11 (3rd train set)
uow_consumptions_inputs_20th_norm_io_3_train_model_2 <- neuralnet(original ~ t1 + t2 + t3 + t4, hidden = c(3, 2), data = uow_consumptions_inputs_20th_norm_io_3_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_3_train_model_2, "uow_consumptions_inputs_20th_norm_io_3_train_model_2", "Set 3", 2, "tanh", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_3_test, original_test_data)

# NN model - 12 (3rd train set)
uow_consumptions_inputs_20th_norm_io_3_train_model_3 <- neuralnet(original ~ t1 + t2 + t3 + t4, hidden = c(4, 3), data = uow_consumptions_inputs_20th_norm_io_3_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_3_train_model_3, "uow_consumptions_inputs_20th_norm_io_3_train_model_3", "Set 3", 2, "tanh", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_3_test, original_test_data)

# NN model - 13 (3rd train set)
uow_consumptions_inputs_20th_norm_io_3_train_model_4 <- neuralnet(original ~ t1 + t2 + t3 + t4, hidden = c(3, 2), data = uow_consumptions_inputs_20th_norm_io_3_train, act.fct = "logistic", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_3_train_model_4, "uow_consumptions_inputs_20th_norm_io_3_train_model_4", "Set 3", 2, "logistic", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_3_test, original_test_data)

# NN model - 14 (4th train set)
uow_consumptions_inputs_20th_norm_io_4_train_model_1 <- neuralnet(original ~ t1 + t2 + t3, hidden = c(3), data = uow_consumptions_inputs_20th_norm_io_4_train, linear.output = TRUE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_4_train_model_1, "uow_consumptions_inputs_20th_norm_io_4_train_model_1", "Set 4", 1, "None", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_4_test, original_test_data)

# NN model - 15 (4th train set)
uow_consumptions_inputs_20th_norm_io_4_train_model_2 <- neuralnet(original ~ t1 + t2 + t3, hidden = c(2, 2), data = uow_consumptions_inputs_20th_norm_io_4_train, act.fct = "tanh", stepmax=1e7, linear.output = FALSE)
insert_comparison_table_row(uow_consumptions_inputs_20th_norm_io_4_train_model_2, "uow_consumptions_inputs_20th_norm_io_4_train_model_2", "Set 4", 2, "tanh", FALSE, "Default", uow_consumptions_inputs_20th_norm_io_4_test, original_test_data)

View(comparison_table)

# Graphical representation - Best performed NN model
par(mfrow=c(1,1))
plot(data.matrix(original_test_data), get_predicted_data_from_nn_model(uow_consumptions_inputs_20th_norm_io_1_train_model_4, uow_consumptions_inputs_20th_norm_io_1_test), col='red', main='Real vs predicted NN', pch = 18, cex = 0.7)
abline(a=0, b=1, h=90, v=90)

plot(uow_consumptions_inputs_20th_norm_io_1_train_model_4)