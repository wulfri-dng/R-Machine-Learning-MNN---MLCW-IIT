library(readxl) # readxl package used to import excel files
library(dplyr)
library(neuralnet)

uow_consumptions <- read_excel("D:\\Coding Area\\University Projects\\Courseworks\\R-Machine-Learning-MNN---MLCW-IIT\\uow_consumption.xlsx")

# ----------------- Data Pre-processing -----------------

# Function to normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Function to unnormalize data
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

# Only keep the data from 20th hour column
uow_consumptions_inputs_20th <- uow_consumptions[-c(1: 3)]

# Normalize the full data set
uow_consumptions_inputs_20th_norm <- as.data.frame(lapply(uow_consumptions_inputs_20th, normalize))

# Divide testing and training data sets
uow_consumptions_inputs_20th_norm_train <- uow_consumptions_inputs_20th_norm[1: 380, ]
uow_consumptions_inputs_20th_norm_test <- uow_consumptions_inputs_20th_norm[381: 470, ]

# Creating lags for training data set
lag_train_1 = lag(uow_consumptions_inputs_20th_norm_train,1)
lag_train_2 = lag(uow_consumptions_inputs_20th_norm_train,2)
lag_train_3 = lag(uow_consumptions_inputs_20th_norm_train,3)
lag_train_4 = lag(uow_consumptions_inputs_20th_norm_train,4)
lag_train_7 = lag(uow_consumptions_inputs_20th_norm_train,7)

# Creating lags for testing data set
lag_test_1 = lag(uow_consumptions_inputs_20th_norm_test,1)
lag_test_2 = lag(uow_consumptions_inputs_20th_norm_test,2)
lag_test_3 = lag(uow_consumptions_inputs_20th_norm_test,3)
lag_test_4 = lag(uow_consumptions_inputs_20th_norm_test,4)
lag_test_7 = lag(uow_consumptions_inputs_20th_norm_test,7)

# Create time-delayed I/O metrics for training data set
uow_consumptions_inputs_20th_norm_io_1_train <- cbind(uow_consumptions_inputs_20th_norm_train,lag_train_1,lag_train_2,lag_train_3,lag_train_4,lag_train_7)
colnames(uow_consumptions_inputs_20th_norm_io_1_train) <- c("original","t1","t2","t3","t4","t7")

uow_consumptions_inputs_20th_norm_io_2_train <- cbind(uow_consumptions_inputs_20th_norm_train,lag_train_2,lag_train_3,lag_train_4,lag_train_7)
colnames(uow_consumptions_inputs_20th_norm_io_2_train) <- c("original", "t2","t3","t4","t7")

uow_consumptions_inputs_20th_norm_io_3_train <- cbind(uow_consumptions_inputs_20th_norm_train,lag_train_1,lag_train_2,lag_train_3,lag_train_4)
colnames(uow_consumptions_inputs_20th_norm_io_3_train) <- c("original", "t1", "t2","t3","t4")

uow_consumptions_inputs_20th_norm_io_4_train <- cbind(uow_consumptions_inputs_20th_norm_train,lag_train_1,lag_train_2,lag_train_3)
colnames(uow_consumptions_inputs_20th_norm_io_4_train) <- c("original", "t1", "t2","t3")

# Create time-delayed I/O metrics for testing data set
uow_consumptions_inputs_20th_norm_io_1_test <- cbind(uow_consumptions_inputs_20th_norm_test,lag_test_1,lag_test_2,lag_test_3,lag_test_4,lag_test_7)
colnames(uow_consumptions_inputs_20th_norm_io_1_test) <- c("original","t1","t2","t3","t4","t7")

uow_consumptions_inputs_20th_norm_io_2_test <- cbind(uow_consumptions_inputs_20th_norm_test,lag_test_2,lag_test_3,lag_test_4,lag_test_7)
colnames(uow_consumptions_inputs_20th_norm_io_2_test) <- c("original", "t2","t3","t4","t7")

uow_consumptions_inputs_20th_norm_io_3_test <- cbind(uow_consumptions_inputs_20th_norm_test,lag_test_1,lag_test_2,lag_test_3,lag_test_4)
colnames(uow_consumptions_inputs_20th_norm_io_3_test) <- c("original", "t1", "t2","t3","t4")

uow_consumptions_inputs_20th_norm_io_4_test <- cbind(uow_consumptions_inputs_20th_norm_test,lag_test_1,lag_test_2,lag_test_3)
colnames(uow_consumptions_inputs_20th_norm_io_4_test) <- c("original", "t1", "t2","t3")

# Formatting time-delayed I/O metrics training data set removing N/As
uow_consumptions_inputs_20th_norm_io_1_train <- uow_consumptions_inputs_20th_norm_io_1_train[complete.cases(uow_consumptions_inputs_20th_norm_io_1_train),]
uow_consumptions_inputs_20th_norm_io_2_train <- uow_consumptions_inputs_20th_norm_io_2_train[complete.cases(uow_consumptions_inputs_20th_norm_io_2_train),]
uow_consumptions_inputs_20th_norm_io_3_train <- uow_consumptions_inputs_20th_norm_io_3_train[complete.cases(uow_consumptions_inputs_20th_norm_io_3_train),]
uow_consumptions_inputs_20th_norm_io_4_train <- uow_consumptions_inputs_20th_norm_io_4_train[complete.cases(uow_consumptions_inputs_20th_norm_io_4_train),]

# Formatting time-delayed I/O metrics testing data set removing N/As
uow_consumptions_inputs_20th_norm_io_1_test <- uow_consumptions_inputs_20th_norm_io_1_test[complete.cases(uow_consumptions_inputs_20th_norm_io_1_test),]
uow_consumptions_inputs_20th_norm_io_2_test <- uow_consumptions_inputs_20th_norm_io_2_test[complete.cases(uow_consumptions_inputs_20th_norm_io_2_test),]
uow_consumptions_inputs_20th_norm_io_3_test <- uow_consumptions_inputs_20th_norm_io_3_test[complete.cases(uow_consumptions_inputs_20th_norm_io_3_test),]
uow_consumptions_inputs_20th_norm_io_4_test <- uow_consumptions_inputs_20th_norm_io_4_test[complete.cases(uow_consumptions_inputs_20th_norm_io_4_test),]

# ----------------- NN Implementation -----------------

# Train NN model
uow_consumptions_inputs_20th_norm_io_1_train_model <- neuralnet(original ~ t1 + t2 + t3 + t4 + t7, hidden = c(15, 10, 5), data = uow_consumptions_inputs_20th_norm_io_1_train, act.fct = "logistic", learningrate = 0.0001, linear.output = TRUE)
plot(uow_consumptions_inputs_20th_norm_io_1_train_model)

# Test NN model
model_result <- neuralnet::compute(uow_consumptions_inputs_20th_norm_io_1_train_model, uow_consumptions_inputs_20th_norm_io_1_test)
model_result$net.result
model_result$neurons

# Not normalized test data set
original_train_data <- uow_consumptions_inputs_20th[1: 380,]
original_test_data <- uow_consumptions_inputs_20th[381: 463,]
View()
data.matrix(original_test_data)

original_train_data_min <- min(original_train_data)
original_train_data_max <- max(original_train_data)

predicted_data <- unnormalize(model_result$net.result, original_train_data_min, original_train_data_max)
#view(predicted_data)

# RMSE evaluation
rmse1 <- function(error)
{
  sqrt(mean(error^2))
}

error <- (data.matrix(original_test_data) - predicted_data)
pred_RMSE <- rmse1(error)

par(mfrow=c(1,1))
plot(data.matrix(original_test_data), predicted_data, col='red', main='Real vs predicted NN', pch = 18, cex = 0.7)
abline(a=0, b=1, h=90, v=90)
