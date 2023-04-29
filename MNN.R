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
uow_consumptions_inputs_20th = uow_consumptions[-c(1: 3)]

# Creating lags
lag1 = lag(uow_consumptions_inputs_20th,1)
lag2 = lag(uow_consumptions_inputs_20th,2)
lag3 = lag(uow_consumptions_inputs_20th,3)
lag4 = lag(uow_consumptions_inputs_20th,4)
lag7 = lag(uow_consumptions_inputs_20th,7)

# Create time-delayed I/O metrics
uow_consumptions_inputs_20th_io_1 <- cbind(uow_consumptions_inputs_20th,lag1,lag2,lag3,lag4,lag7)
colnames(uow_consumptions_inputs_20th_io_1) <- c("original","t1","t2","t3","t4","t7")

uow_consumptions_inputs_20th_io_2 <- cbind(uow_consumptions_inputs_20th,lag2,lag3,lag4,lag7)
colnames(uow_consumptions_inputs_20th_io_2) <- c("original", "t2","t3","t4","t7")

uow_consumptions_inputs_20th_io_3 <- cbind(uow_consumptions_inputs_20th,lag1,lag2,lag3,lag4)
colnames(uow_consumptions_inputs_20th_io_3) <- c("original", "t1", "t2","t3","t4")

uow_consumptions_inputs_20th_io_4 <- cbind(uow_consumptions_inputs_20th,lag1,lag2,lag3)
colnames(uow_consumptions_inputs_20th_io_4) <- c("original", "t1", "t2","t3")

# Formatting time-delayed I/O metrics removing N/As
uow_consumptions_inputs_20th_io_1 <- uow_consumptions_inputs_20th_io_1[complete.cases(uow_consumptions_inputs_20th_io_1),]
uow_consumptions_inputs_20th_io_2 <- uow_consumptions_inputs_20th_io_2[complete.cases(uow_consumptions_inputs_20th_io_2),]
uow_consumptions_inputs_20th_io_3 <- uow_consumptions_inputs_20th_io_3[complete.cases(uow_consumptions_inputs_20th_io_3),]
uow_consumptions_inputs_20th_io_4 <- uow_consumptions_inputs_20th_io_4[complete.cases(uow_consumptions_inputs_20th_io_4),]

view(uow_consumptions_inputs_20th_io_1)
view(uow_consumptions_inputs_20th_io_2)
view(uow_consumptions_inputs_20th_io_3)
view(uow_consumptions_inputs_20th_io_4)

# Normalize I/O matrices
uow_consumptions_inputs_20th_io_1 <- as.data.frame(lapply(uow_consumptions_inputs_20th_io_1, normalize))
uow_consumptions_inputs_20th_io_2 <- as.data.frame(lapply(uow_consumptions_inputs_20th_io_2, normalize))
uow_consumptions_inputs_20th_io_3 <- as.data.frame(lapply(uow_consumptions_inputs_20th_io_3, normalize))
uow_consumptions_inputs_20th_io_4 <- as.data.frame(lapply(uow_consumptions_inputs_20th_io_4, normalize))

# ----------------- NN Implementation -----------------
uow_consumptions_inputs_20th_io_1_train = uow_consumptions_inputs_20th_io_1[1: 350, ]
uow_consumptions_inputs_20th_io_1_test = uow_consumptions_inputs_20th_io_1[351: 463, ]

uow_consumptions_inputs_20th_io_1_model <- neuralnet(original ~ t1 + t2 + t3 + t4 + t7, hidden = 5, data = uow_consumptions_inputs_20th_io_1_train, linear.output = TRUE)
plot(uow_consumptions_inputs_20th_io_1_model)
