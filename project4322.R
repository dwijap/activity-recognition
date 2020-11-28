#READING THE DATA
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, comment = "")
files.1 <- list.files('C:/Users/dwija/Documents/UH/FALL 2020/MATH 4322/project/Datasets_Healthy_Older_People/S1_Dataset/')
files.2 <- list.files('C:/Users/dwija/Documents/UH/FALL 2020/MATH 4322/project/Datasets_Healthy_Older_People/S2_Dataset/')
files.1 <- paste0('C:/Users/dwija/Documents/UH/FALL 2020/MATH 4322/project/Datasets_Healthy_Older_People/S1_Dataset/', files.1[-length(files.1)])  # do not read the readme files
files.2 <- paste0('C:/Users/dwija/Documents/UH/FALL 2020/MATH 4322/project/Datasets_Healthy_Older_People/S2_Dataset/', files.2[-length(files.2)])

data.1 <- do.call(rbind, lapply(files.1, function(x) read.csv(file=x, header = F)))
data.2 <- do.call(rbind, lapply(files.2, function(x) read.csv(file=x, header = F)))

data <- rbind(data.1, data.2)
colnames(data) <- c("Time", "Acceleration.front", "Acceleration.vert","Acceleration.lat",
                    "Antenna","RSSI","Phase","Frequency","Activity")


head(data)
data$Activity <- factor(data$Activity)
data$Antenna <- factor(data$Antenna)
dim(data)

#splitting
library(caret)
RNGkind(sample.kind = "Rounding")
library(class)
set.seed(1234)    # set a seed for reproducibility
trainIndex <- createDataPartition(data$Activity, p = 0.8, list = F)
traindata <- data[trainIndex[, 1], ]
testdata <- data[-trainIndex[, 1], ]

table(traindata$Activity)
table(testdata$Activity)

# Pre-processing
pre <- preProcess(traindata, method = "range")   # compute the transformation
traindata <- predict(pre, traindata)    # apply the transformation on training set
testdata <- predict(pre, testdata)    # apply the transformation on testing set
summary(traindata)
summary(testdata)


