#READING THE DATA
library(tidyverse)
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, comment = "")
setwd("C:/Users/dwija/Documents/UH/FALL 2020/MATH 4322/project/Datasets_Healthy_Older_People")

files.1 <- list.files('S1_Dataset/')
files.2 <- list.files('S2_Dataset/')

files.1 <- paste0('S1_Dataset/', files.1[-length(files.1)])  # do not read the readme files
files.2 <- paste0('S2_Dataset/', files.2[-length(files.2)])

ReadData <- function(file_name) {
  
  data <- read_csv(file_name, col_names = c("Time", "Acceleration.front", "Acceleration.vert","Acceleration.lat","Antenna","RSSI","Phase","Frequency","Activity"))
  # Adding columns by parsing filenames and directories
  file.name <- str_split(file_name, "/", simplify = TRUE, n = 5)
  data['Gender'] <- as.integer(ifelse(str_sub(file.name[5], -1) == "F", 0, 1))
  #data['Room ID'] <- as.integer(ifelse(str_sub(file.name[4], 1,2) == "S1", 1, 2))
  
  return(data)
}
data.1 <- do.call(rbind, lapply(files.1, ReadData))
data.2 <- do.call(rbind, lapply(files.2, ReadData))
data <- rbind(data.1, data.2)

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
