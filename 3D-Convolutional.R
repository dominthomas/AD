library(ggplot2)
library(neurobase)
library(keras)
library(tensorflow)

# Practice 3D-convolutional neural networks.
setwd("/home/dthomas/AD/")


cn_1 <- readnii("extracted_cn/sub-OAS30001_ses-d0129_run-01_T1wBrainExtractionBrain.nii.gz")
cn_2 <- readnii("extracted/sub-OAS30001_ses-d0757_run-01_T1wBrainExtractionBrain.nii.gz")


cn_1d <- cn_1@.Data
cn_2d <- cn_2@.Data

combined <- list(cn_1d, cn_2d)

combined <- append(combined, list(cn_1d))

S.new <- array(NA, dim=c(45, 81, 4, 52))
S.new[,,-4,]<- 5

arr <- array(rep(1:4, each=4), dim=c(2, 2, 2, 2))     # toy array
arr.new <- array(NA, dim=c(2, 2, 3, 2))      

arr <- array(rep(1:2, each=2), dim=c(2,2))
arr2 <- abind(arr, 1:2, along = 3 )

binded <- array(cn_1d, dim = c(dim(cn_1d), 176))

train_x <- runif(10*100*100*100*1, 0, 1)
train_x <- array_reshape(train_x, dim = c(10, 100, 100, 100, 1))

# Outcome data, binary
train_y <- to_categorical(sample(c(0,1,2), size = 10, replace = TRUE))

arr1 <- array(NA, dim=c(1,dim(cn_1d),1))
dim(arr1)

arr <- abind(cn_1d, cn_2d, along = 0)

arr2 <- array(c(1, cn_1d, 1), dim=c(1, dim(cn_1d),1))

arr <- array(c(c(1), cn_1d, c(1)), dim=c(1, dim(cn_1d),1))

arr <- array(c(cn_1d, 1), dim=c(dim(cn_1d),1))


arr <- array()
dim(arr) <- c(1,1,1,1)

arr <- abind(arr, array(c(cn_1d, 1), dim=c(dim(cn_1d), 1)), along = 0)


arr1 <- array(c(cn_1d, 1), dim=c(dim(cn_1d), 1))

arr <- abind(cn_1d, cn_2d, cn_1d, cn_2d, along =0)

arr <- array()

arr <- abind(arr, cn_1d, along = 0)
  
arr[1,,,,] <- array(cn_1d, 1)
arr[2,,,,] <- array(cn_2d, 1)

arr <- array(c(1:32), dim = c(1, 2, 2, 2, 1))

arr <- array(c(cn_1d, 1))

arr <- array(arr, )

