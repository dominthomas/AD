library(neurobase)
#library(ggplot2)
library(tensorflow)
library(keras)
library(abind)
library(foreach)
library(doParallel)
registerDoParallel(16)

# Learn Machine Learning

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# rescale
x_train <- x_train / 255
x_test <- x_test / 255


y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

model <- keras_model_sequential()
model %> %
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)


# Practice run on sample data.
setwd("/home/dthomas/AD/")
cn_all <- list.files("3T_extracted_cn/")
ad_all <- list.files("3T_extracted_ad//")

setwd("3T_extracted_cn//")


train_data <- list()




#####################################################TO DELETE##########################################################
dimensions <- c()
foreach(file = cn_all) %dopar%
{
  nifti_file <- readnii(file)
  bob <- as.array(dim(nifti_file))
  dimensions <- c(dimensions, c(bob))
}
setwd("../3T_extracted_ad/")

for(file in ad_all)
{
  nifti_file <- readnii(file)
  bob <- as.array(dim(nifti_file))
  dimensions <- c(dimensions, bob)
}
setwd("../3T_extracted_cn/")
####################################################TO DELETE##########################################################

# Remove data that isn't 176 x 256 x 256
train_data <- list()
for(file in cn_all)
{
  # Initially, read in 50 CN images.
  if(length(train_data) == 25)
  {
    break
  }
  nifti_file <- readnii(file)
  bob <- as.array(dim(nifti_file))
  bsum <- sum(match(c(176, 256, 256), bob))
  if(!is.na(bsum) && bsum == 5)
  {
    # Reshape the array with dimensions number of subjects * x * y * z * channels,
    # where x*y*z is the dimensions of the MRI cube of image data.
    d <- slot(nifti_file, ".Data")
    array_4d <- array(c(d,1), dim=c(dim(d),1))
    train_data <- append(train_data, list(array_4d))
  }
  else {
    print("Bad")
    print(bob)
  }
}

setwd("../extracted_ad/")
for(file in ad_all)
{
  # Read in 50 more AD images.
  if(length(train_data) == 50)
  {
    break
  }
  nifti_file <- readnii(file)
  bob <- as.array(dim(nifti_file))
  bsum <- sum(match(c(176, 256, 256), bob))
  if(!is.na(bsum) && bsum == 5)
  {
    d <- slot(nifti_file, ".Data")
    array_4d <- array(c(d,1), dim=c(dim(d),1))
    train_data <- append(train_data, list(array_4d))
  }
}
setwd("../")

test_data <- train_data[1:5]
test_data <- append(test_data, train_data[26: 30])

training <- train_data[6:25]
training <- append(training, train_data[31:50])
remove(train_data)
train_data <- training
remove(training)

# Create labels for test data.
x <- rep('0', 5) # CN
y <- rep('1', 5) # AD
test_labels <- c(x,y)

# Create labels for train data.
x <- rep('0', 20) # CN
y <- rep('1', 20) # AD
train_labels <- c(x,y)

# Turn train_data & test_data into a 5D array from a list of 4D arrays.
train_data <- abind(train_data, along=0)
gc() # Perform GC.
test_data <- abind(test_data, along=0)
gc() # Perform GC.

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

gc() # Perform GC.

# Remove unwanted files to save memory.
remove(cn_all, ad_all, nifti_file, bob, bsum, d, array_4d, x, y)

# Create sequential model
model <- keras_model_sequential()

model %>% layer_conv_3d(filters = 16, 
                        kernel_size = c(7,7,7),
                        input_shape=c(176, 256, 256, 1),
                        data_format = 'channels_last') %>%
  layer_flatten() %>% 
  layer_dense(units = 2, activation = 'softmax')

# Compile
model %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')
 # Fit model
history <- model %>%
  fit(train_data,
      train_labels,
      epoch = 200,
      batch_size = 2,
      validation_split = 0.2)

