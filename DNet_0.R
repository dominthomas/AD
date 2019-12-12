library(stringr)
library(tensorflow)
library(keras)
library(abind)
library(imager)
library(EBImage)
#library(SpatialPack)



########################################################
folders_ad <- list.files("/home/dthomas/AD/2D/AD/")
folders_cn <- list.files("/home/dthomas/AD/2D/CN/")

sub_id_ad <- c()
sub_id_cn <- c()

for (folder in folders_ad)
{
  sub_id <- str_extract(folder, "OAS\\d*")
  if (!(sub_id %in% sub_id_ad))
  {
    sub_id_ad <- c(sub_id_ad, sub_id)
  }
}

for (folder in folders_cn)
{
  sub_id <- str_extract(folder, "OAS\\d*")
  if (!(sub_id %in% sub_id_cn))
  {
    sub_id_cn <- c(sub_id_cn, sub_id)
  }
}

# Confirm there are no overlapping subjects.
any(duplicated(sub_id_cn, sub_id_ad))
# 178 AD subjects.
# 588 CN subjects.
########################################################

get_images <- function(folders, train = FALSE, cn = FALSE, ad_data_length = 0)
{
  return_list <- list()
  
  for (folder in folders)
  {
    if(cn && abs(length(return_list) - ad_data_length) <= 3)
    {
      break
    }
    setwd(folder)
    files <- list.files(".")
    file_num_only <- c()
    
    for (file in files)
    {
      file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
    }
    
    file_num_only <- sort(as.numeric(file_num_only))
    png <- paste(file_num_only[88], ".png", sep = "") %>%
      load.image()
    
    if (train)
    {
      png_rotate <- imrotate(png, angle = 3) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      png_rotate <- imrotate(png, angle = -4) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
    }
    png <- png %>%
      autocrop(c(0, 0, 0)) %>%
      resize(w = 227, h = 227)
    
    gray <- png[, , , 1]
    dim(gray) <- c(227, 227, 1)
    return_list <- append(return_list, list(gray))
    setwd("../")
  }
  return(return_list)
}

setwd("/home/dthomas/AD/2D/AD/")
folders <- list.files(".")

# Shuffle
set.seed(10945)
sub_id_ad <- sample(sub_id_ad)
ad_sub_train <- sub_id_ad[1:112]
ad_sub_validate <- sub_id_ad[113:124]
ad_sub_test <- sub_id_ad[125:178]

ad_sub_train_folders <- c()
ad_sub_validate_folders <- c()
ad_sub_test_folders <- c()

for (folder in folders)
{
  folder_sub_id <- str_extract(folder, "OAS\\d*")
  if (folder_sub_id %in% ad_sub_train)
  {
    ad_sub_train_folders <- c(ad_sub_train_folders, folder)
  }
  else if (folder_sub_id %in% ad_sub_validate)
  {
    ad_sub_validate_folders <- c(ad_sub_validate_folders, folder)
  }
  else if (folder_sub_id %in% ad_sub_test)
  {
    ad_sub_test_folders <- c(ad_sub_test_folders, folder)
  }
}

ad_train <- get_images(ad_sub_train_folders, TRUE)
ad_validate <- get_images(ad_sub_validate_folders)
ad_test <- get_images(ad_sub_test_folders)

setwd("/home/dthomas/AD/2D/CN/")
folders <- list.files(".")
# Shuffle
set.seed(10679)
sub_id_cn <- sample(sub_id_cn)
cn_sub_train <- sub_id_cn[1:112]
cn_sub_validate <- sub_id_cn[113:124]
cn_sub_test <- sub_id_cn[125:178]

cn_sub_train_folders <- c()
cn_sub_validate_folders <- c()
cn_sub_test_folders <- c()

for (folder in folders)
{
  folder_sub_id <- str_extract(folder, "OAS\\d*")
  if (folder_sub_id %in% cn_sub_train)
  {
    cn_sub_train_folders <- c(cn_sub_train_folders, folder)
  }
  else if (folder_sub_id %in% cn_sub_validate)
  {
    cn_sub_validate_folders <- c(cn_sub_validate_folders, folder)
  }
  else if (folder_sub_id %in% cn_sub_test)
  {
    cn_sub_test_folders <- c(cn_sub_test_folders, folder)
  }
}

cn_train <- get_images(cn_sub_train_folders, TRUE, TRUE, length(ad_train))
cn_validate <- get_images(cn_sub_validate_folders, cn = TRUE, ad_data_length = length(ad_validate))
cn_test <- get_images(cn_sub_test_folders, cn = TRUE, ad_data_length = length(ad_test))

train_data <- cn_train
train_data <- append(train_data, ad_train)

validation_data <- cn_validate
validation_data <- append(validation_data, ad_validate)
#set.seed(182)
#validation_data <- sample(validation_data)

test_data <- cn_test
test_data <- append(test_data, ad_test)

# Create labels for train data.
x <- rep('0', length(cn_train)) # CN
y <- rep('1', length(ad_train)) # AD
train_labels <- c(x, y)

# Create labels for validation data.
x <- rep('0', length(cn_validate)) # CN
y <- rep('1', length(ad_validate)) # AD
validation_labels <- c(x, y)
#set.seed(182)
#validation_labels <- sample(validation_labels)

# Create labels for test data.# Test model

x <- rep('0', length(cn_test)) # CN
y <- rep('1', length(ad_test)) # AD
test_labels <- c(x, y)

train_labels <- as.numeric(train_labels)
validation_labels <- as.numeric(validation_labels)
test_labels <- as.numeric(test_labels)

# Turn train_data & test_data into a 5D array from a list of 4D arrays.
train_data <- abind(train_data, along = 0)
validation_data <- abind(validation_data, along = 0)
test_data <- abind(test_data, along = 0)

# Remove unwanted data.
remove(cn_train,
       ad_train,
       folder,
       folders,
       x,
       y)

remove(ad_test, ad_validate, cn_test, cn_validate)

remove(
  ad_sub_test,
  ad_sub_test_folders,
  ad_sub_train,
  ad_sub_train_folders,
  ad_sub_validate,
  ad_sub_validate_folders)

remove(
  cn_sub_test,
  cn_sub_test_folders,
  cn_sub_train,
  cn_sub_train_folders,
  cn_sub_validate,
  cn_sub_validate_folders)

remove(folder_sub_id,
       folders_ad,
       folders_cn,
       sub_id,
       sub_id_ad,
       sub_id_cn)
gc()

# Create sequential model
model <- keras_model_sequential()

model %>% layer_conv_2d(
  filters = 32,
  kernel_size = c(7, 7),
  input_shape = c(227, 227, 1),
  strides = c(4, 4),
  padding = "valid",
  data_format = 'channels_last',
  activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),
                       strides = c(2, 2),
                       padding = "valid") %>%
  
  layer_conv_2d(
    filters = 64,
    kernel_size = c(5, 5),
    strides = c(1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2),
                       strides = c(2, 2),
                       padding = "valid") %>%
  
  layer_conv_2d(
    filters = 384,
    kernel_size = c(3, 3),
    strides = c(1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  
  layer_conv_2d(
    filters = 384,
    kernel_size = c(3, 3),
    strides = c(1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  
  layer_conv_2d(
    filters = 256,
    kernel_size = c(3, 3),
    strides = c(1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2),
                       strides = c(2, 2),
                       padding = "valid") %>%
  
  layer_flatten() %>%
  #layer_dense(units = 4096, activation = "relu") %>%
  #layer_dropout(rate = 0.4) %>%
  
  #layer_dense(units = 4096, activation = "relu") %>%
  #layer_dropout(rate = 0.4) %>%
  
  #layer_dense(units = 1000, activation = "relu") %>%
  #layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = "relu") %>%
  
  layer_dense(units = 1, activation = 'sigmoid')

# Compile
model %>%
  compile(loss = 'binary_crossentropy',
          #optimizer = optimizer_sgd(lr = 0.015,
          #                          momentum = 0.9,
          #                          decay = 0.0005,
          #                          nesterov = T),
          optimizer = "adam",
          metrics = 'accuracy')

# Fit model
history <- model %>%
  fit(
    train_data,
    train_labels,
    epoch = 50,
    batch_size = 100,
    validation_data = list(validation_data, validation_labels),
    shuffle = TRUE
  )

# Test model
model %>% evaluate(test_data, test_labels, verbose = 0)

