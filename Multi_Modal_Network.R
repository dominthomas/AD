library(stringr)
library(tensorflow)
library(keras)
library(abind)
library(imager)
library(EBImage)
#library(SpatialPack)



########################################################
loss <- c()
accuracy <- c()
seeds <- 1:500
for(seed in seeds)
{
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

get_images <- function(folders, train = FALSE, same_length = FALSE, data_length = 0, adni = FALSE)
{
  # Since three slices are used as default, without image augmentation.
  data_length <- data_length*3
  
  return_list <- list()
  
  for (folder in folders)
  {
    if(same_length && length(return_list) == data_length)
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
    png2 <- paste(file_num_only[87], ".png", sep = "") %>%
      load.image()
    png3 <- paste(file_num_only[89], ".png", sep = "") %>%
      load.image()
    
    if(adni){
      
      png <- imrotate(png, angle = 90)
      png2 <- imrotate(png2, angle = 90)
      png3 <- imrotate(png3, angle = 90)
      
    }
    
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
      
      png_rotate <- imrotate(png2, angle = 3) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      png_rotate <- imrotate(png2, angle = -4) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      png_rotate <- imrotate(png3, angle = 3) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      png_rotate <- imrotate(png3, angle = -4) %>%
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
    
    png <- png2 %>%
      autocrop(c(0, 0, 0)) %>%
      resize(w = 227, h = 227)
    
    gray <- png[, , , 1]
    dim(gray) <- c(227, 227, 1)
    return_list <- append(return_list, list(gray))
    
    png <- png3 %>%
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
set.seed(seed)
sub_id_ad <- sample(sub_id_ad)
ad_sub_train <- sub_id_ad[1:165]
ad_sub_validate <- sub_id_ad[166:170]
ad_sub_test <- sub_id_ad[171:178]

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

ad_train <- get_images(ad_sub_train_folders)
#ad_validate <- get_images(ad_sub_validate_folders, same_length = TRUE, data_length = 5)
#ad_test <- get_images(ad_sub_test_folders, same_length = TRUE, data_length = 8)

setwd("/home/dthomas/AD/2D/CN/")
folders <- list.files(".")
# Shuffle
set.seed(seed)
sub_id_cn <- sample(sub_id_cn)
cn_sub_train <- sub_id_cn[1:574]
cn_sub_validate <- sub_id_cn[575:580]
cn_sub_test <- sub_id_cn[581:588]

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

cn_train <- get_images(cn_sub_train_folders)
#cn_validate <- get_images(cn_sub_validate_folders, same_length = TRUE, data_length = 5)
#cn_test <- get_images(cn_sub_test_folders, same_length = TRUE, data_length = 8)

############ADNI Data Set, all data will be used for training############
adni_folders_ad <- list.files("/home/dthomas/AD/ADNI/2D/AD_3T/")
adni_folders_cn <- list.files("/home/dthomas/AD/ADNI/2D/CN_3T/")

setwd("/home/dthomas/AD/ADNI/2D/AD_3T/")
adni_ad_train <- get_images(adni_folders_ad, adni = TRUE)
setwd("/home/dthomas/AD/ADNI/2D/CN_3T/")
adni_cn_train <- get_images(adni_folders_cn, adni = TRUE)

train_data <- cn_train
train_data <- append(train_data, ad_train)
train_data <- append(train_data, adni_cn_train)
train_data <- append(train_data, adni_ad_train)

#validation_data <- cn_validate
#validation_data <- append(validation_data, ad_validate)
#set.seed(182)
#validation_data <- sample(validation_data)

#test_data <- cn_test
#test_data <- append(test_data, ad_test)

# Create labels for train data.
x <- rep('0', length(cn_train)) # CN
y <- rep('1', length(ad_train)) # AD
x2 <- rep('0', length(adni_cn_train)) # CN
y2 <- rep('1', length(adni_ad_train)) # AD
train_labels <- c(x, y, x2, y2)

# Create labels for validation data.
#x <- rep('0', length(cn_validate)) # CN
#y <- rep('1', length(ad_validate)) # AD
#validation_labels <- c(x, y)
#set.seed(182)
#validation_labels <- sample(validation_labels)

# Create labels for test data.# Test model

#x <- rep('0', length(cn_test)) # CN
#y <- rep('1', length(ad_test)) # AD
#test_labels <- c(x, y)

train_labels <- as.numeric(train_labels)
#validation_labels <- as.numeric(validation_labels)
#test_labels <- as.numeric(test_labels)

# Turn train_data & test_data into a 5D array from a list of 4D arrays.
train_data <- abind(train_data, along = 0)
#validation_data <- abind(validation_data, along = 0)
#test_data <- abind(test_data, along = 0)

# Remove unwanted data.
remove(cn_train,
       ad_train,
       folder,
       folders,
       x,
       y,
       x2,
       y2,
       adni_cn_train,
       adni_ad_train)

#remove(ad_test, ad_validate, cn_test, cn_validate)

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

#tb_log <- "tb_log"
#tensorboard(tb_log)

first_input <- layer_input(shape = c(227, 227, 1))

pred1 <- first_input %>%
  layer_conv_2d(
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
    filters = 512,
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
  layer_flatten()

second_input <- layer_input(shape = c(227, 227, 1))

pred2 <- second_input %>%
  layer_conv_2d(
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
    filters = 512,
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
  layer_flatten()


merged <- layer_concatenate(c(pred1, pred2)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')

model <- keras_model(
  inputs = c(first_input, second_input),
  outputs = c(merged)
)

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
    x = list(train_data, train_data),
    train_labels,
    epoch = 50,
    batch_size = 20,
    shuffle = TRUE
  )

# Test model
evaluation <- model %>% evaluate(test_data, test_labels, verbose = 0)
print(evaluation)
# Hehehehehehehehehe


loss <- c(loss, evaluation[[1]])
accuracy <- c(accuracy, evaluation[[2]])

print(mean(accuracy))
write(evaluation[[2]], file="/home/dthomas/R_accuracy.txt", append = TRUE)

remove(train_data, validation_data, test_data, model)
gc()

}

