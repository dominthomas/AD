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

for(folder in folders_ad)
{
  sub_id <- str_extract(folder, "OAS\\d*")
  if(!(sub_id %in% sub_id_ad))
  {
    sub_id_ad <- c(sub_id_ad, sub_id)
  }
}

for(folder in folders_cn)
{
  sub_id <- str_extract(folder, "OAS\\d*")
  if(!(sub_id %in% sub_id_cn))
  {
    sub_id_cn <- c(sub_id_cn, sub_id)
  }
}

# Confirm there are no overlapping subjects.
any(duplicated(sub_id_cn, sub_id_ad))
# 178 AD subjects.
# 588 CN subjects.
########################################################



setwd("/home/dthomas/AD/2D/AD/")
folders <- list.files(".")

# Shuffle
set.seed(109)
sub_id_ad <- sample(sub_id_ad)
ad_sub_train <- sub_id_ad[1:112]
ad_sub_validate <- sub_id_ad[113: 124]
ad_sub_test <- sub_id_ad[125:178]

ad_sub_train_folders <- c()
ad_sub_validate_folders <- c()
ad_sub_test_folders <- c()

for(folder in folders)
{
  folder_sub_id <- str_extract(folder, "OAS\\d*")
  if(folder_sub_id %in% ad_sub_train)
  {
    ad_sub_train_folders <- c(ad_sub_train_folders, folder)
  }
  else if (folder_sub_id %in% ad_sub_validate)
  {
    ad_sub_validate_folders <- c(ad_sub_validate_folders, folder)
  }
  else if(folder_sub_id %in% ad_sub_test)
  {
    ad_sub_test_folders <- c(ad_sub_test_folders, folder)
  }
}

ad_train <- list()
ad_validate <- list()
ad_test <- list()

for(folder in ad_sub_train_folders)
{
  setwd(folder)
  files <- list.files(".")
  file_num_only <- c()
  
  for(file in files)
  {
    file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
  }
  
  file_num_only <- sort(as.numeric(file_num_only))
  png <- imrotate(load.image(paste(file_num_only[88], ".png", sep="")), angle = 180)
  png_rotate <- imrotate(png, angle = 3) %>% autocrop(c(0,0,0))
  png_rotate <- resize(png_rotate, w = 480, h = 480)
  
  #png_rotate2 <- imrotate(png, angle = -2)
  #png_rotate2 <- resize(png_rotate2, w = 480, h =480) %>% autocrop(c(0,0,0))
  
  #png_rotate3 <- imrotate(png, angle = 6)
  #png_rotate3 <- resize(png_rotate3, w = 480, h =480) 
  
  #png_rotate4 <- imrotate(png, angle = -6)
  #png_rotate4 <- resize(png_rotate4, w = 480, h =480)  
 
  png <- png %>% autocrop(c(0,0,0))
  gray <- png[,,,1]
  gray2 <- png_rotate[,,,1]
  #gray3 <- png_rotate2[,,,1]
  #gray4 <- png_rotate3[,,,1]
  #gray5 <- png_rotate4[,,,1]
  #v = (0.05*sd(png[,,,1]))^2
  #gray6= imnoise(png[,,,1], 'gaussian', 0, v);

  #png_noise = max(I[,,3])*noise 
  
  dim(gray) <- c(480, 480, 1)
  dim(gray2) <- c(480, 480, 1)
  #dim(gray3) <- c(480, 480, 1)
  #dim(gray4) <- c(480, 480, 1)
  #dim(gray5) <- c(480, 480, 1)
  #dim(gray6) <- c(480, 480, 1)
  
  ad_train <- append(ad_train, list(gray))
  ad_train <- append(ad_train, list(gray2))
  #ad_train <- append(ad_train, list(gray3))
  #ad_train <- append(ad_train, list(gray4))
  #ad_train <- append(ad_train, list(gray5))
  #ad_train <- append(ad_train, list(gray6))
  setwd("../")
}

for(folder in ad_sub_validate_folders)
{
  setwd(folder)
  files <- list.files(".")
  file_num_only <- c()
  
  for(file in files)
  {
    file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
  }
  
  file_num_only <- sort(as.numeric(file_num_only))
  png <- imrotate(load.image(paste(file_num_only[88], ".png", sep="")), angle = 180)
  
  png <- png %>% autocrop(c(0,0,0))
  gray <- png[,,,1]
  dim(gray) <- c(480, 480, 1)
  ad_validate <- append(ad_validate, list(gray))
  setwd("../")
}

for(folder in ad_sub_test_folders)
{
  setwd(folder)
  files <- list.files(".")
  file_num_only <- c()
  
  for(file in files)
  {
    file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
  }
  
  file_num_only <- sort(as.numeric(file_num_only))
  png <- imrotate(load.image(paste(file_num_only[88], ".png", sep="")), angle = 180)
  png <- png %>% autocrop(c(0,0,0))
  gray <- png[,,,1]
  dim(gray) <- c(480, 480, 1)
  ad_test<- append(ad_test, list(gray))
  setwd("../")
}

setwd("/home/dthomas/AD/2D/CN/")
folders <- list.files(".")

# Shuffle
set.seed(109)
sub_id_cn <- sample(sub_id_cn)
cn_sub_train <- sub_id_cn[1:112]
cn_sub_validate <- sub_id_cn[113: 124]
cn_sub_test <- sub_id_cn[125:178]

cn_sub_train_folders <- c()
cn_sub_validate_folders <- c()
cn_sub_test_folders <- c()

for(folder in folders)
{
  folder_sub_id <- str_extract(folder, "OAS\\d*")
  if(folder_sub_id %in% cn_sub_train)
  {
    cn_sub_train_folders <- c(cn_sub_train_folders, folder)
  }
  else if (folder_sub_id %in% cn_sub_validate)
  {
    cn_sub_validate_folders <- c(cn_sub_validate_folders, folder)
  }
  else if(folder_sub_id %in% cn_sub_test)
  {
    cn_sub_test_folders <- c(cn_sub_test_folders, folder)
  }
}

cn_train <- list()
cn_validate <- list()
cn_test <- list()

ad_train_length = length(ad_train)
for(folder in cn_sub_train_folders)
{
  if( abs(ad_train_length - length(cn_train)) <= 4)
  {
    break
  }
  else {
    setwd(folder)
    files <- list.files(".")
    file_num_only <- c()
    
    for(file in files)
    {
      file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
    }
    
    file_num_only <- sort(as.numeric(file_num_only))
    png <- imrotate(load.image(paste(file_num_only[88], ".png", sep="")), angle = 180)
    png_rotate <- imrotate(png, angle = 3) %>% autocrop(c(0,0,0))
    png_rotate <- resize(png_rotate, w = 480, h =480)
    
    #png_rotate2 <- imrotate(png, angle = -2)
    #png_rotate2 <- resize(png_rotate2, w = 480, h =480)
    
    #png_rotate3 <- imrotate(png, angle = 6)
    #png_rotate3 <- resize(png_rotate3, w = 480, h =480) 
    
    #png_rotate4 <- imrotate(png, angle = -6)
    #png_rotate4 <- resize(png_rotate4, w = 480, h =480)  
  
    png <- png %>% autocrop(c(0,0,0))
    gray <- png[,,,1]
    gray2 <- png_rotate[,,,1]
    #gray3 <- png_rotate2[,,,1]
    #gray4 <- png_rotate3[,,,1]
    #gray5 <- png_rotate4[,,,1]
    #v = (0.05*sd(png[,,,1]))^2
    #gray6= imnoise(png[,,,1], 'gaussian', 0, v);
  
    dim(gray) <- c(480, 480, 1)
    dim(gray2) <- c(480, 480, 1)
    #dim(gray3) <- c(480, 480, 1)
    #dim(gray4) <- c(480, 480, 1)
    #dim(gray5) <- c(480, 480, 1)
    #dim(gray6) <- c(480, 480, 1)
    
    cn_train <- append(cn_train, list(gray))
    cn_train <- append(cn_train, list(gray2))
    #cn_train <- append(cn_train, list(gray3))
    #cn_train <- append(cn_train, list(gray4))
    #cn_train <- append(cn_train, list(gray5))
    #cn_train <- append(cn_train, list(gray6))
    setwd("../")
  }
}

ad_validate_length = length(ad_validate)
for(folder in cn_sub_validate_folders)
{
  if(abs(ad_validate_length - length(cn_validate)) <= 2)
  {
    break
  }
  else {
    setwd(folder)
    files <- list.files(".")
    file_num_only <- c()
    
    for(file in files)
    {
      file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
    }
    
    file_num_only <- sort(as.numeric(file_num_only))
    png <- imrotate(load.image(paste(file_num_only[88], ".png", sep="")), angle = 180)
    png <- png %>% autocrop(c(0,0,0))
    gray <- png[,,,1]
    dim(gray) <- c(480, 480, 1)
    cn_validate <- append(cn_validate, list(gray))
    setwd("../")
    }
}

ad_test_length <- length(ad_test)
for(folder in cn_sub_test_folders)
{
  if(abs(ad_test_length - length(cn_test)) <= 2)
  {
    break
  }
  else {
    setwd(folder)
    files <- list.files(".")
    file_num_only <- c()
    
    for(file in files)
    {
      file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
    }
    
    file_num_only <- sort(as.numeric(file_num_only))
    png <- imrotate(load.image(paste(file_num_only[88], ".png", sep="")), angle = 180)
    png <- png %>% autocrop(c(0,0,0))
    gray <- png[,,,1]
    dim(gray) <- c(480, 480, 1)
    cn_test <- append(cn_test, list(gray))
    setwd("../")
    }
}


#set.seed(1382)
#cn_train <- sample(cn_train)
#set.seed(1382)
#ad_train <- sample(ad_train)

train_data <- cn_train
train_data <- append(train_data, ad_train)

validation_data <- cn_validate
validation_data <- append(validation_data, ad_validate)
set.seed(182)
validation_data <- sample(validation_data)

test_data <- cn_test
test_data <- append(test_data, ad_test)

# Create labels for train data.
x <- rep('0', length(cn_train)) # CN
y <- rep('1', length(ad_train)) # AD
train_labels <- c(x,y)

# Create labels for validation data.
x <- rep('0', length(cn_validate)) # CN
y <- rep('1', length(ad_validate)) # AD
validation_labels <- c(x,y)
set.seed(182)
validation_labels <- sample(validation_labels)

# Create labels for test data.# Test model

x <- rep('0', length(cn_test)) # CN
y <- rep('1', length(ad_test)) # AD
test_labels <- c(x,y)

train_labels <- to_categorical(train_labels)
validation_labels <- to_categorical(validation_labels)
test_labels <- to_categorical(test_labels)

# Turn train_data & test_data into a 5D array from a list of 4D arrays.
train_data <- abind(train_data, along=0)
validation_data <- abind(validation_data, along=0)
test_data <- abind(test_data, along=0)

# Remove unwanted data.
remove(cn_train, ad_train, png, folder, folders, files, file, file_num_only, x, y)
remove(gray, gray2, gray3, gray4, gray5, png_rotate, png_rotate2, png_rotate3, png_rotate4)
remove(ad_test, ad_validate, cn_test, cn_validate)
remove(ad_sub_test, ad_sub_test_folders, ad_sub_train, ad_sub_train_folders, ad_sub_validate, ad_sub_validate_folders, ad_test_length, ad_train_length, ad_validate_length)
remove(cn_sub_test, cn_sub_test_folders, cn_sub_train, cn_sub_train_folders, cn_sub_validate, cn_sub_validate_folders)
remove(folder_sub_id, folders_ad, folders_cn, sub_id, sub_id_ad, sub_id_cn)
gc()

# Create sequential model
model <- keras_model_sequential()

model %>% layer_conv_2d(filters = 32, 
                        kernel_size = c(6,6),
                        input_shape=c(480, 480, 1),
                        strides = c(4,4),
                        padding = "valid",
                        data_format = 'channels_last',
                        activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2),
                       strides = c(2,2),
                       padding = "valid") %>%
  
  layer_conv_2d(filters = 64, 
                kernel_size = c(5,5),
                strides = c(1,1),
                padding = "valid",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2),
                       strides = c(2,2),
                       padding = "valid") %>%
  
  layer_conv_2d(filters = 384, 
                kernel_size = c(3,3),
                strides = c(1,1),
                padding = "valid",
                activation = "relu") %>%
  
  layer_conv_2d(filters = 384, 
                kernel_size = c(3,3),
                strides = c(1,1),
                padding = "valid",
                activation = "relu") %>%
  
  layer_conv_2d(filters = 256, 
                kernel_size = c(3,3),
                strides = c(1,1),
                padding = "valid",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2),
                       strides = c(2,2),
                       padding = "valid") %>%
  
  layer_flatten() %>%
    #layer_dense(units = 4096, activation = "relu") %>%
    #layer_dropout(rate = 0.4) %>%
  
    #layer_dense(units = 4096, activation = "relu") %>%
    #layer_dropout(rate = 0.4) %>%
    
    layer_dense(units = 32, activation = "relu") %>%
    #layer_dense(units = 32, activation = "relu") %>%
    #layer_dropout(rate = 0.2) %>%
  
  layer_dense(units = 2, activation = 'softmax')

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
  fit(train_data,
      train_labels,
      epoch = 150,
      batch_size = 100,
      validation_data=list(validation_data, validation_labels),
      shuffle = TRUE) 

# Test model
model %>% evaluate(test_data, test_labels,verbose = 0)

