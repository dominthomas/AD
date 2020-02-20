library(stringr)
library(tensorflow)
library(keras)
library(abind)
library(neurobase)



########################################################
loss <- c()
accuracy <- c()
seeds <- 1:20
for(seed in seeds)
{
folders_ad <- list.files("/home/k1651915/ad/")
folders_cn <- list.files("/home/k1651915/cn/")

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
    
    nifti_file <- readnii(file)
    data <- slot(nifti_file, ".Data")
    array_4d <- array(c(data,1), dim=c(dim(data), 1))
    return_list <- append(return_list, list(array_4d))
  }
  return(return_list)
}

setwd("/home/k1651915/ad/")
folders <- list.files(".")

# Shuffle
set.seed(seed)
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

setwd("/home/k1651915/cn/")
folders <- list.files(".")
# Shuffle
set.seed(seed)
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

#tb_log <- "tb_log"
#tensorboard(tb_log)

# Create sequential model
model <- keras_model_sequential()

model %>% layer_conv_3d(
  filters = 32,
  kernel_size = c(7, 7, 7),
  input_shape = c(176, 256, 256, 1),
  strides = c(4, 4, 4),
  padding = "valid",
  data_format = 'channels_last',
  activation = 'relu') %>%
  
  layer_max_pooling_3d(pool_size = c(2, 2, 2),
                       strides = c(2, 2, 2),
                       padding = "valid") %>%
  
  layer_conv_3d(
    filters = 64,
    kernel_size = c(5, 5, 5),
    strides = c(1, 1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  
  layer_max_pooling_3d(pool_size = c(2, 2, 2),
                       strides = c(2, 2, 2),
                       padding = "valid") %>%
  
  layer_conv_3d(
    filters = 384,
    kernel_size = c(3, 3, 3),
    strides = c(1, 1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  
  layer_conv_3d(
    filters = 384,
    kernel_size = c(3, 3, 3),
    strides = c(1, 1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  
  layer_conv_3d(
    filters = 512,
    kernel_size = c(3, 3, 3),
    strides = c(1, 1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  
  layer_conv_3d(
    filters = 256,
    kernel_size = c(3, 3, 3),
    strides = c(1, 1, 1),
    padding = "valid",
    activation = "relu"
  ) %>%
  layer_max_pooling_3d(pool_size = c(2, 2, 2),
                       strides = c(2, 2, 2),
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
    batch_size = 400,
    validation_data = list(validation_data, validation_labels),
    shuffle = TRUE
    #callbacks = c(callback_tensorboard(
    #  log_dir = tb_log,
    #  embeddings_freq = 1,
    #  histogram_freq = 1
    #))
  )

# Test model
evaluation <- model %>% evaluate(test_data, test_labels, verbose = 0)
print(evaluation)
# Hehehehehehehehehe


loss <- c(loss, evaluation[[1]])
accuracy <- c(accuracy, evaluation[[2]])
}

print(mean(accuracy))

