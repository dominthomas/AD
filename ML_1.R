library(png)
library(stringr)
library(tensorflow)
library(keras)
library(abind)

setwd("/home/dthomas/AD/2D/AD/")
folders <- list.files(".")
ad_hippocampal <- list()

for(folder in folders)
{
  setwd(folder)
  files <- list.files(".")
  file_num_only <- c()
  
  for(file in files)
  {
    file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
  }
  
  file_num_only <- sort(as.numeric(file_num_only))
  png <- readPNG(paste(file_num_only[88], ".png", sep=""))
  gray <- png[,,1]
  dim(gray) <- c(480, 480, 1)
  ad_hippocampal <- append(ad_hippocampal, list(gray))
  setwd("../")
}


setwd("/home/dthomas/AD/2D/CN/")
folders <- list.files(".")
cn_hippocampal <- list()

for(folder in folders)
{
  if(length(cn_hippocampal) == 278)
  {
    break
  }
  else
  {
    setwd(folder)
    files <- list.files(".")
    file_num_only <- c()
    
    for(file in files)
    {
      file_num_only <- c(file_num_only, str_extract(file, '(\\d*)'))
    }
    
    file_num_only <- sort(as.numeric(file_num_only))
    png <- readPNG(paste(file_num_only[88], ".png", sep=""))
    gray <- png[,,1]
    dim(gray) <- c(480, 480, 1)
    cn_hippocampal <- append(cn_hippocampal, list(gray))
    setwd("../")
  }
}

set.seed(1382)
cn_hippocampal <- sample(cn_hippocampal)
set.seed(1382)
ad_hippocampal <- sample(ad_hippocampal)

train_data <- cn_hippocampal[1:150]
train_data <- append(train_data, ad_hippocampal[1:150])

validation_data <- cn_hippocampal[151:200]
validation_data <- append(validation_data, ad_hippocampal[151:200])
set.seed(182)
validation_data <- sample(validation_data)

test_data <- cn_hippocampal[201:278]
test_data <- append(test_data, ad_hippocampal[201:278])

# Create labels for train data.
x <- rep('0', 150) # CN
y <- rep('1', 150) # AD
train_labels <- c(x,y)

# Create labels for validation data.
x <- rep('0', 50) # CN
y <- rep('1', 50) # AD
validation_labels <- c(x,y)
set.seed(182)
validation_labels <- sample(validation_labels)



# Create labels for test data.
x <- rep('0', 78) # CN
y <- rep('1', 78) # AD
test_labels <- c(x,y)

train_labels <- to_categorical(train_labels)
validation_labels <- to_categorical(validation_labels)
test_labels <- to_categorical(test_labels)

# Turn train_data & test_data into a 5D array from a list of 4D arrays.
train_data <- abind(train_data, along=0)
validation_data <- abind(validation_data, along=0)
test_data <- abind(test_data, along=0)

# Remove unwanted data.
remove(cn_hippocampal, ad_hippocampal, png, folder, folders, files, file, file_num_only, x, y)
gc()

# Create sequential model
model <- keras_model_sequential()

model %>% layer_conv_2d(filters = 32, 
                        kernel_size = c(6,6),
                        input_shape=c(480, 480, 1),
                        data_format = 'channels_last',
                        activation = 'relu') %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(6,6),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(5,5)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(6,6),
                activation = 'relu') %>%
   layer_conv_2d(filters = 64,
                kernel_size = c(6,6),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(4,4)) %>%
  layer_dropout(rate = 0.20) %>%
  layer_flatten() %>% 
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.40) %>%
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
      epoch = 50,
      batch_size = 30,
      validation_data=list(validation_data, validation_labels),
      shuffle = TRUE) 

# Test model
model %>% evaluate(test_data, test_labels,verbose = 0)

