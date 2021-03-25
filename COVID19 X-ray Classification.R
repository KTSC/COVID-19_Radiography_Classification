##########################################################
# Download data set and split into train and validation set
##########################################################



# Get the working directory
cwd <- getwd()


# Loading the libraries (installing if required)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tensorflow)) install.packages("tensorflow", repos = "http://cran.us.r-project.org")
if(!require(keras)) install.packages("keras", repos = "http://cran.us.r-project.org")
if(!require(reticulate)) install.packages("reticulate", repos = "http://cran.us.r-project.org")

# Package for image processing and analysis. 
if(!require(BiocManager)) install.packages("BiocManager", repos = "http://cran.us.r-project.org")
BiocManager::install("EBImage")

library(reticulate)
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(tensorflow)
library(keras)
library(EBImage)
library(tidyverse)




# Set the location and get the name of the directories with the images corresponding to the classes
# For this specific dataset the code below will assign the train and validation folders (with 90% and 10% of the dataset respectively)
# to the variables "train_dir" and "test_dir"

image_location <- paste(getwd(), "/COVID-19 Radiography Database", sep="") # smaller dataset



# for the case where all data is separated by class, but no train/test split done.
# classes_dir <- list.dirs(path = image_location, full.names = TRUE, recursive = FALSE)
# classes_dir


# For the case where train and test are in separate directories already: 
split_dir <- list.dirs(path = image_location, full.names = TRUE, recursive = FALSE)

test_dir <- split_dir[1] # in alphabetical order, test is the first directory
train_dir <- split_dir[2]

# train_dir
classes_dir_train <- list.dirs(path = train_dir, full.names = TRUE, recursive = FALSE)
classes_labels <- c("COVID-19", "Normal","Other Viral Pneumoniae")
classes_dir_test <- list.dirs(path = test_dir, full.names = TRUE, recursive = FALSE)

# Get the number of output classes
# n_classes <- length(classes_dir)
# n_classes


# Get the size of the entire data set

dataset_size <- length(list.files(path = image_location, 
                                  full.names = TRUE, 
                                  recursive = TRUE))


n_train_cov <- length(list.files(path = classes_dir_train[1], 
                                 full.names = TRUE, 
                                 recursive = TRUE))

n_train_nor <- length(list.files(path = classes_dir_train[2], 
                                 full.names = TRUE, 
                                 recursive = TRUE))

n_train_vir <- length(list.files(path = classes_dir_train[3], 
                                 full.names = TRUE, 
                                 recursive = TRUE))

n_test_cov <- length(list.files(path = classes_dir_test[1], 
                                 full.names = TRUE, 
                                 recursive = TRUE))

n_test_nor <- length(list.files(path = classes_dir_test[2], 
                                 full.names = TRUE, 
                                 recursive = TRUE))

n_test_vir <- length(list.files(path = classes_dir_test[3], 
                                 full.names = TRUE, 
                                 recursive = TRUE))


train_count <- c(n_train_cov, n_train_nor, n_train_vir)
test_count <- c(n_test_cov, n_test_nor, n_test_vir)

# count_bind <- cbind(train_count, test_count)
sum(train_count)

# Create a data.frame for later visualization of the classes in the train set
files_count_train <- data.frame(Diagnosis = classes_labels,
                                Count = train_count)

# Create a data.frame for later visualization of the classes in the test set
files_count_test <- data.frame(Diagnosis = classes_labels,
                               Count = test_count)

# Create a data.frame for later visualization of the classes in both partitions
files_count <- data.frame( Partition = rep(c("Train", "Test"), each = 3),
                           Diagnosis = rep(classes_labels, 2),
                           Count = c(train_count, test_count), 
                           label_y_position = cumsum(c(train_count, test_count)))




# Plot the count of files per class, showing there's approximately the same number of observations for each diagnosis. 

# Barplot of the frequency of images for each class in the train set
ggplot(files_count_train, aes(x = Diagnosis, y = Count, fill = Diagnosis)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(y = Count, label = Count), 
            vjust=1.6, 
            color = "white")+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal(base_size = 10, base_family = "Caviar Dreams")

# Barplot of the frequency of images for each class in the test set
ggplot(files_count_test, aes(x = Diagnosis, y = Count, fill = Diagnosis)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(y = Count, label = Count), 
            vjust=1.6, 
            color = "white")+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal(base_size = 12, base_family = "Caviar Dreams")





# Barplot of the Train and Test sets showing the proportionality between them. 

ggplot(files_count, aes(x = Partition, y = Count, fill = Diagnosis)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y_position, label = Count), 
            vjust=1.6, 
            color = "white") + 
  theme_minimal(base_size = 12, base_family = "Caviar Dreams")



# Size of the training data set (just an integer as output)
n_samples <- length(list.files(path = train_dir, 
                        full.names = TRUE, 
                        recursive = TRUE))


# Set the dimensions for the transformation to be applied to all images, which can come in several formats

img_height <- 331
img_width <- 331
target_size <- c(img_height, img_width)


# Set the number of color channels. As the Radiographies are in grayscale, we set it to 1. 
channels <- 1 





# Take a look at one of the images: 

one_image_path <- list.files(path = classes_dir_train[2], 
                             full.names = TRUE, 
                             recursive = TRUE)[1]




# We can use EBImage to visualize the files. 
# Function call to read one of the images named in the objects with the classes 
radiography_view <- EBImage::readImage(one_image_path[1])

# Display the values of the first 5 rows and 6 columns read above 
print(max(radiography_view))

# The images are grayscale already, hence only one channel of color. 
# In the case of image 1 (a covid-19 patient) it's a 256x256 array of depth 1. 
EBImage::getFrames(radiography_view, type="total")

# Display the image
EBImage::display(radiography_view)


##########################################################
# Create the image generators for keras
##########################################################



# Keras loads the pictures from a directory or a URL. The function below loads, and can also resize the picture and normalize the values. 
# This dataset is already normalized so there's no need to perform normalization. Still I comment below the data augmentation parameters.  
train_data_gen <- keras::image_data_generator(
  rotation_range = 5,
  width_shift_range = 0.05,
  height_shift_range = 0.05,
  zoom_range = 0.1,
  fill_mode = "wrap",
  horizontal_flip = FALSE,
  vertical_flip = FALSE,
  rescale = 1./255,
  validation_split = 0.1 # creates a subset of data ("training" or "validation") if this parameter is set.
)


# Also for the validation data set, only for normalization
valid_data_gen <- keras::image_data_generator(
  rescale = 1./255, 
  validation_split = 0.1
)


test_data_gen <- keras::image_data_generator(
  rescale = 1./255, 
)


# Parameters for training

batch_size <- 32
epochs <- 50



# The images generated below are for the first model, which takes in grayscale images, so "_BW" is added to each name in comparison to the other images that will come next. 




train_images_BW <- flow_images_from_directory(
  directory = train_dir,
  generator = train_data_gen,
  target_size = target_size,
  color_mode = "grayscale",
  class_mode = "categorical",
  batch_size = batch_size,
  shuffle = TRUE,
  seed = NULL,
  save_to_dir = NULL,
  save_prefix = "",
  save_format = "png",
  follow_links = FALSE,
  subset = "training",
  interpolation = "bicubic"
)

valid_images_BW <- flow_images_from_directory(
  directory = train_dir,
  generator = valid_data_gen,
  target_size = target_size,
  color_mode = "grayscale",
  class_mode = "categorical",
  batch_size = batch_size,
  shuffle = TRUE,
  seed = NULL,
  save_to_dir = NULL,
  save_prefix = "",
  save_format = "png",
  follow_links = FALSE,
  subset = "validation",
  interpolation = "bicubic"
)

test_images_BW <- flow_images_from_directory(
  directory = test_dir,
  generator = test_data_gen,
  target_size = target_size,
  color_mode = "grayscale",
  class_mode = "categorical",
  batch_size = batch_size,
  shuffle = TRUE,
  seed = NULL,
  save_to_dir = NULL,
  save_prefix = "",
  save_format = "png",
  follow_links = FALSE,
  interpolation = "bicubic"
)


##########################################################
# Define model 1
##########################################################


input_shape <- c(img_height, img_width, channels)


model_x_ray <- keras::keras_model_sequential()

model_x_ray %>% 
  keras::layer_conv_2d(filters = 40,
                       kernel_size = c(3,3), 
                       activation = "relu", 
                       input_shape = input_shape) %>% #  # c(100,100,1)
  keras::layer_conv_2d(filters = 40,
                kernel_size = c(3,3), 
                activation = "relu") %>%
  keras::layer_max_pooling_2d(pool_size = c(3,3)) %>%
  keras::layer_dropout(rate = 0.2) %>%
  keras::layer_conv_2d(filters = 80,
                kernel_size = c(3,3), 
                activation = "relu") %>%
  keras::layer_conv_2d(filters = 80, 
                kernel_size = c(3,3), 
                activation = "relu") %>%
  keras::layer_max_pooling_2d(pool_size = c(3,3)) %>%
  keras::layer_dropout(rate = 0.2) %>%
  keras::layer_flatten() %>%
  keras::layer_dense(units = 256, 
              activation = "relu") %>%
  keras::layer_dropout(rate = 0.2) %>%
  keras::layer_dense(units = 3, 
              activation = "softmax") %>%
  keras::compile(loss = "categorical_crossentropy", 
          optimizer = keras::optimizer_adam(lr = 0.0005), 
          metrics = list(tf$keras$metrics$Accuracy, 
                         tf$keras$metrics$Precision,
                         tf$keras$metrics$Recall, 
                         tf$keras$metrics$AUC
                         )
          )


# Fit Model 1


# calculate a good number of steps per epoch by dividing the validation set by the batch size. 
train_samples <- train_images_BW$n

# steps_per_epoch <- 3498%/%batch_size # 3103
steps_per_epoch <- as.integer(train_samples%/%batch_size)

# as the validation data is also a generator, calculate the validation steps similarly, but with the validation set
valid_samples <- valid_images_BW$n

# validation_steps <- 388%/%batch_size # 343
validation_steps <- as.integer(valid_samples%/%batch_size)






################### Train the model #########################

# Set the patience and minimum delta for early stopping


patience <- 8
min_delta <- 0.01



history_x_ray <- model_x_ray %>% 
  keras::fit_generator(generator = train_images,
                       steps_per_epoch = steps_per_epoch,
                       epochs = epochs, 
                       validation_data = valid_images, 
                       validation_steps = validation_steps, 
                       callbacks = callback_early_stopping( 
                         monitor = "val_accuracy",
                         min_delta = min_delta,
                         patience = patience,
                         verbose = 0,
                         mode = c("auto"),
                         baseline = NULL
                         )
                       )



save_model_weights_tf(model_x_ray, cwd)

model_path_x_ray <- paste(getwd(), "/model_x_ray.h5", sep="")

model_x_ray <- load_model_tf(model_path_x_ray) %>% load_model_weights_tf(model_path_x_ray)

# To view the summary of the model

model_x_ray




history_df <- as.data.frame(model_x_ray$history$metrics) 

str(history_df)

df_out <- history$metrics %>% 
  {data.frame(accuracy = .$accuracy[epochs], val_accuracy = .$val_accuracy[epochs], elapsed_time = as.integer(Sys.time()) - as.integer(start))}



weights <- keras::get_weights(model_x_ray)



# Evaluate the model. Outputs are loss, acc (accuracy), precision_1, recall_1, and auc_1 

Evaluate_x_ray <- model_x_ray %>% evaluate_generator(
  test_images_BW, 
  steps = as.integer(test_images_BW$n %/% batch_size)
  )





# Add the F1 score to the metrics for this Convolutional Neural Network
CNN_metrics <- c(Evaluate_x_ray, setNames((2 * (Evaluate_x_ray[3] * Evaluate_x_ray[4]) / (Evaluate_x_ray[3] + Evaluate_x_ray[4])), "F1_score"))



# Get predictions for the test set

y_pred_x_ray <- model_x_ray %>% predict_generator(
  test_images_BW, 
  steps = as.integer(test_images_BW$n %/% batch_size), 
) %>%
  as.data.frame()

colnames(y_pred_x_ray) <- classes_labels

head(y_pred_x_ray)






##########################################################
# Define model 2 using NASNetLarge network
##########################################################



# Preparation of the images with now 3 layers for color since that's the shape accepted by the NASNetLarge model. 

# Update the value for "channels"
channels <- 3 


train_images <- flow_images_from_directory(
  directory = train_dir,
  generator = train_data_gen,
  target_size = target_size,
  color_mode = "rgb",
  class_mode = "categorical",
  batch_size = batch_size,
  shuffle = TRUE,
  seed = NULL,
  save_to_dir = NULL,
  save_prefix = "",
  save_format = "png",
  follow_links = FALSE,
  subset = "training",
  interpolation = "bicubic"
)

valid_images <- flow_images_from_directory(
  directory = train_dir,
  generator = valid_data_gen,
  target_size = target_size,
  color_mode = "rgb",
  class_mode = "categorical",
  batch_size = batch_size,
  shuffle = TRUE,
  seed = NULL,
  save_to_dir = NULL,
  save_prefix = "",
  save_format = "png",
  follow_links = FALSE,
  subset = "validation",
  interpolation = "bicubic"
)

test_images <- flow_images_from_directory(
  directory = test_dir,
  generator = test_data_gen,
  target_size = target_size,
  color_mode = "rgb",
  class_mode = "categorical",
  batch_size = batch_size,
  shuffle = TRUE,
  seed = NULL,
  save_to_dir = NULL,
  save_prefix = "",
  save_format = "png",
  follow_links = FALSE,
  interpolation = "bicubic"
)



# Loading the NASNetLarge model and weights trained on the imagenet dataset. 

nasnetlarge <- application_nasnetlarge(
  include_top = FALSE, 
  weights = "imagenet"
)


################### New layers #########################
## add custom layers on top of the pretrained network
predictions <- nasnetlarge$output %>% 
  layer_global_average_pooling_2d(trainable = TRUE) %>% 
  layer_dense(64, trainable = TRUE) %>%
  layer_activation("relu", trainable = TRUE) %>%
  layer_dropout(0.2, trainable = TRUE) %>%
  layer_dense(3, trainable=TRUE) %>%    ## important to adapt to fit the 3 classes in the dataset!
  layer_activation("softmax", trainable=TRUE)

## Make sure the layers from the pretrained model are not going to be trained again.
for (layer in nasnetlarge$layers)
  layer$trainable <- FALSE

# The new model to be trained:
model_x_ray_nasnetlarge <- keras_model(inputs = nasnetlarge$input, 
                     outputs = predictions) 



################### Compile the model #########################
model_x_ray_nasnetlarge %>% keras::compile(
  loss = "categorical_crossentropy", 
  optimizer = keras::optimizer_adam(lr = 0.0005), 
  metrics = c(tf$keras$metrics$Accuracy, 
                 tf$keras$metrics$Precision,
                 tf$keras$metrics$Recall, 
                 tf$keras$metrics$AUC
  )
)


################### Train the model #########################

history <- model_x_ray_nasnetlarge %>% fit_generator(
  train_images,
  steps_per_epoch = as.integer(train_samples%/%batch_size), 
  epochs = epochs, 
  validation_data = valid_images,
  validation_steps = as.integer(valid_samples%/%batch_size),
  verbose=2, 
  callbacks = callback_early_stopping(
    monitor = "val_accuracy",
    min_delta = min_delta,
    patience = patience,
    mode = c("auto"),
    baseline = NULL
  )
)




save_model_weights_tf(model_x_ray_nasnetlarge, cwd)

model_path <- paste(getwd(), "/model_x_ray_nasnetlarge.h5", sep="")

model_x_ray_nasnetlarge <- load_model_tf(model_path) %>% 
  load_model_weights_tf(model_path)

# To view the summary of the model
# model_x_ray_nasnetlarge


history_df_nas <- as.data.frame(model_x_ray_nasnetlarge) 


weights_nas <- keras::get_weights(model_x_ray_nasnetlarge)



# Evaluate the model. Outputs are loss, acc (accuracy), precision_1, recall_1, and auc_1 

Evaluate_nas <- model_x_ray_nasnetlarge %>% 
  evaluate_generator(
  test_images, 
  steps = as.integer(test_images$n %/% batch_size), 
)



# Definition of the F1_score
F1_score <- (2 * (Evaluate_nas[3] * Evaluate_nas[4]) / (Evaluate_nas[3] + Evaluate_nas[4]))

# WORKS
Evaluate_nas_f1 <- c(Evaluate_nas, setNames(2 * (Evaluate_nas[3] * Evaluate_nas[4]) / (Evaluate_nas[3] + Evaluate_nas[4]), "F1_score") )

# Works too and its simpler to read
NASNetLarge_metrics <- c(Evaluate_nas, setNames(F1_score, "F1_score"))

NASNetLarge_metrics




# Get predictions for the test set

y_pred_x_ray_nasnetlarge <- model_x_ray_nasnetlarge %>% 
  predict_generator(
  test_images, 
  steps = as.integer(test_images$n %/% batch_size)
) %>%
  as.data.frame()  

colnames(y_pred_x_ray_nasnetlarge) <- classes_labels


model_comparison <- as.data.frame(rbind(CNN_metrics, NASNetLarge_metrics))






