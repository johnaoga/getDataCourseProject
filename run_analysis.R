library(stringr)
library(plyr)
library(data.table)

#Merges the training and the test sets to create one data set.

getPaths <- function(type){
  base <- paste("UCI HAR Dataset/",type,"/",sep = "")
  list_files <- c("X","subject","y")
  sapply(list_files,function(x){paste(base,x,"_",type,".txt",sep = "")})  
}

train_paths <- getPaths("train")
test_paths <- getPaths("test")

train <- data.frame(read.table(train_paths["y"]),
                    read.table(train_paths["subject"]),
                    read.table(train_paths["X"]))

test <- data.frame(read.table(test_paths["y"]),
                    read.table(test_paths["subject"]),
                    read.table(test_paths["X"]))

dataset <- rbind(train,test)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("UCI HAR Dataset/features.txt")

listCol_mean_std <- c(grep("mean\\(\\)", features$V2), grep("std\\(\\)", features$V2))+2

data_subset_mean_std <- dataset[,listCol_mean_std]

#Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

colV1 <- dataset$V1

dataset_with_activity <- sapply(colV1,function(x){activity_labels[x,"V2"]})

dataset$V1 <- dataset_with_activity

#Appropriately labels the data set with descriptive variable names.
treat_feature <- function(x){
  gsub("-|,|\\(","_",gsub("\\(\\)|\\)","",x))
}

list_measures_col <- sapply(features$V2, treat_feature)
list_Col_names <- c("Activity","Subject", list_measures_col )

names(dataset) <- list_Col_names

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_dataset <- aggregate(dataset[list_measures_col], by=dataset[c("Activity","Subject")], FUN=mean)
