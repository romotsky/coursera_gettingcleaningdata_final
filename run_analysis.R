## You should create one R script called run_analysis.R that does the following. 

  ##Merges the training and the test sets to create one data set.

  ##Extracts only the measurements on the mean and standard deviation for each measurement.

  ##Uses descriptive activity names to name the activities in the data set

  ##Appropriately labels the data set with descriptive variable names.  


## feature = time and frequency reads
## activity = type of activity (walk, sit, etc)
## subject = person identifier (numered 1-30)

  setwd("C:/Users/dromotsk/Dropbox/LEARNING/getting&cleaning data/final project")
  
    ## activity and feature labels
  activity <- read.table("activity_labels.txt", header=FALSE)
  activity[,2] <- as.character(activity[,2])  
    
  features <- read.table("features.txt", header = FALSE)
  features[,2] <- as.character(features[,2])
  
  
  ## get data that is only mean and standard deviation related.
  
  right_features <- grep(".*[Mm][Ee][Aa][Nn].*|.*[Ss][Tt][Dd].*", features[,2])
  ## 86 out of 561 features are mean and std
  
  # Load the datasets with only the applicable variables
  
    ##test
  X_test <- read.table("test/X_test.txt", header = FALSE)[right_features]
  ##head(X_test)
  test_activities <- read.table("test/y_test.txt", header = FALSE)
  test_subject <- read.table("test/subject_test.txt", header = FALSE)
  ##combine test files
  TEST <- cbind(test_subject,test_activities,X_test)
  
  ##train
  X_train <- read.table("train/X_train.txt", header = FALSE)[right_features]
  ##head(X_train)
  train_activities <- read.table("train/y_train.txt", header = FALSE)
  train_subject <- read.table("train/subject_train.txt", header = FALSE)
  ## combine train files
  TRAIN <- cbind(train_subject,train_activities,X_train)
  
  
  # merge datasets and add labels
  
  data <- rbind(TEST, TRAIN)


##clean up feature names.  Dont forget the "\\"
  clean_features <- gsub("-[Mm]ean\\()","_mean", features[right_features,2])
  clean_features <- gsub("-[Ss]td\\()","_stdev", clean_features)
  clean_features <- gsub("[Mm]ean\\)","_mean\\)", clean_features)
  clean_features <- gsub("[Ff]req\\()","_freq", clean_features)
  clean_features <- gsub("[Mm]ean,","_mean,", clean_features)
  clean_features <- tolower(clean_features)
  
## assign column names
colnames(data) <- c("Activity_Code","Person", clean_features)


## add activity name
colnames(activity) <- c("Activity_Code", "Activity")
data2 <- merge(data, activity, all = TRUE, by = c("Activity_Code"))
data2 <- data2[,c(2,89,3:88)]


##From the data set in step 4, creates a second, independent tidy data set with the 
##average of each variable for each activity and each subject.


library(reshape2)
melted <- melt(data2, id.vars=c("Person", "Activity"))

mean_grouped <- dcast(melted, Person + Activity ~ variable, mean)

write.table(mean_grouped, "./tidy.txt")
