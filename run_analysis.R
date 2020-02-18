#load needed packages
library(dplyr)

filename <- "Coursera_Final.zip"

# Checking if archive exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#Load all data sets into R. Name columns accordingly.
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Step 1: Merges the training and the test sets to create one data set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_D <- cbind(Subject, Y, X)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
Tidy_D <- select(Merged_D, subject, code, contains("mean"), contains("std"))

#Step 3:Uses descriptive activity names to name the activities in the data set

Tidy_D$code <- activities[Tidy_D$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names

names(Tidy_D)[2] = "activity"
names(Tidy_D)<-gsub("Acc", "Accelerometer", names(Tidy_D))
names(Tidy_D)<-gsub("Gyro", "Gyroscope", names(Tidy_D))
names(Tidy_D)<-gsub("BodyBody", "Body", names(Tidy_D))
names(Tidy_D)<-gsub("Mag", "Magnitude", names(Tidy_D))
names(Tidy_D)<-gsub("^t", "Time", names(Tidy_D))
names(Tidy_D)<-gsub("^f", "Frequency", names(Tidy_D))
names(Tidy_D)<-gsub("tBody", "TimeBody", names(Tidy_D))
names(Tidy_D)<-gsub("-mean()", "Mean", names(Tidy_D), ignore.case = TRUE)
names(Tidy_D)<-gsub("-std()", "STD", names(Tidy_D), ignore.case = TRUE)
names(Tidy_D)<-gsub("-freq()", "Frequency", names(Tidy_D), ignore.case = TRUE)
names(Tidy_D)<-gsub("angle", "Angle", names(Tidy_D))
names(Tidy_D)<-gsub("gravity", "Gravity", names(Tidy_D))

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

FinalData <- Tidy_D %>%
  group_by(subject, activity) %>%
  summarise_all(mean)
#export data
write.table(FinalData, "FinalData.txt", row.name=FALSE)

