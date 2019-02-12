# loading required libraries
library(dplyr)

# Url and filename
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

# download zip file (wb, binary mode) if it is not already been downloaded
if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory does not exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

# read the training data
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read the test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read the features from features.txt, supress conversion to text-labels to factors with as.is-option
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# Actual merging the training- and testdata
Activity <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)


# remove unnecessary dataframes to save memory
ls()
rm(trainSubjects, trainValues, trainActivity, 
   testSubjects, testValues, testActivity)
ls()

# assign colnames to Activity
colnames(Activity) <- c("subject", features[, 2], "activity")

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(Activity))

# ... and keep data in these columns only
Activity <- Activity[, columnsToKeep]

# replace activity values with named factor levels
Activity$activity <- factor(Activity$activity, 
  levels = activities[, 1], labels = activities[, 2])

# get column names
ActivityCols <- colnames(Activity)

# remove the sequence ()-
ActivityCols <- gsub("[\\(\\)-]", "", ActivityCols)

# expand abbreviations and clean up names
ActivityCols <- gsub("^f", "frequencyDomain", ActivityCols)
ActivityCols <- gsub("^t", "timeDomain", ActivityCols)
ActivityCols <- gsub("Acc", "Accelerometer", ActivityCols)
ActivityCols <- gsub("Gyro", "Gyroscope", ActivityCols)
ActivityCols <- gsub("Mag", "Magnitude", ActivityCols)
ActivityCols <- gsub("Freq", "Frequency", ActivityCols)
ActivityCols <- gsub("mean", "Mean", ActivityCols)
ActivityCols <- gsub("std", "StandardDeviation", ActivityCols)

# correct a typo in the names
ActivityCols <- gsub("BodyBody", "Body", ActivityCols)

# use new labels as column names
colnames(Activity) <- ActivityCols

# group by subject and activity and summarise using summmarise_all()
ActivityMeans <- Activity %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

# writing ActivityMeans to a csv-file
write.csv2(ActivityMeans, file = "ActivityMeans.csv")

# writing ActivityMeans to a txt-file
write.table(ActivityMeans, file = "ActivityMeans.txt", row.names = FALSE)
