## 1. Merge the training and the test sets to create one data set.
# The final dataSet structure will look like this:
# column 1-561: features
# column 562: subject
# column 563: outcome

library(data.table)

trainData <- read.table(file = "UCI-HAR-Dataset/train/X_train.txt")
trainData <- cbind(trainData, read.table(file = "UCI-HAR-Dataset/train/subject_train.txt", colClasses = "factor"))
trainData <- cbind(trainData, read.table(file = "UCI-HAR-Dataset/train/y_train.txt", colClasses = "factor"))
testData <- read.table(file = "UCI-HAR-Dataset/test/X_test.txt")
testData <- cbind(testData, read.table(file = "UCI-HAR-Dataset/test/subject_test.txt", colClasses = "factor"))
testData <- cbind(testData, read.table(file = "UCI-HAR-Dataset/test/y_test.txt", colClasses = "factor"))                  

dataSet <- rbind(trainData, testData)
rm(trainData)
rm(testData)

# Give useful column names:
features <- read.table(file = "UCI-HAR-Dataset/features.txt", 
                       colClasses = c("character"), 
                       row.names = 1)
colnames(dataSet) <- c(t(features), "subject", "activity")
rm(features)

## 2. Extract only the measurements on the mean and standard deviation for each measurement. 
    # Feature names for measurements on the mean contain "-mean()" and
    # feature names for measurements on the standard deviation contain "-std()".
    
columnSelectionVector <- grepl("mean\\(\\)|std\\(\\)|subject|activity", names(dataSet))
dataSetSelection <- dataSet[ , columnSelectionVector]
rm(columnSelectionVector)
names(dataSetSelection)
ncol(dataSetSelection) # 68 columns
rm(dataSet)

## 3. Use descriptive activity names to name the activities in the data set.
    # Activities are the outcomes ("y-values") of the data set. 
    # 1 WALKING
    # 2 WALKING_UPSTAIRS
    # 3 WALKING_DOWNSTAIRS
    # 4 SITTING
    # 5 STANDING
    # 6 LAYING
activities <- read.table(file = "UCI-HAR-Dataset/activity_labels.txt", 
        colClasses = c("factor"), col.names = c("activity", "Activity") )

library(plyr)
dataSetSelection <- join(x = dataSetSelection, y = activities, by = "activity", type = "inner")    
dataSetSelection <- dataSetSelection[ , -68]
rm(activities)

## 4. Appropriately label the data set with descriptive variable names. 
    # Names of dataSetSelection so far
        # 1 tBodyAcc-mean()-X
        # 2 tBodyAcc-mean()-Y
        # 3 tBodyAcc-mean()-Z
        # 4 tBodyAcc-std()-X
        # 5 tBodyAcc-std()-Y
        # 6 tBodyAcc-std()-Z
        # ...
        # 67 subject
        # 68 activity
    # Remove the paranetheses in the following
colnames(dataSetSelection) <- gsub(pattern = "([()])", replacement = "", x = colnames(dataSetSelection))
colnames(dataSetSelection) <- gsub(pattern = "^t", replacement = "Time-", x = colnames(dataSetSelection))
colnames(dataSetSelection) <- gsub(pattern = "^f", replacement = "Frequency-", x = colnames(dataSetSelection))
colnames(dataSetSelection) <- gsub(pattern = "^activity", replacement = "Activity", x = colnames(dataSetSelection))
colnames(dataSetSelection) <- gsub(pattern = "^subject", replacement = "Subject", x = colnames(dataSetSelection))
colnames(dataSetSelection) <- gsub(pattern = "-mean", replacement = "-Mean", x = colnames(dataSetSelection))
colnames(dataSetSelection) <- gsub(pattern = "-std", replacement = "-StdDev", x = colnames(dataSetSelection))
colnames(dataSetSelection)


## 5. From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
# Please upload your data set as a txt file created with write.table() using 
# row.name=FALSE (do not cut and paste a dataset directly into the text box, 
# as this may cause errors saving your submission).
    # column 1: Subject
    # column 2: Activity
    # column 3: -xxx: average of corresponding feature

dataSetSelection <- dataSetSelection[ , c(68,67, seq(1,66))] # swap the columns
newTidySet <- ddply(dataSetSelection, c("Subject","Activity"), numcolwise(mean))
write.table(x = newTidySet, file = "TidySet.txt", row.names = FALSE)

