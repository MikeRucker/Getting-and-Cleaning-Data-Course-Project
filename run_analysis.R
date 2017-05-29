## Read data
X_test <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/test/X_test.txt", quote="\"", stringsAsFactors=FALSE)
X_train <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/train/X_train.txt", quote="\"", stringsAsFactors=FALSE)
y_test <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/test/y_test.txt", quote="\"", stringsAsFactors=FALSE)
y_train <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/train/y_train.txt", quote="\"", stringsAsFactors=FALSE)
subject_test <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/test/subject_test.txt", quote="\"", stringsAsFactors=FALSE)
subject_train <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/train/subject_train.txt", quote="\"", stringsAsFactors=FALSE)

activity_labels <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/activity_labels.txt", quote="\"", stringsAsFactors=FALSE)
features <- read.table("C:/Users/sharethelove/Desktop/UCI_HAR_Dataset_for_Coursera/features.txt", quote="\"", stringsAsFactors=FALSE)  

## Merge data
dataSet <- rbind(X_train,X_test)

## Extract mean & standard deviation for each measurement 
MeanStdOnly <- grep("mean()|std()", features[, 2]) 
dataSet <- dataSet[,MeanStdOnly]

## Label data set with descriptive names
CleanNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(dataSet) <- CleanNames[MeanStdOnly]

## Bind test & train & give descriptive lables
subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'

## Create final data set
FinalDataSet <- cbind(subject,activity, dataSet)

## Use descriptive names to name the activities in the data set
act_group <- factor(FinalDataSet$activity)
levels(act_group) <- activity_labels[,2]
FinalDataSet$activity <- act_group


## Create an independent tidy data set with the average of each variable for each activity & each subject
if (!"reshape2" %in% installed.packages()) {
	install.packages("reshape2")
}
library("reshape2")

## Create tidy data set to working directory as txt file
baseData <- melt(FinalDataSet,(id.vars=c("subject","activity")))
secondDataSet <- dcast(baseData, subject + activity ~ variable, mean)
names(secondDataSet)[-c(1:2)] <- paste("[mean of]" , names(secondDataSet)[-c(1:2)])
write.table(secondDataSet, "TidyDataSet.txt", row.names = FALSE, sep = ",")
