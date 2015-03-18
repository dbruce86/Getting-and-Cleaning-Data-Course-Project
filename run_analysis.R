# run_analysis.R

# Set working directory on local machine
setwd("C:/Users/110370/Documents/Coursera/GettingCleaningData/CourseProject")

library(tidyr)
library(dplyr)
library(reshape)

# ************ Load Data *****************************************************
# read.table assumes data set at ./UCI HAR Dataset
# table names the same as file names w/o .txt extension

print("Load activity_labels and features")
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
feature<-read.table("./UCI HAR Dataset/features.txt")

print("Load train data")
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

print("Load test data")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

# ************ Create data frames using tbl_df {dplyr} *****************

activity_labels<-tbl_df(activity_labels)
feature<-tbl_df(feature)
subject_test<-tbl_df(subject_test)
X_test<-tbl_df(X_test)
Y_test<-tbl_df(Y_test)
subject_train<-tbl_df(subject_train)
X_train<-tbl_df(X_train)
Y_train<-tbl_df(Y_train)

# ************ Put data tables together into one data table ************
# Note "X_" tables is overwritten with each data wrangle until the final step


print("Put data tables together into one data table")

# Add feature column names to feature tables (X table) --------------------

names(X_test)<-feature[[2]]
names(X_train)<-feature[[2]]
rm(feature)

# Add data_type column and data to identify each X data set as test or train --

X_test[["data_type"]]<-"test"
X_train[["data_type"]]<-"train"

# Add subject data (subject table) to X table -----------------------------

# Add column name to subject tables

names(subject_test)<-"subject"
names(subject_train)<-"subject"

# cbind subjects to X tables

X_test<-cbind(subject_test, X_test)
X_train<-cbind(subject_train, X_train)
rm(subject_test)
rm(subject_train)


# Add activity data (Y tables) to X table -----------------------------------

# Add column names to Y data tables and activity_labels

names(Y_test)<-"activity_key"
names(Y_train)<-"activity_key"

names(activity_labels)[1]<-"activity_key"
names(activity_labels)[2]<-"activity"

# cbind X and Y data tables

X_test<-cbind(Y_test, X_test)
X_train<-cbind(Y_train, X_train)
rm(Y_test)
rm(Y_train)

# Add activity using activity_key to test and train data tables
# note this is done last in order to compensate for merge auto-sort

X_test<-merge(activity_labels, X_test, by="activity_key")
X_train<-merge(activity_labels, X_train, by="activity_key")
rm(activity_labels)

# consolidated test and train data sets into one table ------------------------------

consolidated_temp<-rbind(X_test, X_train)
rm(X_test)
rm(X_train)

# select only mean and std variables and create consolidated_data

consolidated_data<-select(consolidated_temp, data_type, subject, activity, contains("mean()"), contains("std()"), contains("Mean"))
rm(consolidated_temp)

# ************ Rename feature column codes to descriptive column names ************
# Add tailing space in Body, Gravity, Jerk

print("Rename feature column codes to descriptive column names")

for(i in grep("Body", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("Body", "Body ", colnames(consolidated_data)[i])
}

for(i in grep("Gravity", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("Gravity", "Gravity ", colnames(consolidated_data)[i])
}

for(i in grep("Jerk", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("Jerk", "Jerk ", colnames(consolidated_data)[i])
}

# Change detector abbreviations to  motion type

for(i in grep("Acc", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("Acc", "Linear_Acceleration ", colnames(consolidated_data)[i])
}

for(i in grep("Gyro", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("Gyro", "Angular_Velocity ", colnames(consolidated_data)[i])
}

# Change mag to Magnitude

for(i in grep("Mag", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("Mag", "Magnitude", colnames(consolidated_data)[i])
}

# Change motion dimension to something more readable

for(i in grep("-X", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("-X", "in_X", colnames(consolidated_data)[i])
}

for(i in grep("-Y", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("-Y", "in_Y", colnames(consolidated_data)[i])
}

for(i in grep("-Z", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("-Z", "in_Z", colnames(consolidated_data)[i])
}

# Change time and frequency domain abbreviations to something more readable and in a better postion

for(i in grep("^t", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-paste(sub("^t", "", colnames(consolidated_data)[i]), "in_Time")
}

for(i in grep("^f", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-paste(sub("^f", "", colnames(consolidated_data)[i]), "in_Freq")
}

# Change mean and std abbreviations to something more readable and in a better postion

for(i in grep("-mean\\(\\)", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-paste(sub("-mean\\(\\)", "", colnames(consolidated_data)[i]), "Mean")
}

for(i in grep("-meanFreq\\(\\)", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-paste(sub("-meanFreq\\(\\)", "", colnames(consolidated_data)[i]), "Mean_Freq")
}

for(i in grep("-std\\(\\)", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-paste(sub("-std\\(\\)", "", colnames(consolidated_data)[i]), "StDev")
}

# Hack to force angle features to sort to the end of any list
for(i in grep("angle", colnames(consolidated_data))) {
  colnames(consolidated_data)[i]<-sub("angle", "x_angle", colnames(consolidated_data)[i])
}

rm(i)

# ************ Tidy data and find means of means and means of standard deviations ********

print("Tidy data and find means of means and means of standard deviations")

# melt feature columns into rows (variables and values) attached to corresponding activity and subject
consolidated_data_melt<-melt(consolidated_data, id=c(2:3), measure=c(4:89))

# prepare variable names for means of means and means of standard deviations
for(i in nrow(consolidated_data_melt)) {
  consolidated_data_melt$variable <- paste("Means of", consolidated_data_melt$variable)
}

# cast features variables back into mean value columns with attached to corresponding activity and subject
# cast(data, formula = ... ~ variable, fun.aggregate=NULL, ..., margins=FALSE, subset=TRUE)
# formula = what do you want as rows ~ what do you want as columns
wide_tidy_data<-cast(consolidated_data_melt, activity+subject ~ variable, mean)


# dplyr and tidyr
long_tidy_data<-consolidated_data_melt %>% group_by(activity,subject,variable) %>% summarize(mean(value))

# write wide and long tidy data files to current directory

write.table(wide_tidy_data, file="wide_tidy_data.txt", row.name=FALSE)
write.table(long_tidy_data, file="long_tidy_data.txt", row.name=FALSE)

pring("All done!")