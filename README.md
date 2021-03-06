---
title: "README.MD"
author: "David C Bruce"
date: "Monday, March 02, 2015"
output: html_document
---
### About this document "README"
This README file describes the data source and scripts required for the *[Getting and Cleaning Data Course Project](https://class.coursera.org/getdata-012/human_grading/view/courses/973499/assessments/3/submissions)*.


README explains how the data processing script works by breaking down the steps into blocks of work.

An associated document "CodeBook"" describes the data sets, and the strategies used to create a consolidated data set and tidy data set.

##### List and Tables in this document

  * Directory structure of the data set is described in __*List 1*__ below.
  * Technical information about the files are described in __*Table 1*__ below.
  * Original / Descriptive feature names are listed in __*Table 2*__ below.
  * Some anomolous names are mentioned in the "Comments" column of __*Table 2*__.
  
Note these tables are repeated in the CodeBook file for the readers convenience.

### Background on assignment

The assignment is to create one R script called __run_analysis.R__ that does the following:

  1. Merges the training and the test sets to create one data set.
  2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  3. Uses descriptive activity names to name the activities in the data set
  4. Appropriately labels the data set with descriptive variable names. 
  5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Input Data source

  * Data source is a zipped file <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>
  * After the data is unzipped, it is contained in a single data directory "UCI HAR Dataset," structure outlined in __*List 1*__ below.
  * Directory should be ./UCI HAR Dataset in the project directory.

### Output Data

Two tidy files:

  * "Wide" format file tidy data set with the average of each variable for each activity and each subject.
    * 180 Observations by 88 variables
    * Data set submitted for evaluation
    * Variables described in in __*List 2*__ below.

  * "Long" format file tidy data set with the average of each variable for each activity and each subject.
    * 15,840 observations by 4 variables
    * Data set not used for assignment

### run_analysis.R: Script for processing UCI HAR Dataset

Description of how run_analysis.R script works

#### Load Data 

  * read.table assumes data set at ./UCI HAR Dataset
  * Data directories outlined in __*List 1*__ below
  * Data table names the same as file names w/o .txt extension
  * Data tables outlined in in __*Table 1*__ below 
  
#### Put data tables together into one consolidated data table 

  * "X" tables are overwritten with each data wrangle until the final step
  * Add feature column names to feature tables (X table) 
  * Add data_type column and data to identify each X data set as test or train 
  * Add subject data (subject table) to X table 
  * Add column name to subject tables
  * cbind subjects to X tables
  * Add activity data (Y tables) to X table 
  * Add column names to Y data tables and activity_labels
  * cbind X and Y data tables
  * Add activity description using activity_key to test and train data tables
  * consolidated test and train data sets into one table 
  * select only mean and std variables and create consolidated_data
  
#### Rename feature column codes to descriptive column names 

  * Add tailing space in Body, Gravity, Jerk
  * Change detector abbreviations to  motion type
  * Change mag to Magnitude
  * Change motion dimension to something more readable
  * Change time and frequency domain abbreviations to something more readable and in a better position
  * Change mean and std abbreviations to something more readable and in a better position
  * Renames described in __*List 2*__ below.
  
#### Tidy data and find means of means and means of standard deviations 

  * Rename variable names to means of means and means of standard deviations
  * Cast features into variable and value columns with corresponding activity and subject 
  * Create wide and long tidydata tables that have the average of each variable for each activity and each subject.
  * Write wide and long tidy data files to current directory
  * Variables described in in __*List 2*__ below.

***
#### List 1: Directory structure of original data

* **UCI HAR Dataset**
    * activity_labels.txt
    * features.txt
    * features_info.txt
    * README.txt
    * **test**
        * subject_test.txt
        * X_test.txt
        * y_test.txt
        * **Inertial Signals**
            * (Raw data files not used in this exercise)
    * **train**
        * subject_train.txt
        * X_train.txt
        * y_train.txt 
        * **Inertial Signals**
            * (Raw data files not used in this exercise)

***          
#### Table 1: Contents of original data files

Directory | File | Rows | Columns | Description
--------- | ---- | ---- | ------- | -----------
UCI HAR Dataset | activity_labels.txt | 6 | 2 | First column is key field (integer).  The  second column is an activity, such as "WALKING" (string)
UCI HAR Dataset | features.txt | 561 | 2 | First column is key field (integer).  The second column is a sensor parameter-summary descriptive statistic-dimension, such as "tBodyAcc-mean()-X" (numeric)
UCI HAR Dataset/test | subject_test.txt | 2947 | 1 |Test Subject identifier for each observation (integer)
UCI HAR Dataset/test | X_test.txt | 2947 | 561 | Test observation features (numeric)  Note: column labels are found in features.txt and X_test column numbers correspond to feature key rows.
UCI HAR Dataset/test | y_test.txt | 2947 | 1 | Test Activity for each observation (Integer and row keyed to activity_label)
UCI HAR Dataset/train | subject_train.txt | 7352 | 1 | Train Subject identifier for each observation (integer)
UCI HAR Dataset/train | X_train.txt | 7352 | 561 | Train observation features (numeric)  Note: column labels are found in features.txt and X_test column numbers correspond to feature key rows.
UCI HAR Dataset/train | y_train.txt | 7352 | 1 |  Train Activity for each observation (Integer and row keyed to activity_label)

***
#### Table 2: Feature codes and Feature descriptive renames in consolidated data table

Original Data Name | Descriptive Data Name | Comments
------------------ | --------------------- | --------
data_type | data_type | 
subject | subject | 
activity | activity | 
tBodyAcc-mean()-X | Body Linear_Acceleration in_X  in_Time  Mean | 
tBodyAcc-mean()-Y | Body Linear_Acceleration in_Y  in_Time  Mean | 
tBodyAcc-mean()-Z | Body Linear_Acceleration in_Z  in_Time  Mean | 
tGravityAcc-mean()-X | Gravity Linear_Acceleration in_X  in_Time  Mean | 
tGravityAcc-mean()-Y | Gravity Linear_Acceleration in_Y  in_Time  Mean | 
tGravityAcc-mean()-Z | Gravity Linear_Acceleration in_Z  in_Time  Mean | 
tBodyAccJerk-mean()-X | Body Linear_Acceleration Jerk in_X  in_Time  Mean | 
tBodyAccJerk-mean()-Y | Body Linear_Acceleration Jerk in_Y  in_Time  Mean | 
tBodyAccJerk-mean()-Z | Body Linear_Acceleration Jerk in_Z  in_Time  Mean | 
tBodyGyro-mean()-X | Body Angular_Velocity in_X  in_Time  Mean | 
tBodyGyro-mean()-Y | Body Angular_Velocity in_Y  in_Time  Mean | 
tBodyGyro-mean()-Z | Body Angular_Velocity in_Z  in_Time  Mean | 
tBodyGyroJerk-mean()-X | Body Angular_Velocity Jerk in_X  in_Time  Mean | 
tBodyGyroJerk-mean()-Y | Body Angular_Velocity Jerk in_Y  in_Time  Mean | 
tBodyGyroJerk-mean()-Z | Body Angular_Velocity Jerk in_Z  in_Time  Mean | 
tBodyAccMag-mean() | Body Linear_Acceleration Magnitude  in_Time  Mean | 
tGravityAccMag-mean() | Gravity Linear_Acceleration Magnitude  in_Time  Mean | 
tBodyAccJerkMag-mean() | Body Linear_Acceleration Jerk Magnitude  in_Time  Mean | 
tBodyGyroMag-mean() | Body Angular_Velocity Magnitude  in_Time  Mean | 
tBodyGyroJerkMag-mean() | Body Angular_Velocity Jerk Magnitude  in_Time  Mean | 
fBodyAcc-mean()-X | Body Linear_Acceleration in_X  in_Freq  Mean | 
fBodyAcc-mean()-Y | Body Linear_Acceleration in_Y  in_Freq  Mean | 
fBodyAcc-mean()-Z | Body Linear_Acceleration in_Z  in_Freq  Mean | 
fBodyAccJerk-mean()-X | Body Linear_Acceleration Jerk in_X  in_Freq  Mean | 
fBodyAccJerk-mean()-Y | Body Linear_Acceleration Jerk in_Y  in_Freq  Mean | 
fBodyAccJerk-mean()-Z | Body Linear_Acceleration Jerk in_Z  in_Freq  Mean | 
fBodyGyro-mean()-X | Body Angular_Velocity in_X  in_Freq  Mean | 
fBodyGyro-mean()-Y | Body Angular_Velocity in_Y  in_Freq  Mean | 
fBodyGyro-mean()-Z | Body Angular_Velocity in_Z  in_Freq  Mean | 
fBodyAccMag-mean() | Body Linear_Acceleration Magnitude  in_Freq  Mean | 
fBodyBodyAccJerkMag-mean() | Body BodyLinear_Acceleration Jerk Magnitude  in_Freq  Mean | 
fBodyBodyGyroMag-mean() | Body BodyAngular_Velocity Magnitude  in_Freq  Mean | 
fBodyBodyGyroJerkMag-mean() | Body BodyAngular_Velocity Jerk Magnitude  in_Freq  Mean | 
tBodyAcc-std()-X | Body Linear_Acceleration in_X  in_Time  StDev | 
tBodyAcc-std()-Y | Body Linear_Acceleration in_Y  in_Time  StDev | 
tBodyAcc-std()-Z | Body Linear_Acceleration in_Z  in_Time  StDev | 
tGravityAcc-std()-X | Gravity Linear_Acceleration in_X  in_Time  StDev | 
tGravityAcc-std()-Y | Gravity Linear_Acceleration in_Y  in_Time  StDev | 
tGravityAcc-std()-Z | Gravity Linear_Acceleration in_Z  in_Time  StDev | 
tBodyAccJerk-std()-X | Body Linear_Acceleration Jerk in_X  in_Time  StDev | 
tBodyAccJerk-std()-Y | Body Linear_Acceleration Jerk in_Y  in_Time  StDev | 
tBodyAccJerk-std()-Z | Body Linear_Acceleration Jerk in_Z  in_Time  StDev | 
tBodyGyro-std()-X | Body Angular_Velocity in_X  in_Time  StDev | 
tBodyGyro-std()-Y | Body Angular_Velocity in_Y  in_Time  StDev | 
tBodyGyro-std()-Z | Body Angular_Velocity in_Z  in_Time  StDev | 
tBodyGyroJerk-std()-X | Body Angular_Velocity Jerk in_X  in_Time  StDev | 
tBodyGyroJerk-std()-Y | Body Angular_Velocity Jerk in_Y  in_Time  StDev | 
tBodyGyroJerk-std()-Z | Body Angular_Velocity Jerk in_Z  in_Time  StDev | 
tBodyAccMag-std() | Body Linear_Acceleration Magnitude  in_Time  StDev | 
tGravityAccMag-std() | Gravity Linear_Acceleration Magnitude  in_Time  StDev | 
tBodyAccJerkMag-std() | Body Linear_Acceleration Jerk Magnitude  in_Time  StDev | 
tBodyGyroMag-std() | Body Angular_Velocity Magnitude  in_Time  StDev | 
tBodyGyroJerkMag-std() | Body Angular_Velocity Jerk Magnitude  in_Time  StDev | 
fBodyAcc-std()-X | Body Linear_Acceleration in_X  in_Freq  StDev | 
fBodyAcc-std()-Y | Body Linear_Acceleration in_Y  in_Freq  StDev | 
fBodyAcc-std()-Z | Body Linear_Acceleration in_Z  in_Freq  StDev | 
fBodyAccJerk-std()-X | Body Linear_Acceleration Jerk in_X  in_Freq  StDev | 
fBodyAccJerk-std()-Y | Body Linear_Acceleration Jerk in_Y  in_Freq  StDev | 
fBodyAccJerk-std()-Z | Body Linear_Acceleration Jerk in_Z  in_Freq  StDev | 
fBodyGyro-std()-X | Body Angular_Velocity in_X  in_Freq  StDev | 
fBodyGyro-std()-Y | Body Angular_Velocity in_Y  in_Freq  StDev | 
fBodyGyro-std()-Z | Body Angular_Velocity in_Z  in_Freq  StDev | 
fBodyAccMag-std() | Body Linear_Acceleration Magnitude  in_Freq  StDev | 
fBodyBodyAccJerkMag-std() | Body BodyLinear_Acceleration Jerk Magnitude  in_Freq  StDev | 
fBodyBodyGyroMag-std() | Body BodyAngular_Velocity Magnitude  in_Freq  StDev | 
fBodyBodyGyroJerkMag-std() | Body BodyAngular_Velocity Jerk Magnitude  in_Freq  StDev | 
fBodyAcc-meanFreq()-X | Body Linear_Acceleration in_X  in_Freq  Mean_Freq | 
fBodyAcc-meanFreq()-Y | Body Linear_Acceleration in_Y  in_Freq  Mean_Freq | 
fBodyAcc-meanFreq()-Z | Body Linear_Acceleration in_Z  in_Freq  Mean_Freq | 
fBodyAccJerk-meanFreq()-X | Body Linear_Acceleration Jerk in_X  in_Freq  Mean_Freq | 
fBodyAccJerk-meanFreq()-Y | Body Linear_Acceleration Jerk in_Y  in_Freq  Mean_Freq | 
fBodyAccJerk-meanFreq()-Z | Body Linear_Acceleration Jerk in_Z  in_Freq  Mean_Freq | 
fBodyGyro-meanFreq()-X | Body Angular_Velocity in_X  in_Freq  Mean_Freq | 
fBodyGyro-meanFreq()-Y | Body Angular_Velocity in_Y  in_Freq  Mean_Freq | 
fBodyGyro-meanFreq()-Z | Body Angular_Velocity in_Z  in_Freq  Mean_Freq | 
fBodyAccMag-meanFreq() | Body Linear_Acceleration Magnitude  in_Freq  Mean_Freq | 
fBodyBodyAccJerkMag-meanFreq() | Body BodyLinear_Acceleration Jerk Magnitude  in_Freq  Mean_Freq | Not clear on what Body Body refers to.  Possible naming error?
fBodyBodyGyroMag-meanFreq() | Body BodyAngular_Velocity Magnitude  in_Freq  Mean_Freq |  Not clear on what Body Body refers to.  Possible naming error?
fBodyBodyGyroJerkMag-meanFreq() | Body BodyAngular_Velocity Jerk Magnitude  in_Freq  Mean_Freq |  Not clear on what Body Body refers to.  Possible naming error?
angle(tBodyAccMean,gravity) | angle(tBody Linear_Acceleration Mean,gravity) | Partially modified angle name. Note possible naming error. "gravity" may be "gravityMean" as in other angle features
angle(tBodyAccJerkMean),gravityMean) | angle(tBody Linear_Acceleration Jerk Mean),gravityMean) |  Partially modified angle name.Note two right parenthesis - naming error?
angle(tBodyGyroMean,gravityMean) | angle(tBody Angular_Velocity Mean,gravityMean) |  Partially modified angle name. 
angle(tBodyGyroJerkMean,gravityMean) | angle(tBody Angular_Velocity Jerk Mean,gravityMean) |  Partially modified angle name. 
angle(X,gravityMean) | x_angle(X,gravityMean) | x_ added as hack to force angle features to sort to the end of any list
angle(Y,gravityMean) | x_angle(Y,gravityMean) | x_ added as hack to force angle features to sort to the end of any list
angle(Z,gravityMean) | x_angle(Z,gravityMean) | x_ added as hack to force angle features to sort to the end of any list


