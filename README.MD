---
title: "README"
output: html_document
---
##Course Project - Getting and Cleaning Data

###Purpose and Script Function
The purpose of this project is to take the Human Activity Recognition (HAR) Using Smartphones Dataset (available through the University of California, Irvine) and
manipulate it to produce a "tidy" dataset with certain specifications.

Within the HAR dataset there are two sections, test and training.  The 
run_analysis.R file was created to do the following:

 - Merge the two data sets
 - Separate out only those variables that relate to mean or standard deviation
   measurements (79 variables in total).  These variables were separated out based 
   on the mean() or std() designation in the original variable name.
 - Group those variables by subject then by condition and calculate the mean for     
   each subset of data (e.g. the mean of subject one and condition one for each 
   variable, then subject one and condition two and so on for all thirty subjects 
   and six conditions)
   
Along with the above the originally coded condition numbers are replaced with text descriptions of what the condition was.  For instance a condition origially labeled 1 is changed to Walking.  The mean and standard deviation variables are also re-written so that a full text description is used rather than an abbreviation (for example a variable containing the abbreviation tBodyAcc is changed to Time Domain Body Accelleration).  Please see the code book for a complete description of all variables and the modifications that were made to their naming.

###Please Note:
Before running the R script you will need to load the dplyr package as some of the code relies on functions in that package.  Additionally, all of the data files will need to be located in your current working directory (which you will have to specify).  The last thing is the final consolidated data set is formatted as a space delimited text file.  It is recommended that you read it in using the following: read.table("HAR_Summary_Dataset.txt", header = TRUE, sep = " ")

###Data Set Background Information:
The information below is from the Read Me file for the original HAR data set and provides an overview of the files provided as part of the data set and their contents, along with background about how the data was originally collected and processed.

==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the 
  estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature
  vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.
