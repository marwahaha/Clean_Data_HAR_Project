run_analysis <- function(){

##Read in the test data files including subject and condition information, 
##along with the data.
     
testsub <- read.table("subject_test.txt", header = FALSE)
testcond <- read.table("y_test.txt", header = FALSE)
testdata <- read.table("x_test.txt", header = FALSE, sep = "")

##Read in the training data files including subject and condition information, 
##along with the data.

trainsub <- read.table("subject_train.txt", header = FALSE)
traincond <- read.table("y_train.txt", header = FALSE)
traindata <- read.table("x_train.txt", header = FALSE, sep = "")

##Read in the variable names data file, then separate it into a character vector.
##The "-" and "()" characters are then removed from the variable names and all
##of the names are adjusted to be unique.

varnames <- read.table("features.txt", header = FALSE, colClasses = "character")
     tmpvarnames <- varnames[,2]
     tmpvarnames2 <- gsub("-", ".", tmpvarnames)
     tmpvarnames2 <- gsub('[()]', "", tmpvarnames2)
varnames2 <- make.names(tmpvarnames2, unique = TRUE, allow_ = TRUE)

##Assign the revised variable names as the column names in both the training and
##test data frames.

colnames(testdata) <- varnames2
colnames(traindata) <- varnames2

##Combine the test and training data frames.

test_traindata <- rbind(testdata, traindata)

##Select only those variables that are either mean or standard deviations.

test_trainmeansd <- select(test_traindata, contains(".mean"), contains(".std"))

##Assign column names to the subject and condition data frames.

colnames(testsub) <- c("Subject.Number")
colnames(testcond) <- c("Condition.Number")
colnames(trainsub) <- c("Subject.Number")
colnames(traincond) <- c("Condition.Number")

##Combine the test and train subject and condition data then add those columns
##in with the rest of the overall test and train data frame.

subinfo <- rbind(testsub, trainsub)
condinfo <- rbind(testcond, traincond)
testinfo <- cbind(subinfo, condinfo)
overalldata <- cbind(testinfo, test_trainmeansd)

##Group the fully combined data frame by subject and condition.

summarydata <- group_by(overalldata, Subject.Number, Condition.Number)

##Calculate the mean for all the variables by group.

finaldata <- summarise_each(summarydata, funs(mean))

##Assign condition names to a character vector so that the coded values can then
##be replaced.

condnames <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", 
               "STANDING", "LAYING")

for (i in 1:6){
     
     finaldata$Condition.Number[finaldata$Condition.Number == i] <- condnames[i]
     
}

##Obtain and replace the abreviated variable names with full text descriptions.

finalvarnames <- names(finaldata)
finalvarnames <- gsub("Condition.Number", "Condition", finalvarnames)
finalvarnames <- gsub("tBodyAccJerkMag", "Time Domain Body Accelleration Jerk Magnitude", finalvarnames)
finalvarnames <- gsub("tBodyAccJerk", "Time Domain Body Accelleration Jerk", finalvarnames)
finalvarnames <- gsub("tBodyAccMag", "Time Domain Body Accelleration Magnitude", finalvarnames)
finalvarnames <- gsub("tBodyAcc", "Time Domain Body Accelleration", finalvarnames)
finalvarnames <- gsub("fBodyAccJerk", "Frequency Domain Body Accelleration Jerk", finalvarnames)
finalvarnames <- gsub("fBodyAccMag", "Frequency Domain Body Accelleration Magnitude", finalvarnames)
finalvarnames <- gsub("fBodyAcc", "Frequency Domain Body Accelleration", finalvarnames)
finalvarnames <- gsub("fBodyGyro", "Frequency Domain Body Angular Speed", finalvarnames)
finalvarnames <- gsub("fBodyBodyAccJerkMag", "Frequency Domain Body Accelleration Jerk Magnitude", finalvarnames)
finalvarnames <- gsub("fBodyBodyGyroJerkMag", "Frequency Domain Body Angular Accelleration Magnitude", finalvarnames)
finalvarnames <- gsub("fBodyBodyGyroMag", "Frequency Domain Body Angular Speed Magnitude", finalvarnames)
finalvarnames <- gsub("tGravityAccMag", "Gravity Acceleration Magnitude", finalvarnames)
finalvarnames <- gsub("tGravityAcc", "Gravity Acceleration", finalvarnames)
finalvarnames <- gsub("tBodyGyroJerkMag", "Time Domain Body Angular Accelleration Jerk Magnitude", finalvarnames)
finalvarnames <- gsub("tBodyGyroJerk", "Time Domain Body Angular Accelleration", finalvarnames)
finalvarnames <- gsub("tBodyGyroMag", "Time Domain Body Angular Speed Magnitude", finalvarnames)
finalvarnames <- gsub("tBodyGyro", "Time Domain Body Angular Speed", finalvarnames)

##Reassign the modified variable names to the data frame.

colnames(finaldata) <- finalvarnames

##Output the final data frame to a space delimited text file.

write.table(finaldata, file = "HAR_Summary_Dataset.txt", sep = " ", row.names = FALSE)

}