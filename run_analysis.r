##  Unzips the downloaded file in the directory
unzip("getdata-projectfiles-UCI HAR Dataset.zip")

##  Extracting all the data from the files 
##  Columns names are specified in the beginning itself using col.names argument
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", row.names=NULL, col.names=c("ActivityID", "Activity"))
features <- read.table("UCI HAR Dataset/features.txt", row.names=NULL, col.names=c("FeatureID", "Feature"))

## Extracting data from the test folder
# Identifies the person who performed the experiment for test and training sets - range will always be within 1-30
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", row.names=NULL, col.names="SubjectID")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", row.names=NULL, col.names="SubjectID")

# Identifies the activity for test and training sets - range will always be within 1-6
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", row.names=NULL, col.names="ActivityID")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", row.names=NULL, col.names="ActivityID")

# 561-feature vector for training and test set 
# **** Criteria 4 : columns names are descriptive using features dataset****
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", row.names=NULL, col.names=features[,2])
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", row.names=NULL, col.names=features[,2])

##  Creating a merged training and test dataset
# Final merged set combining all the observations from test and training sets
# 2947 obs. + 7352 obs. = 10299 obs. in the final set
# **** Criteria 3 : Uses descriptive activity names to name the activities in the data set
merged_activity <- rbind(activity_train, activity_test)
merged_activity <- merge(merged_activity, activity_labels, by="ActivityID")
# Merging list of activites and subjectID
merged_subject <- rbind(subject_train, subject_test)
merged_data <- rbind(X_test, X_train)
# **** Criteria 1 : Merges the training and the test sets to create one data set.
final_set <- cbind(merged_subject, merged_activity, merged_data)
rm(merged_activity, merged_subject, merged_data)

# **** Criteria 2 : Extracts only the measurements on the mean and standard deviation for each measurement. 
meansCol <- colnames(final_set)[grep("mean()", colnames(final_set))]
stdCol <- colnames(final_set)[grep("std()", colnames(final_set))]
init <- colnames(final_set)[c(1:3)]
mergeCol <- c(init, meansCol, stdCol)
newset <- final_set[,mergeCol]
rm(meansCol, stdCol, init, mergeCol)

# **** Criteria 5 : Creating tidy data set with averages
## Factorizing the relevant variables in the dataset 
newset$ActivityID <- as.factor(newset$ActivityID)
newset$Activity <- as.factor(newset$Activity)
newset$SubjectID <- as.factor(newset$SubjectID)

# Function takes feature names, factor and function to return a dataframe in the
# following format :
# ......     Factor1 Factor2 Factor 3...
# Feature 1
# Feature 2
# ....
stat_byfactor <- function(data, f, variableNames, stat){
  result <- data.frame(NULL)
  for(variable in variableNames){
    newrow <- tapply(data[,variable], f, stat)
    result <- rbind(result, newrow)
  }    
  colnames(result) <- levels(f)
  rownames(result) <- variableNames
  result
}

# Two tables created with means of each variable by activity and subjectID
variableNames <- colnames(newset)[c(4:length(colnames(newset)))]
result_act <- stat_byfactor(newset, newset$Activity, variableNames, stat="mean")
result_subj <- stat_byfactor(newset, newset$SubjectID, variableNames, stat="mean")

# Merging two tables by feature names
tidydata <- merge(result_act, result_subj, by="row.names")

# Printing tidydata into a textfile
write.table(tidydata, file="tidydata.txt", row.name=FALSE, sep="\t\t")