# __GCD_CourseProject__
## Course Project for Getting and Cleaning Data Course on Coursera

#### Steps in _'run_analysis.r'_ :- 
----------------------------------------
* Unzip the downloaded file in the directory
* Extract all the data from the files - 
    * training and test sets - with 10299 observations in total of 561 features
    * activity training and test sets - specifying activity corresponding to the 10299 observations (ranges from 1-6)
    * subject training and test sets - specifying subjects correspong to the 10299 observations (ranges from 1-30)
    * activity labels - specifying the descriptive names of the activities from 1-6
    * features - specifying the decriptive names of the 561 features specified in the training and test sets
  * Columns names are specified in the beginning itself using col.names argument

```r
unzip("getdata-projectfiles-UCI HAR Dataset.zip")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", row.names=NULL, col.names=c("ActivityID", "Activity"))
features <- read.table("UCI HAR Dataset/features.txt", row.names=NULL, col.names=c("FeatureID", "Feature"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", row.names=NULL, col.names="SubjectID")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", row.names=NULL, col.names="SubjectID")
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", row.names=NULL, col.names="ActivityID")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", row.names=NULL, col.names="ActivityID")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", row.names=NULL, col.names=features[,2])
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", row.names=NULL, col.names=features[,2])
```
    


* Creating a merged training and test dataset
  * __Criteria 1__ : Merges the training and the test sets to create one data set.
      * Final merged set combining all the observations from test and training sets
      * 2947 obs. + 7352 obs. = 10299 obs. in the final set
  * __Criteria 3__ : Use descriptive activity names to name the activities in the data set
      * Merging list of activites and subjectID

```r
merged_activity <- rbind(activity_train, activity_test)
merged_activity <- merge(merged_activity, activity_labels, by="ActivityID")
merged_subject <- rbind(subject_train, subject_test)
merged_data <- rbind(X_test, X_train)
final_set <- cbind(merged_subject, merged_activity, merged_data)
rm(merged_activity, merged_subject, merged_data)
```
* __Criteria 2__ : Extracts only the measurements on the mean and standard deviation for each measurement. 
  * Uses grep function and partial string matching to do so

```r
meansCol <- colnames(final_set)[grep("mean()", colnames(final_set))]
stdCol <- colnames(final_set)[grep("std()", colnames(final_set))]
init <- colnames(final_set)[c(1:3)]
mergeCol <- c(init, meansCol, stdCol)
newset <- final_set[,mergeCol]
rm(meansCol, stdCol, init, mergeCol)
```

* __Criteria 5__ : Creating tidy data set with averages
  * Factorize the relevant variables in the dataset 

```r
newset$ActivityID <- as.factor(newset$ActivityID)
newset$Activity <- as.factor(newset$Activity)
newset$SubjectID <- as.factor(newset$SubjectID)
```
  * Function **stat_byfactor** takes feature names, factor and function to return a dataframe in the following format :
    * ......    | Factor1 | Factor2 | Factor 3...
      Feature 1 |
      Feature 2 |
      .... |

```r
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
```
* Two tables created with means of each variable by activity and subjectID

```r
variableNames <- colnames(newset)[c(4:length(colnames(newset)))]
result_act <- stat_byfactor(newset, newset$Activity, variableNames, stat="mean")
result_subj <- stat_byfactor(newset, newset$SubjectID, variableNames, stat="mean")
```
* Merging two tables by feature names

```r
tidydata <- merge(result_act, result_subj, by="row.names")
```
* Printing tidydata into a textfile

```r
write.table(tidydata, file="tidydata.txt", row.name=FALSE, sep="\t\t")
```
