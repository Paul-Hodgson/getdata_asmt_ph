#STEP 1, 3 & 4: LOAD, THEN MERGE THE TRAINING AND TEST SETS TO CREATE ONE SET WITH DESCRIPTIVE HEADINGS

setwd("C://Work//docs//PaulWork//Data_Science_Course//Obtaining_Data//assignment//UCI HAR Dataset//train")
train<-read.table("X_train.txt")
trainhead<-read.table("y_train.txt")
subject_train<-read.table("subject_train.txt")
setwd("C://Work//docs//PaulWork//Data_Science_Course//Obtaining_Data//assignment//UCI HAR Dataset//test")
test<-read.table("X_test.txt")
testhead<-read.table("y_test.txt")
subject_test<-read.table("subject_test.txt")
setwd("../")
fieldnames<-read.table("features.txt")
activity_labels<-read.table("activity_labels.txt")
mergedData<-merge(train,test,all=TRUE)

tfieldnames<-t(fieldnames) ## transpose so that there is one name for each column
t1fieldnames<-tfieldnames[2,] ##remove the first row (which is just reference numbers)
names(mergedData) <- t1fieldnames ##use the file to give the fields their measurement names

mergedsubject<-merge(subject_train,subject_test,all=TRUE)
mergedhead<-rbind(trainhead,testhead)

mergedData2<-cbind(mergedsubject,mergedData) ##add the subject numbers
names(mergedData2)[names(mergedData2)=="V1"] <- "subject" <- "subject" ##name the new column

mergedData3<-cbind(mergedhead,mergedData2) ##add the activity numbers
names(mergedData3)[names(mergedData3)=="V1"] <- "activity_num" ##name the new column

mergedData4<-merge(activity_labels,mergedData3,by.x="V1",by.y="activity_num",all=TRUE)
names(mergedData4)[names(mergedData4)=="V2"] <- "activity"
names(mergedData4)[names(mergedData4)=="V1"] <- "activity_num"

# STEP 2: EXTRACT ONLY MEASUREMENTS ON MEAN AND STANDARD DEVIATION

mean <- mergedData4[,grepl("mean\\(\\)", names(mergedData4))] ##subset the columns whose headings contain the word 'mean'
std <- mergedData4[,grepl("std\\(\\)", names(mergedData4))] ##subset the columns whose headings contain the word 'std'

DataExt <- cbind(mergedData4[,2:3], mean, std) ##join the 2 dataframes, with subject & activity columns

#STEP 5: CREATE A TIDY DATASET WITH AVAERAGE OF EACH VARIABLE BY ACTIVITY AND SUBJECT

DataExt$subject <- as.factor(DataExt$subject) ##convert to factor 
DataExt$activity <- as.factor(DataExt$activity) ##convert to factor
TidyData = aggregate(DataExt, by=list(activity = DataExt$activity, subject=DataExt$subject), mean)

TidyData[,3] = NULL ##remove the mean activity column as this is meaningless

TidyData[,3] = NULL ##remove the mean subject column as this is meaningless

write.table(TidyData, file = "tidy_data.txt", row.names=FALSE) ##write out the final file for uploading
