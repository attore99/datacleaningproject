library(dplyr)
#library(plyr)

#----PART ONE

#import the sets
training<-read.table("UCI HAR Dataset/train/X_train.txt",header=F) #(7352:561)
test<-read.table("UCI HAR Dataset/test/X_test.txt",header=F) #(2947:561)

#import the subjects
training_sub<-read.table("UCI HAR Dataset/train/subject_train.txt",header=F) #(7352:1)
test_sub<-read.table("UCI HAR Dataset/test/subject_test.txt",header=F) #(2947:1)

#import the activities 
training_act<-read.table("UCI HAR Dataset/train/y_train.txt",header=F) #(7352:1)
test_act<-read.table("UCI HAR Dataset/test/y_test.txt",header=F) #(2947:1)

#import the activities names
actnames<-read.table("UCI HAR Dataset/activity_labels.txt") #(6:2)

#import the features
features<-read.table("UCI HAR Dataset/features.txt",header=F) #(561:2)

#name the columns (#----PART FOUR)
colnames(training)<-features[,2]
colnames(test)<-features[,2]
colnames(training_sub)<-"subject"
colnames(test_sub)<-"subject"
colnames(training_act)<-"activity"
colnames(test_act)<-"activity"

#add subjects and activities to the sets
test<-cbind(test_sub, test_act, test) #(2947:563)
training<-cbind(training_sub,training_act, training) #(7352:563)

#merge the sets
set<-rbind(training,test) #(10299:562)

#----PART TWO

#extract mean and std devs 
co<-colnames(set)
condition<-(grepl("subject",co)|grepl("activity",co)|grepl("mean..",co)|grepl("std..",co))
set_mds<-set[,condition==T]

#----PART FOUR#####################################################################################
#associate names of activities
associateact<-function(number){actnames$V2[number]} #create function
activities<-c()
for (i in 1:nrow(set_mds)){
  activities<-c(activities,as.character(associateact(as.numeric(set_mds[i,2]))))
}
set_mds$activity<-activities

#----PART FIVE
set2 <-aggregate(set_mds[,3:ncol(set_mds)], by=list(set_mds$subject,set_mds$activity),  FUN=mean, na.rm=TRUE)
colnames(set2)[1]<-"subject"
colnames(set2)[2]<-"activity"

#----Save output in a file
write.table(set2,"tidyset.txt",row.name=FALSE)
