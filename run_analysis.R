
## Creation of one dataset
##Creating the dataset "train" by merging values AND labels
setwd("~/R/UCI HAR Dataset/train")
trainlabels<-readLines("y_train.txt")
trainset<-readLines("X_train.txt")
train<-cbind(trainlabels,trainset)
train<-as.matrix(train)

##Creating the dataset "test" by merging values AND labels
setwd("~/R/UCI HAR Dataset/test")
testlabels<-readLines("y_test.txt")
testset<-readLines("X_test.txt")
test<-cbind(testlabels,testset)
test<-as.matrix(test)

## Merging train dataset & test dataset (containing values and labels)
alldata<-rbind(test,train)

##Changing columns' names
alldata<-as.data.frame(alldata)
colnames(alldata)<-c("Labels","Set")

## Dividing the "set" into measurements : one column per measurement
library(stringr)
m<-str_split(alldata$Set," ")

## Delete values of m that are equal to ""
i<-1
while (i<=nrow(alldata)){
        m[[i]]<-m[[i]][-which(m[[i]]=="")]
        i<-i+1
}
## Transform characters into numeric values
l<-1
while (l<=nrow(alldata)){
        m[[l]]<-as.numeric(m[[l]])
        l<-l+1
}
## Create 2 dataframes with the mean and standard deviation for all set of measures
i<-1
mean_data<-data.frame()
while (i <=nrow(alldata)){
        mean_data<-rbind(mean(m[[i]]),mean_data)
        i<-i+1
}
colnames(mean_data)="Mean"
i<-1
sd_data<-data.frame()
while (i <=10299){
        sd_data<-rbind(sd(m[[i]]),sd_data)
        i<-i+1}
colnames(sd_data)="Standard_deviation"

## Merge the main dataset with the mean and sd dataframes
alldata<-cbind(alldata,mean_data,sd_data)

##Use descriptive activity names to name the activities in the data set
alldata$Labels<-gsub("6","LAYING",alldata$Labels)
alldata$Labels<-gsub("5","STANDING",alldata$Labels)
alldata$Labels<-gsub("4","SITTING",alldata$Labels)
alldata$Labels<-gsub("3","WALKING_DOWNSTAIRS",alldata$Labels)
alldata$Labels<-gsub("2","WALKING_UPSTAIRS",alldata$Labels)
alldata$Labels<-gsub("1","WALKING",alldata$Labels)

##Download the final dataset
final_data<-alldata[,-2]
setwd("~/R")
write.csv(final_data,file="./tidydataset.csv")

## Get the subject # values and create a binded data set with train and test sets
setwd("~/R/UCI HAR Dataset/train")
train_subject<-readLines("subject_train.txt")
setwd("~/R/UCI HAR Dataset/test")
test_subject<-readLines("subject_test.txt")
test_subject<-as.data.frame(test_subject)
train_subject<-as.data.frame(train_subject)
colnames(test_subject)<-"Subject#"
colnames(train_subject)<-"Subject#"
subject_data<-rbind(test_subject,train_subject)
colnames(subject_data)<-"Subject#"

##Creating one matrix with the tidy one of question 2 AND the one with subjects #
setwd("~/R/UCI HAR Dataset/test")
tidydata<-read.csv("tidydataset.csv",sep=";")
all_data_subject<-cbind(subject_data,tidydata)
all_data_subject<-all_data_subject[,-2]

##Split the database by subject #
all_data_split<-split(all_data_subject,all_data_subject$`Subject#`)


## Loop that groups the splitted database by Activity Label AND applies the mean of mean AND sd
library(dplyr)
l<-data.frame()
for(i in 1:length(all_data_split)){
        s<-summarize(group_by(all_data_split[[i]],Labels),"Mean"=mean(Mean))
        w<-summarize(group_by(all_data_split[[i]],Labels),mean(Standard_deviation))
        s<-as.data.frame(s)
        w<-as.data.frame(w)
        sw<-cbind(s,"Sd_mean"=w[,2])
        subject_nb<-matrix(i,6,1)
        s2<-cbind(sw,subject_nb)
        l<-rbind(l,s2)
}
## Creating the second dataset with the average of each variable for each activity and each subject.
setwd("~/R")
write.csv(l,file="./tidydataset2.csv")
