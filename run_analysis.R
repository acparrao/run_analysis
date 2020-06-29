## Set the working directory you are working at
setwd("~/Desktop/datasciencecoursera/run_analysis")
library(readr)
library(dplyr)
library(stringr)

# read the data
features <- read_delim("UCI HAR Dataset/features.txt", 
                       col_names = FALSE, delim = " ")
activity_labels <- read_delim("UCI HAR Dataset/activity_labels.txt", 
                       col_names = FALSE, delim = " ")


#### TEST
subject_test <- read_csv("UCI HAR Dataset/test/subject_test.txt", 
                         col_names = FALSE)
names(subject_test) <- "subject"
subject_test <- subject_test %>% mutate(subject = as.factor(subject))

X_test <- read_delim("UCI HAR Dataset/test/X_test.txt", 
                         col_names = FALSE, delim = " ")
names(X_test) <- t(features)[2,]
X_test <- select(X_test, contains(c("mean()", "std()")))
X_test <- X_test %>% mutate_all(funs(as.numeric))

Y_test <- read_csv("UCI HAR Dataset/test/Y_test.txt", 
                   col_names = FALSE)
names(Y_test) <- "activity_label"
Y_test <- Y_test %>% mutate(activity_label = as.factor(activity_label))
str(Y_test)
b <- t(activity_labels[,2])
levels(Y_test$activity_label) <- b


test <- cbind(subject_test, Y_test, X_test)
rm(X_test, Y_test, subject_test, b)


#### TRAIN
subject_train <- read_csv("UCI HAR Dataset/train/subject_train.txt", 
                         col_names = FALSE)
names(subject_train) <- "subject"
subject_train <- subject_train %>% mutate(subject = as.factor(subject))

X_train <- read_delim("UCI HAR Dataset/train/X_train.txt", 
                     col_names = FALSE, delim = " ")
names(X_train) <- t(features)[2,]
X_train <- select(X_train, contains(c("mean()", "std()")))
X_train <- X_train %>% mutate_all(funs(as.numeric))

Y_train <- read_csv("UCI HAR Dataset/train/Y_train.txt", 
                   col_names = FALSE)
names(Y_train) <- "activity_label"
Y_train <- Y_train %>% mutate(activity_label = as.factor(activity_label))
str(Y_train)
b <- t(activity_labels[,2])
levels(Y_train$activity_label) <- b


train <- cbind(subject_train, Y_train, X_train)
rm(X_train, Y_train, subject_train, b)

## base de datos total
total <- rbind(train, test)
#total2 <- merge(train, test)
#total <- tibble::as_tibble(total)

names(total) <- gsub("-", "_", names(total))
names(total) <- gsub("\\(\\)", "", names(total))


average <- total %>% group_by(activity_label, subject) %>% 
    summarise_if(is.numeric, funs(mean)) %>% arrange(activity_label, subject) 

run_analysis <- average %>% mutate(subject = as.numeric(subject)) %>% 
    arrange(activity_label, subject)


write.table(run_analysis, "run_analysis.txt", row.names = FALSE)







