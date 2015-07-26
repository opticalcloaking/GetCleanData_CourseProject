# read in features names
features <- read.csv("./features.txt", header=FALSE, sep=" ",
                     col.names=c("num", "feature"))
features$feature <- as.character(features$feature)

# identify features that are either a mean or a standard deviation
logicalMeanStd <- (grepl("mean()", features$feature, fixed=TRUE) | 
                     grepl("std()", features$feature, fixed=TRUE))

# read in X training data
colsToRead <- (logicalMeanStd * 2 - 1) * 16
colsNames <- features$feature[logicalMeanStd]
X_train <- read.fwf("./train/X_train.txt", colsToRead, header=FALSE,
                    col.names=colsNames, buffersize=5)

# read in Y training data
Y_train <- read.csv("./train/y_train.txt", header=FALSE, col.names="activity")

# read in subject training data
subj_train <- read.csv("./train/subject_train.txt", header=FALSE,
                       col.names="subject")

# combine X and Y and subject training data
train <- cbind(Y_train, subj_train, X_train)

# read in X testing data
X_test <- read.fwf("./test/X_test.txt", colsToRead, header=FALSE,
                   col.names=colsNames, buffersize=5)

# read in Y testing data
Y_test <- read.csv("./test/y_test.txt", header=FALSE, col.names="activity")

# read in subject testing data
subj_test <- read.csv("./test/subject_test.txt", header=FALSE,
                      col.names="subject")

# combine X and Y and subject testing data
test <- cbind(Y_test, subj_test, X_test)

# combine training and testing data
df.samsung <- rbind(train, test)

# convert integer activity classifications to factor with descriptive levels
df.samsung$activity <- as.character(df.samsung$activity)
activity_labels <- read.csv("./activity_labels.txt", header=FALSE, sep=" ")
for (activity in 1:6) {
  df.samsung$activity[which(df.samsung$activity == as.character(activity))] <-
    as.character(activity_labels[activity,2])
}

# remove excess ellipses in column names
names(df.samsung) <- gsub("...", ".", names(df.samsung), fixed=TRUE)
names(df.samsung) <- gsub("..", ".", names(df.samsung), fixed=TRUE)

# treat subject as a character rather than an integer
df.samsung$subject <- as.character(df.samsung$subject)

# compute means of the variables by subject and activity
df.means <- aggregate(. ~ activity + subject, data=df.samsung, FUN=mean)

# output the means data frame
write.table(df.means, file="./tidymeans.txt", row.names=FALSE)
