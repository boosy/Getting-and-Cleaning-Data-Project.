# Load packages and set WD
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
setwd("~/Dataset/UCI HAR Dataset")

# 1. Merges the training and the test sets to create one data set.
X_train <- read.table("train/X_train.txt")
X_test <- read.table("test/X_test.txt")
X <- rbind(X_train, X_test)
subj_train <- read.table("train/subject_train.txt")
subj_test <- read.table("test/subject_test.txt")
S <- rbind(subj_train, subj_test)
y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")
Y <- rbind(y_train, y_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt")
features2 <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X1 <- X[, features2]
names(X1) <- features[features2, 2]
names(X1) <- gsub("\\(|\\)", "", names(X1))
names(X1) <- tolower(names(X1)) # see last slide of the lecture Editing Text Variables (week 4)
# 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
Y[,1] = activity[Y[,1], 2]
names(Y) <- "Activity"
names(S) <- "Subject"
# 4. Appropriately labels the data set with descriptive activity names.
Clean <- cbind(S, Y, X1)
write.table(Clean, "merged.txt")
# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
Subject2 = unique(S)[,1]
SubjectCount = length(unique(S)[,1])
ActivitiesCount = length(activity[,1])
Colsn = dim(Clean)[2]
result = Clean[1:(SubjectCount*ActivitiesCount), ]
row = 1
for (s in 1:SubjectCount) {
    for (a in 1:ActivitiesCount) {
        result[row, 1] = Subject2[s]
        result[row, 2] = activity[a, 2]
        tmp <- Clean[Clean$Subject==s & Clean$activity==activity[a, 2], ]
        result[row, 3:Colsn] <- colMeans(tmp[, 3:Colsn])
        row = row+1
    }
}
write.table(result, "Solution.txt")