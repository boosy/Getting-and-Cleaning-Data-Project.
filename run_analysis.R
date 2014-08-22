# Load packages and set WD

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
setwd("~/Dataset")

pathIn <- file.path( "UCI HAR Dataset")

##Read the files
subj_train <- fread(file.path(pathIn, "train", "subject_train.txt"))
subj_test <- fread(file.path(pathIn, "test", "subject_test.txt"))

Y_Train <- fread(file.path(pathIn, "train", "Y_train.txt"))
Y_Test <- fread(file.path(pathIn, "test", "Y_test.txt"))


fileToDataTable <- function(f) {
    df <- read.table(f)
    dt <- data.table(df)
}
X_train <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
X_test <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

##Merges the training and the test sets
S <- rbind(subj_train, subj_test)
setnames(S, "V1", "Subject")
Y <- rbind(Y_Train, Y_Test)
setnames(Y, "V1", "ActivityNum")
X <- rbind(X_train, X_test)

SY <- cbind(S, Y)
dt <- cbind(SY, X)

setkey(dt, Subject, ActivityNum)

## Extract only the mean and standard deviation
Features <- fread(file.path(pathIn, "features.txt"))
setnames(Features, names(Features), c("featureNum", "featureName"))

Features <- Features[grepl("mean\\(\\)|std\\(\\)", featureName)]

Features$featureCode <- Features[, paste0("V", featureNum)]

select <- c(key(dt), Features$featureCode)
dt <- dt[, select, with = FALSE]

##Use descriptive activity names
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("ActivityNum", "activityName"))

## Label with descriptive activity names
dt <- merge(dt, dtActivityNames, by = "ActivityNum", all.x = TRUE)
setkey(dt, Subject, ActivityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
dt <- merge(dt, Features[, list(featureNum, featureCode, featureName)], by = "featureCode", 
            all.x = TRUE)
dt$Activity <- factor(dt$activityName)
dt$Feature <- factor(dt$featureName)

grepthis <- function(regex) {
    grepl(regex, dt$Feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

r1 <- nrow(dt[, .N, by = c("Feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
                           "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

## Create a tidy data set
setkey(dt, Subject, Activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
FdtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
write.table(FdtTidy, "SolutionA.txt")