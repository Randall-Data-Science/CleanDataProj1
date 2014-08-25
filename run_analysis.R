### NOTE: all lines of code below are documented in README.md

tt <- read.table("data//UCI HAR Dataset//test//subject_test.txt")[,1]; names(tt) <- c("Subject")
tr <- read.table("data//UCI HAR Dataset//train//subject_train.txt")[,1]; names(tt) <- c("Subject")
ttnobs <- length(tt); ttnobs
trnobs <- length(tr); trnobs
loadTable <- function(tableLocation, rows, colNames) {
    tempTable <- read.table(file=tableLocation,header = FALSE, nrows = 10)
    tempClass <- sapply(tempTable, class)
    return(read.table(file = tableLocation ,header = FALSE, nrows = rows, 
                      colClasses = tempClass, col.names = colNames))
}
XcolNames <- as.character(read.table("data/UCI HAR Dataset/features.txt")[,2])
Train <- loadTable("data//UCI HAR Dataset/train/X_train.txt", trnobs, XcolNames)
Test <- loadTable("data//UCI HAR Dataset/test/X_test.txt", ttnobs, XcolNames)
InertialStats=sub(pattern = "_train.txt", replacement = "", 
                  x = list.files(path = "data/UCI HAR Dataset/train//Inertial Signals",full.names = FALSE))
tempFrame <- function(path, group, stat) {
    tempFrame <- read.table(
        file = paste(path, "/", group,
                     "/Inertial Signals/",
                     stat, "_", group, ".txt", 
                     sep="")
    )
    outputMean <- rowMeans(tempFrame)
    outputSD <- apply(X = tempFrame, MARGIN = 1, FUN = sd)
    outputFrame <- data.frame(outputMean, outputSD)
    names(outputFrame) <- c(paste(stat, ".mean", sep=""),
                            paste(stat, ".sd", sep=""))
    return(outputFrame)
}
path <- "data//UCI HAR Dataset"
for (stat in InertialStats) {
    temp <- tempFrame(path = path, group = "test", stat = stat)
    Test <- cbind(temp, Test)
}
for (stat in InertialStats) {
    temp <- tempFrame(path = path, group = "train", stat = stat)
    Train <- cbind(temp, Train)
}
activity <- as.factor(read.table("data/UCI HAR Dataset/activity_labels.txt")[,2])
print(as.data.frame(activity))
Train <- cbind(
    loadTable("data//UCI HAR Dataset/train//y_train.txt", trnobs, c("Activity")),
    Train)

Test <- cbind(
    loadTable("data//UCI HAR Dataset/test//y_test.txt", trnobs, c("Activity")),
    Test)
Train <- cbind("Subject"=tr, Train)
Test <- cbind("Subject"=tt, Test)
identical(names(Test), names(Train))
identical(sapply(Test, class), sapply(Train, class))
allTheData <- merge(Train, Test, all = TRUE)
dim(allTheData)
allTheData[sample(10000, 10), 1:10]
if (!require(dplyr)) {install.packages("dplyr")}
a1 <- group_by(allTheData, Activity, Subject)
a2 <- select(a1, 3:581); rm(a1)
tidyData <- summarise(a2, 
                      mean(3),
                      mean(4),
                      mean(5),
                      mean(6),
                      mean(7),
                      mean(8),
                      mean(9),
                      mean(9),
                      mean(10),
                      mean(11),
                      mean(12),
                      mean(13),
                      mean(14),
                      mean(15),
                      mean(16),
                      mean(17),
                      mean(18),
                      mean(19),
                      mean(20)
); rm(a2)
names(tidyData) <- names(allTheData)[1:20]
write.table(x = tidyData, file = "tidyData.txt", row.names = FALSE)
