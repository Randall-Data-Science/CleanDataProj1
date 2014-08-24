---
title: "Clean Data Project Description"
author: "RDsci"
date: "August 23, 2014"
output: html_document
---

## Data Description

These kinetic data were collected from 30 subjects performing 6 different types 
of activities. The subjects were split into a training group and a test group. 
Furthermore the data were put into separate files and directories.

In this exercise we will reconstitute all of the major statistics, summarize the 
data on inertial statistics, and then re-combine the groups of subject. Output 
will be a single large table.

### Collecting the Subjects

The data on the subjects in the two groups are in separate directories which are
laid out the same. As a first steps the subject IDs corresponding to each observation
row in the data files is read.

We get the subjects, and number of observations:


```r
tt <- read.table("data//UCI HAR Dataset//test//subject_test.txt")[,1]; names(tt) <- c("Subject")
tr <- read.table("data//UCI HAR Dataset//train//subject_train.txt")[,1]; names(tt) <- c("Subject")
```

Subjects in TEST Group: **2, 4, 9, 10, 12, 13, 18, 20, 24**

Subjects in TEST Group: **1, 3, 5, 6, 7, 8, 11, 14, 15, 16, 17, 19, 21, 22, 23, 25, 26, 27, 28, 29, 30**

And for the sake of reading the files we find out how many rows of observations: 


```r
ttnobs <- length(tt); ttnobs
```

```
## [1] 2947
```

```r
trnobs <- length(tr); trnobs
```

```
## [1] 7352
```

This will be useful in the following function for reading relatively large tables:
A convenient function for loading large tables:

```r
loadTable <- function(tableLocation, rows, colNames) {
    tempTable <- read.table(file=tableLocation,header = FALSE, nrows = 10)
    tempClass <- sapply(tempTable, class)
    return(read.table(file = tableLocation ,header = FALSE, nrows = rows, 
                      colClasses = tempClass, col.names = colNames))
}
```


### $X$ Measurements

Labels for the 561 features measures in `X_{test,train}.txt` can be found in 
`features.txt


```r
XcolNames <- as.character(read.table("data/UCI HAR Dataset/features.txt")[,2])
```

### Loading the Data


To begin building the final table we will start with TRAIN $X$ and TEST $X$ data,
and use the feature names to label the columns


```r
Train <- loadTable("data//UCI HAR Dataset/train/X_train.txt", trnobs, XcolNames)
Test <- loadTable("data//UCI HAR Dataset/test/X_test.txt", ttnobs, XcolNames)
```

#### Summarizing Inertial Statistics

First we get a list of the statistics names

```r
InertialStats=sub(pattern = "_train.txt", replacement = "", 
         x = list.files(path = "data/UCI HAR Dataset/train//Inertial Signals",full.names = FALSE))
```

Each of these files has a row corresponding to each observation in the main table. 
Each of the 128 columns however corresponds to a single element in a vector for the 
observation period rather than a distinct concept.

R data.frames are composed of columns composed of vectors. The vectors can be of 
different atomic classes, but elements within the vectors can not be other vectors.
They would have to be lists to contain vectors, or perhaps they could be memory 
addresses of vectors as a work around for putting more than two dimensions of data 
in a data.frame.

For the purposes of this assignment, we will ignore the challenge, and instead of 
storing the entire vectors summarize the vectors with `mean` and `sd` standard 
deviation for each vector.

Now using dimensions of the data grab it. First a function for the general case:


```r
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
```

Then add the data to the TEST group:

```r
path <- "data//UCI HAR Dataset"
for (stat in InertialStats) {
    temp <- tempFrame(path = path, group = "test", stat = stat)
    Test <- cbind(temp, Test)
}
```

And the same to the TRAIN group:

```r
for (stat in InertialStats) {
    temp <- tempFrame(path = path, group = "train", stat = stat)
    Train <- cbind(temp, Train)
}
```

### Activities

Now that we have source data we prepend each row with the subjects' activities.
These data can be read into a factor.


```r
activity <- as.factor(read.table("data/UCI HAR Dataset/activity_labels.txt")[,2])
print(as.data.frame(activity))
```

```
##             activity
## 1            WALKING
## 2   WALKING_UPSTAIRS
## 3 WALKING_DOWNSTAIRS
## 4            SITTING
## 5           STANDING
## 6             LAYING
```

Then we read the can read activity index numbers and prepend it to the tables:


```r
Train <- cbind(
    loadTable("data//UCI HAR Dataset/train//y_train.txt", trnobs, c("Activity")),
    Train)

Test <- cbind(
    loadTable("data//UCI HAR Dataset/test//y_test.txt", trnobs, c("Activity")),
    Test)
```

### Add Subject IDs to Tables


```r
Train <- cbind("Subject"=tr, Train)
Test <- cbind("Subject"=tt, Test)
```

###Finalizing

At this point we have two tables that are identical in structure:


```r
identical(names(Test), names(Train))
```

```
## [1] TRUE
```

```r
identical(sapply(Test, class), sapply(Train, class))
```

```
## [1] TRUE
```

They can be merged together:


```r
allTheData <- merge(Train, Test, all = TRUE)
dim(allTheData)
```

```
## [1] 10299   581
```

Here are random rows for the first 10 columns


```r
allTheData[sample(10000, 10), 1:10]
```

```
##      Subject Activity total_acc_z.mean total_acc_z.sd total_acc_y.mean
## 4361      14        6          0.82116       0.006536          0.50083
## 1462       5        4         -0.01949       0.005898         -0.04730
## 467        2        3         -0.26193       0.190966         -0.29800
## 9378      28        4         -0.04134       0.032731         -0.18444
## 5213      17        3         -0.20277       0.167325         -0.01942
## 5883      19        2         -0.28543       0.157389         -0.35444
## 1213       4        5          0.13183       0.015078         -0.06233
## 7784      24        3         -0.13583       0.140925         -0.13885
## 4765      16        1         -0.15849       0.160532          0.10096
## 5891      19        2         -0.13762       0.101031         -0.38299
##      total_acc_y.sd total_acc_x.mean total_acc_x.sd body_gyro_z.mean
## 4361       0.005752           0.2551       0.004315        -0.011971
## 1462       0.005684           1.0273       0.002584        -0.001889
## 467        0.163041           0.9581       0.330033         0.026372
## 9378       0.012350           1.0157       0.008299        -0.049373
## 5213       0.126604           1.0280       0.392023        -0.040570
## 5883       0.196065           0.9251       0.296423        -0.001243
## 1213       0.016286           1.0177       0.007628        -0.004052
## 7784       0.171633           1.0051       0.302189        -0.044859
## 4765       0.114936           1.0246       0.202521         0.008224
## 5891       0.165510           0.9682       0.290494         0.075773
##      body_gyro_z.sd
## 4361       0.006232
## 1462       0.007876
## 467        0.296742
## 9378       0.040806
## 5213       0.215322
## 5883       0.501439
## 1213       0.026090
## 7784       0.281628
## 4765       0.304897
## 5891       0.320690
```


### And finally

Creating the tidy data set. It is not clear which columns are requested so 
I will summarize the summarized Inertial data columns using `dplyr`.


```r
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
names(tidyData) <- names(allTheData)[1:21]
```

```
## Error: 'names' attribute [21] must be the same length as the vector [20]
```

And then the data are saved:


```r
write.table(x = tidyData, file = "tidyData.txt", row.names = FALSE)
```



