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
## 9152      27        6          0.82804       0.004058          0.57872
## 150        1        3         -0.12135       0.137461         -0.23269
## 7919      24        5         -0.03234       0.013693         -0.15084
## 3798      13        1          0.04581       0.120478         -0.29642
## 6009      19        4          0.67955       0.005904          0.44456
## 5982      19        4          0.43182       0.006580          0.27749
## 6096      19        6          0.58202       0.005288          0.83383
## 6786      21        5         -0.22161       0.009377         -0.34692
## 3288      11        4          0.16988       0.009397          0.01409
## 3710      12        6         -0.10128       0.008373          0.99262
##      total_acc_y.sd total_acc_x.mean total_acc_x.sd body_gyro_z.mean
## 9152       0.002749       -0.0577338       0.002884        0.0003161
## 150        0.152278        0.9809194       0.359898        0.0487390
## 7919       0.017752        1.0194774       0.007090       -0.0070575
## 3798       0.217552        0.9814338       0.243422        0.0065397
## 6009       0.004178        0.5928838       0.005089       -0.0024770
## 5982       0.004305        0.8751642       0.005012       -0.0009872
## 6096       0.003906        0.0005881       0.003220        0.0020802
## 6786       0.009452        0.9491385       0.004124       -0.0004632
## 3288       0.013026        1.0112189       0.014857        0.0046117
## 3710       0.005398        0.0878831       0.007565       -0.0003998
##      body_gyro_z.sd
## 9152       0.004364
## 150        0.440817
## 7919       0.020447
## 3798       0.288962
## 6009       0.009473
## 5982       0.011578
## 6096       0.006945
## 6786       0.015767
## 3288       0.022331
## 3710       0.013396
```



### The Inertial data are stored in separate files





