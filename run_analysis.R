library(reshape2)

#set data file path
filepath <- "./UCI HAR Dataset/"

#############################################################
#1. Merges the training and the test sets to create one data set.

#load train data frame & y value(label)
trfeatures <- read.table(paste0(filepath, "train/X_train.txt"), fill = TRUE)
trlabel <- read.table(paste0(filepath, "train/y_train.txt"))
trsubj <- read.table(paste0(filepath, "train/subject_train.txt"))
trdf <- cbind(trsubj, trfeatures)

#load test data frame
tsfeatures <- read.table(paste0(filepath, "test/X_test.txt"), fill = TRUE)
tslabel <- read.table(paste0(filepath, "test/y_test.txt"))
tssubj <- read.table(paste0(filepath, "test/subject_test.txt"))
tsdf <- cbind(tssubj, tsfeatures)

#merge data frame
mergdf <- rbind(trdf, tsdf)

#############################################################
#2. Extracts only the measurements on the mean and 
#standard deviation for each measurement.
featurenames <- read.table(paste0(filepath, "features.txt"), colClasses = character())
extractfeatures <- grep(pattern = "(mean|std)\\(\\)", featurenames[ ,2])
submdf <- subset(mergdf, select = c(1, extractfeatures + 1)) #added by +1 because of subject column

#3. Uses descriptive activity names to name the activities in the data set
actlabel <- read.table(paste0(filepath, "activity_labels.txt"))
actlabel[, 2] <- as.character(actlabel[, 2])
labels <- as.data.frame(rbind(trlabel, tslabel)) #concat train and test label
alldf <- cbind(submdf, labels) # attach labels to the data frame
exfnames <- featurenames[extractfeatures, 2]
colnames(alldf) <- c("subject", as.character(exfnames), "activity")

#4. Appropriately labels the data set with descriptive variable names.
alldf$activity <- factor(alldf$activity, levels = actlabel[,1], labels = actlabel[,2])
alldf$subject <- as.factor(alldf$subject)

#5. From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
alldf.melted <- melt(alldf, id = c("subject", "activity"))
alldf.mean <- dcast(alldf.melted, subject + activity ~ variable, mean)

write.table(alldf.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
