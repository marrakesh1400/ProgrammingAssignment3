# R code to create tidy data set from Samsung Galaxy S smartphone
# accelerometer data. Prior to running to running the code, 
# set working directory to the "UCI HAR Dataset" directory
# in the unzipped data download

# use read.table() to read in text file
xTrain <- data.frame(read.table("train/X_train.txt", stringsAsFactors=FALSE))
yTrain = read.table("train/y_train.txt", header = FALSE)
subjectTrain = read.table("train/subject_train.txt", header = FALSE)
# column bind subject, activity labels and training data
train = cbind(subjectTrain,yTrain,xTrain)

# use read.table() to read in text file
xTest = data.frame(read.table("test/X_test.txt", stringsAsFactors=FALSE))
yTest = read.table("test/y_test.txt", header = FALSE)
subjectTest = read.table("test/subject_test.txt", header = FALSE)
# column bind subject, activity labels and testing data
test = cbind(subjectTest,yTest,xTest)

# merges training data on top of test data of 10299 rows x 563 columns
# first 7352 rows are training, next 2947 rows are test data
data = rbind(train,test)

# read in features text file
features = readLines("features.txt")
# add subjects and activity labels
features = c("subject", "activityLabel",features)

# read in activity labels
activities = read.table("activity_labels.txt", sep = ' ')

# use descriptive activity names to name the activities in the data set
for (i in 1:nrow(activities)){
  data[,2][data[,2] %in% activities[i,1]] <- as.character(activities[i,2])
}

# find which features are means or standard deviations
stds = grepl('std()',features, fixed=T)
means = grepl('mean()',features, fixed=T)
# don't forget the first two columns
mscols = which((stds+means) == 1)
mscols = c(1,2,mscols)

# now extract only those columns, create new data frame called "ms"
ms = data[,mscols]
# extract corresponding feature/variable names
msFeatures = features[mscols]
# apply feature/variable names to data frame
names(ms) = msFeatures

# get list of total and unique subject/activity combinations
sac = interaction(ms[,1], ms[,2])
sau = unique(interaction(ms[,1], ms[,2]))

# create a second, independent tidy data set with the average of each variable 
# for each activity and each subject
msMeans = data.frame(sau)

# for each variable, append column with averages for each subject/activity combination
for (i in 3:ncol(ms)){
  tmp = tapply(ms[,i], sac, mean, na.rm = T)
  msMeans = cbind(msMeans,tmp)  
}

# assign names to msMeans columns
names(msMeans) = c("subject/activity",msFeatures[3:length(msFeatures)])
# write table out as space-delimited text file, "tidydata.txt" 
write.table(msMeans, file = "tidydata.txt",row.name = F)
