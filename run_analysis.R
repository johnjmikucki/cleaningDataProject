# run_analysis.R 
# John J. Mikucki, 20140817

library(data.table)
library(reshape2)

cleanColumnNames = function(dir) {

  # load variable names (to use as column names)
  n = data.table(read.table(paste0(dir,"/features.txt")))
  #normalize variable names -- lower case, minimize troublesome punctuation marks.
  n$V2 = tolower(n$V2)
  n$V2 = gsub("\\(|\\)", "", perl = TRUE, x=n$V2)
  n$V2 = gsub(",","_", x=n$V2)
  
  n$V2
}

cleanActMap  = function(dir) {
  # load file
  actmap = data.table(read.table(paste0(dir,"/activity_labels.txt")))
  # handles
  colnames(actmap) = cbind("id","name")
  # set to lowercase and remove underscores
  actmap$name = tolower(actmap$name)
  actmap$name = gsub("_","",actmap$name)
  
  #return just the names
  actmap$name
}

loadDataset = function(dir,name, varnames, actmap) {
  
  # compute filenames
  # "X" data
  xfn = paste0(dir,"/",name,"/X_",name, ".txt")
  # "Y" data
  yfn = paste0(dir,"/",name,"/y_",name, ".txt")
  # subject info
  sfn = paste0(dir,"/",name,"/subject_",name,".txt")
  
  # load data, associate with column names and the subject being measured.
  data = data.table(read.table(xfn, header = FALSE, col.names = varnames))
  subj = data.table(subject=read.table(sfn))
  data$subject = subj
  
  # load activities, associate with measurements
  # treat activity like a factor so it displays the names nicely  
  activities = data.table(activity=read.table(yfn))  
  data$activity = as.factor(x= activities$activity.V1)
  levels(data$activity)  = actmap$name

  #return the loaded dataset
  data
}

loadMeasurements = function(dir="UCI HAR Dataset") {  
#  - 'features.txt': List of all features.
#  - 'activity_labels.txt': Links the class labels with their activity name.
#  - 'train/X_train.txt': Training set.
#  - 'train/y_train.txt': Training labels.
#  - 'test/X_test.txt': Test set.
#  - 'test/y_test.txt': Test labels.
  
  # fetch cleaned activity/name mappings
  actnames = cleanActMap(dir)
  
  # fetch cleaned measurement names(col names)
  varnames = cleanColumnNames(dir)
  
  # load training exemplars
  train = loadDataset(dir, "train", varnames, actmap)
  
  # load test examplars
  test = loadDataset(dir, "test", varnames, actmap)
  
  #catenate and return complete dataset
  all = rbind(train,test) 
}

extractMeanAndStddev = function(data) {
  # this requirement is a bit underspecified, so I'm taking the loosest interpretation
  # pull out the name, activity, and anything with means or standard deviation in the measurement name
  selection = grep(pattern = "subject|activity|*mean*|*std*", colnames(data))
  
  result = subset(data,TRUE,selection)
  result
}

computeGroupMeans = function(data) {
  # reshape by subject and activity
  meltdata = melt(data, id.vars = c('subject','activity'))
  # compute mean, grouping by subject and activity (in that order)
  extracted = dcast(meltdata,subject+activity ~ variable,mean)
}

doit = function() {
  # 1. Merges the training and the test sets to create one data set.
  # to get this dataset, run:
  data = loadMeasurements()
 # print(dim(data))
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  # To extract these, run:
  extracts = extractMeanAndStddev(data)
 #print(dim(extracts))
  
  
  # 3. Uses descriptive activity names to name the activities in the data set
  # we map these on during the load phase
  
  # 4. Appropriately labels the data set with descriptive variable names. 
  # we map these on during the load phase
  
  # 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  means = computeGroupMeans(data)
  print(means)
  
  # 6. Package tidy data set created in step 5 of the instructions. 
  # Please upload your data set as a txt file created with write.table() using row.name=FALSE 
  write.table(means,row.names=FALSE, file = "extractedgroupmeans.txt")
  
}