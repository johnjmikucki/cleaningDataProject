# run_analysis.R 
# John J. Mikucki, 20140817

library(data.table)
library(reshape2)

cleanColumnNames = function(dir) {

  # load variable names (to use as column names)
  n = data.table(read.table(paste0(dir,"/features.txt")))
  #normalize variable names -- lower case, minimize troublesome punctuation marks.
  n$V2 = tolower(n$V2)
  n$v2 = gsub("\\(|\\)", "", perl = TRUE, x=n$V2)
  n$v2 = gsub(",","_", x=n$v2)
  
  n$V2
}

loadMeasurements = function(dir="UCI HAR Dataset") {
  
#  - 'features.txt': List of all features.
#  - 'activity_labels.txt': Links the class labels with their activity name.
#  - 'train/X_train.txt': Training set.
#  - 'train/y_train.txt': Training labels.
#  - 'test/X_test.txt': Test set.
#  - 'test/y_test.txt': Test labels.
  
  actmap = data.table(read.table(paste0(dir,"/activity_labels.txt")))
  colnames(actmap) = cbind("id","name")
  actmap$name = tolower(actmap$name)
  
  varnames = cleanColumnNames(dir)
  
  # load variable values, associate with column names
  train =data.table(read.table(file = paste0(dir,"/train/X_train.txt"), header = FALSE, col.names = varnames))
  subj = data.table(subject=read.table(file = paste0(dir,"/train/subject_train.txt")))
 # colnames(subj) = "subject"
  train$subject =subj
  
  actions = data.table(action=read.table(file = paste0(dir,"/train/y_train.txt")))
  #  @colnames(actions) = "action"
  #train[, action:= actions]
  #train$action = factor(train$action)

  train$action = factor(actions)
  levels(train$action) = actmap$name
  #  train[, actDesc :=actmap[actionID]]
  
  # load variable values, associate with column names
  test = data.table(read.table(file = paste0(dir,"/test/X_test.txt"), header = FALSE, col.names = varnames))
  subj = data.table(read.table(file = paste0(dir,"/test/subject_test.txt")))
  colnames(subj) = "subject"
  test[,subject := subj]
  
  actions = data.table(read.table(file = paste0(dir,"/test/y_test.txt")))
  colnames(actions) = "action"
  test[, action:= actions]
  test$action = factor(test$action)
  levels(test$action) = actmap$name
#  test[, actionID:= actions]
#  test[, actDesc :=actmap[actionID]]
  
  all = rbind(train,test) 
}

extractMeanAndStddev = function(data) {
  selection1 = grep(pattern = "*mean()|*std()", colnames(all))
  selection2 = grep(pattern = "subject|action", colnames(all))
  
  result = data[c(selection1, selection2)]
  result
}
computeGroupMeans = function(data) {
  meltdata = melt(data, id.vars = c('subject','action'))
  extracted = dcast(meltdata,subject+action ~ variable,mean)
}

doit = function() {
  
  # 1. Merges the training and the test sets to create one data set.
  # to get this dataset, run:
  data = loadMeasurements()
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  # To extract these, run:
  extracts = extractMeanAndStddev(data)
  
  # 3. Uses descriptive activity names to name the activities in the data set
  # we do this during the load phase
  
  # 4. Appropriately labels the data set with descriptive variable names. 
  # we do this during the load phase
  
  # 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  means = computeGroupMeans(data)
  
  # 6. Package tidy data set created in step 5 of the instructions. 
  # Please upload your data set as a txt file created with
  # write.table() using row.name=FALSE 
  # (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).
  write.table(means,row.names=FALSE, file = "extractedgroupmeans.dat")
  
}