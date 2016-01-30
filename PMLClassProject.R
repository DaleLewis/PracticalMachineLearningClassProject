#Let's read in the raw data
trainingrawdata <- read.csv(file="pml-training.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))
testingrawdata <- read.csv(file="pml-testing.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))

#Now make the class attribute a factor variable
trainingrawdata$classe <- as.factor(trainingrawdata$classe) 

#let's look at the raw data
summary(trainingrawdata)


#load caret package to do the analysis
require(caret)

# remove variables with nearly zero variance
nzv <- nearZeroVar(trainingrawdata,saveMetrics=T)
nzv
allrawdata <- trainingrawdata[, nzv$nzv==FALSE]


# remove variables that are almost always NA
mostlyNA <- sapply(allrawdata, function(x) mean(is.na(x))) > 0.95
allrawdata <- allrawdata[, mostlyNA==FALSE]

# remove variables that don't make intuitive sense for prediction (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp), which happen to be the first five variables
allrawdata <- allrawdata[, -(1:5)]


#now split into training and testing portions of the training data set
set.seed(5846)
index_train <- createDataPartition(y=allrawdata$classe, p=0.60, list=FALSE)
data_train <- trainingdata[index_train,]
data_xval <- trainingdata[-index_train,]
dim(data_train); dim(data_xval)

#let's look at the distribution of classes
require(lattice)
barchart(table(data_train$classe))
#this looks like a pretty equal distribution so lets train the model
#first choice is random forest since this is a clasification problem
require(randomForest)
modfit <-train(classe~.,data=data_train,method="rf",trControl=trainControl(method = "cv",number = 4,allowParallel = TRUE,verboseIter = TRUE))
#check the model by cross validation
pred_rf <- predict(modfit,data_xval)
cm_rf <- confusionMatrix(pred_rf,data_xval$classe)
cm_rf
#Accuracy is over 99% 
#test against final test set provided
FinalValidationTest <- predict(modfit, testingrawdata)
FinalValidationTest