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
trainingdata <- trainingrawdata[, -nzv]
nzv

# remove variables that are almost always NA
mostlyNA <- sapply(trainingdata, function(x) mean(is.na(x))) > 0.95
trainingdata <- trainingdata[, mostlyNA==F]

# remove variables that don't make intuitive sense for prediction (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp), which happen to be the first five variables
trainingdata <- trainingdata[, -(1:5)]

#now split into training and testing portions of the training data set
set.seed(5846)
index_train <- createDataPartition(y=trainingdata$classe, p=0.60, list=FALSE)
data_train <- trainingdata[index_train,]
data_xval <- trainingdata[-index_train,]
dim(data_train); dim(data_xval)

#let's look at the distribution of classes
require(lattice)
barchart(table(data_train$classe))

#this still leaves us with 53 variable to predict the exercise class
Correlations<-cor(data_train[,-54])
diag(Correlations)<-0
which(Correlations>.8,arr.ind=T)
#standardize to reduce algoithum churn and simplify model building
PreProcess<-preProcess(data_train[-54],method=c("center","scale"))