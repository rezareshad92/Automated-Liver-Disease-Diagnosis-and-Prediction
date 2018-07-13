data <- read.csv("diabetes.csv")

summary(data)

data$Outcome<-as.factor(data$Outcome)
set.seed(999)

splitIndex<-createDataPartition(data$Outcome,p=.70,list=F,times=1)

train<-data[splitIndex,]
test<-data[-splitIndex,]
training_data<-train
testing_data<-test

#Running KNN Model

knn_pred<-knn(training_data[,-9],testing_data[,-9],training_data[,9],11)
caret::confusionMatrix(testing_data[,9], knn_pred, mode = "prec_recall")

