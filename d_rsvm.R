# k-Fold Cross Validation

# Importing the dataset
dataset = read.csv('diabetes.csv')

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Outcome, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Kernel SVM to the Training set
library(e1071)
classifier = svm(formula = Outcome ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial',
                 sigma = .128514,
                 C = 0.25)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-9])

# Making the Confusion Matrix
cm = table(test_set[, 9], y_pred)

# Applying k-Fold Cross Validation
library(caret)
folds = createFolds(training_set$Outcome, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = Outcome ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-9])
  cm = table(test_fold[, 9], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))  #accuracy = 76.4% after using grid search

# Applying Grid Search to find best parameters
library(caret)
classifier <- train(form = Outcome ~ .,
                    data = training_set,
                    method = 'svmRadial')
classifier
classifier$bestTune
