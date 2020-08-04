#Decision tree and Random forest - Regression and Classification

data<-read.csv("https://assets.datacamp.com/production/repositories/710/datasets/b649085c43111c83ba7ab6ec172d83cdc14a2942/credit.csv")

#structure and summary
str(data)

#Cleaning the dataset the model
data<-data[, c(2, 8, 9, 10, 17)]

#Converting the default to the factors
data$default<-factor(data$default, levels = c("yes", 'no'), labels = c(0,1))

#Test-train split
library(caTools)
set.seed(123)
split=sample.split(data$default, SplitRatio = 0.75)

training_set<-subset(data, split==TRUE)
test_set<-subset(data, split==FALSE)

#Feature scaling
training_set[-5]<-scale(training_set[-5])
test_set[-5]<-scale(test_set[-5])

#Creating the classification
library(randomForest)

#creating the classifier
#x, y, ntree #if too much tree might over fit
classifier<-randomForest(x=training_set[-5], 
                        y=training_set$default,
                        ntree = 1000)

#predicting for test set
y_pred<-predict(classifier, newdata = test_set[-5], type = 'response')

#Making confusion matrix
cm=table(test_set[,5], y_pred)

