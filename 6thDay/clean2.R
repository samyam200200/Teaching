
  ### Loading dependencies
 
library(caret)
library(tidyverse)
library(doSNOW)



### Importing and cleaning data

url<-"https://assets.datacamp.com/production/course_6430/datasets/WisconsinCancer.csv"
wbcd_gradient<-read.csv(url, header = T, sep=",")
wbcd_gradient<-wbcd_gradient[c(-1,-33)]


### Test train spilt

set.seed(123)
inTraining <- createDataPartition(wbcd_gradient$diagnosis, p = .75, list = FALSE)
training <- wbcd_gradient[ inTraining,]
testing  <- wbcd_gradient[-inTraining,]


### Preparig for 10 fold cross validation 10 times

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)


### Fitting GBM without parameters tuning

set.seed(1245)
gbmFit1 <- train(diagnosis ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)

gbmFit1$bestTune
gbmFit1$results
### Results (without parameter tunings)

predicted<-predict(gbmFit1, testing)
confusionMatrix(predicted, as.factor(testing$diagnosis))


### Preparing for repeated cross validation for parameter tunings
## interaction.depth (Maximum nodes per tree) - number of splits it has to perform on a tree (starting from a single node).
## Shrinkage (Learning Rate) - It is considered as a learning rate.
## At each step of the GBM algorithm, a new decision tree is constructed. The question when growing a decision tree is 'when to stop?'. The furthest you can go is to split each node until there is only 1 observation in each terminal node. This would correspond to n.minobsinnode=1. 
gbmGrid<-expand.grid(interaction.depth=c(1, 3, 5), n.trees = (0:50)*50,
                     shrinkage=c(.05,.0005, .1, 0.01),
                     n.minobsinnode=c(5, 10, 15, 20))


### Fitting parametrs into new gbm model to find the best tuned parameters (using accuracy of the fit as the guide)

cl<-makeCluster(6, type="SOCK")
registerDoSNOW(cl)
set.seed(124)
gbmFit2 <- train(diagnosis ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 metric = "Accuracy")
stopCluster(cl)


### Results of accuracy based model

gbmFit2$bestTune
plot(gbmFit2)


predicted<-predict(gbmFit2, testing)
confusionMatrix(predicted, as.factor(testing$diagnosis))


### Preparing for AUC based GBM model (instead of accuracy AUC will be used as measure of model goodness)



### Comparing the GBM method with randomforest
### Tuning grid

tunegrid <- expand.grid(mtry=1:15,
                        splitrule = "gini", 
                        min.node.size = c(5, 10, 15, 20),
                        )


### Fitting model

cl<-makeCluster(6, type="SOCK")
registerDoSNOW(cl)
set.seed(588)
rfTuned <- train(diagnosis~., 
                 data=training, 
                 method="ranger", 
                 metric="ROC", 
                 tuneGrid=tunegrid, 
                 trControl=fitControl)
stopCluster(cl)


### Fitting the model into testing data

predictedrf<-predict(rfTuned, newdata = testing)
confusionMatrix(predictedrf, as.factor(testing$diagnosis))


lrTuned <- train(diagnosis~., 
                 data=training, 
                 method="glm", 
                 metric="ROC", 
                 trControl=fitControl, 
                 family="binomial")





