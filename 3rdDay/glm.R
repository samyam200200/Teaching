library(glmpath)
data("heart.data")
heart<-data.frame(heart.data)

# Lets begin by plotting the data
library(ggplot2)
ggplot(data=heart, aes(x=x.age, y=y))+
  geom_jitter(width=0, height = 0.05, alpha=0.5)

#Turning multiple columns to factors
heart[,c("x.famhist", "y")] <- lapply(heart[,c("x.famhist", "y")], factor)

#Lets plot a linear regression line
ggplot(data=heart, aes(x=x.age, y=as.numeric(y)))+
  geom_jitter(width=0, height = 0.05, alpha=0.5)+
  geom_smooth(method='lm', se=F)

#Splitting the data into test set and training set
library(caTools)
set.seed(123)
split=sample.split(heart$y, SplitRatio = 0.75)
training_set=subset(heart, split==T)
test_set=subset(heart, split==F)

#Feature scaling
training_set[, c(-5, -10)]<-scale(training_set[, c(-5, -10)])
test_set[, c(-5, -10)]<-scale(test_set[, c(-5, -10)])

#Fitting the logistic regression to the training set
# glm - is the generalized linear model - which separates the two classes
classifier=glm(formula = y~., 
               family = binomial, 
               data = training_set)
summary(classifier)

#Predicting the test set result
#Caluculates the probabilities
prob_pred=predict(classifier, type="response", newdata = test_set[,-10])

#Vector of predicted results using ifelse
y_pred=ifelse(prob_pred>0.5, 1, 0)

#Making the confusion matrix
cm=caret::confusionMatrix(data = test_set[,10], as.factor(y_pred))

# Visualizing the data

classi=glm(formula = y~x.age, 
               family = binomial, 
               data = heart)
yage<-predict(classi, list(x.age=seq(5, 100, 0.001)), type='response')
newdata<-data.frame(age=seq(5, 100, 0.001),prob=as.vector(yage))

p<-ggplot()+
         geom_smooth(method="glm",data = newdata, aes(x=age, y=prob), method.args=list(family="binomial"), se=FALSE)+scale_y_continuous(limits=c(0, 1))

library(plotly)
ggplotly(p)

