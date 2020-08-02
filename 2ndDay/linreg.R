#Linear modeling - NC birth dataset - 1450 birth records that statistician John Holcomb selected from the North Carolina State Center for Health and Environmental Statistics.
library(Stat2Data)

#Installing packages 
library(ggplot2)

#Summarizing the data
data(NCbirths)
summary(NCbirths)
?NCbirths

#finding the missing data
sum(is.na(NCbirths))
which(is.na(NCbirths$Weeks))
names(NCbirths)

#Weeks and weights - does late birth lead to higher birth weight - Macrosmia
#Define the independent and dependent variable - Birthweight is related to the weeks
ggplot(data=NCbirths, aes(x=Weeks, y=BirthWeightGm))+
  geom_jitter(alpha=0.5)

#Adding breaks
ggplot(NCbirths, aes(x=cut(Weeks, breaks = 5), y=BirthWeightGm))+
  geom_boxplot()

ggplot(data=NCbirths, aes(x=Weeks, y=BirthWeightGm))+
  geom_abline(slope = 100, intercept = -100)+
  geom_point(alpha=0.5)

ggplot(data=NCbirths, aes(x=Weeks, y=BirthWeightGm))+
  geom_abline(slope = 80, intercept = -150)+
  geom_point(alpha=0.5)


#plotting the geom_line
ggplot(data=NCbirths, aes(x=Weeks, y=BirthWeightGm))+
  geom_point(alpha=0.5)+
  geom_smooth(method = 'loess')

#test training split
library(caTools)
set.seed(123)
split=sample.split(NCbirths, SplitRatio = 0.8)
training_set=subset(NCbirths, split==T)
test_set=subset(NCbirths, split==F)

#train the model in the training set and we will test it powers in the test set
regression<-lm(formula = BirthWeightGm ~ Weeks, 
               data=training_set)
summary(regression)
plot(regression)

#Predicting the test set results
y_pred=predict(object = regression, newdata = test_set)
View(y_pred)

#Visualizing the results
# install.packages(ggplot2)
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$Weeks, y=training_set$BirthWeightGm), 
             color='red')+
  geom_line(aes(x=training_set$Weeks, y=predict(object = regression, newdata = training_set)),
            color='blue')+
  ggtitle("Birth Weight in Gram vs No. of Weeks training set")+
  xlab("No. of Weeks")+
  ylab("Birth weight in Gram")

# Lets see for the new observations
ggplot()+
  geom_point(aes(x=test_set$Weeks, y=test_set$BirthWeightGm), 
             color='red')+
  geom_line(aes(x=test_set$Weeks, y=predict(object = regression, newdata = test_set)), 
            color='blue')+
  ggtitle("Birth Weight in Gram vs No. of Weeks test set")+
  xlab("No. of Weeks")+
  ylab("Birth weight in Gram")

# Baby weight in relation to Mom's age, Sex, Weeks, Smoking
NCbirths2<-NCbirths[,c(3, 4, 5, 10, 12)]


# Multiple regression
#Smoking is a categorical varibale - we have to encode it as factor
summary(NCbirths2)
#Let us begin by removing the NAs
NCbirths2<-na.omit(NCbirths2)

NCbirths2$Smoke<-ifelse(NCbirths2$Smoke==1, "Smoker", "Non-smoker")

#Creating the factors
NCbirths2$Smoke<-factor(NCbirths2$Smoke, levels = c("Non-smoker", "Smoker"), labels=c(0,1))

NCbirths2$Sex<-as.factor(NCbirths2$Sex)

#Splitting in testing and training Set
library(caTools)
set.seed(123)
split=sample.split(NCbirths2, SplitRatio = 0.8)
training_set=subset(NCbirths2, split==T)
test_set=subset(NCbirths2, split==F)

# Lets build the regression formula Sex+MomAge+Weeks+Smoke
regression=lm(formula = BirthWeightGm ~ Sex+MomAge+Weeks+Smoke,
              data=training_set)

#Summarizing the model - lower the p-value more impact the independent variable will have on the dependent variable
summary(regression)

#Predicting the Test set results 
y_pred=predict(regression, newdata = test_set)

#3-d scatterplot for multiple regression
library(plotly)
p<-plot_ly(data=NCbirths2, z=~BirthWeightGm, x=~Weeks, y=~MomAge, opacity=0.5)%>%
  add_markers(color=~factor(Smoke), colors = c('blue' ,'red'), marker = list(size = 2))

p
# For non-linear relationships - to solve non-linear problems - we use the polynomial regression
# Dependent - Weights, Independent -weeks
NCbirths3<-NCbirths[,c("Weeks", "BirthWeightGm")]

#Lets not do a test train split
# Which is better/appropriate - linear or plynomial
linreg<-lm(formula = BirthWeightGm~., data = NCbirths3)
summary(linreg)
NCbirths3$linearpredicted<-predict(object = linreg, NCbirths3)

#Polyreg model
polyreg<-lm(formula = BirthWeightGm~Weeks+I(Weeks^2), data = NCbirths3)
summary(polyreg)

NCbirths3$predicted<-predict(object = polyreg, NCbirths3)

#Visualizng the results of the linear regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x=NCbirths3$Weeks, y=NCbirths3$BirthWeightGm), 
             color='red')+
  geom_line(aes(x=NCbirths3$Weeks, y=predict(object = linreg, newdata=NCbirths3) 
                ), color='blue')+
  ggtitle("Checking the birth wt at 40 weeks")+
  xlab("Weeks")+
  ylab("Birthweight in GM")

#Vizualing the polynomial regression model
ggplot()+
  geom_point(aes(x=NCbirths3$Weeks, y=NCbirths3$BirthWeightGm), 
             color='red')+
  geom_line(aes(x=NCbirths3$Weeks, y=predict(object = polyreg, NCbirths3) 
  ), color='blue')+
  ggtitle("Checking the birth wt at 40 weeks")+
  xlab("Weeks")+
  ylab("Birthweight in GM")

# Predicting individual data
# Predicting with linear regression
y_pred=predict(linreg, newdata = data.frame(Weeks=25))

#Predicting with polynomial regression model
y_pred=predict(polyreg, newdata = data.frame(Weeks=25))
