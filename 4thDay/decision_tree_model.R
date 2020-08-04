#Decision tree and Random forest - Regression and Classification

data<-read.csv("https://assets.datacamp.com/production/repositories/710/datasets/b649085c43111c83ba7ab6ec172d83cdc14a2942/credit.csv")

#structure and summary
str(data)

#Cleaning the dataset the model
data<-data[, c(2, 8, 9, 10, 17)]

#credit model
library(rpart)
credit_model<-rpart(formula = default~., 
                    data=data, 
                    method = 'class', 
                    parms = list(split="gini"))

#Displaying the result
library(rpart.plot)
rpart.plot(x=credit_model, 
           yesno=2, 
           type=1, 
           extra=0)

