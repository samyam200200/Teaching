August<-read.csv("C:/Users/gocoo/OneDrive/Desktop/Projects/3rdDay/BikesAugust.csv")
July<-read.csv("C:/Users/gocoo/OneDrive/Desktop/Projects/3rdDay/BikesJuly.csv")
str(July)

#Variable column
vars<-c("hr" ,"holiday","workingday", "weathersit", "temp", "atemp","hum","windspeed")
# Outcome column
outcome<-"cnt"

#pasting everything for the formula
fmla <- paste("cnt", "~", paste(vars, collapse = " + "))

#Bikes count for July
#Poissons assumes the mean is equal to the variance, however, here we see:
mean(July$cnt)
var(July$cnt)

#We use a quasipossisons
#Bike model
bike_model <- glm(fmla, July, family="quasipoisson")
summary(bike_model)

#Making prediction on the Augsut data
August$pred<-predict(bike_model, newdata = August, type='response')

#Calculating RMSE
library(dplyr)
August%>%mutate(residual=pred-cnt)%>%summarise(rmse=sqrt(mean(residual^2)))

#Plotting the prediction vs the count
library(ggplot2)
ggplot(August, aes(x=pred, y=cnt))+
  geom_point()+
  geom_abline(color='darkblue')

#Plotting to see if the prediction is correct
library(tidyr)
August%>%
  mutate(instant=(instant-min(instant))/24)%>%
  gather(key=valuetype, value = value, cnt, pred)%>%
  filter(instant<14)%>%
  ggplot(aes(x=instant, y=value, color=valuetype))+
  geom_point()+ geom_line()+
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14)
