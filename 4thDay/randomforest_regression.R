August<-read.csv("C:/Users/gocoo/OneDrive/Desktop/Projects/3rdDay/BikesAugust.csv")
July<-read.csv("C:/Users/gocoo/OneDrive/Desktop/Projects/3rdDay/BikesJuly.csv")
str(July)

#Variable column
July<-July[,c("hr" ,"holiday","workingday", "weathersit", "temp", "atemp","hum","windspeed", "cnt")]


library(randomForest)

#We use a quasipossisons
#Bike model
library(randomForest)
bike_model <- randomForest(x = July[-9], y=July$cnt, ntree = 500)

#Making predictions
August$pred<-predict(bike_model, newdata = August, type='response')

#Prediction and count plot
library(ggplot2)
ggplot(August, aes(x=pred, y=cnt))+
  geom_point()+
  geom_abline(color='darkblue')

library(dplyr)
August%>%mutate(residual=pred-cnt)%>%summarise(rmse=sqrt(mean(residual^2)))

#Plotting to see if the prediction is correct
library(tidyr)
August%>%
  mutate(instant=(instant-min(instant))/24)%>%
  gather(key=valuetype, value = value, cnt, pred)%>%
  filter(instant<14)%>%
  ggplot(aes(x=instant, y=value, color=valuetype))+
  geom_point()+ geom_line()+
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14)
