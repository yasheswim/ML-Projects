install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)

###Salary vs Experience dataset
sl1<-read.csv(file.choose())
View(sl1)
attach(sl1)
plot(sl1$YearsExperience,sl1$Salary)
cor(Salary,YearsExperience)
reg1<-lm(Salary~YearsExperience)
summary(reg1)
confint(reg1,level = 0.95)
predict(reg1,inteval="predict")
reg1$residuals
reg1$fitted.values
pred <- predict(reg1)
sum(reg1$residuals)
sqrt(mean(reg1$residuals^2))
confint(reg1,level = 0.95)
qqnorm(reg1$residuals)
qqline(reg1$residuals)
ggplot(data =sl1, aes(x =YearsExperience, y =Salary)) + 
  geom_point(color='blue') +geom_line(color='red',data =sl1, aes(x=YearsExperience, y=pred))


##Calories consumed dataset###
sl2<-read.csv(file.choose())
View(sl2)
attach(sl2)
plot(sl2$Calories.Consumed,sl2$Weight.gained..grams.)
cor(Weight.gained..grams.,Calories.Consumed)
reg2<-lm(Weight.gained..grams.~Calories.Consumed)
reg2$residuals
reg2$fitted.values
reg2$coefficients
summary(reg2)
pred1<-predict(reg2)
View(pred1)
confint(reg2,level = 0.95)
predict(reg2,inteval="predict")
sum(reg2$residuals)
sqrt(mean(reg2$residuals^2))
View(reg2)
qqnorm(reg2$residuals)
qqline(reg2$residuals)
ggplot(data =sl2, aes(x =Calories.Consumed, y =Weight.gained..grams.)) + 
  geom_point(color='blue') +geom_line(color='red',data =sl2, aes(x=Calories.Consumed, y=pred1))

###Delivery time###
sl3<-read.csv(file.choose())
View(sl3)
attach(sl3)
plot(sl3$Sorting.Time,sl3$Delivery.Time)
cor(Delivery.Time,Sorting.Time)
reg3<-lm(Delivery.Time~Sorting.Time)
reg3$residuals
reg3$fitted.values
reg3$coefficients
summary(reg3)
###The probability is low but the r squared value is moderate so we will apply transformation techniques and find which is the best fit model##
###Exponential model##
reg3_exp<-lm(log(Delivery.Time)~Sorting.Time)
summary(reg3_exp)
###Quadratic model###
quad_mod<-lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2))
summary(quad_mod)
##Squaring x model###
poly_3<-lm(Delivery.Time~(Sorting.Time^2))
summary(poly_3)
###From the test and trial methods,we find that exponential model is the best fit##
confint(reg3_exp,level=0.95)
predict(reg3_exp,interval="predict")
pred_3<-exp(predict(reg3_exp))
Final<-cbind(Sorting_time=sl3$Sorting.Time,Delivery_time=sl3$Delivery.Time,Predicted_delivery_time=pred_3)
View(Final)
rmse<-sqrt(mean((Delivery.Time-pred_3)^2))
ggplot(data =sl3, aes(x =Sorting.Time, y =Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data =sl3, aes(x=Sorting.Time, y=pred_3))

###Emp data###
sl4<-read.csv(file.choose())
View(sl4)
plot(sl4$Salary_hike,sl4$Churn_out_rate)
attach(sl4)
cor(Churn_out_rate,Salary_hike)
reg_4<-lm(Churn_out_rate~Salary_hike)
confint(reg_4,level=0.95)
predict(reg_4,interval="predict")
###After checking a couple of transformation techniques, we found that cubic model gives the best r squared value###
poly_4<-lm(log(Churn_out_rate)~Salary_hike + I(Salary_hike^2) + I(Salary_hike^3))

summary(poly_4)
logpol4 <- predict(poly_4)
expy4 <- exp(logpol4)
rmse2<-sqrt(mean(Churn_out_rate-expy4)^2)

# visualization
ggplot(data =sl4, aes(x = Salary_hike + I(Salary_hike^2) + I(Salary_hike^3), y =Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data =sl4, aes(x=Salary_hike+I(Salary_hike^2)+I(Salary_hike^3), y=expy4))
