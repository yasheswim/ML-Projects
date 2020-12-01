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
##As the r suared value is 0.957, we need not perform transformation models##
confint(reg1,level = 0.95)
predict(reg1,interval="predict")
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
##Logarithamic model###
reg_log2 <- lm(Weight.gained..grams.~ log(Calories.Consumed))
summary(reg_log2)
##Exponential model##
reg_exp2 <- lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(reg_exp2)
##Quadratic model##
reg_quad2 <- lm(log(Weight.gained..grams.) ~ Calories.Consumed+ I(Calories.Consumed^2))
summary(reg_quad2)
##Cubic model##
reg_cubic2 <- lm(log(Weight.gained..grams.) ~ Calories.Consumed+ I(Calories.Consumed^2)+ I(Calories.Consumed^3))
summary(reg_cubic2)
##The cubic model gives the best r squared value##
pred2<-predict(reg_cubic2)
expy2<-exp(pred2)
sqrt(mean(Weight.gained..grams.-expy2)^2)  #rmse
confint(reg_cubic2,level = 0.95)
predict(reg_cubic2,interval="predict")
plot(pred2)
ggplot(data =sl2, aes(x =Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3), y =Weight.gained..grams.)) + 
  geom_point(color='blue') +geom_line(color='red',data =sl2, aes(y=expy2))

###Delivery time dataset###
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
##Logarithamic model##
reg3_log<-lm(Delivery.Time~log(Sorting.Time))
summary(reg3_log)
###Exponential model##
reg3_exp<-lm(log(Delivery.Time)~Sorting.Time)
summary(reg3_exp)
###Quadratic model###
quad_mod<-lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2))
summary(quad_mod)
##Cubic x model###
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

###Emp dataset###
sl4<-read.csv(file.choose())
View(sl4)
plot(sl4$Salary_hike,sl4$Churn_out_rate)
attach(sl4)
cor(Churn_out_rate,Salary_hike)
reg_4<-lm(Churn_out_rate~Salary_hike)
confint(reg_4,level=0.95)
predict(reg_4,interval="predict")
summary(reg_4)
##To improve the r squared value, we perform the following transformation models##
##Logarithamic model##
reg4_log<-lm(Churn_out_rate~log(Salary_hike))
summary(reg4_log)
##Exponential model##
reg4_exp<-lm(log(Churn_out_rate)~Salary_hike)
summary(reg4_exp)
##Quadratic model##
reg4_quad<-lm(log(Churn_out_rate)~Salary_hike+I(Salary_hike^2))
summary(reg4_quad)
##Cubic model##
poly_4<-lm(log(Churn_out_rate)~Salary_hike + I(Salary_hike^2) + I(Salary_hike^3))
summary(poly_4)
##Cubic model gives the best r squared value##
logpol4 <- predict(poly_4)
expy4 <- exp(logpol4)
rmse2<-sqrt(mean(Churn_out_rate-expy4)^2)

# visualization
ggplot(data =sl4, aes(x = Salary_hike + I(Salary_hike^2) + I(Salary_hike^3), y =Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data =sl4, aes(x=Salary_hike+I(Salary_hike^2)+I(Salary_hike^3), y=expy4))
?Indexplot
