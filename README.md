# ML-Projects
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
