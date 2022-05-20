install.packages("tidyverse")
install.packages("car")
install.packages("boot")
install.packages("dplyr")
#install.packages("cor.test")
library(tidyverse)
library(car)
library(boot)
#library(QuantPsyc)
library(dplyr)


attach(data)
names(data)
class(data)
model1<- lm(data$price~data$mes)
lm(data$price~data$mes)
coefficientsLine<-model1$coefficients
model1$coefficients
#LINE EQUATION y=a+bx: dependentVariable=intercept+slopeLine(x=intialValue)
equationLine = 7.647835e+02+(5.021842e-07*125)
plot(data$price~data$mes, 
     col=('purple'),
     main = 'DATA CORRELATION',
     xlab = 'YEAR',
     ylab = 'PRICE')
abline(model1, col=('red'))


