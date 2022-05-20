install.packages('ggplot2')
install.packages('e1071')
install.packages('quantmod')
install.packages('tseries')
install.packages('fImport')
library(tseries)
library(quantmod)
library(fImport)
library(e1071)
library(ggplot2)
View(sugar)


##########################
#FREQUENCIES DISTRIBUTION#
##########################
n<-14432

data <- (sugar)

tabla <- as.data.frame(table(data$`price`))


FrequencyPrice <- transform(tabla,
                            FreqAcum = cumsum(tabla$Freq),
                            Rel = round(prop.table(tabla$Freq), 6),
                            RelAc = round(cumsum(prop.table(tabla$Freq)),6))

#HISTOGRAM#
priceHistogram <- hist(data$price,
                       main = ("FRECUENCY DISTRIBUTION - PRICE"),
                       xlab = ("PRICES"),
                       ylab = ("FREQUENCY"),
                       col = c("red",'blue', 'green', 'purple', 'orange','gray', 'yellow', 'pink'))

##############################
#MEASURES OF CENTRAL TENDENCY#
##############################

#mean#
mean = round(mean(data$`price`),2)


#median#
median = round(median(data$`price`),2)


#mode#
mode <- as.data.frame(table(data$`price`))



########################
#MEASURES OF DISPERSION#
########################

#RANGE#
range <- range(data$`price`)

rangeValue = max(data$price) - min(data$price)

#VARIANCE#
variance <- round(var(data$`price`),2)

#STANDARD DEVIATION#
SDS <- round(sd(data$`price`),2)

#COEFFICIENT OF VARIATION#
coefficientVariation <- (round((SDS/mean),2)*100)

#DIAGRAM DISPERSION#
diagramDispertion <- plot(data$`price`,
                          col = 'red',
                          main = "DISPERSION DIAGRAM - PRICE",
                          xlab = 'DATA',
                          ylab = 'PRICES')

#DATA SUMMARY#
dataSummary<-summary(data$`price`)

##################
#MEASURES OF FORM#
##################

#SIMETRY#
simetry <- round(skewness(data$`price`),2)


#KURTOSIS#
Kurtosis <- round(kurtosis(data$`price`),2)


######################
#POSITION MEASUREMENT#
######################
cuartil <- quantile(data$`price`)
percentil <- quantile(data$`price`, seq(0,1, length = 101))

######################
#POISSON DISTRIBUTION#
######################
#CALCULO DE VALORES ESPECIFICOS#
#I want to take 3 case for this

#probability that the price of sugar is free taking into account the mean as the success probability, p(x=0)
zero<-dpois(0, mean)

#probability that the sugar's price is between 800 and 1200 cop, approximately p(800 <= x <= 1200)
#I am going to take values from 50 in 50
between<-round(dpois(800, mean)+dpois(850, mean)+dpois(900, mean)+dpois(950, mean)+dpois(1000, mean)+dpois(1050, mean)+dpois(1100, mean)+dpois(1150,mean)+dpois(1200, mean),3)

#probability that the sugar's price is more than 1000 cop, p(x>1000)
greater<- round(1-(dpois(0,mean)+dpois(50, mean)+dpois(100, mean)+dpois(150, mean)+dpois(200, mean)+dpois(250, mean)+dpois(300, mean)+dpois(350, mean)+dpois(400,mean)+dpois(450, mean)+dpois(500, mean)+dpois(550, mean)+dpois(600, mean)+dpois(650, mean)+dpois(700, mean)+dpois(750, mean)+dpois(800, mean)+dpois(850, mean)+dpois(900, mean)+dpois(950, mean)+dpois(1000, mean)+dpois(1050, mean)+dpois(1100, mean)+dpois(1150,mean)+dpois(1200, mean)),3)

#POISSON PROBABILITY FUNTION#
x<-seq(0:10000)
plot(dpois(x,mean), col='green',
     main ='POISSON PROBABILITY FUNCTION ',
     xlab = 'SUCCESS NUMBER',
     ylab = 'PROBABILITY')

######################
#BINOMIAL DISTRIBUTION#
######################
#I want to select a sample of 10000 prices aleatory 
#what is the probability that the price is greater than 1000 cop, p(x>1000)
j=10000
great<-(1-dbinom(1000,j,greater))

#what is the probability that the price is between 800 and 1200 cop, p(800 <= x <= 1200)
btween<-dbinom(800,j, greater)+dbinom(850,j, greater)+dbinom(900,j, greater)+dbinom(950,j, greater)+dbinom(1000,j, greater)+dbinom(1050,j, greater)+dbinom(1100, j,greater)+dbinom(1150,j,greater)+dbinom(1200, j, greater)

#BINOMIAL PROBABILITY PLOT
x<-seq(0:10000)
plot(dbinom(1100,x,0.98), col='blue',
     main ='BINOMIAL PROBABILITY FUNCTION ',
     xlab = 'SUCCESS NUMBER',
     ylab = 'PROBABILITY')

######################
#NORMAL DISTRIBUTION#
######################
# someone wish to know the probability that the sugar's price is less than 1000 cop p(x>1000)
less<-round(pnorm(1000,mean,SDS),2)

# someone wish to know the probability that the sugar's price is between 800 and 1200 cop p(800<x<1200)
bt<-round(pnorm(1200,mean,SDS)-pnorm(800,mean,SDS),2)

# someone wish to know the probability that the sugar's price is more than 1000 cop p(x>1000)
more<- round(1-dnorm(1000,mean,SDS),2)

#NORMAL PROBABILITY PLOT
x<-seq(0:10000)
plot(dnorm(x,mean,SDS), col='orange',
     main ='NORMAL PROBABILITY FUNCTION ',
     xlab = 'SAMPLE',
     ylab = 'PROBABILITY')
