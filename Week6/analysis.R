library(car)
library(haven)

setwd("/Users/younghokim/Desktop/Academic/1121/Data Analysis/Homeworks/Week 6")

#####################EX_A#####################

agepop <- read.fwf("age400rr.txt", widths = 2)
age120 <- read.table("age400_120.csv", header=T, sep=",")
age60 <- read.table("age400_60.csv", header=T, sep=",")
age10 <- read.table("age400_10.csv", header=T, sep=",")

mean(agepop$V1) #35.035
sd(agepop$V1) #13.44491

#age_population
par(family="Yuanti TC")
hist(agepop$V1, ylim=c(0,40), breaks = 30, 
     max(agepop), xlab = "年齡", ylab = "次數", 
     main = "圖一、社區母群體之年齡分配", 
     col = "grey")

#age120
means120 <- aggregate(age120$age~age120$Replicate, data=age120, FUN=mean)
ax <- means120$'age120$age'
mean(ax) #35.02978
sd(ax) #1.011829

plot(means120$'age120$age', means120$`age120$Replicate`, 
     main = "圖二a、各組樣本年齡平均數 散佈圖(n=120)", 
     xlim=c(20,50))

hist(ax, xlim=c(25,45), ylim=c(0,80), breaks = 30, 
     max(ax), xlab = "各組樣本之平均年齡", ylab = "次數", 
     main = "圖二b、樣本平均數所形成之抽樣分配(n=120, 600次)", 
     col = "grey")
age120xfit <- seq(min(ax), max(ax), length.out=500)
age120yfit <- dnorm(age120xfit, mean=mean(ax), sd=sd(ax))
age120yfit <- age120yfit*length(ax)/5
lines(age120xfit, age120yfit, col="red")

#age60
means60 <- aggregate(age60$age~age60$Replicate, data=age60, FUN=mean)
ay <- means60$'age60$age'
mean(ay) #34.93531
sd(ay) #1.622716

plot(means60$'age60$age', means60$`age60$Replicate`, 
     main = "圖三a、各組樣本年齡平均數 散佈圖(n=60)", 
     xlim=c(20,50))

hist(ay, xlim=c(20,50), ylim=c(0,80), breaks = 40, 
     max(ay), xlab = "各組樣本之平均年齡", ylab = "次數", 
     main = "圖三b、樣本平均數所形成之抽樣分配(n=60, 600次)", 
     col = "grey")
age60xfit <- seq(min(ay), max(ay), length.out=500)
age60yfit <- dnorm(age60xfit, mean=mean(ay), sd=sd(ay))
age60yfit <- age60yfit*length(ay)/5
lines(age60xfit, age60yfit, col="red")


#age10
means10 <- aggregate(age10$age~age10$Replicate, data=age10, FUN=mean)
az <- means10$'age10$age'
mean(az) #34.73333
sd(az) #4.243839


plot(means10$'age10$age', means10$`age10$Replicate`, 
     main = "圖四a、各組樣本年齡平均數 散佈圖(n=10)", 
     xlim=c(20,50))

hist(az, xlim=c(20,50), ylim=c(0,80), breaks = 90, 
     max(az), xlab = "各組樣本之平均年齡", ylab = "次數", 
     main = "圖四b、樣本平均數所形成之抽樣分配(n=10, 600次)", 
     col = "grey")
age10xfit <- seq(min(az), max(az), length.out=500)
age10yfit <- dnorm(age10xfit, mean=mean(az), sd=sd(az))
age10yfit <- age10yfit*length(az)/5
lines(age10xfit, age10yfit, col="red")


#모집단의 평균이 아래의 신뢰할 수 있는 구간에 있을 것이라 추정
##95%의 표본평균이 아래의 구간에 있음
###신뢰수준 95%에서 모평균은 a~b 사이로 추정된다
mean(ax) + c(-1.96, 1.96) * sd(ax) / sqrt(120) #34.84874 35.21082
mean(ay) + c(-1.96, 1.96) * sd(ay) / sqrt(60) #34.52470 35.34591
mean(az) + c(-1.96, 1.96) * sd(az) / sqrt(10) #32.10297 37.36369
mean(az) + c(-2.262, 2.262) * sd(az) / sqrt(10) #31.69768 37.76898
