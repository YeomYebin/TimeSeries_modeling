# 데이터 합치기

setwd("C:/Users/User/Desktop/코딥 데이터 확인")
library(dplyr)
install.packages("imputeTS")
library(imputeTS)
na_interpolation(c(1, 2, 3, 4, NA, NA, 6))

BTC <- read.csv("BTC.csv")

sentiment <- read.csv("sentiment.csv")
colnames(sentiment) <- c("Date", "sentiment")

KOSPI <- read.csv("KOSPI.csv")
Nasdaq <- read.csv("Nasdaq.csv")
SP500 <- read.csv("SP500.csv")

VIX <- read.csv("VIX.csv")
VKOSPI <- read.csv("VKOSPI.csv")

Nvidia <- read.csv("Nvidia.csv")
AMD <- read.csv("AMD.csv")
MA <- read.csv("MA.csv")
PYPL <- read.csv("PYPL.csv")
SQ <- read.csv("SQ.csv")

vidente <- read.csv("vidente.csv")
Atinum <- read.csv("Atinum.csv")
Woori <- read.csv("Woori.csv")
colnames(Woori) <- c("Date", "Woori")

dat <- BTC


dat <- left_join(BTC, sentiment, by="Date")

dat <- left_join(dat, KOSPI, by="Date")
dat <- left_join(dat, Nasdaq, by="Date")
dat <- left_join(dat, SP500, by="Date")

dat <- left_join(dat, VIX, by="Date")
dat <- left_join(dat, VKOSPI, by="Date")

dat <- left_join(dat, Nvidia, by="Date")
dat <- left_join(dat, AMD, by="Date")
dat <- left_join(dat, MA, by="Date")
dat <- left_join(dat, PYPL, by="Date")
dat <- left_join(dat, SQ, by="Date")

dat <- left_join(dat, vidente, by="Date")
dat <- left_join(dat, Atinum, by="Date")
dat <- left_join(dat, Woori, by="Date")

dat <- dat %>% arrange(Date)

dat %>% head()


#데이터 선형보간법을 사용하여 Imputation하기
for (i in 6:18) {
  print(i)
  dat[,i] <- na_interpolation(dat[,i])
}

write.csv(dat,"codeep_data.csv")


#차원축소 : Factor 만들기
data <- read.csv("codeep_data.csv")

data %>% head()

PC1 <- prcomp(data[,c(6,7,8)], scale = T)
summary(PC1)
factor1 <- predict(PC1)[,1]

PC2 <- prcomp(data[,c(9,10)], scale = T)
summary(PC2)
factor2 <- predict(PC2)[,1]

PC3 <- prcomp(data[,c(11, 12, 13, 14, 15)], scale = T)
summary(PC3)
factor3 <- predict(PC3)[,1]

PC4 <- prcomp(data[,c(16, 17, 18)], scale = T)
summary(PC4)
factor4 <- predict(PC4)[,1]

final_factor_data <- cbind(data, factor1, factor2, factor3, factor4)
write.csv(final_factor_data, "codeep_final_factor_data.csv") 
