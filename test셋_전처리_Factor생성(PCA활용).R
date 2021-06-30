####### TEST 데이터 전처리
setwd("C:/Users/User/Desktop/코딥 데이터 확인/TEST셋")
library(dplyr)
install.packages("imputeTS")
library(imputeTS)
na_interpolation(c(1, 2, 3, 4, NA, NA, 6))

BTC <- read.csv("BTC_test.csv")

sentiment <- read.csv("sentiment_test.csv")
colnames(sentiment) <- c("Date", "sentiment")

KOSPI <- read.csv("KOSPI_test.csv")
Nasdaq <- read.csv("Nasdaq_test.csv")
SP500 <- read.csv("SP500_test.csv")

VIX <- read.csv("VIX_test.csv")
VKOSPI <- read.csv("VKOSPI_test.csv")

Nvidia <- read.csv("Nvidia_test.csv")
AMD <- read.csv("AMD_test.csv")
MA <- read.csv("MA_test.csv")
PYPL <- read.csv("PYPL_test.csv")
SQ <- read.csv("SQ_test.csv")

vidente <- read.csv("vidente_test.csv")
Atinum <- read.csv("Atinum_test.csv")
Woori <- read.csv("Woori_test.csv")
colnames(Woori) <- c("Date", "Woori")


dat <- dplyr::left_join(BTC[,c(1,2,5,6)], sentiment, by="Date")

dat <- dplyr::left_join(dat, KOSPI, by="Date")
dat <- dplyr::left_join(dat, Nasdaq, by="Date")
dat <- dplyr::left_join(dat, SP500, by="Date")

dat <- dplyr::left_join(dat, VIX, by="Date")
dat <- dplyr::left_join(dat, VKOSPI, by="Date")

dat <- dplyr::left_join(dat, Nvidia, by="Date")
dat <- dplyr::left_join(dat, AMD, by="Date")
dat <- dplyr::left_join(dat, MA, by="Date")
dat <- dplyr::left_join(dat, PYPL, by="Date")
dat <- dplyr::left_join(dat, SQ, by="Date")

dat <- dplyr::left_join(dat, vidente, by="Date")
dat <- dplyr::left_join(dat, Atinum, by="Date")
dat <- dplyr::left_join(dat, Woori, by="Date")

dat <- dat %>% dplyr::arrange(Date)

dat %>% head()
dat %>% str()

for (i in 6:18) {
  print(i)
  dat[,i] <- na_interpolation(as.numeric(dat[,i]))
}

dat %>% head()

write.csv(dat,"codeep_test_data.csv")


data <- read.csv("codeep_data.csv")
test <- read.csv("codeep_test_data.csv")
test <- test[,-1]
colnames(test)[7] <- "Nasdaq"

#Test셋 Factor 생성
#install.packages("FactoMineR")
library(FactoMineR)

test %>% head()

data %>% head()

PC1 <- prcomp(data[,c(6,7,8)], scale = T)
summary(PC1)
factor1 <- predict(PC1, test[,c(6, 7, 8)])[,1]

PC2 <- prcomp(data[,c(9,10)], scale = T)
summary(PC2)
factor2 <- predict(PC2, test[,c(9,10)])[,1]

PC3 <- prcomp(data[,c(11, 12, 13, 14, 15)], scale = T)
summary(PC3)
factor3 <- predict(PC3,test[,c(11, 12, 13, 14, 15)])[,1]

PC4 <- prcomp(data[,c(16, 17, 18)], scale = T)
summary(PC4)
factor4 <- predict(PC4,test[,c(16, 17, 18)] )[,1]

final_factor_data <- cbind(test, factor1, factor2, factor3, factor4)
write.csv(final_factor_data, "codeep_final_factor_test_data.csv")

