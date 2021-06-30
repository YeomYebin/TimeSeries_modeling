
library(tidyverse)
library(forecast)
library(MLmetrics)
library(data.table)
library("timetk")
library("data.table")
library("gridExtra")
library("caret")
library("MLmetrics")
library("lubridate")
library("factoextra")
library("corrplot")
library("Hmisc")



rf_data <- read.csv("C:/Users/User/Desktop/코딥 데이터 확인/codeep_final_factor_data2.csv")
rf_data %>% str()
rf_data <- rf_data[,c(2, 4, 5, 6, 7, 8, 9)]
colnames(rf_data)[1] <- "BTC_price"
rf_data[1,] <- as.numeric(rf_data[1,])
rf_data %>% str()
economic_data <- rf_data

##차분
for (i in 1:7){
  a <- economic_data[,i] %>% diff()
  a <- c(NA, a)
  economic_data[,i] <- a
}

economic_data<-economic_data[-1,]
row.names(economic_data) <- NULL
economic_data %>% head()


##################################################################################
#  [ 시계열 교차검증 ]
# - 비 누적 교차검증 사용.
###############################################################################


#그리드서치할 파라미터 생성
window_size <-  seq(from=15, to=300, by=5)
window_size


window_tuning <- expand.grid(window_size, NA)
names(window_tuning) <- c("#_of_window", "RMSE")

window_tuning
economic_data <- economic_data  %>% dplyr::select(-c(volume))

t = 1
for (k in window_size) {
  n = dim(economic_data)[1] # 끝행 값
  m = n-k-61+1 # 자르는 처음 값
  
  data_cut = economic_data[m:n,]
  
  x=1:k # 윈도우 사이즈 
  
  folds <-  list() 
  for(i in 1:61){ # 여기서 61은 며칠 전까지 CV돌릴것인가를 의미 
    folds[[i]]= x +i- 1
  } # 각 리스트 번호는 몇번쨰 CV인지를 나타냄. 리스트번호가 60까지 있다면 60일 전까지 CV한다는거.
  # folds의 각 list가 CV에 사용할 x들임
  
  folds_test=list()
  for (i in 1:61){
    folds_test[[i]]=folds[[i]][k]+1
  } # folds_test가 각 CV마다 테스트할 시점들을 모아놓은 거.
  
  eco_y <- ts(data_cut$BTC_price) # 예측하고자 하는 y값
  eco_x <- data_cut[, -1] # 그레인1져 인과검정을 기각한 변수들만 넣음.
  
  eco_x_test <- list()
  eco_y_test <- list()
  
  for (i in (1:length(folds_test))){
    eco_y_test[[i]] <- ts(eco_y[folds_test[[i]]])
    eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
  } # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값
  
  eco_yfit_1 <- c()
  for (i in (1:length(folds_test))) {
    print(paste0(i, "th"))
    y_train <- ts(eco_y[folds[[i]]])   # y_train은 train하는 시점들의 y값(과거 y값으로 미래 값 예측하니까)
    xreg_train <- as.matrix(eco_x[folds[[i]],]) # xreg_train은 train하는 시점들의 외생변수값
    eco_arima <-auto.arima(y_train, xreg = xreg_train)
    y_fit1 <- forecast::forecast(eco_arima, xreg = eco_x_test[[i]], h=1)$mean
    eco_yfit_1 <- c(eco_yfit_1, y_fit1)
  }
  
  
  
  eco_test_final_y <- unlist(eco_y_test) # eco_test_final_y는 eco_test_y값에서 하나의 벡터로 넣은것.
  eco_rmse1 <- RMSE(eco_yfit_1, eco_test_final_y)
  window_tuning[t,"RMSE"] <- eco_rmse1
  t = t+1
}

#========================================================================#
window_tuning

write.csv(window_tuning, "C:/Users/User/Desktop/코딥 데이터 확인/arimax_tuning_결과.csv")

best <- window_tuning[which(window_tuning[,"RMSE"] == min(window_tuning[,"RMSE"])), ]
k <- best[1,1]
k
best

###튜닝된 파리미터로 학습시켜 예측값 도출
rf_data <- read.csv("C:/Users/User/Desktop/코딥 데이터 확인/codeep_final_factor_data2.csv")
rf_data %>% str()
rf_data <- rf_data[,c(2, 4, 5, 6, 7, 8, 9)]
colnames(rf_data)[1] <- "BTC_price"
rf_data[1,] <- as.numeric(rf_data[1,])
rf_data %>% str()
economic_data <- rf_data


#차분
for (i in 1:7){
  a <- economic_data[,i] %>% diff()
  a <- c(NA, a)
  economic_data[,i] <- a
}

economic_data<-economic_data[-1,]
row.names(economic_data) <- NULL
economic_data <- economic_data  %>% dplyr::select(-c(volume))
economic_data %>% head()



n = dim(economic_data)[1] # 끝행 값
m = n-k-61+1 # 자르는 처음 값

data_cut = economic_data[m:n,]

x=1:k # 윈도우 사이즈 

folds <-  list() 
for(i in 1:61){ # 여기서 61은 며칠 전까지 CV돌릴것인가를 의미 
  folds[[i]]= x +i- 1
} # 각 리스트 번호는 몇번쨰 CV인지를 나타냄. 리스트번호가 60까지 있다면 60일 전까지 CV한다는거.
# folds의 각 list가 CV에 사용할 x들임

folds_test=list()
for (i in 1:61){
  folds_test[[i]]=folds[[i]][k]+1
} # folds_test가 각 CV마다 테스트할 시점들을 모아놓은 거.

eco_y <- ts(data_cut$BTC_price) # 예측하고자 하는 y값
eco_x <- data_cut[, -1] # 그레인져 인과검정을 기각한 변수들만 넣음.

eco_x_test <- list()
eco_y_test <- list()

for (i in (1:length(folds_test))){
  eco_y_test[[i]] <- ts(eco_y[folds_test[[i]]])
  eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
} # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값

eco_yfit_1 <- c()
for (i in (1:length(folds_test))) {
  print(paste0(i, "th"))
  y_train <- ts(eco_y[folds[[i]]])   # y_train은 train하는 시점들의 y값(과거 y값으로 미래 값 예측하니까)
  xreg_train <- as.matrix(eco_x[folds[[i]],]) # xreg_train은 train하는 시점들의 외생변수값
  eco_arima <- auto.arima(y_train, xreg = as.matrix(xreg_train), approximation = FALSE)
  y_fit1 <- forecast::forecast(eco_arima, xreg = eco_x_test[[i]], h=1)$mean
  eco_yfit_1 <- c(eco_yfit_1, y_fit1)
}

eco_test_final_y %>% length
eco_test_final_y

eco_test_final_y <- unlist(eco_y_test) # eco_test_final_y는 eco_test_y값에서 하나의 벡터로 넣은것.
eco_rmse1 <- RMSE(eco_yfit_1, eco_test_final_y)
eco_rmse1


#################

data <- read.csv("C:/Users/User/Desktop/코딥 데이터 확인/codeep_final_factor_data2.csv")
data %>% str()
data <- data[,c(2, 4, 5, 6, 7, 8, 9)]
colnames(data)[1] <- "BTC_price"
data[1,] <- as.numeric(data[1,])

###########
##차분풀고 시계열 그리기!###
price <- data$BTC_price[(length(data$BTC_price)-61):(length(data$BTC_price)-1)]
true_y <- data$BTC_price[(length(data$BTC_price)-61+1):(length(data$BTC_price))]
undiff_yhat <- eco_yfit_1 + price
RMSE(true_y, undiff_yhat) ## 차분 되돌린 후 RMSE
arimax_yhat <- cbind(undiff_yhat, true_y)
arimax_yhat <-arimax_yhat %>% data.frame()
colnames(arimax_yhat) <- c("예측값", "실제값")
row.names(arimax_yhat) <- NULL

write.csv(arimax_yhat, "C:/Users/User/Desktop/코딥 데이터 확인/arimax_예측값.csv")