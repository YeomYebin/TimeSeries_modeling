## [ library ]
library("tidyverse")
library("data.table")
library("gridExtra")
library("caret")
library("MLmetrics")
library("lubridate")
library("factoextra")
library("corrplot")
library("Hmisc")
library("forecast")
library("randomForest")
library(urca)
library(tseries)
library(vars)


#차분하기 : 정상성을 만족하지 않는 데이터를 차분을 통해 정상성 확보

rf_data <- read.csv("C:/Users/User/Desktop/코딥 데이터 확인/codeep_final_factor_data2.csv")
rf_data %>% str()
rf_data <- rf_data[,c(2, 4, 5, 6, 7, 8, 9)]
colnames(rf_data)[1] <- "BTC_price"
rf_data[1,] <- as.numeric(rf_data[1,])
rf_data %>% str()

rf_data %>% diff()

for (i in 1:7){
  a <- rf_data[,i] %>% diff()
  a <- c(NA, a)
  rf_data[,i] <- a
}


#그리드 서치할 파라미터들 설정
window_size <- c(100)
mtry = c(2)
ntree = c( 20 )
window_rf_RMSE <- expand.grid(mtry = mtry,
                              ntree = ntree,
                              window_size = window_size,
                              RMSE = NA)
window_rf_RMSE

true_y <- rf_data$BTC_price[(dim(rf_data)[1]-60):(dim(rf_data)[1])]


RMSE_save <- NULL
window_rf_RMSE %>% dim()


#비누적교차검증(Sliding Window)를 사용하여 시계열데이터 랜덤포레스트 튜닝
for (t in 1:dim(window_rf_RMSE)[1]) {
  k = window_rf_RMSE[t,3]
  mt = window_rf_RMSE[t,1]
  nt =  window_rf_RMSE[t,2]
  
  n = dim(rf_data)[1] # 끝행 값
  m = n-k-61+1 # 자르는 처음 값
  
  data_cut = rf_data[m:n,]
  x=1:k # 윈도우 사이즈 
  
  folds <-  list() 
  for(i in 1:61){ # 여기서 61은 며칠 전까지 CV돌릴것인가를 의미 
    folds[[i]]= x +i - 1
  } # 각 리스트 번호는 몇번쨰 CV인지를 나타냄. 리스트번호가 60까지 있다면 60일 전까지 CV한다는거.
  # folds의 각 list가 CV에 사용할 x들임
  
  folds_test=list()
  for (i in 1:61){
    folds_test[[i]]=folds[[i]][k]+1
  } # folds_test가 각 CV마다 테스트할 시점들을 모아놓은 거.
  
  eco_y <- data_cut$BTC_price # 예측하고자 하는 y값
  eco_x <- data_cut %>% dplyr::select(-BTC_price) # 모든 변수를 다 넣음.
  
  eco_x_test <- list()
  eco_y_test <- list()
  
  for (i in (1:length(folds_test))){
    eco_y_test[[i]] <- eco_y[folds_test[[i]]]
    eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
  } # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값
  
  RMSE_save <- NULL
  yhat <- NULL
  
  for ( i in (1:length(folds_test))) {
    set.seed(990706)
    rf_model <- randomForest(BTC_price~ volume + sentiment + factor1 + factor2 + factor3 + factor4,
                             data = data_cut[folds[[i]],], 
                             mtry = mt, ntree = nt)
    #print(paste0("mtry :", mt, ",  ntree :", nt ,", ",i, "th"))
    eco_test_final_y <- unlist(eco_y_test[[i]])
    rf_fit <- predict(rf_model, newdata = eco_x_test[[i]])
    yhat <- c(yhat, rf_fit)
    rmse_1 = RMSE(rf_fit, eco_test_final_y)
    RMSE_save <- c(RMSE_save, rmse_1)
  }
  window_rf_RMSE[t, 4] <- RMSE(true_y, yhat)
  RMSE_save <- NULL # RMSE_save 초기화
  yhat <- NULL
  print(paste0(t,"/",dim(window_rf_RMSE)))
}

varImpPlot(rf_model)

window_rf_RMSE
window_rf_RMSE[which(window_rf_RMSE[,"RMSE"] == min(window_rf_RMSE[,"RMSE"])), ]
best <- window_rf_RMSE[which(window_rf_RMSE[,"RMSE"] == min(window_rf_RMSE[,"RMSE"])), ]
best %>% dim()

#튜닝된 파라미터로 훈련시키기

k = best[1,3]
mt = best[1,1]
nt =  best[1,2]
yhat <- list()


rf_data <- read.csv("C:/Users/User/Desktop/코딥 데이터 확인/TEST셋/codeep_final_factor_data_테스트데이터 합친것2.csv")

RMSE(rf_data$BTC_price_O[1186:1205],rf_data$BTC_price_O[1185:1204] )

rf_data %>% str()
rf_data <- rf_data[,c(2, 4, 5, 6, 7, 8, 9)]
colnames(rf_data)[1] <- "BTC_price"
rf_data[1,] <- as.numeric(rf_data[1,])
rf_data %>% str()

#rf_data %>% diff()

for (i in 1:7){
  a <- rf_data[,i] %>% diff()
  a <- c(NA, a)
  rf_data[,i] <- a
}

n = dim(rf_data)[1] # 끝행 값
m = n-k-20+1 # 자르는 처음 값
  
data_cut = rf_data[m:n,]
x=1:k # 윈도우 사이즈 
  
folds <-  list() 
for(i in 1:20){ # 여기서 61은 며칠 전까지 CV돌릴것인가를 의미 
  folds[[i]]= x +i - 1
} # 각 리스트 번호는 몇번쨰 CV인지를 나타냄. 리스트번호가 60까지 있다면 60일 전까지 CV한다는거.
  # folds의 각 list가 CV에 사용할 x들임
  
folds_test=list()
for (i in 1:20){
  folds_test[[i]]=folds[[i]][k]+1
} # folds_test가 각 CV마다 테스트할 시점들을 모아놓은 거.
  
eco_y <- data_cut$BTC_price # 예측하고자 하는 y값
eco_x <- data_cut %>% dplyr::select(-BTC_price) # 모든 변수를 다 넣음.
  
eco_x_test <- list()
eco_y_test <- list()
  
for (i in (1:length(folds_test))){
  eco_y_test[[i]] <- eco_y[folds_test[[i]]]
  eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
} # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값
  
RMSE_save <- NULL
for ( i in (1:length(folds_test))) {
  set.seed(990706)
  rf_model <- randomForest(BTC_price~ volume + sentiment + factor1 + factor2 + factor3 + factor4,
                             data = data_cut[folds[[i]],], 
                             mtry = mt, ntree = nt)
    #print(paste0("mtry :", mt, ",  ntree :", nt ,", ",i, "th"))
  eco_test_final_y <- unlist(eco_y_test[[i]])
  rf_fit <- predict(rf_model, newdata = as.matrix(eco_x_test[[i]]))
  yhat <- c(yhat, as.numeric(rf_fit))
}
diff_rmse <-  RMSE(unlist(yhat), unlist(eco_y_test))

diff_rmse  

yhat <- unlist(yhat)

#변수중요도 확인하기
varImpPlot(rf_model)


#전체 학습데이터에 대하여 훈련시키기

rf_data <- read.csv("C:/Users/User/Desktop/코딥 데이터 확인/codeep_final_factor_data2.csv")
rf_data %>% str()
rf_data <- rf_data[,c(2, 4, 5, 6, 7, 8, 9)]
colnames(rf_data)[1] <- "BTC_price"
rf_data[1,] <- as.numeric(rf_data[1,])
rf_data %>% str()


for (i in 1:7){
  a <- rf_data[,i] %>% diff()
  a <- c(NA, a)
  rf_data[,i] <- a
}

rf_data <- rf_data[-1,]
row.names(rf_data) <- NULL
rf_data %>% dim()

k = best[1,3]
mt = best[1,1]
nt =  best[1,2]
yhat <- list()

#n = dim(rf_data)[1] # 끝행 값
#m = n-k-61+1 # 자르는 처음 값

data_cut = rf_data
x=1:k # 윈도우 사이즈 
data_cut %>% dim()

folds <-  list() 
for(i in 1:(dim(data_cut)[1]-k)){ # 여기서 61은 며칠 전까지 CV돌릴것인가를 의미 
  folds[[i]]= x +i - 1
} # 각 리스트 번호는 몇번쨰 CV인지를 나타냄. 리스트번호가 60까지 있다면 60일 전까지 CV한다는거.
# folds의 각 list가 CV에 사용할 x들임

folds_test=list()
for (i in 1:(dim(data_cut)[1]-k)){
  folds_test[[i]]=folds[[i]][k]+1
} # folds_test가 각 CV마다 테스트할 시점들을 모아놓은 거.

eco_y <- data_cut$BTC_price # 예측하고자 하는 y값
eco_x <- data_cut %>% dplyr::select(-BTC_price) # 모든 변수를 다 넣음.

eco_x_test <- list()
eco_y_test <- list()

for (i in (1:length(folds_test))){
  eco_y_test[[i]] <- eco_y[folds_test[[i]]]
  eco_x_test[[i]] <- eco_x[folds_test[[i]],] %>% as.matrix
} # eco_y_test는 test할 시점의 y값, eco_x_test 는 test할 시점의 x값

RMSE_save <- NULL
for ( i in (1:length(folds_test))) {
  set.seed(990706)
  rf_model <- randomForest(BTC_price~ volume + sentiment + factor1 + factor2 + factor3 + factor4,
                           data = data_cut[folds[[i]],], 
                           mtry = mt, ntree = nt)
  #print(paste0("mtry :", mt, ",  ntree :", nt ,", ",i, "th"))
  eco_test_final_y <- unlist(eco_y_test[[i]])
  rf_fit <- predict(rf_model, newdata = eco_x_test[[i]])
  rmse_1 = RMSE(rf_fit, eco_test_final_y)
  RMSE_save <- c(RMSE_save, rmse_1)
  yhat <- c(yhat, rf_fit)
}
diff_rmse <-  mean(RMSE_save)

diff_rmse  

yhat <- unlist(yhat)

data <- read.csv("C:/Users/User/Desktop/코딥 데이터 확인/codeep_final_factor_data2.csv")
data %>% str()
data <- data[,c(2, 4, 5, 6, 7, 8, 9)]
colnames(data)[1] <- "BTC_price"
data[1,] <- as.numeric(data[1,])
###########
price <- data$BTC_price[(k+1):(length(data$BTC_price)-1)]
true_y <- data$BTC_price[(k+2):(length(data$BTC_price))]
undiff_yhat <- yhat + price
RMSE(true_y, undiff_yhat)

final_yhat <- c(rep(NA, k+1), undiff_yhat)

length(final_yhat)==length(data$BTC_price)

data$yhat <- final_yhat
data %>% str()

length(data$BTC_price - data$yhat)

error <- data$BTC_price - data$yhat

data$error <- error

write.csv(data, "C:/Users/User/Desktop/코딥 데이터 확인/error_포함_X변수는 미룬것것.csv")
