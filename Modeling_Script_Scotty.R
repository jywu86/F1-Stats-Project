library(randomForest)
library(caret)
library(lime)
library(tree)
library(pROC)
library(ROCR)
library(car)
library(MASS)

install.packages(randomForest)

# Importing Data and Prepping it for modeling
mydata_all <- read.csv('Model_Data_w_Turns.csv')
str(mydata)
mydata_win <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))
mydata_top5 <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))
mydata_top3 <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))

mydata_top5$finish_tier[mydata_top5$position<=5] <-'Podium'
mydata_top5$finish_tier[(mydata_top5$position>5)] <- 'Back'

mydata_top3$finish_tier[mydata_top3$position<=3] <-'Podium'
mydata_top3$finish_tier[(mydata_top3$position>3)] <- 'Back'


mydata_top5 <- na.omit(mydata_top5)
mydata_top3 <- na.omit(mydata_top3)
mydata_win <- na.omit(mydata_win)


mydata_win$win[mydata_win$position ==1] <- 'Win'
mydata_win$win[mydata_win$position !=1] <- 'Lose'


#################### Model1 data set (Using Finish_Tier as dependent variable) ###############################

model_data_win <- subset(mydata_win, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef))
str(model_data_win)
factor_cols_win <- c('Rainfall2','driverRef','win','Type')
model_data_win[,factor_cols_win] <- lapply(model_data_win[,factor_cols_win],factor)

model_data_top5 <- subset(mydata_top5, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef))
str(model_data_top5)
factor_cols_top5 <- c('Rainfall2','driverRef','finish_tier','Type')
model_data_top5[,factor_cols_top5] <- lapply(model_data_top5[,factor_cols_top5],factor)

model_data_top3 <- subset(mydata_top3, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef))
str(model_data_top3)
factor_cols_top3 <- c('Rainfall2','driverRef','finish_tier','Type')
model_data_top3[,factor_cols_top3] <- lapply(model_data_top3[,factor_cols_top3],factor)



# forest model win  (60 -Train, 40-Test) 90% forest 90% boost
str(model_data_win)
set.seed(24)
ind <- sample(2, nrow(model_data_win), replace=T, prob=c(0.6,0.4))
train <- model_data_win[ind==1,]
test <- model_data_win[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest3 <- train(win ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)

boostwin <- train(win ~., 
                data = train,
                method='xgbTree',
                trControl = cvcontrol,
                tuneGrid = expand.grid(nrounds = 1000,
                                       max_depth =5,
                                       eta = .3,
                                       gamma = 2,
                                       colsample_bytree =1,
                                       min_child_weight = 1,
                                       subsample =1 ))


forest3predict <- predict(forest3,test)

confusionMatrix(forest3predict, test$win)

plot(varImp(forest3))

boostwin

# forest model top5  (60 -Train, 40-Test) .856 Forest .838 Boost
str(model_data_top5)
set.seed(24)
ind <- sample(2, nrow(model_data_top5), replace=T, prob=c(0.6,0.4))
train <- model_data_top5[ind==1,]
test <- model_data_top5[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest3 <- train(finish_tier ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)

boost5 <- train(finish_tier ~., 
                data = train,
                method='xgbTree',
                trControl = cvcontrol,
                tuneGrid = expand.grid(nrounds = 1000,
                                       max_depth =5,
                                       eta = 0.3,
                                       gamma = 2,
                                       colsample_bytree =1,
                                       min_child_weight = 1,
                                       subsample =1 ))


forest3predict <- predict(forest3,test)

confusionMatrix(forest3predict, test$finish_tier) 

plot(varImp(forest3))

boost5

# forest model top3  (60 -Train, 40-Test) .8687
str(model_data_top3)
set.seed(24)
ind <- sample(2, nrow(model_data_top3), replace=T, prob=c(0.6,0.4))
train <- model_data_top3[ind==1,]
test <- model_data_top3[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest3 <- train(finish_tier ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)

boost3 <- train(finish_tier ~., 
                data = train,
                method='xgbTree',
                trControl = cvcontrol,
                tuneGrid = expand.grid(nrounds = 1000,
                                       max_depth =5,
                                       eta = 0.3,
                                       gamma = 2,
                                       colsample_bytree =1,
                                       min_child_weight = 1,
                                       subsample =1 ))


forest3predict <- predict(forest3,test)

confusionMatrix(forest3predict, test$finish_tier)

plot(varImp(forest3))

boost3


#### Single Drivers
str(mydata_top5)
mydata_drivers <- subset(mydata_top5, select= c(finish_tier,dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))
str(mydata_drivers)

model_data_drivers <- subset(mydata_drivers, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position))
str(model_data_drivers)
factor_cols_drivers <- c('Rainfall2','Type','driverRef','finish_tier')
racing_point[,factor_cols_drivers] <- lapply(racing_point[,factor_cols_drivers],factor)


model_data_drivers %>% distinct(driverRef)
mercedes <- model_data_drivers %>% filter(constructorRef == 'meredes')
mercedes <- subset(mercedes,select = -c(constructorRef))
racing_point <- model_data_drivers %>% filter(constructorRef == 'racing_point')
racing_point <- subset(racing_point,select = -c(constructorRef))
str(racing_point)



driver_list <- list(hamilton,vettel)
library(tidyverse)
pots <- c()
par(mfrow=c(2,2))
ploter <- function (driver){
  set.seed(24)
  ind2 <- sample(2, nrow(driver), replace=T, prob=c(0.8,0.2))
  train2 <- driver[ind2==1,]
  test2 <- driver[ind2==2,]
  
  cvcontrol <- trainControl(method ='repeatedcv',
                            number =5,
                            repeats=2,
                            allowParallel = TRUE)
  
  forest4 <- train(finish_tier ~.,
                   data=train2,
                   method='rf',
                   trControl = cvcontrol,
                   importance=TRUE,ntree=400)
  log <- glm(finish_tier~., data=train2, family='binomial')
  stepAIC(log)
  log_predict <- predict(log, test2,type='response')
  rlog <- multiclass.roc(test2$finish_tier,log_predict, percent=TRUE)
  roc_log <- rlog[['rocs']]
  r_log<-roc_log[[1]]
  #plot.roc(r1, col='red',lwd=3, main= 'ROC Curve for Logistic (Win)')
  plot.roc(r_log,
           print.auc = T,
           auc.polygon = T,
           max.auc.polygon = T,
           auc.polygon.col ='lightblue',
           print.thres = T,
           main = 'ROC Curve for Logistic (Podium)')
  
  forest4predict <- predict(forest4,test2)
  
  print(plot(varImp(forest4)))
  forest4
  
  
  
}
par(mfrow=c(2,2))

ploter(racing_point)
ploter(vettel)
str(hamilton)
