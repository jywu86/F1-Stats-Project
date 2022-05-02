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

mydata_top5$finish_tier[mydata_top5$position<=5] <-'Top5'
mydata_top5$finish_tier[(mydata_top5$position>5)] <- 'Back_Marker'

mydata_top3$finish_tier[mydata_top3$position<=3] <-'Podium'
mydata_top3$finish_tier[(mydata_top3$position>3)] <- 'Back_Marker'


mydata_top5 <- na.omit(mydata_top5)
mydata_top3 <- na.omit(mydata_top3)
mydata_win <- na.omit(mydata_win)


mydata_win$win[mydata_win$position ==1] <- 'Win'
mydata_win$win[mydata_win$position !=1] <- 'Lose'


#################### Model1 data set (Using Finish_Tier as dependent variable) ###############################

model_data_win <- subset(mydata_win, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_win)
factor_cols_win <- c('Rainfall2','driverRef','win','Type')
model_data_win[,factor_cols_win] <- lapply(model_data_win[,factor_cols_win],factor)

model_data_top5 <- subset(mydata_top5, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
str(model_data_top5)
factor_cols_top5 <- c('Rainfall2','driverRef','finish_tier','Type')
model_data_top5[,factor_cols_top5] <- lapply(model_data_top5[,factor_cols_top5],factor)

model_data_top3 <- subset(mydata_top3, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
str(model_data_top3)
factor_cols_top3 <- c('Rainfall2','driverRef','finish_tier','Type')
model_data_top3[,factor_cols_top3] <- lapply(model_data_top3[,factor_cols_top3],factor)



# forest model win  (60 -Train, 40-Test) 90% forest 90% boost
#str(model_data_win)
set.seed(24)
ind <- sample(2, nrow(model_data_win), replace=T, prob=c(0.6,0.4))
train_win <- model_data_win[ind==1,]
test_win <- model_data_win[ind==2,]

# logistic regression model
log_win <- glm(win~., data=train_win, family='binomial')
#stepAIC(log_win)

log_win_predict <- predict(log_win, test_win,type='response')

############################ ROC Curve####################################
rlog <- multiclass.roc(test_win$win,log_win_predict, percent=TRUE)
roc_log <- rlog[['rocs']]
r_log<-roc_log[[1]]
#plot.roc(r1, col='red',lwd=3, main= 'ROC Curve for Logistic (Win)')
plot.roc(r_log,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Logistic (Win)')



cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest_win <- train(win ~.,
                 data=train_win,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)

plot(varImp(forest_win))

######################### ROC CURVE FOR FOREST ######################################################
forest_win_pred <- predict(forest_win, test_win, type='prob')
rforest <- multiclass.roc(test_win$win,forest_win_pred$Win, percent=TRUE)
roc_forest <- rforest[['rocs']]
r_forest <-roc_forest[[1]]
#plot.roc(r1, col='red',lwd=3, main= 'ROC Curve for Forest 3 (Top 5)')
plot.roc(r_forest,
         #add = T,
         col = 'red',
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Forest')



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

plot(varImp(boostwin))

plot(varImp(forest3))

# plotting ROC curves for all
par(mfrow=c(1,3))
par(mar= c(4,4,4,4)+.1)
plot.roc(r_log,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Logistic (Win)')
plot.roc(r_forest,
         #add = T,
         col = 'red',
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Forest')




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

# ROC Curve 
f3_predict <- predict(forest3,test, type='prob')
r <- multiclass.roc(test$finish_tier,f3_predict$Top5, percent=TRUE)
roc <- r[['rocs']]
r1 <-roc[[1]]
plot.roc(r1, col='red',lwd=3, main= 'ROC Curve for Forest 3 (Top 5)')
plot.roc(r1,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Forest 3 (Top 5 Finish)')

forest4 <- train(finish_tier ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,
                 ntree = 500)


boost5 <- train(finish_tier ~., 
                data = train,
                method='xgbTree',
                trControl = cvcontrol,
                tuneGrid = expand.grid(nrounds = 500,
                                       max_depth =3,
                                       #max_depth =5,
                                       eta = 0.2,
                                       gamma = 0,
                                       colsample_bytree =1,
                                       min_child_weight = 1,
                                       subsample =1 ))




plot(varImp(forest3))

boost5predict <- predict(boost5,test)
boost5_roc_pred <- ifelse(boost5predict == 'Back',1,0)
boost5_r <- multiclass.roc(test$finish_tier, boost5_roc_pred, percent=TRUE)
boost_roc <- boost5_r[['rocs']]
b1_b <- boost_roc[[1]]
plot.roc(b1_b,
         print.auc =TRUE,
         print.thres=TRUE)

plot(boost5)

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

# ROC Curve 
f3_predict <- predict(forest3,test, type='prob')
r <- multiclass.roc(test$finish_tier,f3_predict$Podium, percent=TRUE)
roc <- r[['rocs']]
r1 <-roc[[1]]
#plot.roc(r1, col='red',lwd=3, main= 'ROC Curve for Forest 3 (Podium)')
plot.roc(r1,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Forest 3 (Top 5 Finish)')

plot(varImp(forest3))

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

mydata_drivers <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,final_position))
str(mydata_drivers)

model_data_drivers <- subset(mydata_drivers, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef))
str(model_data_drivers)
factor_cols_drivers <- c('Rainfall2','Type')
model_data_drivers[,factor_cols_drivers] <- lapply(model_data_drivers[,factor_cols_drivers],factor)


model_data_drivers %>% distinct(driverRef)
hamilton <- model_data_drivers %>% filter(driverRef == 'hamilton')
hamilton <- subset(hamilton,select = -c(driverRef))
vettel<- model_data_drivers %>% filter(driverRef == 'vettel')
vettel <- subset(vettel,select = -c(driverRef))
raikkonen<- model_data_drivers %>% filter(driverRef == 'raikkonen')
raikkonen <- subset(raikkonen,select = -c(driverRef))
perez<- model_data_drivers %>% filter(driverRef == 'perez')
perez <- subset(perez,select = -c(driverRef))
bottas<- model_data_drivers %>% filter(driverRef == 'bottas')
bottas <- subset(bottas,select = -c(driverRef))
max_verstappen<- model_data_drivers %>% filter(driverRef == 'max_verstappen')
max_verstappen <- subset(max_verstappen,select = -c(driverRef))
sainz<- model_data_drivers %>% filter(driverRef == 'sainz')
sainz <- subset(sainz,select = -c(driverRef))
stroll<- model_data_drivers %>% filter(driverRef == 'stroll')
stroll <- subset(stroll,select = -c(driverRef))
leclerc<- model_data_drivers %>% filter(driverRef == 'leclerc')
leclerc <- subset(leclerc,select = -c(driverRef))
gasly<- model_data_drivers %>% filter(driverRef == 'gasly')
gasly <- subset(gasly,select = -c(driverRef))
str(vettel)
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
  
  forest4 <- train(final_position ~.,
                   data=train2,
                   method='rf',
                   trControl = cvcontrol,
                   importance=TRUE,ntree=400)
  
  
  forest4predict <- predict(forest4,test2)
  
  plot(varImp(forest4))
  forest4
  forest4predict
  
  
  
}
par(mfrow=c(2,2))

ploter(hamilton)
ploter(vettel)
str(hamilton)
