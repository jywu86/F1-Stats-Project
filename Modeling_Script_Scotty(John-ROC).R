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
mydata_win <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,year))
mydata_top5 <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,year))
mydata_top3 <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,year))

mydata_top5$finish_tier[mydata_top5$position<=6] <-'Top6'
mydata_top5$finish_tier[(mydata_top5$position>6)] <- 'Back_Marker'

mydata_top3$finish_tier[mydata_top3$position<=3] <-'Podium'
mydata_top3$finish_tier[(mydata_top3$position>3)] <- 'Back_Marker'


mydata_top5 <- na.omit(mydata_top5)
mydata_top3 <- na.omit(mydata_top3)
mydata_win <- na.omit(mydata_win)


mydata_win$finish_tier[mydata_win$position ==1] <- 'Win'
mydata_win$finish_tier[mydata_win$position !=1] <- 'Lose'


#################### Model1 data set (Using Finish_Tier as dependent variable) ###############################

model_data_win <- subset(mydata_win, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_win)
factor_cols_win <- c('Rainfall2','driverRef','win','Type')
model_data_win[,factor_cols_win] <- lapply(model_data_win[,factor_cols_win],factor)

model_data_top5 <- subset(mydata_top5, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_top5)
factor_cols_top5 <- c('Rainfall2','driverRef','finish_tier','Type')
model_data_top5[,factor_cols_top5] <- lapply(model_data_top5[,factor_cols_top5],factor)

model_data_top3 <- subset(mydata_top3, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_top3)
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


log_win_predict <- predict(log_win, test_win,type='response')
log_win_predict_f<- as.factor(ifelse(log_win_predict_r>0.1,'Win','Lose'))

confusionMatrix(log_win_predict_f, test_win$win, positive='Win')

# StepAIC for logistic regression model

stepAIC(log_win)

# creating pareto chart of wins (for report)
win_data <- mydata_win %>% 
  group_by(driverRef) %>% 
  mutate(win_total = sum(win =='Win'))

driver_win <- unique(win_data[,c('driverRef','win_total')]) %>% arrange((win_total))
driver_win$driverRef <- gsub('max_verstappen','max',driver_win$driverRef)
driver_win$driverRef <- gsub('raikkonen','rai',driver_win$driverRef)
driver_win$highlight <- 'no'
driver_win[6,'highlight'] <- 'yes'
driver_win[3,'highlight'] <- 'yes'


par(mfrow=c(1,1))
driver_win <- arrange(driver_win,win_total)
driver_win$driverRef <- factor(driver_win$driverRef, levels=driver_win$driverRef)
win_pareto <-ggplot(driver_win, aes(x=driverRef,y=win_total, fill=highlight)) +
                      geom_col() +coord_flip() + 
  theme(legend.position='none', plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c('grey69','tomato1')) +
  ggtitle('Wins by Driver') +
  xlab('Driver Name') +
  ylab('Total Wins (2018-2021)')
  
win_pareto


# Logistic Win Final Model
log_win_grid <- glm(win~grid+driverRef, data= train_win, family='binomial')
log_win_g_pred <- as.factor(ifelse(predict(log_win_grid, test_win, type='response')>0.1,'Win','Lose'))

confusionMatrix(log_win_g_pred, test_win$win, positive='Win')


############################ ROC Curve for Logistic####################################
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

plot(varImp(forest_win), main='Variable Importance for Race Win (Forest Model)')

confusionMatrix(log_win_predict_f, test_win$win, positive='Win')

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

forest_win_factor <- as.factor(ifelse(forest_win_pred$Win>0.1,"Win","Lose"))

confusionMatrix(forest_win_factor, test_win$win, positive='Win')

f_win_reduced <- train(win ~ driverRef + grid + Rainfall2 + qualifying_dif,
                       data=train_win,
                       method='rf',
                       trControl = cvcontrol,
                       importance=TRUE,ntree=400)
f_win_reduced_pred <- predict(f_win_reduced, test_win, type='prob')

f_win_reduced_factor <- as.factor(ifelse(f_win_reduced_pred$Win>0.1,"Win","Lose"))

confusionMatrix(f_win_reduced_factor, test_win$win, positive='Win')

plot(varImp(f_win_reduced), main='Variable Importance for Race Win (Forest Model)')


### BOOST MODEL

boost_win<- train(win ~., 
                data = train_win,
                method='xgbTree',
                trControl = cvcontrol,
                tuneGrid = expand.grid(nrounds = 50,
                                       max_depth = 3,
                                       eta = .1,
                                       gamma = 2,
                                       colsample_bytree =1,
                                       min_child_weight = 1,
                                       subsample =1 ))


boost_win_pred <- predict(boost_win, test_win, type='prob')
rboost <- multiclass.roc(test_win$win,boost_win_pred$Win, percent=TRUE)
roc_boost <- rboost[['rocs']]
r_boost <-roc_boost[[1]]

boost_win_factor <- as.factor(ifelse(boost_win_pred$Win>0.05,"Win","Lose"))

confusionMatrix(boost_win_factor, test_win$win, positive='Win')

# Finding the best threshold
coords(r_boost,"best", ret='threshold',transpose=FALSE)
coords(r_forest,"best", ret='threshold',transpose=FALSE)
coords(r_log,"best", ret='threshold',transpose=FALSE)

plot(boostwin_depth, main = 'Max tree depth vs CV-Accuracy')

boostwin_eta<- train(win ~., 
                       data = train_win,
                       method='xgbTree',
                       trControl = cvcontrol,
                       tuneGrid = expand.grid(nrounds = 300,
                                              max_depth = 1,
                                              eta = c(0.1,0.3,0.5),
                                              gamma = 2,
                                              colsample_bytree =1,
                                              min_child_weight = 1,
                                              subsample =1 ))



plot(varImp(boostwin_depth))

plot(varImp(forest3))

# plotting ROC curves for all
par(mfrow=c(2,2))
#par(mar= c(4,4,4,4)+.1)
plot.roc(r_log,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Logistic (Win)')
plot.roc(r_forest,
         #add = T,
         #col = 'red',
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         main = 'ROC Curve for Forest (Win)')
plot.roc(r_boost,
         #add = T,
         #col = 'red',
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='Tomato',
         print.thres = T,
         main = 'ROC Curve for Boost (Win)')
         #cex = 1.5)



forest3predict <- predict(forest3,test)

confusionMatrix(forest3predict, test$win)

plot(varImp(forest3))

####################################################### PODIUM FINISH ANALYSIS #################################################################
# forest model podium (60 -Train, 40-Test) .856 Forest .838 Boost
str(model_data_top3)
set.seed(24)
ind <- sample(2, nrow(model_data_top3), replace=T, prob=c(0.6,0.4))
train_pod <- model_data_top3[ind==1,]
test_pod <- model_data_top3[ind==2,]

# Logistic Regression Podium
log_pod <- glm(finish_tier~., data=test_pod, family='binomial')

# Creating ROC Curve of Logistic
log_pod_predict <- predict(log_pod, test_pod,type='response')
rlog_pod <- multiclass.roc(test_pod$finish_tier,log_pod_predict, percent=TRUE)
roc_log_pod <- rlog_pod[['rocs']]
r_log_pod<-roc_log_pod[[1]]
plot.roc(r_log_pod,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Logistic (Podium)')

# Confusion Matrix
log_pod_predict_f<- as.factor(ifelse(log_pod_predict>0.2,'Podium','Back_Marker'))
confusionMatrix(log_pod_predict_f, test_pod$finish_tier, positive='Podium')

# Forest Model Podium
cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

f_pod <- train(finish_tier ~.,
                 data=train_pod,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)

# ROC Curve 
f_pod_pred <- predict(f_pod,test_pod, type='prob')
rf_pod <- multiclass.roc(test_pod$finish_tier,f_pod_pred$Podium, percent=TRUE)
roc_f_pod <- rf_pod[['rocs']]
r_f_pod <-roc_f_pod[[1]]
plot.roc(r_f_pod,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         main = 'ROC Curve for Forest (Podium)')

# Confusion Matrix
f_pod_predict_f<- as.factor(ifelse(f_pod_pred$Podium>0.4,'Podium','Back_Marker'))
confusionMatrix(f_pod_predict_f, test_pod$finish_tier, positive='Podium')

# Boost Model

b_pod <- train(finish_tier ~., 
                data = train_pod,
                method='xgbTree',
                trControl = cvcontrol,
                tuneGrid = expand.grid(nrounds = 100,
                                       max_depth =3,
                                       eta = 0.1,
                                       gamma = 0,
                                       colsample_bytree =1,
                                       min_child_weight = 1,
                                       subsample =1 ))

plot(b_pod, main='Different Gamma Function Podium Boost Model')

# ROC Curve 
b_pod_pred <- predict(b_pod,test_pod, type='prob')
rb_pod <- multiclass.roc(test_pod$finish_tier,b_pod_pred$Podium, percent=TRUE)
roc_b_pod <- rb_pod[['rocs']]
r_b_pod <-roc_b_pod[[1]]
plot.roc(r_b_pod,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='Tomato',
         print.thres = T,
         main = 'ROC Curve for Boost (Podium)')

# Confusion Matrix
b_pod_predict_f<- as.factor(ifelse(b_pod_pred$Podium>0.1,'Podium','Back_Marker'))
confusionMatrix(b_pod_predict_f, test_pod$finish_tier, positive='Podium')


# WINNING MODELS FOR PODIUM

# Logistic Regression Podium stepAIC
stepAIC(log_pod)

log_pod_final <- glm(finish_tier ~ grid + driverRef + Rainfall2 + qualifying_dif + dist_s_turns, data=test_pod, family='binomial')
log_pod_final_pred <- predict(log_pod_final, test_pod,type='response')
rlog_pod_final <- multiclass.roc(test_pod$finish_tier,log_pod_final_pred, percent=TRUE)
roc_log_pod_final <- rlog_pod_final[['rocs']]
r_log_pod_final <-roc_log_pod_final[[1]]
plot.roc(r_log_pod_final,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Logistic-Reduced (Podium)')

log_pod_final_pred_f<- as.factor(ifelse(log_pod_final_pred>0.3,'Podium','Back_Marker'))
confusionMatrix(log_pod_final_pred_f, test_pod$finish_tier, positive='Podium')

# creating pareto chart of podiums (for report)
pod_data <- mydata_top3 %>% 
  group_by(driverRef) %>% 
  mutate(pod_total = sum(finish_tier =='Podium'), race_total = n())

driver_pod <- unique(pod_data[,c('driverRef','pod_total','race_total')]) %>% arrange((pod_total))
driver_pod$driverRef <- gsub('max_verstappen','max',driver_win$driverRef)
driver_pod$driverRef <- gsub('raikkonen','rai',driver_win$driverRef)
driver_pod$highlight <- 'no'
driver_pod[6,'highlight'] <- 'yes'
driver_pod[3,'highlight'] <- 'yes'

driver_pod$pod_percent <- (driver_pod$pod_total/driver_pod$race_total)*100


par(mfrow=c(1,1))
driver_pod <- arrange(driver_pod,pod_total)
driver_pod$driverRef <- factor(driver_pod$driverRef, levels=driver_pod$driverRef)
pod_pareto <-ggplot(driver_pod, aes(x=driverRef,y=pod_total, fill=highlight)) +
  geom_col() +coord_flip() + 
  theme(legend.position='none', plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c('grey69','tomato1')) +
  ggtitle('Podiums by Driver') +
  xlab('Driver Name') +
  ylab('Total Podiums (2018-2021)')

pod_pareto


# Podium Loss Model
plot(varImp(f_pod))

f_pod_reduced <- train(finish_tier ~grid + qualifying_dif + driverRef,
               data=train_pod,
               method='rf',
               trControl = cvcontrol,
               importance=TRUE,ntree=400)

# ROC Curve and Confusion Matrix
f_pod_reduced_pred <- predict(f_pod_reduced,test_pod, type='prob')
rf_pod_reduced <- multiclass.roc(test_pod$finish_tier,f_pod_reduced_pred$Podium, percent=TRUE)
roc_f_pod_reduced <- rf_pod_reduced[['rocs']]
r_f_pod_reduced <-roc_f_pod_reduced[[1]]
plot.roc(r_f_pod_reduced,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         main = 'ROC Curve for Reduced Forest (Podium)')

f_pod_reduced_factor <- as.factor(ifelse(f_pod_reduced_pred$Podium>0.4,"Podium","Back_Marker"))

confusionMatrix(f_pod_reduced_factor, test_pod$finish_tier, positive='Podium')

plot(varImp(f_pod_reduced), main='Variable Importance for Podium (Forest Model)') 

################################## TOP TEAM (1-6 analysis) #############################################################
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

###Purpose 2 Prep

mydata_win <- subset(mydata_win, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,finish_tier,year))
mydata_3 <- subset(mydata_top3, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,finish_tier,year))
mydata_6 <- subset(mydata_top5, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,finish_tier,year))

mydata_team_win <- subset(mydata_win, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))


mydata_team_3 <- subset(mydata_3, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))


mydata_team_6 <- subset(mydata_6, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))


mydata_indiv_win <- subset(mydata_win, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))


mydata_indiv_3 <- subset(mydata_3, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))


mydata_indiv_6 <- subset(mydata_6, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))


###Teams
factor <- function(team) {
  team$Rainfall2 <- as.factor(team$Rainfall2)
  team$Type <- as.factor(team$Type)
  team$finish_tier <- as.factor(team$finish_tier)
  team$driverRef <- as.factor(team$driverRef)
  team
}

factor_teams <- c('Rainfall2','Type','finish_tier','driverRef')
str(mydata_team_win)
ferrari_team_win <- mydata_team_win %>% filter(constructorRef == 'ferrari'& (year == 2018 | year == 2019))
str(ferrari_team_win)
ferrari_team_win <- subset(ferrari_team_win,select = -c(constructorRef,team_rank,year))
#ferrari_team_win[,factor_teams] <- lapply(ferrari_team_win[,factor_teams],factor)
ferrari_team_3 <- mydata_team_3 %>% filter(constructorRef == 'ferrari'& (year == 2018 | year == 2019))
ferrari_team_3 <- subset(ferrari_team_3,select = -c(constructorRef,team_rank,year))
#ferrari_team_3[,factor_teams] <- lapply(ferrari_team_3[,factor_teams],factor)
ferrari_team_6<- mydata_team_6 %>% filter(constructorRef == 'ferrari'& (year == 2018 | year == 2019))
ferrari_team_6 <- subset(ferrari_team_6,select = -c(constructorRef,team_rank,year))
#ferrari_team_wi6[,factor_teams] <- lapply(ferrari_team_6[,factor_teams],factor)

mercedes_team_win <- mydata_team_win %>% filter(constructorRef == 'mercedes')
mercedes_team_win <- subset(mercedes_team_win,select = -c(constructorRef,team_rank,year))
#mercedes_team_win[,factor_teams] <- lapply(mercedes_team_win[,factor_teams],factor)
mercedes_team_3<- mydata_team_3 %>% filter(constructorRef == 'mercedes')
mercedes_team_3 <- subset(mercedes_team_3,select = -c(constructorRef,team_rank,year))
#mercedes_team_3[,factor_teams] <- lapply(mercedes_team_3[,factor_teams],factor)
mercedes_team_6<- mydata_team_6 %>% filter(constructorRef == 'mercedes')
mercedes_team_6 <- subset(mercedes_team_6,select = -c(constructorRef,team_rank,year))
#mercedes_team_6[,factor_teams] <- lapply(mercedes_team_6[,factor_teams],factor)

racingpoint_team_win <- mydata_team_win %>% filter(constructorRef == 'racing_point'& (year == 2020 | year == 2019))
racingpoint_team_win <- subset(racingpoint_team_win,select = -c(constructorRef,team_rank,year))
#racingpoint_team_win[,factor_teams] <- lapply(racingpoint_team_win[,factor_teams],factor)
racingpoint_team_3 <- mydata_team_3 %>% filter(constructorRef == 'racing_point'& (year == 2020 | year == 2019))
racingpoint_team_3 <- subset(racingpoint_team_3,select = -c(constructorRef,team_rank,year))
#racingpoint_team_3[,factor_teams] <- lapply(racingpoint_team_3[,factor_teams],factor)
racingpoint_team_6 <- mydata_team_6 %>% filter(constructorRef == 'racing_point'& (year == 2020 | year == 2019))
racingpoint_team_6 <- subset(racingpoint_team_6,select = -c(constructorRef,team_rank,year))
#racingpoint_team_6[,factor_teams] <- lapply(racingpoint_team_6[,factor_teams],factor)

mercedes_team_win <- factor(mercedes_team_win)
mercedes_team_3 <- factor(mercedes_team_3)
mercedes_team_6 <- factor(mercedes_team_6)

ferrari_team_win <- factor(ferrari_team_win)
ferrari_team_3 <- factor(ferrari_team_3)
ferrari_team_6 <- factor(ferrari_team_6)

racingpoint_team_win <- factor(racingpoint_team_win)
racingpoint_team_3 <- factor(racingpoint_team_3)
racingpoint_team_6 <- factor(racingpoint_team_6)
str(mercedes_team_win)

set.seed(24)
ind_f_t_w <- sample(2, nrow(ferrari_team_win), replace=T, prob=c(0.5,0.5))
train_pod_f_t_w <- ferrari_team_win[ind_f_t_w==1,]
test_pod_f_t_w <- ferrari_team_win[ind_f_t_w==2,]
ind_f_t_3 <- sample(2, nrow(ferrari_team_3), replace=T, prob=c(0.5,0.5))
train_pod_f_t_3 <- ferrari_team_3[ind_f_t_3==1,]
test_pod_f_t_3 <- ferrari_team_3[ind_f_t_3==2,]
ind_f_t_6 <- sample(2, nrow(ferrari_team_6), replace=T, prob=c(0.9,0.1))
train_pod_f_t_6 <- ferrari_team_6[ind_f_t_6==1,]
test_pod_f_t_6 <- ferrari_team_6[ind_f_t_6==2,]


f_m_w <- glm(finish_tier ~grid+Rainfall2+dist.mi+qualifying_dif,family='binomial',data=train_pod_f_t_w)
f_m_3 <- glm(finish_tier ~Air.Temp+grid,family='binomial',data=train_pod_f_t_3)
f_m_6 <- glm(finish_tier ~Rainfall2+Track.Temp+driverRef+qualifying_dif+grid+Air.Temp,family='binomial',data=train_pod_f_t_6)

f_m_w_pred <- predict(f_m_w,test_pod_f_t_w,type='response')
f_m_3_pred <- predict(f_m_3,test_pod_f_t_3,type='response')
f_m_6_pred <- predict(f_m_6,test_pod_f_t_6,type='response')
par(mfrow=c(3,1))
f_m_w <- multiclass.roc(test_pod_f_t_w$finish_tier,f_m_w_pred, percent=TRUE)
f_m_w  <- f_m_w[['rocs']]
f_m_w  <-f_m_w[[1]]
plot.roc(f_m_w,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Ferrari Win/Loss')
f_m_3 <- multiclass.roc(test_pod_f_t_3$finish_tier,f_m_3_pred, percent=TRUE)
f_m_3  <- f_m_3[['rocs']]
f_m_3  <-f_m_3[[1]]
plot.roc(f_m_3,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Ferrari Top3')
f_m_6 <- multiclass.roc(test_pod_f_t_6$finish_tier,f_m_6_pred, percent=TRUE)
f_m_6  <- f_m_6[['rocs']]
f_m_6  <-f_m_6[[1]]
plot.roc(f_m_6,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Ferrari Top6')

###########
set.seed(24)
ind_m_t_w <- sample(2, nrow(mercedes_team_win), replace=T, prob=c(0.7,0.3))
train_pod_m_t_w <- mercedes_team_win[ind_m_t_w==1,]
test_pod_m_t_w <- mercedes_team_win[ind_m_t_w==2,]
ind_m_t_3 <- sample(2, nrow(mercedes_team_3), replace=T, prob=c(0.7,0.3))
train_pod_m_t_3 <- mercedes_team_3[ind_m_t_3==1,]
test_pod_m_t_3 <- mercedes_team_3[ind_m_t_3==2,]
ind_m_t_6 <- sample(2, nrow(mercedes_team_6), replace=T, prob=c(0.7,0.3))
train_pod_m_t_6 <- mercedes_team_6[ind_m_t_6==1,]
test_pod_m_t_6 <- mercedes_team_6[ind_m_t_6==2,]


m_m_w <- glm(finish_tier ~.,family='binomial',data=train_pod_m_t_w)
m_m_3 <- glm(finish_tier ~.,family='binomial',data=train_pod_m_t_3)
m_m_6 <- glm(finish_tier ~.,family='binomial',data=train_pod_m_t_6)

m_m_w_pred <- predict(m_m_w,test_pod_m_t_w,type='response')
m_m_3_pred <- predict(m_m_3,test_pod_m_t_3,type='response')
m_m_6_pred <- predict(m_m_6,test_pod_m_t_6,type='response')
par(mfrow=c(3,1))
m_m_w <- multiclass.roc(test_pod_m_t_w$finish_tier,m_m_w_pred, percent=TRUE)
m_m_w  <- m_m_w[['rocs']]
m_m_w  <-m_m_w[[1]]
plot.roc(m_m_w,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for mercedes Win/Loss')
m_m_3 <- multiclass.roc(test_pod_m_t_3$finish_tier,m_m_3_pred, percent=TRUE)
m_m_3  <- m_m_3[['rocs']]
m_m_3  <-m_m_3[[1]]
plot.roc(m_m_3,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for mercedes Top3')
m_m_6 <- multiclass.roc(test_pod_m_t_6$finish_tier,m_m_6_pred, percent=TRUE)
m_m_6  <- m_m_6[['rocs']]
m_m_6  <-m_m_6[[1]]
plot.roc(m_m_6,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Mercedes Top6')

########
set.seed(24)
ind_rp_t_w <- sample(2, nrow(racingpoint_team_win), replace=T, prob=c(0.9,0.1))
train_pod_rp_t_w <- racingpoint_team_win[ind_rp_t_w==1,]
test_pod_rp_t_w <- racingpoint_team_win[ind_rp_t_w==2,]
ind_rp_t_3 <- sample(2, nrow(racingpoint_team_3), replace=T, prob=c(0.9,0.1))
train_pod_rp_t_3 <- racingpoint_team_3[ind_rp_t_3==1,]
test_pod_rp_t_3 <- racingpoint_team_3[ind_rp_t_3==2,]
ind_rp_t_6 <- sample(2, nrow(racingpoint_team_6), replace=T, prob=c(0.9,0.1))
train_pod_rp_t_6 <- racingpoint_team_6[ind_rp_t_6==1,]
test_pod_rp_t_6 <- racingpoint_team_6[ind_rp_t_6==2,]


rp_m_w <- glm(finish_tier ~.,family='binomial',data=train_pod_rp_t_w)
rp_m_3 <- glm(finish_tier ~.,family='binomial',data=train_pod_rp_t_3)
rp_m_6 <- glm(finish_tier ~.,family='binomial',data=train_pod_rp_t_6)

rp_m_w_pred <- predict(rp_m_w,test_pod_rp_t_w,type='response')
rp_m_3_pred <- predict(rp_m_3,test_pod_rp_t_3,type='response')
rp_m_6_pred <- predict(rp_m_6,test_pod_rp_t_6,type='response')
par(mfrow=c(3,1))
rp_m_w <- multiclass.roc(test_pod_rp_t_w$finish_tier,rp_m_w_pred, percent=TRUE)
rp_m_w  <- rp_m_w[['rocs']]
rp_m_w  <-rp_m_w[[1]]
plot.roc(rp_m_w,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for racingpoint Win/Loss')
rp_m_3 <- multiclass.roc(test_pod_rp_t_3$finish_tier,rp_m_3_pred, percent=TRUE)
rp_m_3  <- rp_m_3[['rocs']]
rp_m_3  <-rp_m_3[[1]]
plot.roc(rp_m_3,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for racingpoint Top3')
rp_m_6 <- multiclass.roc(test_pod_rp_t_6$finish_tier,rp_m_6_pred, percent=TRUE)
rp_m_6  <- rp_m_6[['rocs']]
rp_m_6  <-rp_m_6[[1]]
plot.roc(rp_m_6,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for racingpoint Top6')


###Individuals

hamilton_win
hamilton_3
hamilton_6


bottas_win
bottas_3
bottas_6


vettel_win
vettel_3
vettel_6


verstappen_win
verstappen_3
verstappen_6
