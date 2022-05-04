library(randomForest)
library(caret)
library(lime)
library(tree)
library(pROC)
library(ROCR)
library(car)
library(MASS)
library(xgboost)

install.packages(randomForest)

# Importing Data and Prepping it for modeling
mydata_all <- read.csv('Model_Data_w_Turns.csv')
#str(mydata)
mydata_win <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))
mydata_top6 <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))
mydata_top3 <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))

mydata_top6$finish_tier[mydata_top6$position<=6] <-'Top6'
mydata_top6$finish_tier[(mydata_top6$position>6)] <- 'Back_Marker'

mydata_top3$finish_tier[mydata_top3$position<=3] <-'Podium'
mydata_top3$finish_tier[(mydata_top3$position>3)] <- 'Back_Marker'


mydata_top6 <- na.omit(mydata_top6)
mydata_top3 <- na.omit(mydata_top3)
mydata_win <- na.omit(mydata_win)


mydata_win$win[mydata_win$position ==1] <- 'Win'
mydata_win$win[mydata_win$position !=1] <- 'Lose'


#################### Data Cleaning and dropping columns ###############################

model_data_win <- subset(mydata_win, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_win)
factor_cols_win <- c('Rainfall2','driverRef','win','Type')
model_data_win[,factor_cols_win] <- lapply(model_data_win[,factor_cols_win],factor)

model_data_top6 <- subset(mydata_top6, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_top5)
factor_cols_top6 <- c('Rainfall2','driverRef','finish_tier','Type')
model_data_top6[,factor_cols_top6] <- lapply(model_data_top6[,factor_cols_top6],factor)

model_data_top3 <- subset(mydata_top3, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_top3)
factor_cols_top3 <- c('Rainfall2','driverRef','finish_tier','Type')
model_data_top3[,factor_cols_top3] <- lapply(model_data_top3[,factor_cols_top3],factor)


################################################ Win Lose Model ##############################################
# Training and Testing for Win Lose Model
#str(model_data_win)
set.seed(24)
ind <- sample(2, nrow(model_data_win), replace=T, prob=c(0.6,0.4))
train_win <- model_data_win[ind==1,]
test_win <- model_data_win[ind==2,]

############# logistic regression model for Win-Lose
log_win <- glm(win~., data=train_win, family='binomial')

# create prediction based on test
log_win_predict <- predict(log_win, test_win,type='response')

# ROC Curve for logistic model
rlog <- multiclass.roc(test_win$win,log_win_predict, percent=TRUE)
roc_log <- rlog[['rocs']]
r_log<-roc_log[[1]]
plot.roc(r_log,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Logistic (Win)')

log_win_predict_f<- as.factor(ifelse(log_win_predict_r>0.1,'Win','Lose'))
confusionMatrix(log_win_predict_f, test_win$win, positive='Win')

# creating pareto chart of total wins (for report)
win_data <- mydata_win %>% 
  group_by(driverRef) %>% 
  mutate(win_total = sum(win =='Win'))

driver_win <- unique(win_data[,c('driverRef','win_total')]) %>% arrange((win_total))
driver_win$driverRef <- gsub('max_verstappen','max',driver_win$driverRef)
driver_win$driverRef <- gsub('raikkonen','rai',driver_win$driverRef)
driver_win$highlight <- 'no'
driver_win[6,'highlight'] <- 'yes'
driver_win[3,'highlight'] <- 'yes'

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

######### Forest Model for Win-Lose
cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)
forest_win <- train(win ~.,
                 data=train_win,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)

# Create initial probability predictions based on test
forest_win_pred <- predict(forest_win, test_win, type='prob')
# ROC curve for random forest
rforest <- multiclass.roc(test_win$win,forest_win_pred$Win, percent=TRUE)
roc_forest <- rforest[['rocs']]
r_forest <-roc_forest[[1]]
plot.roc(r_forest,
         col = 'red',
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         main = 'ROC Curve for Forest')

coords(r_boost,"best", ret='threshold',transpose=FALSE)

# Prediction using ROC threshold
forest_win_factor <- as.factor(ifelse(forest_win_pred$Win>0.1,"Win","Lose"))

# Confusion Matrix 
confusionMatrix(forest_win_factor, test_win$win, positive='Win')
plot(varImp(forest_win), main='Variable Importance for Race Win (Forest Model)')

######### Boost Model for Win-Lose
cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)
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
# Prediction using test 
boost_win_pred <- predict(boost_win, test_win, type='prob')
# ROC Curve
rboost <- multiclass.roc(test_win$win,boost_win_pred$Win, percent=TRUE)
roc_boost <- rboost[['rocs']]
r_boost <-roc_boost[[1]]
plot.roc(r_boost,
         print.auc = T,
         print.auc.cex=1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='Tomato',
         #show.thres = T,
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Boost (Win)')

boost_win_factor <- as.factor(ifelse(boost_win_pred$Win>0.05,"Win","Lose"))

confusionMatrix(boost_win_factor, test_win$win, positive='Win')

# Finding the best threshold
 coords(r_boost,"best", ret='threshold',transpose=FALSE)
# coords(r_forest,"best", ret='threshold',transpose=FALSE)
# coords(r_log,"best", ret='threshold',transpose=FALSE)

plot(boostwin_depth, main = 'Max tree depth vs CV-Accuracy')

# plotting ROC curves for all
par(mfrow=c(2,2))
#par(mar= c(4,4,4,4)+.1)
plot.roc(r_log,
         print.auc = T,
         print.auc.cex=1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Logistic (Win)')
plot.roc(r_forest,
         print.auc = T,
         print.auc.cex=1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Forest (Win)')
plot.roc(r_boost,
         print.auc = T,
         print.auc.cex=1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='Tomato',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Boost (Win)')
       
##################### Models used for Analysis in Win Lose
# StepAIC for logistic regression model
stepAIC(log_win)

## Logistic Reduced Model (Explanation in Report)
log_win_grid <- glm(win~grid+driverRef, data= train_win, family='binomial')
log_win_g_pred <- as.factor(ifelse(predict(log_win_grid, test_win, type='response')>0.1,'Win','Lose'))
confusionMatrix(log_win_g_pred, test_win$win, positive='Win')

## Forest Model for Explaining Loss
f_win_reduced <- train(win ~ driverRef + grid + Rainfall2 + qualifying_dif,
                       data=train_win,
                       method='rf',
                       trControl = cvcontrol,
                       importance=TRUE,ntree=400)
f_win_reduced_pred <- predict(f_win_reduced, test_win, type='prob')
f_win_reduced_factor <- as.factor(ifelse(f_win_reduced_pred$Win>0.1,"Win","Lose"))

confusionMatrix(f_win_reduced_factor, test_win$win, positive='Win')
plot(varImp(f_win_reduced), main='Variable Importance for Race Win (Forest Model)')
####################################################### PODIUM FINISH ANALYSIS #################################################################
# Sampling and Training for Podium Models
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

# Predicting with Threshold
log_pod_predict_f<- as.factor(ifelse(log_pod_predict>0.2,'Podium','Back_Marker'))
# Confusion Matrix
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


# Forest Prediction with Threshold
f_pod_predict_f<- as.factor(ifelse(f_pod_pred$Podium>0.2,'Podium','Back_Marker'))
# Confusion Matrix
confusionMatrix(f_pod_predict_f, test_pod$finish_tier, positive='Podium')

plot(varImp(f_pod))
# Boost Model
cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

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


#### Models Used for Explanation

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
driver_pod[5,'highlight'] <- 'yes'
driver_pod[3,'highlight'] <- 'yes'
driver_pod[,'highlight'] <- 'yes'

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

# Podium Win Model
plot(varImp(f_pod))

f_pod_reduced <- train(finish_tier ~grid + qualifying_dif + driverRef + Type +dist.mi,
               data=train_pod,
               method='rf',
               trControl = cvcontrol,
               importance=TRUE,ntree=400)

log_pod_final <- glm(finish_tier ~ grid + driverRef + qualifying_dif, data=test_pod, family='binomial')
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

log_pod_final

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

f_pod_reduced_factor <- as.factor(ifelse(f_pod_reduced_pred$Podium>0.2,"Podium","Back_Marker"))

confusionMatrix(f_pod_reduced_factor, test_pod$finish_tier, positive='Podium')

plot(varImp(f_pod_reduced), main='Variable Importance for Podium (Forest Model)') 

# ROC Curves for Report
par(mfrow=(c(2,2)))
plot.roc(r_log_pod,
         print.auc = T,
         print.auc.cex = 1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Logistic (Podium)')
plot.roc(r_f_pod,
         print.auc = T,
         print.auc.cex = 1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         print.thres.cex = 1.05,
         main = 'ROC Curve for Forest (Podium)')
plot.roc(r_b_pod,
         print.auc = T,
         print.auc.cex =1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='Tomato',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Boost (Podium)')

################################## TOP TEAM (1-6 analysis) #############################################################
# forest model top6  (60 -Train, 40-Test) .8687
str(model_data_top6)
set.seed(24)
ind <- sample(2, nrow(model_data_top6), replace=T, prob=c(0.6,0.4))
train_tm <- model_data_top6[ind==1,]
test_tm <- model_data_top6[ind==2,]

# Logistic Regression Podium
log_tm <- glm(finish_tier~., data=test_tm, family='binomial')

# Creating ROC Curve of Logistic
log_tm_predict <- predict(log_tm, test_tm,type='response')
rlog_tm <- multiclass.roc(test_tm$finish_tier,log_tm_predict, percent=TRUE)
roc_log_tm <- rlog_tm[['rocs']]
r_log_tm<-roc_log_tm[[1]]
plot.roc(r_log_tm,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'ROC Curve for Logistic (Top 6)')

# Confusion Matrix
log_tm_predict_f<- as.factor(ifelse(log_tm_predict>0.5,'Top6','Back_Marker'))
confusionMatrix(log_tm_predict_f, test_tm$finish_tier, positive='Top6')

# Forest Model Podium
cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

f_tm <- train(finish_tier ~.,
               data=train_tm,
               method='rf',
               trControl = cvcontrol,
               importance=TRUE,ntree=400)

# ROC Curve 
f_tm_pred <- predict(f_tm,test_tm, type='prob')
rf_tm <- multiclass.roc(test_tm$finish_tier,f_tm_pred$Top6, percent=TRUE)
roc_f_tm <- rf_tm[['rocs']]
r_f_tm <-roc_f_tm[[1]]
plot.roc(r_f_tm,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         main = 'ROC Curve for Forest (Top 6)')

plot(varImp(f_tm), main = 'Backmarker Model')

# Confusion Matrix
f_tm_predict_f<- as.factor(ifelse(f_tm_pred$Top6>0.6,'Top6','Back_Marker'))
confusionMatrix(f_tm_predict_f, test_tm$finish_tier, positive='Top6')

# Boost Model
b_tm <- train(finish_tier ~., 
               data = train_tm,
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
b_tm_pred <- predict(b_tm,test_tm, type='prob')
rb_tm <- multiclass.roc(test_tm$finish_tier,b_tm_pred$Top6, percent=TRUE)
roc_b_tm <- rb_tm[['rocs']]
r_b_tm <-roc_b_tm[[1]]
plot.roc(r_b_tm,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='Tomato',
         print.thres = T,
         main = 'ROC Curve for Boost (Top 6)')

# Confusion Matrix
b_tm_predict_f<- as.factor(ifelse(b_tm_pred$Top6>0.6,'Top6','Back_Marker'))
confusionMatrix(b_tm_predict_f, test_tm$finish_tier, positive='Top6')


# WINNING MODELS FOR Top6

# Boost Model Top6 
plot(varImp(b_tm), main='Not Reduced Important Variables', ylab='Variables')

b_tm_rounds <- train(finish_tier ~ grid + qualifying_dif + Track.Temp + driverRef, 
              data = train_tm,
              method='xgbTree',
              trControl = cvcontrol,
              tuneGrid = expand.grid(nrounds = c(50,100,300,500,700,1600,3000,6000,10000,20000),
                                     max_depth =3,
                                     eta = 0.01,
                                     gamma = 0,
                                     colsample_bytree =1,
                                     min_child_weight = 1,
                                     subsample =1 ))

b_tm_eta <- train(finish_tier ~ grid + qualifying_dif + Track.Temp + driverRef, 
                    data = train_tm,
                    method='xgbTree',
                    trControl = cvcontrol,
                    tuneGrid = expand.grid(nrounds = 1600,
                                           max_depth =5,
                                           eta = c(0.01,0.1,0.3),
                                           gamma = 0,
                                           colsample_bytree =1,
                                           min_child_weight = 1,
                                           subsample =1 ))

b_tm_depth <- train(finish_tier ~ grid + qualifying_dif + Track.Temp + driverRef, 
                    data = train_tm,
                    method='xgbTree',
                    trControl = cvcontrol,
                    tuneGrid = expand.grid(nrounds = 1600,
                                           max_depth =c(1,3,5,7,10),
                                           eta = 0.1,
                                           gamma = 0,
                                           colsample_bytree =1,
                                           min_child_weight = 1,
                                           subsample =1 ))

b_tm_gamma <- train(finish_tier ~ grid + qualifying_dif + Track.Temp + driverRef, 
                  data = train_tm,
                  method='xgbTree',
                  trControl = cvcontrol,
                  tuneGrid = expand.grid(nrounds = 1600,
                                         max_depth =5,
                                         eta = 0.1,
                                         gamma = c(0,1,2,3),
                                         colsample_bytree =1,
                                         min_child_weight = 1,
                                         subsample =1 ))

b_tm_reduced<- train(finish_tier ~ grid + qualifying_dif + Track.Temp + driverRef, 
                    data = train_tm,
                    method='xgbTree',
                    trControl = cvcontrol,
                    tuneGrid = expand.grid(nrounds = 300,
                                           max_depth =5,
                                           eta = 0.1,
                                           gamma = 1,
                                           colsample_bytree =1,
                                           min_child_weight = 1,
                                           subsample =1 ))


par(mfrow=(c(2,2)))

library(DiagrammeR)
xgb.plot.tree(model=b_tm$finalModel, trees=50)
xgb.plot.tree(model=b_tm$finalModel, trees=1)
### CHECK THIS OUT 
plot(b_tm_rounds, main='Boost Iterations (eta = 0.01)')
plot(b_tm_eta, main='Learning Rate')
plot(b_tm_depth, main='Max Depth (eta = 0.1)')
plot(b_tm_gamma, main='Gamma Function')

plot(varImp(b_tm), main='Variable Importance for Boost Model (Top 6)')


# ROC Curve and Confusion Matrix
b_tm_reduced_pred <- predict(b_tm_reduced,test_tm, type='prob')
rb_tm_reduced <- multiclass.roc(test_tm$finish_tier,b_tm_reduced_pred$Top6, percent=TRUE)
rocb_tm_reduced <- rb_tm_reduced[['rocs']]
r_b_tm_reduced <-rocb_tm_reduced[[1]]
plot.roc(r_b_tm_reduced,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         main = 'ROC Curve for Boost (Top6)')

b_tm_reduced_factor <- as.factor(ifelse(b_tm_reduced_pred$Top6>0.7,"Top6","Back_Marker"))

confusionMatrix(b_tm_reduced_factor, test_tm$finish_tier, positive='Top6')

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

# ROC Curves for Report
par(mfrow=(c(2,2)))
plot.roc(r_log_tm,
         print.auc = T,
         print.auc.cex=1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Logistic (Top 6)')
plot.roc(r_f_tm,
         print.auc = T,
         print.auc.cex=1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         print.thres.cex =1.05,
         main = 'ROC Curve for Forest (Top 6)')
plot.roc(r_b_tm,
         print.auc = T,
         print.auc.cex =1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='Tomato',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Boost (Top 6)')



str(model_data_top6)
set.seed(24)
ind <- sample(2, nrow(model_data_top6), replace=T, prob=c(0.6,0.4))
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



