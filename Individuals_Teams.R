library(randomForest)
library(caret)
library(lime)
library(tree)
library(pROC)
library(ROCR)
library(car)
library(MASS)
library(dplyr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(tidyverse)
library(traineR)
library(rstanarm)
library(cowplot)
data(efc)
theme_set(theme_sjplot())

install.packages(randomForest)

# Importing Data and Prepping it for modeling
mydata_all <- read.csv('Model_Data_w_Turns.csv')
str(mydata)
mydata_win <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,year))
mydata_win <- na.omit(mydata_win)
mydata_win$finish_tier[mydata_win$position ==1] <- 'Win'
mydata_win$finish_tier[mydata_win$position !=1] <- 'Lose'
mydata_team_win <- subset(mydata_win, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))
mydata_ind_w <- subset(mydata_win, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))

mydata_3 <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type,year))
mydata_3 <- na.omit(mydata_3)
mydata_3$finish_tier[mydata_3$position <=3] <- 'Top3'
mydata_3$finish_tier[mydata_3$position >3] <- 'Not'
mydata_ind_3 <- subset(mydata_3, select= c(dist.mi,grid,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,turns_s_mile,Type,finish_tier,year))


###Teams
factor2 <- function(team) {
  team$Rainfall2 <- as.factor(team$Rainfall2)
  team$finish_tier <- as.factor(team$finish_tier)
  team$driverRef <- as.factor(team$driverRef)
  team
}
factor <- function(ind) {
  ind$Rainfall2 <- as.factor(ind$Rainfall2)
  ind$finish_tier <- as.factor(ind$finish_tier)
  ind
}


ferrari_team_win <- mydata_team_win %>% filter(constructorRef == 'ferrari'& (driverRef == "vettel" | driverRef == "leclerc"))
ferrari_team_win <- subset(ferrari_team_win,select = -c(constructorRef,team_rank,year,dist.mi,Air.Temp,qualifying_dif,Type))
mercedes_team_win <- mydata_team_win %>% filter(constructorRef == 'mercedes')
mercedes_team_win <- subset(mercedes_team_win,select = -c(constructorRef,team_rank,year,dist.mi,Air.Temp,qualifying_dif,Type))
botta_3 <-mydata_ind_3 %>% filter(driverRef == 'bottas')
botta_3 <- subset(botta_3,select= -c(constructorRef,driverRef,year,dist.mi,Air.Temp,Type,qualifying_dif))
max_w <-mydata_ind_w %>% filter(driverRef == 'max_verstappen')
max_w <- subset(max_w,select= -c(constructorRef,driverRef,year,dist.mi,Air.Temp,Type,qualifying_dif))
hamilton_w <-mydata_ind_w %>% filter(driverRef == 'hamilton')
hamilton_w <- subset(hamilton_w,select= -c(constructorRef,driverRef,year,dist.mi,Air.Temp,Type,qualifying_dif))

str(botta_w)

max_w <- factor(max_w)
hamilton_w <- factor(hamilton_w)
botta_3 <- factor(botta_3)
mercedes_team_win <- factor(mercedes_team_win)
ferrari_team_win <- factor(ferrari_team_win)



set.seed(24)
ind_f_t_w <- sample(2, nrow(ferrari_team_win), replace=T, prob=c(0.8,0.2))
train_pod_f_t_w <- ferrari_team_win[ind_f_t_w==1,]
test_pod_f_t_w <- ferrari_team_win[ind_f_t_w==2,]


cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

f_m_w_rf <- train(finish_tier ~grid+Wind.Speed+Track.Temp+Rainfall2+driverRef+turns_s_mile,data=train_pod_f_t_w,method='rf',trControl = cvcontrol,importance=TRUE)
f_m_w_log <- train.glm(finish_tier~ grid+driverRef+Wind.Speed+Track.Temp+Rainfall2+turns_s_mile,data=train_pod_f_t_w,method='glm.fit')




f_m_w_rf_pred <- predict(f_m_w_rf,test_pod_f_t_w,type='prob')
f_m_w_log_pred <- predict(f_m_w_log,test_pod_f_t_w,type='prob')

f_m_w_rf



plot(varImp(f_m_w_rf))
f_m_w <- multiclass.roc(test_pod_f_t_w$finish_tier,f_m_w_rf_pred$Win, percent=TRUE)
f_m_w  <- f_m_w[['rocs']]
f_m_w  <-f_m_w[[1]]




f_m_w2 <- multiclass.roc(test_pod_f_t_w$finish_tier,f_m_w_log_pred$prediction[,'Win'], percent=TRUE)
f_m_w2  <- f_m_w2[['rocs']]
f_m_w2  <-f_m_w2[[1]]

roc1<- plot.roc(f_m_w,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Ferrari Win/Loss RF',  print.auc.cex = 1.5,
         print.thres.cex = 1.5,
         cex.main=1.5,
         cex.lab=1.3)
labels <- c('Grid','Wind Speed','Track Temp','Rained','Driver Vettel','Sharp Turns/Mi')
values <- as.numeric(varImp(f_m_w_rf)[1]$importance[,'Win'])
data <- data.frame(labels,values)
?ggplot
dev.new(width=10,height=5,unit='in')
gg1 <- ggplot(data,aes(values,reorder(labels,+values)),width=100)+   
  geom_bar(stat = "identity",width=0.2,fill='skyblue') +
  xlab('Importance') +
  ylab(NULL) +
  ggtitle('Variable Importance Ferrari W/L RF') +
  theme(text=element_text(size=10,color='grey3'),axis.text.y=element_text(colour='black'))
plot(varImp(f_m_w_rf),main='Var Imp Ferrari')
roc2 <- plot.roc(f_m_w2,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Ferrari Win/Loss Logit',
         print.auc.cex = 1.5,
         print.thres.cex = 1.5,
         cex.main=1.5,
         cex.lab=1.3)
set_theme(axis.textsize.y = 1.3,axis.textcolor.y = "Black",title.size = 2)
pm <- plot_model(f_m_w_log,show.values=TRUE,transform = NULL,ci.lvl=NA,title='Ferrari Logit Coefficients',dot.size=3,value.size=5)
levels(pm$data$term)<- c('Sharp Turns/Mi','Rained','Track Temp','Wind Speed','Driver Vettel','Grid')
pm
###########
set.seed(24)
ind_m_t_w <- sample(2, nrow(mercedes_team_win), replace=T, prob=c(0.7,0.3))
train_pod_m_t_w <- mercedes_team_win[ind_m_t_w==1,]
test_pod_m_t_w <- mercedes_team_win[ind_m_t_w==2,]



m_m_w <- train(finish_tier ~.,data=train_pod_m_t_w,method='rf',trControl = cvcontrol,importance=TRUE)
m_m_w_log.glm <- train.glm(finish_tier~ .,data=train_pod_m_t_w,method='glm.fit')




m_m_w_pred <- predict(m_m_w,test_pod_m_t_w,type='prob')
m_m_w_lpred.glm <- predict(m_m_w_log.glm,test_pod_m_t_w,type="prob")
par(mfrow=c(2,2))
plot(roc1)
plot(roc2)
roc3
roc4

m_m_wroc <- multiclass.roc(test_pod_m_t_w$finish_tier,m_m_w_lpred.glm$prediction[,'Win'], percent=TRUE)
m_m_wroc  <- m_m_wroc[['rocs']]
m_m_wroc  <-m_m_wroc[[1]]
m_m_rocw <- multiclass.roc(test_pod_m_t_w$finish_tier,m_m_w_pred$Win, percent=TRUE)
m_m_rocw  <- m_m_rocw[['rocs']]
m_m_rocw  <-m_m_rocw[[1]]


roc3 <- plot.roc(m_m_wroc,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Mercedes W/L Logit',  print.auc.cex = 1.5,
         print.thres.cex = 1.5,
         cex.main=1.5,
         cex.lab=1.3)
roc4 <- plot.roc(m_m_rocw,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Mercedes W/L Random Forest',  print.auc.cex = 1.5,
         print.thres.cex = 1.5,
         cex.main=1.5,
         cex.lab=1.3)
pm2 <- plot_model(m_m_w_log,show.values=TRUE,transform = NULL,ci.lvl=NA,title='Mercedes Logit Coefficients',dot.size=3,value.size=5)
levels(pm2$data$term)<- c('Sharp Turns/Mi','Rained','Driver Hamilton','Wind Speed','Track Temp','Grid')
pm2
labels2 <- c('Grid','Track Temp','Wind Speed','Driver Hamilton','Rained','Sharp Turns/Mi')
values2 <- as.numeric(varImp(m_m_w)[1]$importance[,'Win'])
data2 <- data.frame(labels2,values2)
gg2<- ggplot(data2,aes(values2,reorder(labels2,+values2)),width=100)+   
  geom_bar(stat = "identity",width=0.2,fill='skyblue') +
  xlab('Importance') +
  ylab(NULL) +
  ggtitle('Variable Importance Mercedes W/L RF') +
  theme(text=element_text(size=10,color='grey3'),axis.text.y=element_text(colour='black'))
plot(varImp(f_m_w_rf),main='Var Imp Ferrari')

plot_grid(pm,pm2)

plot_grid(gg1,gg2)
plot_grid(gg3,gg4,gg5,nrow=1)

plot_grid(pm3,pm4,pm5,nrow=1)

ggroc(c(roc1,roc2,roc3,roc4),aes="group")
######## Max
par(mfrow=c(2,3))
set.seed(24)
ind_max_w <- sample(2, nrow(max_w), replace=T, prob=c(.7,.30))
train_max_w<- max_w[ind_max_w==1,]
test_max_w<- max_w[ind_max_w==2,]

max_rf_w <- train(finish_tier ~.,data=train_max_w,method='rf',trControl = cvcontrol,importance=TRUE)
max_log_w <- train.glm(finish_tier~ .,data=train_max_w,method='glm.fit')

max_rf_wpred <- predict(max_rf_w,test_max_w,type='prob')
max_log_wpred <- predict(max_log_w,test_max_w,type="prob")
max_rf_wpred



summary(max_rf_wpred)

max_w_roc <- multiclass.roc(test_max_w$finish_tier,max_rf_wpred$Win, percent=TRUE)
max_w_roc <- max_w_roc[['rocs']]
max_w_roc <-max_w_roc[[1]]
max_w_rocl <- multiclass.roc(test_max_w$finish_tier,max_log_wpred$prediction[,'Win'], percent=TRUE)
max_w_rocl <- max_w_rocl[['rocs']]
max_w_rocl <-max_w_rocl[[1]]
plot.roc(max_w_roc,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Max Verstappen W/L RF',print.auc.cex = 1.2,
         print.thres.cex = 1.2,
         cex.main=1.2,
         cex.lab=1.2)
plot.roc(max_w_rocl,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Max Verstappen W/L Logit',print.auc.cex = 1.2,
         print.thres.cex = 1.2,
         cex.main=1.2,
         cex.lab=1.2)

labels3 <- c('Grid','Track Temp','Wind Speed','Rained','Team Rank','Sharp Turns/Mi')
values3 <- as.numeric(varImp(max_rf_w)[1]$importance[,'Win'])
data3 <- data.frame(labels3,values3)
gg3<- ggplot(data3,aes(values3,reorder(labels3,+values3)),width=100)+   
  geom_bar(stat = "identity",width=0.2,fill='skyblue') +
  xlab('Importance') +
  ylab(NULL) +
  ggtitle('VI Max W/L RF') +
  theme(text=element_text(size=10,color='grey3'),axis.text.y=element_text(colour='black'))
gg3

pm3 <- plot_model(max_log_w,show.values=TRUE,transform = NULL,ci.lvl=NA,title='Max W/L',dot.size=1,value.size=5)
levels(pm3$data$term)<- c('Sharp Turns/Mi','Team Rank','Rained','Wind Speed','Track Temp','Grid')
pm3
varImp(max_rf_w)[1]$importance
levels(pm$data$term)<- c('Sharp Turns/Mi','Rained','Track Temp','Wind Speed','Driver Vettel','Grid')
plot(varImp(max_rf_w))
####### Lewis
set.seed(24)
ind_hamilton_w <- sample(2, nrow(hamilton_w), replace=T, prob=c(0.75,0.25))
train_hamilton_w<- hamilton_w[ind_hamilton_w==1,]
test_hamilton_w<- hamilton_w[ind_hamilton_w==2,]
str(train_hamilton_w)
hamilton_rf_w <- train(finish_tier ~.,data=train_hamilton_w,method='rf',trControl = cvcontrol,importance=TRUE)
hamilton_log_w <- train.glm(finish_tier~ .,data=train_hamilton_w,method='glm.fit')
plot_model(hamilton_log_w,show.values=TRUE,transform = NULL,ci.lvl=NA)
hamilton_rf_w


hamilton_rf_wpred <- predict(hamilton_rf_w,test_hamilton_w,type='prob')
hamilton_log_wpred <- predict(hamilton_log_w,test_hamilton_w,type="prob")

hamilton_w_roc <- multiclass.roc(test_hamilton_w$finish_tier,hamilton_rf_wpred$Win, percent=TRUE)
hamilton_w_roc <- hamilton_w_roc[['rocs']]
hamilton_w_roc <-hamilton_w_roc[[1]]
plot.roc(hamilton_w_roc,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Lewis Hamilton W/L RF',print.auc.cex = 1.2,
         print.thres.cex = 1.2,
         cex.main=1.2,
         cex.lab=1.2)
hamilton_w_rocl <- multiclass.roc(test_hamilton_w$finish_tier,hamilton_log_wpred$prediction[,'Win'], percent=TRUE)
hamilton_w_rocl <- hamilton_w_rocl[['rocs']]
hamilton_w_rocl <-hamilton_w_rocl[[1]]
plot.roc(hamilton_w_rocl,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Lewis Hamilton W/L Logit',print.auc.cex = 1.2,
         print.thres.cex = 1.2,
         cex.main=1.2,
         cex.lab=1.2)
plot_model(hamilton_log_w,show.values=TRUE,transform = NULL,ci.lvl=NA)
pm4 <- plot_model(hamilton_log_w,show.values=TRUE,transform = NULL,ci.lvl=NA,title='Lewis W/L',dot.size=1,value.size=5)
levels(pm4$data$term)<- c('Sharp Turns/Mi','Team Rank','Rained','Wind Speed','Track Temp','Grid')
pm4
plot(varImp(hamilton_rf_w))


labels4 <- c('Grid','Track Temp','Wind Speed','Rained','Team Rank','Sharp Turns/Mi')
values4 <- as.numeric(varImp(hamilton_rf_w)[1]$importance[,'Win'])
data4 <- data.frame(labels4,values4)
gg4<- ggplot(data4,aes(values4,reorder(labels4,+values4)),width=100)+   
  geom_bar(stat = "identity",width=0.2,fill='skyblue') +
  xlab('Importance') +
  ylab(NULL) +
  ggtitle('VI Hamilton W/L RF') +
  theme(text=element_text(size=10,color='grey3'),axis.text.y=element_text(colour='black'))
gg4
varImp(hamilton_rf_w)[1]$importance
###### Bottas

set.seed(24)
ind_botta_3 <- sample(2, nrow(botta_3), replace=T, prob=c(0.83,0.17))
train_botta_3<- botta_3[ind_botta_3==1,]
test_botta_3<- botta_3[ind_botta_3==2,]


botta_rf_3 <- train(finish_tier ~.,data=train_botta_3,method='rf',trControl = cvcontrol,importance=TRUE)
botta_log_3 <- train.glm(finish_tier~ .,data=train_botta_3,method='glm.fit')
plot_model(botta_log_3,show.values=TRUE,transform = NULL,ci.lvl=NA)
botta_rf_3

botta_rf_3pred <- predict(botta_rf_3,test_botta_3,type='prob')
botta_log_3pred <- predict(botta_log_3,test_botta_3,type="prob")


botta_3_roc <- multiclass.roc(test_botta_3$finish_tier,botta_rf_3pred$Top3, percent=TRUE)
botta_3_roc <- botta_3_roc[['rocs']]
botta_3_roc <-botta_3_roc[[1]]
plot.roc(botta_3_roc,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Valtteri Bottas Top3 RF',print.auc.cex = 1.2,
         print.thres.cex = 1.2,
         cex.main=1.2,
         cex.lab=1.2)
botta_3_rocl <- multiclass.roc(test_botta_3$finish_tier,botta_log_3pred$prediction[,'Top3'], percent=TRUE)
botta_3_rocl <- botta_3_rocl[['rocs']]
botta_3_rocl <-botta_3_rocl[[1]]
plot.roc(botta_3_rocl,
         print.auc = T,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightblue',
         print.thres = T,
         main = 'Valtteri Bottas Top3 Logit',print.auc.cex = 1.2,
         print.thres.cex = 1.2,
         cex.main=1.2,
         cex.lab=1.2)

plot_model(botta_log_3,show.values=TRUE,transform = NULL,ci.lvl=NA)
pm5 <- plot_model(botta_log_3,show.values=TRUE,transform = NULL,ci.lvl=NA,title='Bottas Top3',dot.size=1,value.size=5)
levels(pm5$data$term)<- c('Sharp Turns/Mi','Team Rank','Rained','Wind Speed','Track Temp','Grid')
pm5
plot(varImp(botta_rf_3))

labels5 <- c('Grid','Track Temp','Wind Speed','Rained','Team Rank','Sharp Turns/Mi')
values5 <- as.numeric(varImp(botta_rf_3)[1]$importance[,'Top3'])
data5 <- data.frame(labels5,values5)
gg5<- ggplot(data5,aes(values5,reorder(labels5,+values5)),width=100)+   
  geom_bar(stat = "identity",width=0.2,fill='skyblue') +
  xlab('Importance') +
  ylab(NULL) +
  ggtitle('VI Bottas Top3 RF') +
  theme(text=element_text(size=10,color='grey3'),axis.text.y=element_text(colour='black'))
gg5
varImp(botta_rf_3)[1]$importance
