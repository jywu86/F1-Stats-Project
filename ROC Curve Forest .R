library(randomForest)
library(caret)
library(lime)
library(tree)
library(pROC)

# Creating ROC Curves with Random Forest

# Importing Data and Prepping it for modeling
mydata_all <- read.csv('Model_Data_w_Turns.csv')

mydata_win <- subset(mydata_all, select= c(dist.mi,grid,position,Air.Temp,Track.Temp,Wind.Speed,driverRef,constructorRef,Rainfall2,qualifying_dif,team_rank,dist_turns,dist_s_turns,turns_mile,turns_s_mile,Turns,Sharp.Turns,Type))
mydata_win$win[mydata_win$position ==1] <- 'Win'
mydata_win$win[mydata_win$position !=1] <- 'Lose'
mydata_win <- na.omit(mydata_win)

#################### Data Cleaning and dropping columns ###############################

model_data_win <- subset(mydata_win, select = -c(turns_mile,turns_s_mile,Turns,dist_turns,Sharp.Turns,position,constructorRef,team_rank))
#str(model_data_win)
factor_cols_win <- c('Rainfall2','driverRef','win','Type')
model_data_win[,factor_cols_win] <- lapply(model_data_win[,factor_cols_win],factor)


# Training and Testing for Win Lose Model
#str(model_data_win)
set.seed(24)
ind <- sample(2, nrow(model_data_win), replace=T, prob=c(0.6,0.4))
train_win <- model_data_win[ind==1,]
test_win <- model_data_win[ind==2,]

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
         #col = 'red',
         print.auc = T,
         print.auc.cex = 1.1,
         auc.polygon = T,
         max.auc.polygon = T,
         auc.polygon.col ='lightgoldenrod',
         print.thres = T,
         print.thres.cex=1.05,
         main = 'ROC Curve for Forest')

coords(r_forest,"best", ret='threshold',transpose=FALSE)  # <----- This gives you the actual threshold value

# Prediction using ROC threshold
forest_win_factor <- as.factor(ifelse(forest_win_pred$Win>0.0765,"Win","Lose")) 

# Confusion Matrix 
confusionMatrix(forest_win_factor, test_win$win, positive='Win')

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



