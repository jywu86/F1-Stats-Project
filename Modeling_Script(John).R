library(randomForest)
library(caret)
library(lime)
library(tree)
library(pROC)

install.packages(randomForest)

# Importing Data and Prepping it for modeling
mydata_all <- read.csv('Modeling_Data_Full.csv')
str(mydata)
mydata <- subset(mydata_all, select= c(grid,position,Air.Temp,Track.Temp,Wind.Speed,track_group,driverRef,final_position,constructorRef,
                                   Rainfall2,qualifying_dif))

mydata$finish_tier[mydata$position<=3] <-'Podium'
mydata$finish_tier[(mydata$position>3)&(mydata$position<=10)] <-'Points'
mydata$finish_tier[(mydata$position>10)] <- 'Back'
mydata <- na.omit(mydata)


mydata$win[mydata$position ==1] <- 'Win'
mydata$win[mydata$position !=1] <- 'Lose'


#################### Model1 data set (Using Finish_Tier as dependent variable) ###############################
model_data <- subset(mydata, select = -c(final_position,position,constructorRef))
str(model_data)

factor_cols <- c('Rainfall2','driverRef','track_group','grid','finish_tier')
model_data[,factor_cols] <- lapply(model_data[,factor_cols],factor)

model_data1b <- subset(mydata, select = -c(final_position,position,constructorRef))
factor_cols <- c('Rainfall2','driverRef','track_group','finish_tier')
model_data1b[,factor_cols] <- lapply(model_data1b[,factor_cols],factor)



# 1st model using Finish_tier with bagging
set.seed(24)
ind <- sample(2, nrow(model_data), replace=T, prob=c(0.8,0.2))
train <- model_data[ind==1,]
test <- model_data[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                         number =5,
                         repeats=2,
                         allowParallel = TRUE)

bag1 <- train(finish_tier ~.,
             data=train,
             method='treebag',
             trControl = cvcontrol,
             importance=TRUE)

bag1predict <- predict(bag,test)

plot(varImp(bag1))


# 2nd model(Forest Model) Finish_tier(50/50 - split)
set.seed(24)
ind <- sample(2, nrow(model_data), replace=T, prob=c(0.8,0.2))
train <- model_data[ind==1,]
test <- model_data[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest1 <- train(finish_tier ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE)
forest1

forest1predict <- predict(forest1,test)

confusionMatrix(forest1predict, test$finish_tier)

# 3rd model using tier with forest (80 -Train, 20-Test)
set.seed(24)
ind <- sample(2, nrow(model_data), replace=T, prob=c(0.8,0.2))
train <- model_data[ind==1,]
test <- model_data[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest2 <- train(finish_tier ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE)


forest2predict <- predict(forest2,test)

confusionMatrix(forest2predict, test$finish_tier)

# 4th model using tier with forest (60 -Train, 40-Test) #### THIS ONE HAS the 75% I'm seeing in the test results
set.seed(24)
ind <- sample(2, nrow(model_data1b), replace=T, prob=c(0.6,0.4))
train <- model_data1b[ind==1,]
test <- model_data1b[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest3 <- train(finish_tier ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)


forest3predict <- predict(forest3,test)

confusionMatrix(forest3predict, test$finish_tier)

plot(varImp(forest3))


#5 using grid as numeric
set.seed(24)
ind <- sample(2, nrow(model_data), replace=T, prob=c(0.6,0.4))
train <- model_data[ind==1,]
test <- model_data[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest4 <- train(finish_tier ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE,ntree=400)


forest4predict <- predict(forest4,test)

confusionMatrix(forest4predict, test$finish_tier)

plot(varImp(forest4))

# 3rd model using tier with xgBoost
set.seed(24)
ind <- sample(2, nrow(model_data), replace=T, prob=c(0.5,0.5))
train <- model_data[ind==1,]
test <- model_data[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

boost1 <- train(finish_tier ~., 
                data = train,
                method='xgbTree',
                trControl = cvcontrol,
                tuneGrid = expand.grid(nrounds = 1000,
                                       max_depth =5,
                                       eta = 0.05,
                                       gamma = 2,
                                       colsample_bytree =1,
                                       min_child_weight = 1,
                                       subsample =1 ))


############################ TESTED with FINISH_POSITION ################################################

# 2nd model using finish_position with bagging
model_data2 <- subset(mydata, select = -c(resultId,year,raceId,circuitRef,constructorRef,position,Fastest_Qual,finish_tier))
factor_cols <- c('Rainfall2','driverRef','track_group','grid','final_position')
model_data2[,factor_cols] <- lapply(model_data2[,factor_cols],factor)
model_data2$team_rank <- as.numeric(model_data$team_rank)

set.seed(24)
ind <- sample(2, nrow(model_data2), replace=T, prob=c(0.8,0.2))
train <- model_data2[ind==1,]
test <- model_data2[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

bag2 <- train(final_position ~.,
              data=train,
              method='treebag',
              trControl = cvcontrol,
              importance=TRUE)

bag2predict <- predict(bag2, test)

confusionMatrix(bag2predict, test$finish_position)

# 2nd model using finish_position with forest
set.seed(24)
ind <- sample(2, nrow(model_data2), replace=T, prob=c(0.8,0.2))
train <- model_data2[ind==1,]
test <- model_data2[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest2a <- train(final_position ~.,
                 data=train,
                 method='rf',
                 trControl = cvcontrol,
                 importance=TRUE)

forest2apredict <- predict(forest2a,test)

plot(forest2apredict ~ test$final_position)

plot(varImp(forest2a))

forest3

##################################### CLASSIFYING WINNER/LOSER ######################################################################
#  model 3 data set (Classifying winner)
model_data3 <- subset(mydata, select = -c(final_position,position,constructorRef,finish_tier))
str(model_data)

factor_cols <- c('Rainfall2','driverRef','track_group','grid','win')
model_data3[,factor_cols] <- lapply(model_data3[,factor_cols],factor)

# 3rd model using tier with bagging
set.seed(24)
ind <- sample(2, nrow(model_data), replace=T, prob=c(0.8,0.2))
train <- model_data3[ind==1,]
test <- model_data3[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

bag3 <- train(win ~.,
              data=train,
              method='treebag',
              trControl = cvcontrol,
              importance=TRUE)


bag3predict <- predict(bag3,test)

confusionMatrix(bag3predict, test$win, positive='Win')

plot(varImp(bag1))



