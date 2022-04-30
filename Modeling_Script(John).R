library(randomForest)
library(caret)
library(lime)

install.packages(randomForest)
# Data Modeling Random Forest
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
#str(mydata)

# model1 data set (Classifying via final tier)
model_data <- subset(mydata, select = -c(final_position,position,constructorRef))
str(model_data)

factor_cols <- c('Rainfall2','driverRef','track_group','grid','finish_tier')
model_data[,factor_cols] <- lapply(model_data[,factor_cols],factor)
#model_data$team_rank <- as.numeric(model_data$team_rank)

# 1st model using tier with bagging
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

bag1

bag1predict <- predict(bag,test)

plot(varImp(bag1))




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

bag2preidct <- predict(bag2, test)
#plot(varImp(bag2))

#### HERE IS MY CODE TO PREDICT WIN/LOSE 
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


# 1st model using tier with forest
set.seed(24)
ind <- sample(2, nrow(model_data), replace=T, prob=c(0.5,0.5))
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



plot(varImp(forest1))

#randomForest(formula=finish_tier~., data=model_data, proximity = TRUE)



# 2nd model using finish_position with forest
set.seed(24)
ind <- sample(2, nrow(model_data2), replace=T, prob=c(0.8,0.2))
train <- model_data2[ind==1,]
test <- model_data2[ind==2,]

cvcontrol <- trainControl(method ='repeatedcv',
                          number =5,
                          repeats=2,
                          allowParallel = TRUE)

forest2 <- train(final_position ~.,
              data=train,
              method='rf',
              trControl = cvcontrol,
              importance=TRUE)

forest2predict <- predict(forest2,test)

plot(forest2predict ~ test$final_position)

plot(varImp(forest2))

