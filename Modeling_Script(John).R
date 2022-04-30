library(randomForest)
library(caret)
library(lime)

install.packages(randomForest)
# Data Modeling Random Forest
mydata <- read.csv('Modeling_Only.csv')
#model_data <- subset(mydata, select = -c(positionText,positionOrder,resultId,year,raceId,circuitRef,constructorRef))
str(mydata)

mydata$finish_tier[mydata$position<=3] <-'Podium'
mydata$finish_tier[(mydata$position>3)&(mydata$position<=10)] <-'Points'
mydata$finish_tier[(mydata$position>10)] <- 'Back'
mydata <- na.omit(mydata)

model_data <- subset(mydata, select = -c(circuitRef,year,resultId,raceId,constructorRef,position,Fastest_Qual,final_position))
str(model_data)

factor_cols <- c('Rainfall2','driverRef','track_group','grid','finish_tier')
model_data[,factor_cols] <- lapply(model_data[,factor_cols],factor)
model_data$team_rank <- as.numeric(model_data$team_rank)

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


# 1st model using tier with forest
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

forest1predict <- predict(forest1,test)

forest1

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

