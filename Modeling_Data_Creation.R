library(dplyr)
library(tidyverse)

# Reading in data
results <- read.csv('Raw_Data/results.csv')
races <- read.csv('Old_Transformation/Relevant_Races.csv')
driver_id <- read.csv('Raw_Data/drivers.csv')
circuits <- read.csv('Old_Transformation/Relevant_Circuits_Final.csv')
qualify <- read.csv('Raw_Data/qualifying.csv')
team_id <- read.csv('Raw_Data/constructors.csv')
r_qualify <- read.csv('Old_Transformation/Relevant_Qualify_2.csv')
r_qualify <- select(r_qualify, -X.1, -X) # removing X.1 and X columns

# Removing columns from results that are not needed
results <- results[,c('resultId','raceId','driverId',
                      'constructorId','number','grid',
                      'position','positionText','positionOrder','time')]


# Merging data and combining columns 
r_races <- races[,c('year','raceId','circuitId','Air.Temp','Track.Temp','Rainfall','Wind.Speed')] # creating a reference dataframe
final_results <- merge(x=results, y=r_races, by='raceId',all.y=TRUE) 

# Merging Final Results with Driver Name
#driver_id <- driver_id[,c('driverId','driverRef')]
#final_results <- merge(x =driver_id, y = final_results, by = 'driverId', all.y=TRUE)

#race_id <- r_races[,c('raceId','year')]

# Adding track_group (track speed) to results
circuits <- transform(circuits, track_group = ifelse(Avg_Speed_MPH<127.6, 'Low_Speed',ifelse(Avg_Speed_MPH<152.4, 'Med_Speed','High_Speed')))
speed_cols <- circuits[,c('circuitId','track_group')]
final_results <- merge(x=final_results, y=speed_cols, by='circuitId',all.x=TRUE)

r_qualify
r_qualify2 <- r_qualify[c("raceId","driverId","Fastest_Qual")] 
r_qualify2

# Filtering out only relevant qualifying data
qualify_filter <- r_qualify %>% distinct(raceId)
qualify_filter
final_results
final_results <- merge(x=final_results, y = qualify_filter, by.x='raceId',all.y=TRUE)
final_results
# Removing results where people DQ'ed or Retired
final_results <- filter(final_results, positionText != 'R')
final_results2 <- merge(x=final_results,y=r_qualify2,by=c("raceId","driverId"))
final_results2
# Finding Top 10 drivers
driver_group <- final_results2 %>% group_by(driverId) %>% summarise(n=n()) %>%  arrange(desc(n))
driver_group <- merge(x=driver_group, y= driver_id, by='driverId',all.x=TRUE)
driver_group <- driver_group %>% arrange(desc(driver_group$n))

# Filtering by Top 10 Drivers (with most races) and merging with Driver Names
affected_drivers <- driver_group[1:10,c('driverId','driverRef')] # filtering top 10 drivers
final_results2 <- merge(x=final_results2, y=affected_drivers, by='driverId',all.y=TRUE) # filtering out only results with top 8 drivers

final_results2
# Creating points system for positions 1-22 (Maybe not needed)
#points_model <- c(22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
#positionOrder <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
#position_df <- data.frame(positionOrder,points_model)
#final_results <- merge(x=final_results, y=position_df, by='positionOrder',all.x=TRUE) # adding points to modeling data

# Creating team_rank by finding fastest average qualifying
team_rank <- r_qualify[,c('raceId','position_team','constructorId')]
team_rank <- team_rank %>% distinct(raceId,constructorId, .keep_all=TRUE)

race_rank <- team_rank %>% 
  arrange(raceId, constructorId, position_team) %>% 
  group_by(raceId) %>% 
  mutate(team_rank = rank(position_team))

race_rank <- race_rank[,c('team_rank','raceId','constructorId')]

final_results2 <- merge(x=final_results2,y=race_rank, by=c('constructorId','raceId'),all.x=TRUE)

final_results2 <- final_results2 %>% 
  arrange(raceId, constructorId, team_rank) %>% 
  group_by(raceId) %>% 
  mutate(team_rank = rank(team_rank, ties.method='min'))

final_results2$position <- as.numeric(final_results2$position)

final_results2 <- final_results2 %>% 
  arrange(raceId, driverId,position) %>% 
  group_by(raceId) %>% 
  mutate(final_position = rank(position, ties.method='min'))

# Adding team name
final_results2 <- merge(x=final_results2, y=team_id[,c('constructorId','constructorRef')], by='constructorId',all.x=TRUE)

# Adding track name
final_results2 <- merge(x=final_results2, y=circuits[,c('circuitId','circuitRef')], by='circuitId',all.x=TRUE)

# Converting Dry and Wet to bimodal
final_results2$Rainfall2[final_results2$Rainfall != 'Dry'] <- 'Wet'
final_results2$Rainfall2[final_results2$Rainfall == 'Dry'] <- 'Dry'

# Creating only modeling data
modeling_results <- subset(final_results2, select = -c(circuitId,constructorId,
                                               driverId,number,
                                               positionText,
                                               positionOrder,time, Rainfall))


#write.csv(final_results2,'Modeling_Data_Full.csv', row.names=FALSE)
write.csv(modeling_results, 'Modeling_Only.csv', row.names=FALSE)
