library(dplyr)
library(tidyverse)

# Reading in data
results <- read.csv('results.csv')
race_id <- read.csv('Relevant_Races.csv')
driver_id <- read.csv('drivers.csv')

# Merging data and combining columns 
race_id <- race_id[,c('year','raceId','circuitId','Air.Temp','Track.Temp','Rainfall','Wind.Speed')] # creating a reference dataframe
final_results <- merge(x=results, y=race_id, by='raceId',all.y=TRUE) 
driver_id <- driver_id[,c('driverId','driverRef')]
#final_results <- merge(x = driver_id[,c('driverId','driverRef')], y = final_results, by = 'driverId', all.y=TRUE)


# Data Exploration
driver_group <- final_results %>% group_by(driverId) %>% summarise(n=n()) %>%  arrange(desc(n))
driver_group <- merge(x=driver_group, y= driver_id, by='driverId',all.x=TRUE)
driver_group <- driver_group %>% arrange(desc(driver_group$n))
affected_drivers <- driver_group[1:8,c('driverId','driverRef')] # filtering top 8 drivers

# Filtering by Driver
final_results <- merge(x=final_results, y=affected_drivers, by='driverId',all.y=TRUE)

# Split Test and Train Data
test_data <- final_results %>% filter(year==2021)
train_data <- final_results %>% filter(year<2021)






str(final_results)

  
final <- read.csv('Final.csv')

race <- final %>% group_by(raceId.x) %>% summarise(n())

driver_group <- final %>% group_by(driverId) %>% summarise(n()) %>% arrange(desc())

qualify <- qualify %>% filter(year == '2021')
qualify <- qualify %>% filter(Fastest_Qual_Secs != 0)

driver_group <- qualify %>% group_by(driverId) %>%summarise(n())


qualify$car <- as.factor(qualify$constructorId)
qualify$driver <- as.factor(qualify$driverId)
qualify$rain <- as.factor(qualify$Rainfall)

driver_quals <- qualify %>% group_by()

m1 <- lm(Fastest_Qual_Secs ~ driver + car + Rainfall + Air.Temp + Track.Temp , data= qualify)
m2 <- lm(Fastest_Qual_Secs ~  car + Rainfall + Air.Temp + Track.Temp , data= qualify)

summary(m1)
summary(m2)

driver843 <- qualify %>% filter(driver=='843')
