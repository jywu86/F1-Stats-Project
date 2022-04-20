library(tidyverse)
library(dplyr)

# Loading files
qualify <- read.csv('Relevant_Qualifying.csv', colClasses=c('Fastest_Qual'='character'))
circuit <- read.csv('circuits.csv')
races <- read.csv('Relevant_Races.csv')

# function to convert qualifying time to seconds
time_convert <- function(qual_time){
  lst_str <- strsplit(qual_time, ':')[[1]]
# sec_str <- strsplit(lst_str, '[.]')[[1]]
  min <- as.numeric(lst_str[1])
  sec <- as.numeric(lst_str[2])
  final_time <- 60*min + sec
  return(final_time)
}

# assigning as fastest qualifying
qualify$Fastest_Qual_Secs <- unlist(map(qualify$Fastest_Qual, time_convert))


# finding relevant circuits
race_id <- unique(data.frame(races$circuitId))
colnames(race_id) <- c('circuitId')
circuit <- circuit %>% inner_join(race_id, by='circuitId')

# merge with circuit and raceId
race_circuit_id <- circuit %>% select(raceId,circuitId)
qualify <- merge(x=qualify, y=race_circuit_id, on ='raceId', how='left')

# finding year for qualify
qual_2018 <- qualify %>% filter(year == 2018)
qual_2019 <- qualify %>% filter(year == 2019)
qual_2020 <- qualify %>% filter(year == 2020)
qual_2021 <- qualify %>% filter(year == 2021)

rainfall_2018 <- qual_2018 %>% group_by(Rainfall) %>% summarise(n())
rainfall_2019 <- qual_2019 %>% group_by(Rainfall) %>% summarise(n())
rainfall_2020 <- qual_2020 %>% group_by(Rainfall) %>% summarise(n())
rainfall_2021 <- qual_2021 %>% group_by(Rainfall) %>% summarise(n())

# 2019 will be used as there are very little rain races and had a large amount of races compared to 2020
track_time <- qual_2019 %>% group_by(circuitId) %>% summarise(mean(Fastest_Qual_Secs))
circuit <- merge(x=circuit, y=track_time, on='circuitId',how='left') # this adds average lap time to circuit
circuit <- rename(circuit,  'Lap_time' = 'mean(Fastest_Qual_Secs)')
circuit$speedmph <- (circuit$dist.mile./circuit$Lap_time)*3600


