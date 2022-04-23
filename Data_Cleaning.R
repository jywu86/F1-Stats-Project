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
cirId = data.frame(races$raceId, races$circuitId)
colnames(cirId) <- c('raceId','circuitId')
qualify <- qualify %>% inner_join(cirId, by='raceId')

# finding relevant circuits
race_id <- data.frame(qualify$circuitId, qualify$raceId)
colnames(race_id) <- c('circuitId','raceId')
race_id <- race_id[!duplicated(race_id[,c('circuitId')]),]
circuit <- circuit %>% inner_join(race_id, by='circuitId')

# merge with qualify with circuit_Id
race_circuit_id <- races %>% select(raceId,circuitId)  # creates a df with race_id and circuit_id to merge with qualify
qualify <- merge(x=qualify, y=race_circuit_id, on ='raceId', how='left')

# adding average speed for each circuit
track_time <- qualify %>% group_by(circuitId) %>% summarise(mean(Fastest_Qual_Secs))
circuit <- merge(x=circuit, y=track_time, on='circuitId',how='left') # this adds average lap time to circuit
circuit <- rename(circuit,  'Avg_Speed' = 'mean(Fastest_Qual_Secs)')
circuit$Avg_Speed_MPH <- (circuit$dist.mile./circuit$Avg_Speed)*3600

# adding telemetry throttle for each circuit
tele_data <- qualify %>% group_by(circuitId) %>% summarise(mean(Throttle))
circuit <- merge(x=circuit, y=tele_data, on='circuitId',how='left') # this adds average throttle to circuit
circuit <- rename(circuit,  'Avg_Throttle' = 'mean(Throttle)')

# outputting Circuit Information
write.csv(circuit,'Relevant_Circuits.csv')
write.csv(qualify, 'Relevant_Qualify_2.csv')

avg_speed <- as.vector(circuit$Avg_Speed_MPH)
hist(avg_speed)
