####################
# Bootstrap Results Ratio
# Caira Clark
# 18 February 2021
####################

data <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/ratiotest.csv')

#Remove multipliers that are less than zero
data$MULTIPLIER <- ifelse(data$MULTIPLIER>0,data$MULTIPLIER,"")

#Convert landings (kg) to mt
data$OBSLAND <- data$OBSLAND/1000
data$NONOBSLAND <- data$NONOBSLAND/1000

#Calculate discards and then remove discard values if they are calculated as less than zero

data <- data %>%  mutate(
  DISCARDS = case_when(
    MULTIPLIER > 1 ~ (data$MULTIPLIER*data$UNOBSERVED)-data$UNOBSERVED))
    
data$DISCARDS <- ifelse(data$DISCARDS>0,data$DISCARDS,0)

data$DISCARDS[is.na(data$DISCARDS)] <- 0
