####################
# Bootstrap Results Ratio
# Caira Clark and Tim Barrett
# 18 February 2021
####################

head(aggroup) #from EGB_CleanData.R

DF <- aggroup
DF$OBS <- ifelse(DF$TRIP == "", 0, 1)
zones_q <- unique(DF$GROUP)

set.seed(86)

DF$COD[DF$COD==0]<-1
DF$HAD[DF$HAD==0]<-1

RES <- data.frame(zones_q=zones_q,mean=NA,SE=NA,bias=NA,VAR=NA)

nsamp <- 10

for(i in 1:length(zones_q)){
  DO <- DF[DF$OBS==1 & DF$GROUP==zones_q[i],]
  DU <- DF[DF$OBS==0 & DF$GROUP==zones_q[i],]
  
  RES$mean[i] <- mr <- (sum(DO$COD)/sum(DO$HAD))/(sum(DU$COD)/sum(DU$HAD))
  
  if(nrow(DO) < 3 | nrow(DU) < 3){next}
  
  r1list<-list()
  r2list<-list()
  ratios <- list()
  
  for(s in 1:1000){
    so <- sample.int(nrow(DO), size = nsamp, replace = T) 
    su <- sample.int(nrow(DU), size = nsamp, replace = T) 
    r1list[[s]] <- sum(DO$COD[so])/sum(DO$HAD[so])
    r2list[[s]] <- sum(DU$COD[su])/sum(DU$HAD[su])
    ratios[[s]] <- (sum(DO$COD[so])/sum(DO$HAD[so])) / (sum(DU$COD[su])/sum(DU$HAD[su]))
  }
  
  RES$SE[i] <- sd(unlist(ratios))
  RES$bias[i]<- mean(unlist(ratios))-mr
  
  r1<-unlist(r1list)
  r2<-unlist(r2list)
  
}

rm(DF)

land <- aggroup %>% mutate(OBS = if_else(TRIP == "", 0, 1)) %>% group_by(GROUP, OBS) %>% summarize(LANDINGS = sum(COD)) %>% pivot_wider(names_from = OBS, values_from = LANDINGS) %>% rename(zones_q = GROUP, UNOBSLAND = `0`, OBSLAND = `1`)

data <- left_join(RES, land)

#Load ratio data file from APL runs
#data <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/ratiotest.csv')

#Remove multipliers that are less than zero
data$mean <- ifelse(data$mean>0,data$mean,"")

#Convert landings (kg) to mt
data$OBSLAND <- data$OBSLAND/1000
data$UNOBSLAND <- data$UNOBSLAND/1000

#Calculate discards and then remove discard values if they are calculated as less than zero

data <- data %>%  mutate(
  DISCARDS = case_when(
    mean > 1 ~ (data$mean*data$UNOBSLAND)-data$UNOBSLAND))
    
data$DISCARDS <- ifelse(data$DISCARDS>0,data$DISCARDS,0)

data$DISCARDS[is.na(data$DISCARDS)] <- 0

#Determine if the values are significant using BS 1000 APL file. Then create another data column for significant/not significant.

  data$SIGNIFICANT <- c(1,1,0,0,0,0,0,0,0,0,0)

data$SIGDISCARDS <- data$DISCARDS*data$SIGNIFICANT
