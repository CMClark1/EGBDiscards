####################
# Assign Fleet and Zone
# Caira Clark
# 22 February 2021
####################

library(dplyr)
library(ROracle)
library(Mar.fleets)
library(Mar.utils)
library(ROracle)
library(leaflet)
library(gstat)

#Load QAQC'd marfis data from file EGB_QAQC.R

head(marfis_qaqc)

#Add fleet ("SECTOR") column
marfis <- marfis_qaqc %>%  mutate(
  SECTOR = case_when(
    SFLT_DESC_ID %in% c(90:95, 8976, 8977, 9318, 9319, 10865, 10866, 12305, 12195) ~ 90 #New Fleet Desc 90 (FG<45’) 
    , SFLT_DESC_ID %in% c(89, 9317) ~ 89 #New Fleet Desc 89 (HL) (Gear = 41 so not handline)
    , SFLT_DESC_ID %in% c(881) ~ 881 #New Fleet Desc 881 (MG<65’ – SF ITQ)
    , SFLT_DESC_ID %in% c(884, 6875) ~ 884 #New Fleet Desc 884 (FG 45’ to 64’)
    , SFLT_DESC_ID %in% c(885, 1472) ~ 885 #New Fleet Desc 885 (MG>100’)
    , SFLT_DESC_ID %in% c(886, 1817) ~ 886 #New Fleet Desc 886 (FG 65’ to 100’)
    , SFLT_DESC_ID %in% c(311, 1523) ~ 311 #New Fleet Desc 311(MG 65’ to 100’)
    , SFLT_DESC_ID %in% c(3328) ~ 3328) ) #New Fleet Desc 3328 (First Nations)

#Assign zones (based on a script from Mike)

sf::sf_use_s2(FALSE)

marfis <- Mar.utils::identify_area(df=marfis, lat.field = "LAT", lon.field = "LON",
                                            agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/zones.shp",
                                            agg.poly.field = "Id")

marfis$ZONE <- as.numeric(marfis$Id)

marfis <- marfis %>% select(!Id)

#Correct zone if missing or 0 

##Create two dataframes, one that is correct and one that needs corrections

zone_correct <- marfis %>% filter(!is.na(ZONE))

zone_corrections <- marfis %>% filter(is.na(ZONE))

##Correct observed trips with missing zones

observed <- zone_corrections %>% filter(nchar(TRIP.x)>1)

observed <- rename(observed, TRIP = TRIP.x)

head(isdbtrips) #Loaded in the file EGB_ISDB.R

temp <- isdbtrips %>% select(TRIP, LAT2, LON2)

temp <- left_join(observed, temp, by="TRIP")

observed <- Mar.utils::identify_area(df=temp, lat.field = "LAT2", lon.field = "LON2",
                                     agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/zones.shp",
                                     agg.poly.field = "Id")

observed$ZONE <- observed$Id

observed <- observed %>% select(!Id)

##Correct unobserved trips with missing zones

unobserved <- zone_corrections %>% filter(nchar(TRIP.x)<1)

unobserved$LANDED_DATE <- lubridate::as_date(unobserved$LANDED_DATE)

##Run this on each fleet type that is required for the analysis
est_5Z_2021<- fishin_CHPs(type="MOBILE", stock = "5Z", dateStart = "2021-01-01", dateEnd= "2021-12-31", returnISDB=TRUE, useLocal = T, data.dir='C:/LocalDataDump', socks=T)

chpVMS<-get_vmstracks(data=est_5Z_2021, 
                      oracle.username= oracle.username, 
                      oracle.password=oracle.password, 
                      oracle.dsn=oracle.dsn, 
                      usepkg="roracle")

#Filter VMS data using VR_NUMBER and LANDED_DATE from unobserved trips missing zone numbers

chpVMS_filter <- chpVMS %>% filter(VR_NUMBER %in% unobserved$VR_NUMBER_FISHING)

quick_map(est_5Z_2021, vms = chpVMS_filter)


#Identify Non-Commercial Trips

head(isdbtrips) #Load this in script EGB_ISDB.R

isdb_noncom <- isdbtrips %>% filter(SETCD_ID == 7) %>% select(TRIP, SETCD_ID)

marfis <- marfis %>% rename(TRIP = TRIP.x)

marfis <- left_join(marfis, isdb_noncom, by="TRIP")

noncomtrips <- marfis %>% filter(SETCD_ID == 7) #This is the data frame where the removed non-commercial trips appear

marfis <- marfis %>% filter(is.na(SETCD_ID) | SETCD_ID != 7) #Data frame with non-commercial trips removed

#Identify no-panel trips

##Replace these with ROracle query
sep1 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q12/Separator_Q12.csv')
sep2 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q34/Separator_Q34.csv')

separator<- rbind(sep1, sep2)

sep_nopan <- separator %>% filter(GEARFCD_ID==89) %>% select(TRIP, GEARFCD_ID)

marfis <- left_join(marfis, sep_nopan, by="TRIP")

nopaneltrips <- marfis %>% filter(GEARFCD_ID == 89) #This is the data frame where the removed no panel trips appear

marfis <- marfis %>% filter(is.na(GEARFCD_ID) | GEARFCD_ID != 89) #Data frame with no panel trips removed


#Identify non-cod/had observed trips

##Replace these with ROracle query
sought1 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q12/Sought_Q12.csv')
sought2 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q34/Sought_Q34.csv')

sought <- rbind(sought1, sought2)

noncodhad <- sought %>% group_by(TRIP) %>% filter(!SPECSCD_ID %in% c(10,11)) #non cod/had directed sets



marfis <- left_join(marfis, sought_nocodhad, by="TRIP") #creates many more lines of data

noncodhad <- marfis %>% filter(!SPECSCD_ID %in% c(10, 11))


#Check cod/had ratios and pollock ratios

df <- marfis
df$X100[is.na(df$X100)] <- 0 #replace NAs with zero
df$X110[is.na(df$X110)] <- 0
df$X170[is.na(df$X110)] <- 0

#Potential to add that sets are eliminated when cod or haddock landings = 0

#Remove trips where cod:had ratio for all sets in the trip are greater than 0.8

df$codhad_ratio <- df$X100/df$X110 #Calculate cod:had ratio
df$test1<-with(df, ifelse(codhad_ratio < 0.8, "met", "not met")) #Basic ifelse; evaluation of a value for each record/set.
temp1<-subset(df, select=c('TRIP_ID','test1')) #Removes the value column, to help apply the test1 command to groups instead of individual sets.
temp1<-unique(temp1) #removes replicates in the group-level test dataset.
temp2 <- temp1 %>% group_by(TRIP_ID) %>% summarise(n = n()) %>% filter(n==1) #select trips where sets all met or all did not meet conditions
temp3 <- temp1 %>% filter(TRIP_ID %in% temp2$TRIP_ID & test1 == "not met") #select trips where all sets did not meet the condition
names(temp3)[2]<-'test2' #Renames the test field to faciliate merging with df
df2<-merge(df, temp3, all=TRUE) #merges the two, identifying groups which did not meet the conditions for at least one set
df2$test2<-with(df2, ifelse(is.na(test2), 'met',test2)) #Replaces 'NAs' in the test column (created NAs becaused join in only groups which DID NOT meet conditions at least once)
df2$test1<-NULL
rm(df, temp1, temp2, temp3)

cod_directed1 <- df2 %>% filter(test2 == "not met")
df <- df2 %>% filter(test2 == "met")

#Remove sets at the end of the trip with a ratio greater than 0.8

df$test2 <- NULL
df$test1<-with(df, ifelse(codhad_ratio < 0.8, "met", "not met")) 
temp1 <- df %>% filter(codhad_ratio > 0.8) %>% 
  select(TRIP_ID) %>% distinct(TRIP_ID)  #select any remaining trips with a set with a codhad ratio greater than 0.8

temp2 <- df %>% 
  filter(TRIP_ID %in% temp1$TRIP_ID) %>% 
  select(LOG_EFRT_STD_INFO_ID, TRIP_ID, codhad_ratio, test1) %>%
  group_by(TRIP_ID) %>%
  slice(which.max(LOG_EFRT_STD_INFO_ID)) %>%
  filter(test1 == "not met") #select any trips where the last set has a codhad ratio greater than 0.8

temp3 <- df %>%
  filter(TRIP_ID %in% temp2$TRIP_ID) %>%
  select(LOG_EFRT_STD_INFO_ID, TRIP_ID, codhad_ratio, test1) %>%
  group_split(TRIP_ID) #trip dataframes in list. check to make sure 'not met' occurs only at the end of the trip

temp4 <- df %>% 
  filter(TRIP_ID %in% temp2$TRIP_ID) %>%
  filter(test1 == "met") #select data from the trips with high codhad ratios that meet the condition

temp5 <- df %>%
  filter(!TRIP_ID %in% temp2$TRIP_ID) #select all data that are not from the trips with the high/late cod had ratios

cod_directed2 <- df %>% filter(TRIP_ID %in% temp2$TRIP_ID) %>% filter(test1 == "not met")
cod_directed <- rbind(cod_directed1, setNames(cod_directed2, names(cod_directed1))) 
cod_directed$test2 <- NULL #dataframe of sets identified as cod directed

df <- rbind(temp4, temp5)
df$test1 <- NULL #dataframe of remaining sets identified as not cod directed

rm(temp1, temp2, temp3, temp4, temp5)

#Remove sets where pollock landings are greater than combined cod and haddock landings

df$pol_ratio <- df$X170>(df$X100+df$X110)
pol_directed <- df %>% filter(pol_ratio == "TRUE") #dataframe with pollock directed sets
haddock_directed <- df %>% filter(pol_ratio == "FALSE") #dataframe with haddock directed sets


#Remove unwanted records

#quarters with 100% observer coverage
#cod directed sets or trips
#pollock directed sets or trips
#no panel trips
#non-commercial trips
#trips with no zones

#Aggregate data



