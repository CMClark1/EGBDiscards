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

marfis <- marfis %>% rename(TRIP = OBS_Trip)

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

sought_nocodhad <- sought %>% select(TRIP, SPECSCD_ID)

marfis <- left_join(marfis, sought_nocodhad, by="TRIP") #creates many more lines of data==

noncodhad <- marfis %>% filter(!SPECSCD_ID %in% c(10, 11))


#Check cod/had ratios
#Remove unwanted records
#Aggregate data



