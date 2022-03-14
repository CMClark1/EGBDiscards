####################
# Assign Fleet and Zone
# Caira Clark and Irene Andrushchenko
# 22 February 2021
####################

library(dplyr)
library(ROracle)
library(Mar.fleets)
library(Mar.utils)
library(ROracle)
library(leaflet)
library(gstat)

###Load QAQC'd marfis data from file EGB_QAQC.R----------------------

head(marfis_qaqc)

###Step 4. Add fleet ("SECTOR") column--------------------
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

rm(marfis_qaqc)

###Step 5. Assign zones (based on a script from Mike)--------------

sf::sf_use_s2(FALSE)

marfis <- Mar.utils::identify_area(df=marfis, lat.field = "LAT", lon.field = "LON",
                                            agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/zones.shp",
                                            agg.poly.field = "Id")

marfis$ZONE <- as.numeric(marfis$Id)

marfis <- marfis %>% select(!Id)

###Step 6. Correct zone if missing or 0-------------------

##Create two dataframes, one that is correct and one that needs corrections

zone_correct <- marfis %>% filter(!is.na(ZONE))
zone_corrections <- marfis %>% filter(is.na(ZONE))

##Correct observed trips with missing zones

observed <- zone_corrections %>% filter(nchar(TRIP.x)>1)
observed <- dplyr::rename(observed, TRIP = TRIP.x)

head(isdbtrips) #Loaded in the file EGB_ISDB.R

temp <- isdbtrips %>% select(TRIP, LAT2, LON2)

temp <- left_join(observed, temp, by="TRIP")

observed <- Mar.utils::identify_area(df=temp, lat.field = "LAT2", lon.field = "LON2",
                                     agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/zones.shp",
                                     agg.poly.field = "Id")

observed$ZONE <- observed$Id

observed <- observed %>% select(!Id)

rm(temp)

##Correct unobserved trips with missing zones

unobserved <- zone_corrections %>% filter(nchar(TRIP.x)<1)
unobserved$LANDED_DATE <- lubridate::as_date(unobserved$LANDED_DATE)
unobserved$DATE_FISHED <- lubridate::dmy(unobserved$DATE_FISHED)

##Run this on each fleet type with unobserved trips with missing zones
mobile_5Z_2021<- fishin_CHPs(type="MOBILE", stock = "5Z", dateStart = "2021-01-01", dateEnd= "2021-12-31", returnISDB=TRUE, useLocal = T, data.dir='C:/LocalDataDump', socks=T) #change the dates if you want a different year

fixed_5Z_2021<- fishin_CHPs(type="FIXED", stock = "5Z", dateStart = "2021-01-01", dateEnd= "2021-12-31", returnISDB=TRUE, useLocal = T, data.dir='C:/LocalDataDump', socks=T) #change the dates if you want a different year

mobileVMSRaw <- Mar.utils::VMS_from_MARFIS(df=mobile_5Z_2021$marf$MARF_TRIPS, VR_field = "VR_NUMBER_FISHING", usepkg = "roracle", make_segments = F, LANDED_field = "T_DATE2" )
mobileVMSRaw <- mobileVMSRaw[["marf_VMS"]]

fixedVMSRaw <- Mar.utils::VMS_from_MARFIS(df=fixed_5Z_2021$marf$MARF_TRIPS, VR_field = "VR_NUMBER_FISHING", usepkg = "roracle", make_segments = F, LANDED_field = "T_DATE2" ) #Depending on the license conditions, there may only be VMS data for unobserved trips of mobile gear
fixedVMSRaw <- fixedVMSRaw[["marf_VMS"]]

chpVMS <- mobileVMSRaw#If only mobile VMS data are available
#chpVMS <- rbind(mobileVMS, fixedVMS) #If both mobile and fixed VMS data are available

#Filter existing VMS data using VR_NUMBER and LANDED_DATE from unobserved trips missing zone numbers

chpVMS$DATE <- as.Date(format(as.POSIXct(chpVMS$POSITION_UTC_DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y/%m/%d'))
chpVMS_filter <- chpVMS %>% 
  filter(VR_NUMBER %in% unobserved$VR_NUMBER_FISHING & DATE %in% unobserved$DATE_FISHED) %>% 
  group_by(VR_NUMBER, DATE) %>%
  dplyr::summarize(MeanLat = mean(LATITUDE, na.rm=TRUE), MeanLon = mean(LONGITUDE, na.rm=TRUE)) #Available VMS data for unobserved trips

chpVMS_filter <- as.data.frame(chpVMS_filter)

unobserved <- left_join(unobserved, chpVMS_filter, by = (c("VR_NUMBER_FISHING" = "VR_NUMBER", "DATE_FISHED" = "DATE")))

unobserved <- Mar.utils::identify_area(df=unobserved, lat.field = "MeanLat", lon.field = "MeanLon",
                                     agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/zones.shp",
                                     agg.poly.field = "Id")

unobserved$ZONE <- unobserved$Id

#Bind observed and unobserved dataframes together and remove trips missing Zone

head(zone_correct)
head(observed)
head(unobserved)

observed1 <- observed %>% select(c(1:23))
unobserved1 <- unobserved %>% select(c(1:23))

temp <- rbind(zone_correct, setNames(observed1, names(zone_correct)), setNames(unobserved1, names(zone_correct)))

marfis <- temp %>% filter(ZONE>=1)
noZone <- temp %>% filter(!ZONE>=1)

rm(list=setdiff(ls(), c("marfis", "isdbtrips", "noZone")))

###Step 7. Identify Non-Commercial Trips--------------------

head(isdbtrips) #Load this in script EGB_ISDB.R
isdb_noncom <- isdbtrips %>% filter(SETCD_ID == 7) %>% select(TRIP, SETCD_ID)
marfis <- marfis %>% dplyr::rename(TRIP = TRIP.x)
marfis <- left_join(marfis, isdb_noncom, by="TRIP")
noncomtrips <- marfis %>% filter(SETCD_ID == 7) #This is the data frame where the removed non-commercial trips appear
marfis <- marfis %>% filter(is.na(SETCD_ID) | SETCD_ID != 7) #Data frame with non-commercial trips removed

rm(isdb_noncom)

###Step 8. Identify no-panel trips---------------------------

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

separator <- dbGetQuery(channel, "select a.CFV, a.VESSEL_NAME, b.TRIP, b.LANDING_DATE, c.GEARCD_ID, d.GEARFCD_ID, Count(e.SET_NO)

from isdb.isvessels a, isdb.istrips b, isdb.isgears c, isdb.isgearfeatures d, isdb.isfishsets e, isdb.issetprofile f

where b.VESS_ID = a.VESS_ID
and f.FISHSET_ID = e.FISHSET_ID
and b.TRIP_ID = e.TRIP_ID
and e.GEAR_ID = c.GEAR_ID
and c.GEAR_ID = d.GEAR_ID
and TO_CHAR(b.LANDING_DATE,'yyyy')=2021 
and e.NAFAREA_ID like '5Z%'
and f.PNTCD_ID=2
and c.GEARCD_ID=12
and d.GEARFCD_ID in (89, 90)

group by a.CFV, a.VESSEL_NAME, b.TRIP, b.LANDING_DATE, c.GEARCD_ID, d.GEARFCD_ID

")

sep_nopan <- separator %>% filter(GEARFCD_ID==89) %>% select(TRIP, GEARFCD_ID)

marfis <- left_join(marfis, sep_nopan, by=c("TRIP" = "TRIP"))

nopaneltrips <- marfis %>% filter(GEARFCD_ID == 89) #This is the data frame where the removed no panel trips appear

marfis <- marfis %>% filter(is.na(GEARFCD_ID) | GEARFCD_ID != 89) #Data frame with no panel trips removed

rm(sep_nopan, separator)

###Step 9. Identify non-cod/had observed trips-------------------------

sought <- dbGetQuery(channel, "select a.CFV, a.VESSEL_NAME, b.TRIP, c.GEARCD_ID, b.LANDING_DATE, d.SPECSCD_ID, d.SET_NO, e.SETDATE

from isdb.isvessels a, isdb.istrips b, isdb.isgears c, isdb.isfishsets d, isdb.issetprofile e

where b.VESS_ID = a.VESS_ID
and e.FISHSET_ID = d.FISHSET_ID
and b.TRIP_ID = d.TRIP_ID
and d.GEAR_ID = c.GEAR_ID
and TO_CHAR(b.LANDING_DATE,'yyyy')=2021 
and d.NAFAREA_ID like '5Z%'
and e.PNTCD_ID in (1,2)
and d.SETCD_ID=1

group by a.CFV, a.VESSEL_NAME, b.TRIP, c.GEARCD_ID, b.LANDING_DATE, d.SPECSCD_ID, d.SET_NO, e.SETDATE")

noncodhad <- sought %>% group_by(TRIP) %>% filter(!SPECSCD_ID %in% c(10,11)) %>% select(TRIP, SPECSCD_ID) %>% distinct(TRIP, SPECSCD_ID, .keep_all = TRUE)

marfis <- left_join(marfis, noncodhad, by=c("TRIP" = "TRIP"))

noncodhad <- marfis %>% filter(!SPECSCD_ID %in% c(10, 11) & !is.na(SPECSCD_ID))

marfis <- marfis %>% filter(SPECSCD_ID %in% c(10, 11) | is.na(SPECSCD_ID))

rm(sought)

###Step 10. Check cod/had ratios and pollock ratios---------------------

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
temp2 <- temp1 %>% group_by(TRIP_ID) %>% dplyr::summarise(n = n()) %>% filter(n==1) #select trips where sets all met or all did not meet conditions
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
  arrange(LOG_EFRT_STD_INFO_ID) %>%
  group_split(TRIP_ID) #trip with high cod ratio at the end of the trip in list. check to make sure 'not met' occurs only in the last set of the trip

cod_directed2 <- df %>% filter(LOG_EFRT_STD_INFO_ID %in% temp2$LOG_EFRT_STD_INFO_ID) #cod directed sets at the end of trips
cod_directed <- rbind(cod_directed1, setNames(cod_directed2, names(cod_directed1)))

df <- df %>% filter(!LOG_EFRT_STD_INFO_ID %in% temp2$LOG_EFRT_STD_INFO_ID)
df$test1 <- NULL #dataframe of remaining sets identified as not cod directed

rm(temp1, temp2, temp3, cod_directed1, cod_directed2)

#Remove sets where pollock landings are greater than combined cod and haddock landings

df <- df %>% replace_na(list(X100 = 0, X110 = 0, X170 = 0))

df$pol_ratio <- df$X170>(df$X100+df$X110)
pol_directed <- df %>% filter(pol_ratio == "TRUE") #dataframe with pollock directed sets
haddock_directed <- df %>% filter(pol_ratio == "FALSE") #dataframe with haddock directed sets

rm(df, df2, cod_directed1, cod_directed2)

###Step 11. Removed unwanted records----------------------------

#quarters with 100% observer coverage

rm1 <- noncodhad %>% select(c(1:23)) #sets directed for species other than cod or haddock
rm2 <- cod_directed %>% select(c(1:23)) #cod directed sets or trips
rm3 <- pol_directed %>% select(c(1:23)) #pollock directed sets or trips
rm4 <- nopaneltrips %>% select(c(1:23)) #no panel trips
rm5 <- noncomtrips %>% select(c(1:23)) #non-commercial trips
rm6 <- noZone %>% select(c(1:23)) #trips with no zones

removed <- rbind(rm1,
                     setNames(rm2, names(rm1)),
                     setNames(rm3, names(rm1)),
                     setNames(rm4, names(rm1)),
                     setNames(rm5, names(rm1)),
                     setNames(rm6, names(rm1)))

rm(rm1, rm2, rm3, rm4, rm5, rm6)

###Step 12. Aggregate data----------------------------------

aggregated <- haddock_directed %>%
  group_by(VR_NUMBER_FISHING, VESSEL_NAME.x, LICENCE_ID, TC, LC, TRIP_ID, LANDED_DATE, GEAR_CODE, Q, TRIP, ZONE, SECTOR) %>%
  dplyr::summarize(COD = sum(X100, na.rm=TRUE), HAD = sum(X110, na.rm=TRUE), POL = sum(X170, na.rm=TRUE)) 

###Step 12b. Check for quarters with 100% observer coverage and remove them; remove longline

aggregated <- aggregated %>% filter(GEAR_CODE != 51) #remove longline

coverage1 <- aggregated %>% group_by (SECTOR, Q) %>% mutate(OBS = 1) %>% summarise(UNOBS = sum(OBS[TRIP == ""]), OBS = sum(OBS[TRIP != ""])) %>% mutate(COVERAGE=OBS/(OBS+UNOBS)) %>% filter(COVERAGE == 1) #Quarters with 100% observer coverage

coverage2 <- aggregated %>% group_by(SECTOR) %>% mutate(OBS=1) %>% summarise(UNOBS = sum(OBS[TRIP == ""]), OBS = sum(OBS[TRIP != ""])) %>% mutate(COVERAGE=OBS/(OBS+UNOBS)) %>% filter(COVERAGE == 1) #Fleets with 100% observer coverage

cov100perc <- aggregated %>% filter(SECTOR %in% coverage2$SECTOR) #Dataframe of trips from sectors with 100% coverage

aggregated <- aggregated %>% filter(!SECTOR %in% coverage2$SECTOR)


