####################
# Assign Fleet and Zone
# Caira Clark
# 22 February 2021
####################

library(dplyr)

#Load clean MARFIS data - these files are not clean, so they should be replaced

marfis1 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q12/MARFISXTAB_Q12_CC.csv')
marfis2 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q34/MARFISXTAB_Q34_CC.csv')

marfis <- rbind(marfis1, marfis2)

marfis <- marfis %>% filter(!is.na(Year))

#Add fleet ("SECTOR") column
marfis <- marfis %>%  mutate(
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
                                            agg.poly.shp = "C:/R/zones.shp",
                                            agg.poly.field = "Id")

marfis$ZONE <- as.numeric(marfis$Id)

#Correct zone if missing or 0 

##Create two dataframes, one that is correct and one that needs corrections

zone_correct <- marfis %>% filter(!is.na(ZONE))

zone_corrections <- marfis %>% filter(is.na(ZONE))

##Correct observed trips with missing zones

observed <- zone_corrections %>% filter(nchar(OBS_Trip)>1)

observed <- rename(observed, TRIP = OBS_Trip)

head(isdbtrips) #Loaded in the file EGB_ISDB.R

temp <- isdbtrips %>% select(TRIP, LAT2, LON2)

temp <- left_join(observed, temp, by="TRIP")

observed <- Mar.utils::identify_area(df=temp, lat.field = temp$LAT2, lon.field = temp$LON2,
                                   agg.poly.shp = "C:/R/zones.shp",
                                   agg.poly.field = "Id")

marfis$ZONE <- as.numeric(marfis$Id)


##Correct unobserved trips with missing zones

unobserved <- zone_corrections %>% filter(nchar(OBS_Trip)<1)



