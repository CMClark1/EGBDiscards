####################
# EGB Observer and Landings Merge QAQC
# Caira Clark and Irene Andrushchenko
# 9 Feb 2022
####################

library(ROracle)
library(tidyverse)
library(lubridate)
library(fuzzyjoin)

#Step 1. Load MARFIS data from Access query (to be replaced with EGB_MARFIS.R code once finished)

marfis1 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q12/MARFISXTAB_Q12.csv')
marfis2 <- read.csv('S:/Science/Population Ecology/Georges Bank/Spec Comp/2021 2022/Q34/MARFISXTAB_Q34.csv')

marfis <- rbind(marfis1, marfis2)

marfis$LANDED_DATE <- dmy(marfis$LANDED_DATE)

marfis <- rename(marfis, TRIP = DATA_VALUE)

#Step 2. Load ISDB data directly

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

isdb <- dbGetQuery(channel, "

select a.CFV, a.VESSEL_NAME, b.TRIP, b.TRIPCD_ID, b.MARFIS_CONF_NUMBER, b.LANDING_DATE, Min(c.SETDATE) as MINDATE, Max(c.SETDATE) as MAXDATE, d.GEARCD_ID, e.SETCD_ID, Min(c.LATITUDE), Min(c.LONGITUDE)

from isdb.isvessels a, isdb.istrips b, isdb.issetprofile c, isdb.isgears d, isdb.isfishsets e

where TO_CHAR(c.SETDATE,'yyyy')=2021 
  and e.NAFAREA_ID like '5Z%'
  and b.VESS_ID = a.VESS_ID
  and c.FISHSET_ID = e.FISHSET_ID
  and b.TRIP_ID = e.TRIP_ID
  and e.GEAR_ID = d.GEAR_ID

group by a.CFV, a.VESSEL_NAME, b.TRIP, b.TRIPCD_ID, b.MARFIS_CONF_NUMBER, b.LANDING_DATE, d.GEARCD_ID, e.SETCD_ID

having d.GEARCD_ID not in (71, 62)

order by b.TRIP")

isdb$LANDING_DATE <- as.Date(isdb$LANDING_DATE)
isdb$LANDED_DATE <- isdb$LANDING_DATE
isdb <- rename(isdb, VR_NUMBER_FISHING = CFV)

isdb$VR_NUMBER_FISHING <- as.integer(isdb$VR_NUMBER_FISHING)

#Step 3. QAQC MARFIS using ISDB data 

#If there are any duplicate VRN and landed dates in ISDB, remove them for this part of the QAQC.
isdb2 <- distinct(isdb, VR_NUMBER_FISHING, LANDED_DATE, .keep_all = TRUE)

##Join MARFIS and ISDB based on VRN and landed date
join <- left_join(marfis, isdb2, by = c("VR_NUMBER_FISHING", "LANDED_DATE"))
join_match <- join %>% filter(!is.na(TRIPCD_ID)) #Records that match 
join_nomatch <- join %>% filter(is.na(TRIPCD_ID)) #Records that don't match

#For MARFIS data that did match ISDB based on VRN and landed date, match trip numbers in joined MARFIS and ISDB records, and replace missing/mistake MARFIS trip numbers with ISDB trip numbers
join_good1 <- join_match %>% filter(join_match$TRIP.x == join_match$TRIP.y)
join_nomatch2 <- join_match %>% filter(join_match$TRIP.x != join_match$TRIP.y)
marfis_error1 <- join_nomatch2
marfis_error1$COMMENT <- ifelse(marfis_error1$TRIP.x == "", "MARFIS trip missing", "MARFIS trip incorrect")
join_nomatch2$TRIP.x <- ifelse(join_nomatch2$TRIP.x == "", join_nomatch2$TRIP.y, join_nomatch2$TRIP.x)
join_nomatch2$TRIP.x <- join_nomatch2$TRIP.y #Use this to replace MARFIS values with those from ISDB, if trusted. Do not use otherwise.
join_good2 <- join_nomatch2

#For MARFIS data that did not match ISDB based on VRN and landed date, match MARFIS data to ISDB data +/- 2 days. Do not replace MARFIS dates with ISDB dates.
marfis2 <- join_nomatch %>% select(c(1:21)) #select only original columns from MARFIS

isdb$start <- isdb$LANDING_DATE-2
isdb$end <- isdb$LANDING_DATE+2

join2 <- fuzzy_left_join(
  marfis2, isdb,
  by = c(
    "VR_NUMBER_FISHING" = "VR_NUMBER_FISHING",
    "LANDED_DATE" = "start",
    "LANDED_DATE" = "end"
  ),
  match_fun = list(`==`, `>=`, `<=`)
) 

join2_match <- join2 %>% filter(!is.na(TRIPCD_ID)) #Records that match
join2_nomatch <- join2 %>% filter(is.na(TRIPCD_ID)) #Records that do not match

#For matched dataframe, match trip numbers in joined MARFIS and ISDB records, and replace missing/mistake MARFIS trip numbers with ISDB trip numbers
join_good3 <- join2_match %>% filter(join2_match$TRIP.x == join2_match$TRIP)
join2_nomatch2 <- join2_match %>% filter(join2_match$TRIP.x != join2_match$TRIP)
marfis_error2 <- join2_nomatch2
marfis_error2$COMMENT <- ifelse(marfis_error2$TRIP.x == "", "MARFIS trip missing", "MARFIS trip incorrect")
join2_nomatch2$TRIP.x <- ifelse(join2_nomatch2$TRIP.x == "", join2_nomatch2$TRIP, join2_nomatch2$TRIP.x)
join2_nomatch2$TRIP.x <- join2_nomatch2$TRIP #Use this to replace MARFIS values with those from ISDB, if trusted. Do not use otherwise.
join_good4 <- join2_nomatch2

#MARFIS records with no match to ISDB based on VRN and date, fall into two categories: observer records in MARFIS that have not yet been entered into the ISDB (will have a TRIP#) and non-observed trips (no TRIP#)

join_good5 <- join2_nomatch %>% filter(TRIP.x == "") #These are the unobserved trips
join_good6 <- join2_nomatch %>% filter(TRIP.x != "") #These are the observed trips not entered in the ISDB (aka ISDB errors)

#Select MARFIS columns and bind the QAQC'd parts of the MARFIS dataframe back together

good1 <- join_good1 %>% select(c(1:21))
good2 <- join_good2 %>% select(c(1:21))
good3 <- join_good3 %>% select(c(1:21))
good4 <- join_good4 %>% select(c(1:21))
good5 <- join_good5 %>% select(c(1:21))
good6 <- join_good6 %>% select(c(1:21))

marfis_qaqc <- rbind(good1, good2,
                  setNames(good3, names(good2)),
                  setNames(good4, names(good2)),
                  setNames(good5, names(good2)),
                  setNames(good6, names(good2)))

#MARFIS and ISDB errors to report

#MARFIS errors
marfis_error1 <- marfis_error1 %>% rename(ISDBTRIP = TRIP.y) %>% select(c(1:21, 33), ISDBTRIP)
marfis_error2 <- marfis_error2 %>% rename(ISDBTRIP = TRIP) %>% select(c(1:21, 37), ISDBTRIP)

names(marfis_error2) <- names(marfis_error1)

marfis_errors <- rbind(marfis_error1, marfis_error2) #Observed trips entered in MARFIS that do not match the ISDB.

marfis_errors_missing <- marfis_errors %>% select(VR_NUMBER_FISHING, TRIP_ID, LANDED_DATE, TRIP.x, ISDBTRIP) %>% filter(TRIP.x == "")
marfis_errors_missing <- distinct(marfis_errors_missing, VR_NUMBER_FISHING, TRIP_ID, LANDED_DATE, TRIP.x, ISDBTRIP, .keep_all = TRUE)
write.csv(marfis_errors_missing, "marfis_missing.csv") #File sent to CDD to correct missing MARFIS trip numbers

marfis_errors_incorrect <- marfis_errors %>% select(VR_NUMBER_FISHING, LANDED_DATE, TRIP.x) %>% filter(TRIP.x != "")
isdb_check <- isdb %>% filter(TRIP %in% marfis_errors_incorrect$ISDBTRIP) 
write.csv(isdb_check, "isdb_check.csv") #File sent to observer program to check if VRN, date landed, and trip number are correct.

##ISDB errors
isdb_errors <- good6 %>% mutate(COMMENT = "trip entered in MARFIS but not ISDB") #Observed trips entered in MARFIS but not entered in the ISDB

write.csv(isdb_errors, "isdb_missing.csv") #File sent to observer program as outstanding 
