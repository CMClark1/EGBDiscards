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

#If there are any duplicate VRN and landed dates in ISDB, remove them.
isdb <- distinct(isdb, VR_NUMBER_FISHING, LANDED_DATE, .keep_all = TRUE)

##Join MARFIS and ISDB based on VRN and landed date
join <- left_join(marfis, isdb, by = c("VR_NUMBER_FISHING", "LANDED_DATE"))
join_match <- join %>% filter(!is.na(TRIPCD_ID)) #Records that match 
join_nomatch <- join %>% filter(is.na(TRIPCD_ID)) #Records that don't match

#For MARFIS data that did match ISDB based on VRN and landed date, match trip numbers in joined MARFIS and ISDB records, and replace missing/mistake MARFIS trip numbers with ISDB trip numbers
join_good1 <- join_match %>% filter(join_match$TRIP.x == join_match$TRIP.y)
join_nomatch2 <- join_match %>% filter(join_match$TRIP.x != join_match$TRIP.y)
join_nomatch2$TRIP.x <- join_nomatch2$TRIP.y
join_good2 <- join_nomatch2

#For MARFIS data that did not match ISDB based on VRN and landed date, match MARFIS data to ISDB data +/- 2 days and replace missing/mistaken MARFIS dates with ISDB dates
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
join2_match$LANDED_DATE.x <- join2_match$LANDED_DATE.y

#For matched dataframe with corrected dates, match trip numbers in joined MARFIS and ISDB records, and replace msising/mistake MARFIS trip numbers with ISDB trip numbers
join_good3 <- join2_match %>% filter(join2_match$TRIP.x == join2_match$TRIP)
join2_nomatch2 <- join2_match %>% filter(join2_match$TRIP.x != join2_match$TRIP)
join2_nomatch2$TRIP.x <- join2_nomatch2$TRIP
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
