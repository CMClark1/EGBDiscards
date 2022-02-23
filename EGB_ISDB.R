####################
# EGB Discard Data Pull - ISDB
# Caira Clark
# 9 Feb 2022
####################

library(ROracle)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

isdbtrips <- dbGetQuery(channel, "

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

isdbtrips <- rename(isdbtrips, LAT2 = 11)
isdbtrips <- rename(isdbtrips, LON2 = 12)
isdbtrips$LON2 <- -isdbtrips$LON2
