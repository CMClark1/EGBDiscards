####################
# EGB Discard Data Pull - MARFIS
# Caira Clark
# 26 January 2021
####################

library(ROracle)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

isdbtrips <- dbGetQuery(channel, "

select a.CFV, a.VESSEL_NAME, b.TRIP, b.TRIPCD_ID, b.MARFIS_CONF_NUMBER, b.LANDING_DATE, Min(c.SETDATE) as MINDATE, Max(c.SETDATE) as MAXDATE, d.GEARCD_ID, e.SETCD_ID

from isdb.isvessels a, isdb.istrips b, isdb.issetprofile c, isdb.isgears d, isdb.isfishsets e

where TO_CHAR(c.SETDATE,'yyyy')=2016 
  and e.NAFAREA_ID like '5Z%'
  
group by a.CFV, a.VESSEL_NAME, b.TRIP, b.TRIPCD_ID, b.MARFIS_CONF_NUMBER, b.LANDING_DATE, d.GEARCD_ID, e.SETCD_ID

having d.GEARCD_ID not in (71, 62)

order by b.TRIP")