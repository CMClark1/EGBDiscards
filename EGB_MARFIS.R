####################
# EGB Discard Data Pull - MARFIS
# Caira Clark
# 26 January 2021
####################

library(ROracle)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

marfisxtab <- dbGetQuery(channel, "
                         
select a.VR_NUMBER_FISHING, b.VESSEL_NAME, b.GROSS_TONNAGE, b.LOA, a.TRIP_ID, a.LANDED_DATE, a.LOG_EFRT_STD_INFO_ID, a.DATE_FISHED, a.GEAR_CODE, c.AREA, a.LATITUDE, a.LONGITUDE, a.LICENCE_ID, d.SFLT_DESC_ID, d.DESC_ENG, h.DATA_VALUE, 
sum(decode(a.SPECIES_CODE, 100, a.RND_WEIGHT_KGS, 0)) cod,
sum(decode(a.SPECIES_CODE, 110, a.RND_WEIGHT_KGS, 0)) haddock,
sum(decode(a.SPECIES_CODE, 170, a.RND_WEIGHT_KGS, 0)) pollock
                         
from marfissci.pro_spc_info a, marfissci.vessels b, marfissci.areas c, marfissci.sflt_descs d, marfissci.sflt_quotas e, marfissci.fleet_quotas f, marfissci.lic_quotas g, marfissci.mon_doc_entrd_dets h
                         
where TO_CHAR(a.LANDED_DATE,'yyyy')=2016
      and a.NAFO_UNIT_AREA_ID in (198,199,200,201,202,203,204,205)
      and a.VR_NUMBER_FISHING = b.VR_NUMBER
      and a.NAFO_UNIT_AREA_ID = c.AREA_ID
      and d.SFLT_DESC_ID = e.SFLT_DESC_ID
      and e.FLEET_QUOTA_ID = f.FLEET_QUOTA_ID
      and e.SFLT_QUOTA_ID = g.SFLT_QUOTA_ID
      and a.LIC_QUOTA_ID = g.LIC_QUOTA_ID
      and a.MON_DOC_ID = h.MON_DOC_ID
      and h.column_defn_id = 741
      
group by a.VR_NUMBER_FISHING, b.VESSEL_NAME, b.GROSS_TONNAGE, b.LOA, a.TRIP_ID, a.LANDED_DATE, a.LOG_EFRT_STD_INFO_ID, a.DATE_FISHED, a.GEAR_CODE, c.AREA, a.LATITUDE, a.LONGITUDE, a.LICENCE_ID, d.SFLT_DESC_ID, d.DESC_ENG, h.DATA_VALUE")

#Add year column
marfisxtab$YEAR <- as.numeric(substr(marfisxtab$LANDED_DATE, start = 1, stop = 4))

#Add month column
marfisxtab$MONTH <- as.numeric(substr(marfisxtab$LANDED_DATE, start = 6, stop = 7))

#Add quarter column
marfisxtab <- marfisxtab %>%  mutate(
  Q = case_when(
    MONTH %in% c(1:3) ~ 1
    , MONTH %in% c(4:6) ~ 2
    , MONTH %in% c(7:9) ~ 3
    , MONTH %in% c(10:12) ~ 4 ) )

#Add tonnage class column
marfisxtab <- marfisxtab %>%  mutate(
  TC = case_when(
    GROSS_TONNAGE <25 ~ 1
    , GROSS_TONNAGE >=25 & GROSS_TONNAGE <50 ~ 2
    , GROSS_TONNAGE >=50 & GROSS_TONNAGE <150 ~ 3
    , GROSS_TONNAGE >=150 & GROSS_TONNAGE <500 ~ 4
    , GROSS_TONNAGE >=500 & GROSS_TONNAGE <1000 ~ 5
    , GROSS_TONNAGE >=1000 & GROSS_TONNAGE <2000 ~ 6
    , GROSS_TONNAGE >= 2000 ~ 7) )

#Add length class column
marfisxtab <- marfisxtab %>%  mutate(
  LC = case_when(
    LOA <35 ~ 1
    , LOA >=35 & LOA <45 ~ 2
    , LOA >=45 & LOA <65 ~ 3
    , LOA >=65 & LOA <100 ~ 4
    , LOA >=100 & LOA <125 ~ 5
    , LOA >=125 ~ 6) )

#Convert latitude and longitude
marfisxtab$LATITUDE[!is.na(marfisxtab$LATITUDE)] =
  as.numeric(substr(marfisxtab$LATITUDE[!is.na(marfisxtab$LATITUDE)], 1, 2)) +
  as.numeric(substr(marfisxtab$LATITUDE[!is.na(marfisxtab$LATITUDE)], 3, 4)) / 60 +
  as.numeric(substr(marfisxtab$LATITUDE[!is.na(marfisxtab$LATITUDE)], 5, 6)) / 6000

marfisxtab$LONGITUDE[!is.na(marfisxtab$LONGITUDE)] = -1 *
  (as.numeric(substr(marfisxtab$LONGITUDE[!is.na(marfisxtab$LONGITUDE)], 1, 2)) +
     as.numeric(substr(marfisxtab$LONGITUDE[!is.na(marfisxtab$LONGITUDE)], 3, 4)) / 60 +
     as.numeric(substr(marfisxtab$LONGITUDE[!is.na(marfisxtab$LONGITUDE)], 5, 6)) / 6000)
