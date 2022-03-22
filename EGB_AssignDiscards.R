####################
# Assign Discards to Vessels
# Caira Clark
# 22 March 2022
####################

head(aggroup) #From EGB_CleanData.R
head(data) #from EGB_BSREsultsRatio.R

disc <- aggroup %>% filter(GROUP %in% data$zones_q) %>% 
  left_join(select(data, zones_q, DISCARDS, SIG), by = c("GROUP" = "zones_q")) %>%
  group_by(VESSEL_NAME.x, LICENCE_ID, GROUP, DISCARDS) %>% 
  filter(TRIP == "" & SIG == 1) %>% 
  summarise(CODkg = sum(COD), HADDOCKkg = sum(HAD)) %>%
  group_by(GROUP) %>%
  mutate(PORTION = HADDOCKkg/sum(HADDOCKkg)) %>%
  mutate(DISCARDS2 = PORTION*DISCARDS)

disc %>% group_by(GROUP) %>% summarise(sum(DISCARDS2)) #check that the discards add up for each quarter

ready <- disc %>% rename(VESSEL = VESSEL_NAME.x, DISCARDSmt = DISCARDS2) %>% select(!DISCARDS) %>% group_split(GROUP) #discards assigned to vessels by fleet
