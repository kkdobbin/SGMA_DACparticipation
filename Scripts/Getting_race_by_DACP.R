# Trying to join demographic data to intersected data to get demographic data joined to DACPs through BGs

library(dplyr)
library(tidyverse)
RACE <- read_csv("Data/BG_HispanicLatino_editedandcut_3.16.18.csv") # All BGs in the state
BG <- read_csv("Data/BG_DACP_Intersection_3.14.18cut_3.16.18.csv") # only BGs that intersect with DACPs
BG <- BG[rowSums(is.na(BG)) != ncol(BG),] # Get rid of all those stupid extra NA rows

# Try joining
Race_by_BG <- left_join(BG, RACE, by = c("BG_GEOID_Data" = "GEOID")) # OMG it worked. This is table with demographics for all BGs that intersect DACPs

# Try summarizing to demographic data by DACP
DEM_by_BG <- Race_by_BG %>% filter(DAC_Population <= 10000) %>% group_by(DACP_Place_IDNumber) %>% summarise(sum(Total_population_e), sum(NotHispanic_White_pop_e), sum(Hispanic_or_Latino_population_e), sum(Not_Hispanic_or_Latino_pop_e), median(DAC_Population)) # This is demographics summed up to the DACP scale (Demographics and population for all DACPs with the note that this isn't perfect because includes all people within those intersecting BGs not just the people within the DAC boundaries)

# Add column for percent non-hispanic white
DEM_by_BG$Percent_white_not_hispanic <- DEM_by_BG$`sum(NotHispanic_White_pop_e)` / DEM_by_BG$`sum(Total_population_e)` * 100

# Add column for percent latino
DEM_by_BG$Percent_latino <- DEM_by_BG$`sum(Hispanic_or_Latino_population_e)` / DEM_by_BG$`sum(Total_population_e)` * 100

# Rename columns to avoid messy back ticks
colnames(DEM_by_BG)[2] <- "Total_populatio_e"
colnames(DEM_by_BG)[3] <- "NotHispanic_White_pop_e"
colnames(DEM_by_BG)[4] <- "Hispanic_or_Latino_population_e"
colnames(DEM_by_BG)[5] <- "Not_Hispanic_or_Latino_pop_e"
colnames(DEM_by_BG)[6] <- "DAC_Population"

# Add column for difference in population of the DAC versus what my BG sum up strategy is getting for reference on how good/poor this approach is. 
DEM_by_BG$DAC_Population <- as.integer(DEM_by_BG$DAC_Population)
DEM_by_BG$Pop_difference <- NA
DEM_by_BG$Pop_difference <- DEM_by_BG$Total_populatio_e - DEM_by_BG$DAC_Population

# Only remaining question is if I should go back into this and eliminate some intersections that were really small....

# If I were only do to those that intersect by 5% or more
DEM_by_BG2 <- Race_by_BG %>% filter(DAC_Population <= 10000) %>% filter(Percent_intersection > 0.05) %>% group_by(DACP_Place_IDNumber) %>% summarise(sum(Total_population_e), sum(NotHispanic_White_pop_e), sum(Hispanic_or_Latino_population_e), sum(Not_Hispanic_or_Latino_pop_e), median(DAC_Population))
DEM_by_BG2$Percent_white_not_hispanic <- DEM_by_BG2$`sum(NotHispanic_White_pop_e)` / DEM_by_BG2$`sum(Total_population_e)` * 100
DEM_by_BG2$Percent_latino <- DEM_by_BG2$`sum(Hispanic_or_Latino_population_e)` / DEM_by_BG2$`sum(Total_population_e)` * 100
colnames(DEM_by_BG2)[2] <- "Total_populatio_e"
colnames(DEM_by_BG2)[3] <- "NotHispanic_White_pop_e"
colnames(DEM_by_BG2)[4] <- "Hispanic_or_Latino_population_e"
colnames(DEM_by_BG2)[5] <- "Not_Hispanic_or_Latino_pop_e"
colnames(DEM_by_BG2)[6] <- "DAC_Population"
DEM_by_BG2$DAC_Population <- as.integer(DEM_by_BG2$DAC_Population)
DEM_by_BG2$Pop_difference <- NA
DEM_by_BG2$Pop_difference <- DEM_by_BG2$Total_populatio_e - DEM_by_BG2$DAC_Population

# If I were to do only intersection bigger than 50% 
DEM_by_BG3 <- Race_by_BG %>% filter(DAC_Population <= 10000) %>% filter(Percent_intersection > 0.5) %>% group_by(DACP_Place_IDNumber) %>% summarise(sum(Total_population_e), sum(NotHispanic_White_pop_e), sum(Hispanic_or_Latino_population_e), sum(Not_Hispanic_or_Latino_pop_e), median(DAC_Population))
DEM_by_BG3$Percent_white_not_hispanic <- DEM_by_BG3$`sum(NotHispanic_White_pop_e)` / DEM_by_BG3$`sum(Total_population_e)` * 100
DEM_by_BG3$Percent_latino <- DEM_by_BG3$`sum(Hispanic_or_Latino_population_e)` / DEM_by_BG3$`sum(Total_population_e)` * 100
colnames(DEM_by_BG3)[2] <- "Total_populatio_e"
colnames(DEM_by_BG3)[3] <- "NotHispanic_White_pop_e"
colnames(DEM_by_BG3)[4] <- "Hispanic_or_Latino_population_e"
colnames(DEM_by_BG3)[5] <- "Not_Hispanic_or_Latino_pop_e"
colnames(DEM_by_BG3)[6] <- "DAC_Population"
DEM_by_BG3$DAC_Population <- as.integer(DEM_by_BG3$DAC_Population)
DEM_by_BG3$Pop_difference <- NA
DEM_by_BG3$Pop_difference <- DEM_by_BG3$Total_populatio_e - DEM_by_BG3$DAC_Population

#average differences
mean(DEM_by_BG$Pop_difference)
mean(DEM_by_BG2$Pop_difference)
mean(DEM_by_BG3$Pop_difference)

# Writing CSVs
write_csv(x = DEM_by_BG, path = "Outputs/DEM_by_DACO_viaBG_all.csv")  # This is for all interesections, the first part of this code
write_csv(x = DEM_by_BG3, path = "Outputs/DEM_by_DACO_viaBG_fiftypercent.csv")


