# Re-doing demographic data to experiment with weight and population weighting

library(dplyr)
library(tidyverse)
RACE <- read_csv("Data/BG_HispanicLatino_editedandcut_3.16.18.csv") # All BGs in the state
BG <- read_csv("Data/BG_DACP_intersection_4.23.18.csv") # only BGs that intersect with DACPs
BG <- BG[rowSums(is.na(BG)) != ncol(BG),] # Get rid of all those stupid extra NA rows

# Try joining
Race_by_BG <- left_join(BG, RACE, by = c("GEOID_Data" = "GEOID")) # OMG it worked. This is table with demographics for all BGs that intersect DACPs

# Export as csv 
write_csv(x = Race_by_BG, path = "Outputs/Race_by_BG.csv") 


# Area weighting ----------------------------------------------------------

# estimate DACP population using area weighting
Race_by_BG$pop_e_areaweight <- Race_by_BG$Area_weight*Race_by_BG$Total_population_e

# Try to summarize area weight estimated population by DACP
Area_weighted_pop <- aggregate(Race_by_BG$pop_e_areaweight, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Area_weighted_pop)[2] <- "area_weighted_pop_e"

# estimate percent white using area weighting
Race_by_BG$percWhite_areaweightestimated <- (Race_by_BG$Area_weight*Race_by_BG$NotHispanic_White_pop_e)/(Race_by_BG$Area_weight*Race_by_BG$Total_population_e)

# estimate number of white people
Race_by_BG$NotHispanic_White_pop_e_areaweight <- (Race_by_BG$Area_weight*Race_by_BG$NotHispanic_White_pop_e)

# summarise aggregate white people by DACP
Area_weighted_pop_White <- aggregate(Race_by_BG$NotHispanic_White_pop_e_areaweight, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Area_weighted_pop_White)[2] <- "area_weighted_pop_White"

# Calculate percent white using area weight estimated number of white people
DACPpop <- select(Race_by_BG, `Place ID Number`, DACP_population, pop_e_areaweight)
Area_weighted_pop_White <- left_join(Area_weighted_pop_White, DACPpop, by = c("DACP" = "Place ID Number"))
Area_weighted_pop_White$percent_white_e_areaweighted <- Area_weighted_pop_White$area_weighted_pop_White/Area_weighted_pop_White$pop_e_areaweight # yields same proportions as other way which probably should have been anticipated

# Summarize aggregate count of white people
Area_weighted_Whitepeople_byDACP <- aggregate(Area_weighted_pop_White, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Area_weighted_Whitepeople_byDACP)[2] <- "area_weighted_pop_White_byDACP"

# Try to summarize percent white using area weight by DACP
Area_weighted_pop_perWhite <- aggregate(Race_by_BG$percWhite_areaweightestimated, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Area_weighted_pop_perWhite)[2] <- "area_weighted_pop_percWhite"

# Estimate percent white using population weighting
Race_by_BG$Pop_weight <- Race_by_BG$DACP_population/Race_by_BG$Total_population_e # this yeilds lots of pop weights that are bigger than 1 because they are covering more than one BG. I probabably just cap them at one right?
Race_by_BG$Pop_weight <- ifelse(Race_by_BG$Pop_weight > 1, 1, Race_by_BG$Pop_weight)

# estimate percent white using population weighting
Race_by_BG$percWhite_popweightedestimate <- (Race_by_BG$Pop_weight*Race_by_BG$NotHispanic_White_pop_e)/(Race_by_BG$DACP_population)

# Try to summarize percent white using population weight by DACP
Area_weighted_pop_perWhite <- aggregate(Race_by_BG$percWhite_areaweightestimated, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Area_weighted_pop_perWhite)[2] <- "area_weighted_pop_percWhite"

# Merging datasets and exporting
Percent_white <- merge(Area_weighted_pop, Area_weighted_pop_perWhite, by="DACP")
write_csv(x = Percent_white, path = "Outputs/Percent_white.csv") 

