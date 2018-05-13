# Race_4.26.18

library(dplyr)
library(tidyverse)
Race_by_BG <- read_csv("Data/Race_by_BG_4.26.18.csv")

# bound pop weight to 1 (area weight naturally bounded)
Race_by_BG$Pop_weight <- ifelse(Race_by_BG$Pop_weight > 1, 1, Race_by_BG$Pop_weight)

# Area weighted calculations ----------------------------------------------

# area weighted population estimates
Race_by_BG$Pop_e_areaweight <- Race_by_BG$Area_weight*Race_by_BG$Total_population_e

# Summarize area weight estimated population by DACP and add it into data table
Area_weighted_pop <- aggregate(Race_by_BG$Pop_e_areaweight, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Area_weighted_pop)[2] <- "Area_weighted_pop_e"

# area weighted number of white people estimates
Race_by_BG$NotHispanic_White_pop_e_areaweight <- Race_by_BG$Area_weight*Race_by_BG$NotHispanic_White_pop_e

#summarise area weighted number of white people 
Area_weighted_white <- aggregate(Race_by_BG$NotHispanic_White_pop_e_areaweight, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Area_weighted_white)[2] <- "Area_weighted_white_e"

# calculate percent white for area weighted estimates
Area_weighted_e <- left_join(Area_weighted_pop, Area_weighted_white, by = "DACP")
Area_weighted_e$Area_weighted_percent_white_e <- Area_weighted_e$Area_weighted_white_e/Area_weighted_e$Area_weighted_pop_e


# Population weighted estimates -------------------------------------------

# population weighted number of white people estimates
Race_by_BG$NotHispanic_White_pop_e_popweight <- Race_by_BG$Pop_weight*Race_by_BG$NotHispanic_White_pop_e

#summarise area weighted number of white people 
Pop_weighted_white <- aggregate(Race_by_BG$NotHispanic_White_pop_e_popweight, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=sum)
colnames(Pop_weighted_white)[2] <- "Pop_weighted_white_e"

# need to summarise here by DACP first then calculate percent
DACP_pop <- aggregate(Race_by_BG$DACP_population, by=list(DACP=Race_by_BG$`Place ID Number`), FUN=mean)
colnames(DACP_pop)[2] <- "DACP_population"
Pop_weighted_e <- left_join(Pop_weighted_white, DACP_pop, by = "DACP")

# calculate percent white for pop weighted estimates
Pop_weighted_e$Pop_weighted_percent_white_e <- Pop_weighted_e$Pop_weighted_white_e/Pop_weighted_e$DACP_population


