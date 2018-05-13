#SHE data

# Load libraries 
library(tidyverse)
library(dplyr)
library(rms)
library(broom)
library(tidyverse)
library(car)

# Load data
Data <- read_csv("Data/DACP_GSApostings_3.27.18.csv")
Regions <- read_csv("Data/hyrdoregions_DACP_int_3.27.18_dupsdeleted.csv")

# add in regions
RegionsCut <- Regions %>% select(HR, DACP_Place_IDNumber, FIRST_HRNA)
Data <- left_join(Data, RegionsCut, by = c("DACP_IDNumber" = "DACP_Place_IDNumber"))

#write CSV
write_csv(Data, path = "Outputs/SHE.csv")
