# comparing policy link and DACP data
# Load libraries 
library(tidyverse)
library(dplyr)

# Load data
Data <- read_csv("Data/DACP_GSApostings_3.27.18.csv")
Regions <- read_csv("Data/hyrdoregions_DACP_int_3.27.18_dupsdeleted.csv")

Regions8VC <- Regions %>% filter(Regions$HR == 6 | Regions$HR == 7)