# Getting and IRWM output ready
library(dplyr)
library(tidyverse)
IRWM <- read_csv("Data/DACP_Database_IRWMsheet_3.16.18.csv")
IRWM$Any_Pilot <- ifelse(IRWM$DAC_Pilot == "Y" | IRWM$TLB == "Y", "Y", "N")

# Writing CSVs
write_csv(x = IRWM, path = "Outputs/DACP_IRWM.csv") 
