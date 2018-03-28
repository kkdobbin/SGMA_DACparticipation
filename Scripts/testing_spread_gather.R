# playign with gather and spread
library(tidyverse)
library(dplyr)
Data <- read_csv("Data/DACP_GSApostings_3.26.18.csv")

Data %>% 
  gather(key = genus, value = mean_weight, Baiomys:Spermophilus)