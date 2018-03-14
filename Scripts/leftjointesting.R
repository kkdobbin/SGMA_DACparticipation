# Testing join between my data and census place data. How many of them have demographics this way?
library(dplyr)
SGMAData <- read.csv("Data/GSApostingsdata3.12.18.csv")
CensusData <- read.csv("Data/TESTcensusplace.csv")

test <- left_join(SGMAData, CensusData, c("DACP_IDNumber" = "Id2"), copy = F)
