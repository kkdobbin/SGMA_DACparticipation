#Set up
library(dplyr)
library(tidyverse)


# Creating master data ------------------------------------------------

# Load data
DACP <- read_csv("Data/DACP_GSApostings_3.16.18.csv")
IRWM <- read_csv("Outputs/DACP_IRWM.csv")
DEM <- read_csv("Outputs/DEM_by_DACO_viaBG_all.csv")

# Clarify some variables before joining
colnames(DACP)[17] <- "GSA_Int_a_"
colnames(DACP)[18] <- "GSA_Int_Percent"

# Create master dataframe
Data <- left_join(DACP, DEM, by = c("DACP_IDNumber" = "DACP_Place_IDNumber"))
Data <- left_join(Data, IRWM, by = c("DACP_IDNumber" = "DACPlace_ID_Number"))

# Clean up data -----------------------------------------------------------

# Deal with surrogates
# Consider all Surrogates as Ns
table(Data$GSAMember) ## First count to be sure
Data$GSAMember[Data$GSAMember == "S"] <- "N"
table(Data$GSAMember) # It works! 
table(Data$DecisionMaker)
Data$DecisionMaker[Data$DecisionMaker == "S"] <- "N"
table(Data$DecisionMaker)
table(Data$GSA_eligible_entity)
Data$GSA_eligible_entity[Data$GSA_eligible_entity == "S"] <- "N"
table(Data$GSA_eligible_entity)

# Make variable types correct
Data$GSAType <- as.factor(Data$GSAType)
Data$GSAMember <- as.factor(Data$GSAMember)
Data$DecisionMaker <- as.factor(Data$DecisionMaker)
Data$Listed_IP <- as.factor(Data$Listed_IP)
Data$Incorporated <- as.factor(Data$Incorporated)
Data$DAC_Population <- as.numeric(Data$DAC_Population)
Data$GSA_eligible_entity <- as.factor(Data$GSA_eligible_entity)
Data$Any_Pilot <- as.factor(Data$Any_Pilot)
Data$AdvisoryCommittee <- as.factor(Data$AdvisoryCommittee)

# Make MOU and MOA the same
Data$GSAType[Data$GSAType == "MOA"] <- "MOU"
table(Data$GSAType)
Data$GSAType <- factor(Data$GSAType)
table(Data$GSAType)


# Create Dependent Variables ----------------------------------------------

# Add participation dependent variable (DV) DV = master 0 to 5 composite
Data$DV <- NA
Data$DV <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "N", 0, NA)
Data$DV <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "N", 1, Data$DV)
Data$DV <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "Y" & Data$Voting_type == "S", 2, Data$DV)
Data$DV <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "Y" & Data$Voting_type == "V", 3, Data$DV)
Data$DV <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "Y" & Data$Voting_type == "E", 3, Data$DV)
Data$DV <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y" & Data$Voting_type == "S", 4, Data$DV)
Data$DV <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y" & Data$Voting_type == "V", 5, Data$DV)
Data$DV <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y" & Data$Voting_type == "E", 5, Data$DV)

# Make new DV2 with less variation. DV2 = 0 to 2 variable just based on decision making and voting type
Data$DV2 <- ifelse(Data$DecisionMaker == "N", 0, NA)
Data$DV2 <- ifelse(Data$DecisionMaker == "Y" & Data$Voting_type == "S", 1, Data$DV2)
Data$DV2 <- ifelse(Data$DecisionMaker == "Y" & Data$Voting_type == "V", 2, Data$DV2)
Data$DV2 <- ifelse(Data$DecisionMaker == "Y" & Data$Voting_type == "E", 2, Data$DV2)
table(Data$DV2)

# Make new DV3 focused on just membership and decision making
Data$DV3 <- NA
Data$DV3 <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "N", 0, NA)
Data$DV3 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "N", 1, Data$DV3)
Data$DV3 <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "Y", 2, Data$DV3)
Data$DV3 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y", 3, Data$DV3)

# Make yet another DV4 like DV but where E and V aren't treated the same. Zero to six
Data$DV4 <- NA
Data$DV4 <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "N", 0, NA)
Data$DV4 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "N", 1, Data$DV4)
Data$DV4 <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "Y" & Data$Voting_type == "S", 2, Data$DV4)
Data$DV4 <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "Y" & Data$Voting_type == "V", 3, Data$DV4)
Data$DV4 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y" & Data$Voting_type == "S", 4, Data$DV4)
Data$DV4 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y" & Data$Voting_type == "V", 5, Data$DV4)
Data$DV4 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y" & Data$Voting_type == "E", 6, Data$DV4)

# Compare DVs
table(Data$DV)
table(Data$DV2)
table(Data$DV3)
table(Data$DV4)

# OLS ---------------------------------------------------------------------

# OLS using full dataset --------------------------------------------------

# DV
TEST <- lm(Data$DV ~ Data$GSAType + Data$GSA_Int_Percent + Data$DACP_MHI + Data$DAC_Population + Data$AREAofDACP_a_ + Data$Incorporated + Data$Percent_white_not_hispanic + Data$Any_Pilot)
summary(TEST)

# DV2
TEST2 <- lm(Data$DV2 ~ Data$GSAType + Data$GSA_Int_Percent + Data$DACP_MHI + Data$DAC_Population + Data$AREAofDACP_a_ + Data$Incorporated + Data$Percent_white_not_hispanic + Data$Any_Pilot)
summary(TEST2) # worse

# DV3
TEST3 <- lm(Data$DV3 ~ Data$GSAType + Data$GSA_Int_Percent + Data$DACP_MHI + Data$DAC_Population + Data$AREAofDACP_a_ + Data$Incorporated + Data$Percent_white_not_hispanic + Data$Any_Pilot)
summary(TEST3) # similar to 1

# DV4
TEST4 <- lm(Data$DV4 ~ Data$GSAType + Data$GSA_Int_Percent + Data$DACP_MHI + Data$DAC_Population + Data$AREAofDACP_a_ + Data$Incorporated + Data$Percent_white_not_hispanic + Data$Any_Pilot)
summary(TEST4)


# OLS using only 100% intersections ---------------------------------------

# Create filtered Data2
Data2 <- Data %>% filter(Data$GSA_Int_Percent == 1)

#Try DV
TEST5 <- lm(Data2$DV ~ Data2$GSAType + Data2$DACP_MHI + Data2$DAC_Population + Data2$AREAofDACP_a_ + Data2$Incorporated + Data2$Percent_white_not_hispanic + Data2$Any_Pilot)
summary(TEST5)

# Try DV3
TEST6 <- lm(Data2$DV3 ~ Data2$GSAType + Data2$DACP_MHI + Data2$DAC_Population + Data2$AREAofDACP_a_ + Data2$Incorporated + Data2$Percent_white_not_hispanic + Data2$Any_Pilot)
summary(TEST6)


# OLS using intersections greater than 50% --------------------------------

# Create filtered Data3
Data3 <- Data %>% filter(Data$GSA_Int_Percent >= 0.5)

#Try DV
TEST7 <- lm(Data3$DV ~ Data3$GSAType + Data3$DACP_MHI + Data3$DAC_Population + Data3$AREAofDACP_a_ + Data3$Incorporated + Data3$Percent_white_not_hispanic + Data3$Any_Pilot)
summary(TEST7)

# Try DV3
TEST7 <- lm(Data3$DV3 ~ Data3$GSAType + Data3$DACP_MHI + Data3$DAC_Population + Data3$AREAofDACP_a_ + Data3$Incorporated + Data3$Percent_white_not_hispanic + Data3$Any_Pilot)
summary(TEST7)


# OLS with just JPA and MOU intersections ---------------------------------

Data4 <- Data %>% filter(Data$GSAType == "JPA" | Data$GSAType == "MOU" | Data$GSAType == "AD")
TEST8 <- lm(Data4$DV ~ Data4$GSAType + Data4$DACP_MHI + Data4$DAC_Population + Data4$AREAofDACP_a_ + Data4$Incorporated + Data4$Percent_white_not_hispanic + Data4$Any_Pilot)
summary(TEST8) # worse

# just final check, what if ADs and JPAs were the same
Data5 <- Data4
Data5$GSAType[Data5$GSAType == "AD"] <- "JPA"
table(Data5$GSAType)
factor(Data5$GSAType)
TEST9 <- lm(Data5$DV ~ Data5$GSAType + Data5$DACP_MHI + Data5$DAC_Population + Data5$AREAofDACP_a_ + Data5$Incorporated + Data5$Percent_white_not_hispanic + Data5$Any_Pilot)
summary(TEST9) # worse still


# OLS with just those with GSA eligible entities --------------------------

Data6 <- Data %>% filter(Data$GSA_eligible_entity == "Y")
TEST10 <- lm(Data6$DV ~ Data6$GSAType + Data6$GSA_Int_Percent + Data6$DACP_MHI + Data6$DAC_Population + Data6$AREAofDACP_a_ + Data6$Incorporated + Data6$Percent_white_not_hispanic + Data6$Any_Pilot)
summary(TEST10)

