# Load libraries and data
library(tidyverse)
library(dplyr)
# set working directory by hand because I still don't get how to do that another way
SGMAData <- read.csv("Data/GSApostingsdata3.12.18.csv") # update data as needed

# General -----------------------------------------------------------------

# Number of DACPs interesected by at least 10% by GSAs
uniqueDACP <- table(SGMAData$DACP_IDNumber)
length(uniqueDACP)

# Number intersected by more than one

# Number of DACP-GSA intersections
str(SGMAData)

# Response Variable descriptive stats -------------------------------------

# How many DACPs are members of their respective GSAs? (S = surrogate, a larger entity is representing that community (for example cities representing uninciporated communities on their fringes) can be coded as Y or N depending on the question to be asked)
table(SGMAData$GSAMember)

# How many DACPs are decision makers in their respective GSAs? (again S = surrogate, they have a surrogate)
table(SGMAData$DecisionMaker)

# Of those DACPs that are decision makers in their GSA, what type of decision making authority do they have? 
votingDACPs <- SGMAData %>% filter(DecisionMaker == "Y") 
table(votingDACPs$Voting_type) 


# Predictor variable descriptive stats ------------------------------------


# Incorporation -----------------------------------------------------------

# How many of the DACPs that are members are not incorporated cities?
unincorporatedDACPs <- SGMAData %>% filter(Incorporated == "N")
table(unincorporatedDACPs$GSAMember)

# How many of the DACPs that are decision makers are not incorporated cities?
table(unincorporatedDACPs$DecisionMaker)

# for unincporated DACPs what type of decision makers are they?
table(unincorporatedDACPs$Voting_type)

# GSA type ------------------------------------

# GSA Type (I predict that single agency GSAs are much less likely to include DACPs)
table(SGMAData$GSAType)

# How many of the DACPs that are members are in each type of GSA?
SingleGSAs <- SGMAData %>% filter(GSAType == "Single")
table(SingleGSAs$GSAMember)
JPAGSAa <- SGMAData %>% filter(GSAType == "JPA")
table(JPAGSAa$GSAMember)
ADGSAs <- SGMAData %>% filter(GSAType == "AD")
table(ADGSAs$GSAMember)
MOUMOAGSAs <- SGMAData %>% filter(GSAType == "MOU" | GSAType == "MOA")
table(MOUMOAGSAs$GSAMember)

#Another way I could approach this sort of thing for the future
SGMAData %>% filter(SGMAData$GSAMember == "Y") %>% group_by(GSAType) %>% summarise(n())

# How many of the unnincorporated DACPs that are members are in each type of GSA?
SingleGSAsun <- unincorporatedDACPs %>% filter(GSAType == "Single")
table(SingleGSAs$GSAMember)
JPAGSAaun <- unincorporatedDACPs %>% filter(GSAType == "JPA")
table(JPAGSAa$GSAMember)
ADGSAsun <- unincorporatedDACPs %>% filter(GSAType == "AD")
table(ADGSAs$GSAMember)
MOUMOAGSAsun <- unincorporatedDACPs %>% filter(GSAType == "MOU" | GSAType == "MOA")
table(MOUMOAGSAs$GSAMember)

# How many DACPs are decision makers by type of GSA?
table(MOUMOAGSAs$DecisionMaker)
table(ADGSAs$DecisionMaker)
table(JPAGSAa$DecisionMaker)
table(SingleGSAs$DecisionMaker)
# hmm its odd the single GSA numbers are different here from member to DM
SGMAData %>% filter(SGMAData$GSAType == "Single") %>% filter(DecisionMaker == "Y")  %>% 
  filter(GSAMember == "N") # Its tehama the weird single GSA with a delegated GW commission with voting members that are other agencies 

# How many of the unincoporated DACPs that are decision makers are there per GSA type?
table(MOUMOAGSAsun$DecisionMaker)
table(ADGSAsun$DecisionMaker)
table(JPAGSAaun$DecisionMaker)
table(SingleGSAsun$DecisionMaker)


# Percent Intersection ----------------------------------------------------

# Percent intersection (I predict that the more a DACP is intersected by a GSA the more likely they would be to be involved)
summary(SGMAData$Percent_Intersection_a_) # The min is .10 which makes sense because I removed everything below this. THe median is 1 meaning that most DACPs are fully covered by just one GSA
sd(SGMAData$Percent_Intersection_a_)


# Median Household Income -------------------------------------------------

# MHI of the DACP (I predict the poorer the DACP the less likely they are to be involved)
summary(SGMAData$DACP_MHI)# The max is 48,784 which makes sense because DACPs are defined as having 80% of the state's MHI or less which I think is almost exactly that. The min is 0 which reminds me that there are a few DACPs that don't have income data. I should probably change those to NAs. 
DAC_MHI_NA <- as.data.frame(SGMAData$DACP_MHI)
DAC_MHI_NA[DAC_MHI_NA == 0] <- NA
summary(DAC_MHI_NA) # brought up the mean a little bit to $33,272


# Population of DACP ------------------------------------------------------

# Population of the DACP (I predict the larger DACPs will be more likely to be involved)
summary(SGMAData$DACP_Population)
sd(SGMAData$DACP_Population)


# Area of DACP ------------------------------------------------------------

# Size (area) of DACP (I predict the larger a DACP is the most likely it will be to be involved)
summary(SGMAData$AREAofDACP_a_)
sd(SGMAData$AREAofDACP_a_)
