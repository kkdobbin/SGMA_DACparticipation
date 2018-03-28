# Load libraries 
library(tidyverse)
library(dplyr)

# Load data
Data <- read_csv("Data/DACP_GSApostings_3.27.18.csv")
Regions <- read_csv("Data/hyrdoregions_DACP_int_3.27.18_dupsdeleted.csv")

# Clean up data -----------------------------------------------------------

# Clarify some variables
colnames(Data)[17] <- "GSA_Int_a_"
colnames(Data)[18] <- "GSA_Int_Percent"

# Make variable types correct
Data$GSAType <- as.factor(Data$GSAType)
Data$GSAMember <- as.factor(Data$GSAMember)
Data$DecisionMaker <- as.factor(Data$DecisionMaker)
Data$Listed_IP <- as.factor(Data$Listed_IP)
Data$Incorporated <- as.factor(Data$Incorporated)
Data$DAC_Population <- as.numeric(Data$DACP_Population)
Data$GSA_eligible_entity <- as.factor(Data$GSA_eligible_entity)
Data$AdvisoryCommittee <- as.factor(Data$AdvisoryCommittee)
Data$DACP_IDNumber <- as.factor(Data$DACP_IDNumber)
Regions$FIRST_HRNA <- as.factor(Regions$FIRST_HRNA)
Regions$HR <- as.factor(Regions$HR)


# Make MOU and MOA the same
Data$GSAType[Data$GSAType == "MOA"] <- "MOU"
table(Data$GSAType)
Data$GSAType <- factor(Data$GSAType)
table(Data$GSAType)

# NEED TO DEAL WITH MHI zeros
Data$DACP_MHI[Data$DACP_MHI == 0] <- NA 

# NEED TO DEAL WITH POP zero
Data$DACP_Population[Data$DACP_Population == 0] <- NA

# Number of GSA eligible entities NA make zeros
Data$number_GSA_eligible_entities[is.na(Data$number_GSA_eligible_entities)] <- 0

#Data has 286 (changed since with data improvements/changes I think) observations total before limiting it any more than just small DACs and intersections ten percent or greater. Plus nine NAs. Any other NAs or observations removed? what is the total universe here? narrowed it down to have just 9 NAs (one observation has NAs in two categories)

# clean up stuff I haven't done yet

# Change NA is Listed_IP variable to yes's
Data$Listed_IP[is.na(Data$Listed_IP)] <- "Y"

#now a lot fewer NAs but three in advisory committee column that should be Ns
# Data$AdvisoryCommittee[is.na(Data$AdvisoryCommittee)] <- "N"
# table(Data$AdvisoryCommittee)

# add in regions
tableunique$Var1 <- as.factor(tableunique$Var1)
colnames(tableunique)[1] <- "DACP_Place_IDNumber"
Regions2 <- Regions %>% select(HR, DACP_Place_IDNumber, DACP_Place_Name)
DACbyRegion <- left_join(tableunique, Regions2, by = "DACP_Place_IDNumber")

# General stats for Unique DACPs -----------------------------------------------------------------

# Figure out how many joined to more than one
tableunique <- as.data.frame(table(Data$DACP_IDNumber)) #243
count(tableunique %>% filter(tableunique$Freq == 1)) #205
count(tableunique %>% filter(tableunique$Freq >1)) # 38 DACPs joined with more than one GSA. 
count(tableunique %>% filter(tableunique$Freq == 2)) #36 joined with two
count(tableunique %>% filter(tableunique$Freq == 3)) #2 joined with three
max(tableunique$Freq)

# DACs impacted by SGMA by region
table(DACbyRegion$HR)

# Figure out how many fully intersect GSAs
fullinteresections <- Data %>% filter(Data$GSA_Int_Percent == 1)
fullunique <- as.data.frame(unique(fullinteresections$DACP_IDNumber)) #141 unique DACPs fully inside just one GSA

# unique DACPs that are members 
#First need to add frequency of GSA joins to data frame
colnames(tableunique)[1] <- "DACP_IDNumber"
colnames(tableunique)[2] <- "Count_GSA_Int"
Data <- left_join(Data, tableunique, by = "DACP_IDNumber")
MorethanOneGSA <- Data %>% select(DACP_IDNumber, DACP_Name, GSA_Name, GSAMember, DecisionMaker, Listed_IP, Count_GSA_Int, Incorporated) %>% filter(Count_GSA_Int > 1)
uniqueMorethanOneGSA <- as.data.frame(unique(MorethanOneGSA$DACP_IDNumber))
OnlyOne <- Data %>% select(DACP_IDNumber, DACP_Name, GSA_Name, GSAMember, DecisionMaker, Listed_IP, Count_GSA_Int, Incorporated) %>% filter(Count_GSA_Int == 1)
uniqueOnlyOne <- as.data.frame(unique(OnlyOne$DACP_IDNumber))
table(OnlyOne$GSAMember)
table(OnlyOne$DecisionMaker)
# Because there were 8 that were both members and decision makers in the mroe than one gsa subset, All together this means that Only 37 out of the 243 DACs, or 15%, were members of their GSA. 47, or 19%, were decision-makers. 

# Numbers for incorporated DACs
Incorporated_OnlyOne <- OnlyOne %>% filter(Incorporated == "Y") 
table(Incorporated_OnlyOne$GSAMember)
table(Incorporated_OnlyOne$DecisionMaker)
Incorproated_MorethanOneGSA <- MorethanOneGSA %>% filter(Incorporated == "Y")
unique(Incorproated_MorethanOneGSA$DACP_IDNumber) # 10 incoporated DACs in the more than one GSA set tota
# Plus five incorporated communities in the more than one GSA list that are members and 5 that are decision makers (out of ten total incoporated in that set) so 15 out of 32 or 47% of the incoporated ones are GSA members. Five plus 12 so 17 decision makers out of 32 or 53%. 
allincorporated <- Data %>% filter(Incorporated == "Y")
uniqueallincorporated <- as.data.frame(unique(allincorporated$DACP_IDNumber))
nrow(uniqueallincorporated) # confirms there are 32 total

#including surrogates. 11/22 plus 5/10 is 16/32 or 50% for members. For DMs 13/22 plus 5/10 is 18/32 or 56%

# Numbers for unincoporated DACs
Unincorporated_OnlyOne <- OnlyOne %>% filter(Incorporated == "N") 
table(Unincorporated_OnlyOne$GSAMember)
table(Unincorporated_OnlyOne$DecisionMaker)
Unincorporated_MorethanOne <- MorethanOneGSA %>% filter(Incorporated == "N")
unique(Unincorporated_MorethanOne$DACP_IDNumber) #29 in this more than one set plus 10 equals the 39 joining with more than one total. yay! 3 unincoporated communities in this set were both members and decision makers. so three plus 19 for GSA members or 22 out of 211 or 10% members for unincoporated community. For decision makers, three plus 27 so 30 out of 211 or 14%
allunincorporated <- Data %>% filter(Incorporated == "N")
uniqueallunincorporated <- as.data.frame(unique(allunincorporated$DACP_IDNumber))
nrow(uniqueallunincorporated) # 211 total unincporated communities (which adds to 243!)

#including surrogates
table(Unincorporated_MorethanOne$GSAMember) #68/182 plus 11/29 is 79/211 or 37.4%
table(Unincorporated_MorethanOne$DecisionMaker) #71/182 plus 11/29 is 82/211 or 39%

# Number of unique DACPs listed on interested parties list
summary(Data$Listed_IP) #247 intersections not counting where the DAC is a member. 
ListedIPs <- Data %>% filter(Data$Listed_IP == "Y")
NotlistedIPs <- Data %>% filter(Data$Listed_IP == "N")
IPNA <- Data %>% filter(is.na(Data$Listed_IP))
uniqueListedIPs <- as.data.frame(unique(ListedIPs$DACP_IDNumber))


# GSA level stats ---------------------------------------------------------

# how many GSAs have DACs?
uniqueGSAs <- as.data.frame(unique(Data$GSA_ID))
uniqueGSAs2 <- as.data.frame(unique(Data$GSA_Name))
uniqueGSAs3 <- as.data.frame(unique(Data$GSADWR_GSA_ID)) # hmm these are slightly off
uniqueGSAs4 <- Data %>% select(GSA_Name, GSADWR_GSA_ID, GSA_ID) # I can't tell why the names one is lower but I'm going to go with 109

# by region
# First need list of GSAs and DACs joined
DACsandGSAsOnly <- Data %>% select(GSADWR_GSA_ID, GSA_Name, DACP_IDNumber, DACP_Name)
# join thsi to DACs by region
DACsbyGSAbyRegion <- left_join(DACsandGSAsOnly, DACbyRegion, by = c("DACP_IDNumber" = "DACP_Place_IDNumber"))
DACsbyGSAbyRegion2 <- DACsbyGSAbyRegion[!duplicated(DACsbyGSAbyRegion$GSADWR_GSA_ID), ]
table(DACsbyGSAbyRegion2$HR)

# add variable for number of DACs in that GSA
tableDACsperGSA <- as.data.frame(table(Data$GSADWR_GSA_ID)) # 109 unique 
colnames(tableDACsperGSA)[1] <- "GSADWR_GSA_ID"
colnames(tableDACsperGSA)[2] <- "Count_DAC_perGSA"
Data <- left_join(Data, tableDACsperGSA, by = "GSADWR_GSA_ID")
max(Data$Count_DAC_perGSA)
min(Data$Count_DAC_perGSA)
mean(Data$Count_DAC_perGSA)

# number of GSAs with DAC members
summary(Data$GSAMember)
GSAMember <- Data %>% filter(Data$GSAMember == "Y") # 37 are members
uniqueGSAMember <- as.data.frame(unique(GSAMember$GSADWR_GSA_ID)) #27 unique GSAs so 27 out of 109 or 25% have DAC decision makers

#by region
GSAswithDACMembers <- GSAMember[!duplicated(GSAMember$GSADWR_GSA_ID), ]
table(GSAswithDACMembers$HR)

# number of GSAs with DAC decision-makers
summary(Data$DecisionMaker)
GSADecisionMaker <- Data %>% filter(Data$DecisionMaker == "Y") # 47 decison makers
uniqueGSADecisionMaker <- as.data.frame(unique(GSADecisionMaker$GSADWR_GSA_ID)) #31 unique GSAs so 31 our of 109 or 28%

#by region
GSAswithDACDMs <- GSADecisionMaker[!duplicated(GSADecisionMaker$GSADWR_GSA_ID), ]
table(GSAswithDACDMs$HR)

# DAC participation by GSA by GSA type

# ADs
summary(Data$GSAType)
ADGSAs <- Data %>% filter(Data$GSAType == "AD")
uniqueADGSAs <- as.data.frame(unique(ADGSAs$GSADWR_GSA_ID)) # 15 joins between just 3 ADs
ADGSAsyesMembers <- ADGSAs %>% filter(ADGSAs$GSAMember == "Y")
uniqueADGSAsyesMembers <- as.data.frame(unique(ADGSAsyesMembers$GSADWR_GSA_ID)) # two of the three or 66.66% have DAC members 
ADGSAsyesDMs <- ADGSAs %>% filter(ADGSAs$DecisionMaker == "Y")
uniqueADGSAsyesDMs <- as.data.frame(unique(ADGSAsyesDMs$GSADWR_GSA_ID)) # two of the three or 66.66% have DAC decision-makers

#JPAs
JPAGSAs <- Data %>% filter(Data$GSAType == "JPA")
uniqueJPAGSAs <- as.data.frame(unique(JPAGSAs$GSADWR_GSA_ID)) # 89 joins between 29 JPAs
JPAGSAsyesMembers <- JPAGSAs %>% filter(JPAGSAs$GSAMember == "Y") # 
uniqueJPAGSAsyesMembers <- as.data.frame(unique(JPAGSAsyesMembers$GSADWR_GSA_ID)) # nine of the 29 JPAs or 31% have DAC members 

#MOUs and MOAs
MOUGSAs <- Data %>% filter(Data$GSAType == "MOU") #32 joins
uniqueMOUGSAs <- as.data.frame(unique(MOUGSAs$GSADWR_GSA_ID)) # 32 joins between 18 MOUs
MOUGSAsyesMembers <- MOUGSAs %>% filter(MOUGSAs$GSAMember == "Y") # 
uniqueMOUGSAsyesMembers <- as.data.frame(unique(MOUGSAsyesMembers$GSADWR_GSA_ID)) # 7 of the 18 MOUs or 39% have DAC members 

#Singles
SingleGSAs <- Data %>% filter(Data$GSAType == "Single") # 148 obs
uniqueSingleGSAs <- as.data.frame(unique(SingleGSAs$GSADWR_GSA_ID)) # between 59 single GSAs
SingleGSAsyesMembers <- SingleGSAs %>% filter(SingleGSAs$GSAMember == "Y") # 
uniqueSignleGSAsyesMembers <- as.data.frame(unique(SingleGSAsyesMembers$GSADWR_GSA_ID)) # 9 of the 59 single GSAs or 15% have DAC members 

# of GSAs that identified all the DACs in their area in their notice? 

Test <- Simplified %>%
  group_by(GSADWR_GSA_ID, Listed_IP) %>% 
  summarise(n= n()) 
IPsYes <- Test3 %>% filter(Listed_IP == "Y")
IPsNo <- Test3 %>% filter(Listed_IP == "N")
JoinedIPs <- full_join(IPsNo, IPsYes, by = "GSADWR_GSA_ID", copy = F)
colnames(JoinedIPs)[3] <- "Count_N"
colnames(JoinedIPs)[5] <- "Count_Y"
DataDACCountOnly <- Data %>% select(GSADWR_GSA_ID, Count_DAC_perGSA) %>% group_by(GSADWR_GSA_ID) %>% summarise(mean(Count_DAC_perGSA))
JoinedIPs <- left_join(JoinedIPs, DataDACCountOnly, by = "GSADWR_GSA_ID")
JoinedIPs$Perc_Y <- (JoinedIPs$Count_Y/JoinedIPs$`mean(Count_DAC_perGSA)`)

allIPsidentified <- JoinedIPs %>% filter(Perc_Y == 1) # 56 identified all

JoinedIPs$Perc_Y[is.na(JoinedIPs$Perc_Y)] <- 0
LessthanhalfIPsidentified <- JoinedIPs %>% filter(Perc_Y < .5) #32 less than 50% identified

# of GSAs that identified none of the DACs in their notice? 
JoinedIPs$Perc_N <- (JoinedIPs$Count_N/JoinedIPs$`mean(Count_DAC_perGSA)`)
noIPsidentified <- JoinedIPs %>% filter(Perc_N == 1) # 25 identified none
JoinedIPs$Perc_N[is.na(JoinedIPs$Perc_N)] <- 0


# Advisory committees/stakeholder committees ------------------------------
summary(Data$AdvisoryCommittee)
Committees <- Data %>% filter(Data$AdvisoryCommittee == "Y") #67 obs
uniqueGSAcommittees <- as.data.frame(unique(Committees$GSADWR_GSA_ID)) # 21 unique GSAs saying they will have committees out of 109, 19%


