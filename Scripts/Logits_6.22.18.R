# Final (hopefully) logit model

# Load libraries 
library(tidyverse)
library(dplyr)
library(rms)
library(broom)
library(tidyverse)
library(car)
library(pscl)
library(DAMisc)
library(pROC)
library(Zelig)
library(ggplot2)
library(ResourceSelection)
library(MASS)
library(nnet)
library(reshape2)
library(ZeligChoice)
library(QRM)

# Load data
Data <- read_csv("Data/DACP_GSApostings_6.6.18.csv")
GSApostings <- read_csv("Data/DACP_GSApostings_6.6.18.csv")
Regions <- read_csv("Data/hyrdoregions_DACP_int_3.27.18_dupsdeleted.csv")
IRWM <- read_csv("Data/IRWM_dupsdeleted_4.10.18.csv")
DEM <- read_csv("Data/Place_demographics_cutandedited_5_4_18.csv")
GWdependent <- read_csv("Data/DACP_GWdependent_5.16.18.csv")
SWdependent <- read_csv("Data/DACP_SWdependent_5.16.18.csv")
MHI <- read_csv("Data/DACP_MHI_5.13.18.csv")
facilitation <- read_csv("Data/Facilitation_6.12.18.csv")

# Clean up data -------------------------------------------------------

# get rid of extra columns
IRWM <- select(IRWM, -13, -14)
GWdependent <- select(GWdependent, -4, -5, -6)
SWdependent <- select(SWdependent, -4, -5, -6)

#get rid of extra rows
GWdependent <- GWdependent[!is.na(GWdependent$DACP_id),]
SWdependent <- SWdependent[!is.na(SWdependent$DACP_id),]

# turn ACS -- and ** into NA
MHI$MHI_e[MHI$MHI_e == "-"] <- NA

# Clarify some variables names
colnames(Data)[17] <- "GSA_Int_a_"
colnames(Data)[18] <- "GSA_Int_Percent"

# Make MOU and MOA the same
Data$GSAType[Data$GSAType == "MOA"] <- "MOU" # NOTE that when results are interpreted it needs to be understood and clear that this is for both MOAs and MOUs
table(Data$GSAType)
Data$GSAType <- factor(Data$GSAType)
table(Data$GSAType)

# NEED TO DEAL WITH MHI zeros
Data$DACP_MHI[Data$DACP_MHI == 0] <- NA # This needs to be noted in methods or notes

# NEED TO DEAL WITH POP zero
Data$DACP_Population[Data$DACP_Population == 0] <- NA # This needs to be noted in methods or notes

# Number of GSA eligible entities NA make zeros
Data$number_GSA_eligible_entities[is.na(Data$number_GSA_eligible_entities)] <- 0

# Change NA is Listed_IP variable to yes's (this needs to be noted in text)
Data$Listed_IP[is.na(Data$Listed_IP)] <- "Y"

#Can't have pop 0
DEM$ACS_pop_e[DEM$ACS_pop_e == 0] <- NA

# Make variable types correct
Data$GSAType <- as.factor(Data$GSAType)
Data$GSAMember <- as.factor(Data$GSAMember)
Data$DecisionMaker <- as.factor(Data$DecisionMaker)
Data$Listed_IP <- as.factor(Data$Listed_IP)
Data$Incorporated <- as.factor(Data$Incorporated)
Data$DAC_Population <- as.integer(Data$DACP_Population)
Data$AdvisoryCommittee <- as.factor(Data$AdvisoryCommittee)
Data$DACP_IDNumber <- as.factor(Data$DACP_IDNumber)
Data$GSADWR_GSA_ID <- as.factor(Data$GSADWR_GSA_ID)
Data$GSA_Basin <- as.factor(Data$GSA_Basin)
Data$GSAstatus <- as.factor(Data$GSAstatus)
Data$GSAMember <- as.factor(Data$GSAMember)
Data$DecisionMaker <- as.factor(Data$DecisionMaker)
Data$Voting_type <- as.factor(Data$Voting_type)
Data$Listed_IP <- as.factor(Data$Listed_IP)
Data$GSA_eligible_entity <- as.factor(Data$GSA_eligible_entity)

DEM$DACP_IDNumber <- as.factor(DEM$DACP_IDNumber)
DEM$ACS_white_epercent <- as.numeric(DEM$ACS_white_epercent)
DEM$ACS_white_mepercent <- as.numeric(DEM$ACS_white_mepercent)
DEM$ACS_whitenotHOL_epercent <- as.numeric(DEM$ACS_whitenotHOL_epercent)
DEM$ACS_HOL_epercent <- as.numeric(DEM$ACS_HOL_epercent)

Regions$FIRST_HRNA <- as.factor(Regions$FIRST_HRNA)
Regions$HR <- as.factor(Regions$HR)
Regions$DACP_Place_IDNumber <- as.factor(Regions$DACP_Place_IDNumber)
colnames(Regions)[7] <- "DACP_IDNumber"

SWdependent$SW <- as.factor(SWdependent$SW)
GWdependent$GW <- as.factor(GWdependent$GW)

colnames(IRWM)[1] <- "DACP_IDNumber"
IRWM$DAC_Pilot <- as.factor(IRWM$DAC_Pilot)
IRWM$TLB <- as.factor(IRWM$TLB)

colnames(MHI)[2] <- "DACP_IDNumber"
MHI$DACP_IDNumber <- as.factor(MHI$DACP_IDNumber)
MHI$MHI_e <- as.numeric(MHI$MHI_e)

facilitation$Basin <- as.factor(facilitation$Basin)
facilitation$Phase1 <- as.factor(facilitation$Phase1)
facilitation <- select(facilitation, -8)


# change S in GSA eligible entity to Ns (needs to be discussed in methods or notes)
Data$GSA_eligible_entity[Data$GSA_eligible_entity == "S"] <- "N"
Data$GSA_eligible_entity <- factor(Data$GSA_eligible_entity)

# Change S in decision maker and GSA member categories to Ns (needs to be discussed in methods or notes)
Data$GSAMember[Data$GSAMember == "S"] <- "N"
Data$GSAMember <- factor(Data$GSAMember)
Data$DecisionMaker[Data$DecisionMaker == "S"] <- "N"
Data$DecisionMaker <- factor(Data$DecisionMaker)



# Combine data sets -------------------------------------------------------

# add in regions
RegionsCut <- Regions %>% select(HR, DACP_IDNumber, FIRST_HRNA)
Data <- left_join(Data, RegionsCut, by = "DACP_IDNumber")
# make into factor again
Data$DACP_IDNumber <- as.factor(Data$DACP_IDNumber)
# Re-order factor
relevel(Data$GSAType, "Single")
Data <- within(Data, GSAType <- relevel(GSAType, ref = "Single"))

# add in ACS data
DemCut <- DEM %>% select(DACP_IDNumber, ACS_pop_e, ACS_whitenotHOL_e, ACS_whitenotHOL_epercent, ACS_HOL_epercent)
Data <- left_join(Data, DemCut, by = "DACP_IDNumber")
# re-factor
Data$DACP_IDNumber <- as.factor(Data$DACP_IDNumber) # yay still 243

# Add in IRWM data
#test if I have only unique DACPs in IRWM dataset
max(table(unique(IRWM$DACP_IDNumber))) # yes
#Add any pilot variable
IRWM$AnyPilot <- NA
IRWM$AnyPilot <- ifelse(IRWM$DAC_Pilot == "Y" | IRWM$TLB == "Y", "Y", "N")
#Add in ANypilot variable to data set
IRWMcut <- IRWM %>% select(DACP_IDNumber, AnyPilot, DAC_Pilot, TLB)
IRWMcut$DACP_IDNumber <- as.factor(IRWMcut$DACP_IDNumber)
Data <- left_join(Data, IRWMcut, by = "DACP_IDNumber")
table(Data$AnyPilot)
# coerce into correct types again
Data$AnyPilot <- as.factor(Data$AnyPilot)
# add Ns for those DACPs outside of any IRWM so are currently NA
Data$AnyPilot[is.na(Data$AnyPilot)] <- "N"
Data$DAC_Pilot[is.na(Data$DAC_Pilot)] <- "N"
Data$TLB[is.na(Data$TLB)] <- "N"

# add in GW and SW dependent
GWorSW <- full_join(SWdependent, GWdependent, by = "DACP_id")
GWorSW <- select(GWorSW, -2, -4)
GWorSW$GW <- as.character(GWorSW$GW)
GWorSW$GW[is.na(GWorSW$GW)] <- "N"
GWorSW$GW <- as.factor(GWorSW$GW)
GWorSW$SW <- as.character(GWorSW$SW)
GWorSW$SW[is.na(GWorSW$SW)] <- "N"
GWorSW$SW <- as.factor(GWorSW$SW)
table(GWorSW$SW)
table(GWorSW$GW)
GWorSW %>% filter(GWorSW$SW == "Y" & GWorSW$GW == "Y")
GWorSW$DACP_id <- as.factor(GWorSW$DACP_id)
Data$DACP_IDNumber <- as.factor(Data$DACP_IDNumber)
Data <- left_join(Data, GWorSW, by = c("DACP_IDNumber" = "DACP_id"))

# Assume those without a water system on are GW with no surface water (needs to be discussed in methods or notes)
Data$GW[is.na(Data$GW)] <- "Y"
Data$SW[is.na(Data$SW)] <- "N"

# add in MHI from ACS
MHIcut <- MHI %>% select(DACP_IDNumber, MHI_e)
Data <- left_join(Data, MHIcut, by = "DACP_IDNumber")

# add in facilitation
facilitationphase1 <- select(facilitation, 1, 2)
Data <- left_join(Data, facilitationphase1, by = c("GSA_Basin" = "Basin"))
Data$Phase1 <- as.character(Data$Phase1)
Data$Phase1[is.na(Data$Phase1)] <- "N"
Data$Phase1 <- as.factor(Data$Phase1)
Test <- Data %>% filter(Phase1 == "Y")
table(Test$GSA_Basin)

# Create new variables -------------------------------------------------------

# add in new dependent variable 0-1 zeros for zeros and one for everything else
Data$DV1 <- NA
Data$DV1 <- ifelse(Data$GSAMember == "Y" | Data$DecisionMaker == "Y" , 1, 0)
table(Data$DV1)
histogram(Data$DV1)

#Add in variable for number of GSAs intersecting
NumberGSAs <- as.data.frame(table(Data$DACP_IDNumber))
colnames(NumberGSAs)[1] <- "DACP_IDNumber"
colnames(NumberGSAs)[2] <- "Number_GSAs"
Data <- left_join(Data, NumberGSAs, by = "DACP_IDNumber")

# Add in variable for number of DACs in their same GSA
NumberDACs <- as.data.frame(table(Data$GSA_ID))
colnames(NumberDACs)[1] <- "GSA_ID"
colnames(NumberDACs)[2] <- "Number_DACs"
Data$GSA_ID <- as.factor(Data$GSA_ID)
Data <- left_join(Data, NumberDACs, by = "GSA_ID")

# add in binary variable for GSA type (collaboraive or not)
Data$GSAcol <- NA
Data$GSAcol <- ifelse(Data$GSAType == "Single" , 0, 1)
table(Data$GSAType)
table(Data$GSAcol) # good 147 of single entities matches

# Create a multinomial logit DV
Data$DV2 <- NA
Data$DV2 <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "N" , 0, Data$DV2)
Data$DV2 <- ifelse(Data$GSAMember == "N" & Data$DecisionMaker == "Y" , 1, Data$DV2)
Data$DV2 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "N" , 1, Data$DV2)
Data$DV2 <- ifelse(Data$GSAMember == "Y" & Data$DecisionMaker == "Y" , 2, Data$DV2)
table(Data$DV2)
histogram(Data$DV2)
Data$DV2 <- factor(Data$DV2)

# subset data to only what will be used for model -------------------------

# New dataset with only variables I care about
Data2 <- Data %>% select(DACP_IDNumber, DACP_Name, DV1, DV2, DACP_Population, DACP_MHI, Number_GSAs, Incorporated, AnyPilot, GSAType, ACS_whitenotHOL_epercent, ACS_HOL_epercent, GW, DACP_MHI, DACP_Population, GSAcol, GSA_eligible_entity, GSA_Int_Percent, Phase1, Number_GSAs, Number_DACs, Listed_IP)
sum(table(unique(Data2$DACP_IDNumber))) # still 290 AND 243 unique!

# Make variable types right again
Data2$DACP_IDNumber <- as.factor(Data2$DACP_IDNumber)
Data2$DV1 <- as.factor(Data2$DV1)
Data2$GSAcol <- as.factor(Data2$GSAcol)

# subset data to only those DACs that use GW (but not exclusively GW necessarily)
Data3 <- Data2 %>% filter(GW == "Y") #brings it down to 264 observations
sum(table(unique(Data3$DACP_IDNumber))) #with 226 unique DACPs

#subset data to only those DACs that use GW and have GSA eligible entities
Data4 <-  Data3 %>% filter(GSA_eligible_entity == "Y") # brings down to 112 obs
sum(table(unique(Data4$DACP_IDNumber))) #90 unique DACPs

#subset to only GSA eligible entities
Data5 <- Data2 %>% filter(GSA_eligible_entity == "Y") # 127 obs
sum(table(unique(Data5$DACP_IDNumber))) # 99 unique DACPs

#Subset to only collaborative GSAs
Data6 <- Data2 %>% filter(GSAcol == 1) #128 unique, 142 total
sum(table(unique(Data6$DACP_IDNumber)))


table(Data2$DV1)
table(Data3$DV1)
table(Data4$DV1)
table(Data2$DV2)

# Logit model options for datasets -----------------------------------------------------

# LOGIT 1 (Data 2)
logit1 <- glm(DV1 ~ GSAcol + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data2, family = binomial(link="logit"))
summary(logit1)
pR2(logit1)

# multinomial logit for comparison
mlogit1 <- multinom(DV2 ~ GSAcol + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data2, Hess = TRUE)
summary(mlogit1)

# Logit one with GSA types instead of GSAcol
logit2 <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data2, family = binomial(link="logit"))
summary(logit2)
pR2(logit2)

# logit for listed interested party 
logit3 <- glm(Listed_IP ~ GSAcol + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data2, family = binomial(link="logit"))
summary(logit3)
pR2(logit3)

# Logits for data subsets -------------------------------------------------

#GW dependent DACs only
logitA <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + GSA_eligible_entity + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data3, family = binomial(link="logit"))
summary(logitA)
pR2(logitA)

#GSA eligible entities only
logitB <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + GW +  ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data5, family = binomial(link="logit"))
summary(logitB)
pR2(logitB)

#Both GW dependent and GSA eligible
logitC <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data4, family = binomial(link="logit"))
summary(logitC)
pR2(logitC)

# Facilitation (only colaborative GSAs)
logitD <- glm(DV1 ~ Phase1 + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data6, family = binomial(link="logit"))
summary(logitD)
pR2(logitD)

logitE <- glm(DV1 ~ Phase1 + GSAType + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent + DACP_Population + ACS_whitenotHOL_epercent + Number_GSAs + Number_DACs, data = Data6, family = binomial(link="logit"))
summary(logitE)
pR2(logitE)