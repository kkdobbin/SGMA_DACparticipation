# POL 213 logit with CONTROLS

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

# Load data
Data <- read_csv("Data/DACP_GSApostings_3.27.18.csv")
GSApostings <- read_csv("Data/DACP_GSApostings_3.27.18.csv")
Regions <- read_csv("Data/hyrdoregions_DACP_int_3.27.18_dupsdeleted.csv")
IRWM <- read_csv("Data/IRWM_dupsdeleted_4.10.18.csv")
DEM <- read_csv("Data/Place_demographics_cutandedited_5_4_18.csv")
GWdependent <- read_csv("Data/DACP_GWdependent_5.16.18.csv")
SWdependent <- read_csv("Data/DACP_SWdependent_5.16.18.csv")
MHI <- read_csv("Data/DACP_MHI_5.13.18.csv")

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

# Clarify some variables
colnames(Data)[17] <- "GSA_Int_a_"
colnames(Data)[18] <- "GSA_Int_Percent"

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

# Assume those without a water system on are GW with no surface water (needs to be discussed)
Data$GW[is.na(Data$GW)] <- "Y"
Data$SW[is.na(Data$SW)] <- "N"

# add in MHI from ACS
MHIcut <- MHI %>% select(DACP_IDNumber, MHI_e)
Data <- left_join(Data, MHIcut, by = "DACP_IDNumber")

# change S in GSA eligible entity to Ns
Data$GSA_eligible_entity[Data$GSA_eligible_entity == "S"] <- "N"
Data$GSA_eligible_entity <- factor(Data$GSA_eligible_entity)

# Add new variables -------------------------------------------------------

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

# subset data to only what will be used for model -------------------------

Data2 <- Data %>% select(DACP_IDNumber, DACP_Name, DV1, Number_GSAs, Incorporated, AnyPilot, GSAType, ACS_whitenotHOL_epercent, ACS_HOL_epercent, GW, DACP_MHI, DACP_Population, GSAcol, GSA_eligible_entity)
sum(table(unique(Data2$DACP_IDNumber))) # still 234 AND 197 unique!

# Descriptive stats -------------------------------------------------------

table(Data2$DV1)
table(Data3$DV1)# only loose three 1s.

# Logits ------------------------------------------------------------------

# Logit model (no subsetting of master 234 obs data) for 0 - 1 logit (zeros versus everything else)
logit1 <- glm(DV1 ~ GSAcol + DACP_MHI + Incorporated + AnyPilot + Number_GSAs + DACP_Population + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit1) #33 obs deleted due to missingness mostly due to MHI_e variable

#Try logit 1 with percent latino instead of percent white
logit1a <- glm(DV1 ~ GSAcol + DACP_MHI + Incorporated + AnyPilot + Number_GSAs + DACP_Population + GW + GSA_eligible_entity + ACS_HOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit1a) # not better AIC

# try logit 1 with GSAType instead of GSAcol
logit1b <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + AnyPilot + Number_GSAs + DACP_Population + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit1b)

# Compare logits ----------------------------------------------------------

# pseudo R2

pR2(logit1) # pscl package
pR2(logit1a)
pR2(logit1b) # b2 is way better on the mcfadden

# odds ratio
#logit1
oddratios <- exp(coef(logit1))
print(oddratios)
round(exp(cbind(Estimate=coef(logit1), confint(logit1))), 2)
#logit1b
round(exp(cbind(Estimate=coef(logit1b), confint(logit1b))), 2) # all the same