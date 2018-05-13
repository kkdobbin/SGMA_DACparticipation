#Logits April 10 2018

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
IRWM <- read_csv("Data/IRWM_dupsdeleted_4.10.18.csv")

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
Regions$DACP_Place_IDNumber <- as.factor(Regions$DACP_Place_IDNumber)

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

#Data has 283 observations total before limiting it any more than just small DACs and intersections ten percent or greater. 

# Change NA is Listed_IP variable to yes's
Data$Listed_IP[is.na(Data$Listed_IP)] <- "Y"

# add in regions
RegionsCut <- Regions %>% select(HR, DACP_Place_IDNumber, FIRST_HRNA)
Data <- left_join(Data, RegionsCut, by = c("DACP_IDNumber" = "DACP_Place_IDNumber"))

# make into factor again
Data$HR <- as.factor(Data$HR)
Data$FIRST_HRNA <- as.factor(Data$FIRST_HRNA)

#Check everything is right with regions
uniqueDACPs <- as.data.frame(unique(Data$DACP_IDNumber))
table(Data$HR) # can't figure it out exactly but looks fine

# Re-order factor
relevel(Data$GSAType, "Single")
Data <- within(Data, GSAType <- relevel(GSAType, ref = "Single"))

# Logits ------------------------------------------------------------------

#test if I have only unique DACPs in IRWM dataset
max(table(unique(IRWM$`DACPlace ID Number`))) # yes

#Add any pilot variable
IRWM$AnyPilot <- NA
IRWM$AnyPilot <- ifelse(IRWM$DAC_Pilot == "Y" | IRWM$TLB == "Y", "Y", "N")

# add in new variable 0-1 zeros for zeros and one for everything else
Data$DV1 <- NA
Data$DV1 <- ifelse(Data$GSAMember == "Y" | Data$DecisionMaker == "Y" , 1, 0)
table(Data$DV1)
histogram(Data$DV1)

#Add in ANypilot variable to data set
AnyPilot <- IRWM %>% select(`DACPlace ID Number`, AnyPilot)
colnames(AnyPilot)[1] <- "DACP_IDNumber"
AnyPilot$DACP_IDNumber <- as.factor(AnyPilot$DACP_IDNumber)
Data$DACP_IDNumber <- as.factor(Data$DACP_IDNumber)
Data2 <- left_join(Data, AnyPilot, by = "DACP_IDNumber")
table(Data2$AnyPilot)
Data2$AnyPilot <- as.factor(Data2$AnyPilot)
Data2$AnyPilot[is.na(Data2$AnyPilot)] <- "N"
Data2$DV1 <- as.factor(Data2$DV1)

#Add in variable for number of GSAs intersecting
NGSAs <- as.data.frame(table(Data2$DACP_IDNumber))
colnames(NGSAs)[1] <- "DACP_IDNumber"
colnames(NGSAs)[2] <- "Number_GSAs"
Data3 <- left_join(Data2, NGSAs, by = "DACP_IDNumber")

# Add in variable for number of DACs in their same GSA
NDACs <- as.data.frame(table(Data3$GSA_ID))
colnames(NDACs)[1] <- "GSA_ID"
colnames(NDACs)[2] <- "Number_DACs"
Data3$GSA_ID <- as.factor(Data3$GSA_ID)
Data4 <- left_join(Data3, NDACs, by = "GSA_ID")

# adding in percent white
Demographics <- read_csv("Outputs/Percent_white.csv")
Data4$DACP_IDNumber <- as.factor(Data4$DACP_IDNumber)
Demographics$DACP <- as.factor(Demographics$DACP)
Data4 <- left_join(Data4, Demographics, by = c("DACP_IDNumber" = "DACP"))

# Logit model (no subsetting of master 283 obs data) for 0 - 1 logit (zeros versus everything else)
logita <- glm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + AnyPilot + Number_GSAs + Number_DACs, data = Data4, family = binomial(link="logit"))
summary(logita)

logitb <- glm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + AnyPilot + Number_GSAs + Number_DACs + area_weighted_pop_percWhite, data = Data4, family = binomial(link="logit"))
summary(logitb)

logit1 <- glm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent, data = Data2, family = binomial(link="logit"))
summary(logit1)

# older logits
logit2 <- glm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + HR, data = Data, family = "binomial")
summary(logit2)

logit3 <- glm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + AnyPilot, data = Data2, family = binomial(link="logit"))
summary(logit3)

# Diagnostics -------------------------------------------------------------

# Using rms package
logit4 <- lrm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + AnyPilot + GSA_Int_Percent, data = Data2)
print(logit4)

# percentage predicted correctly
predicted<-as.numeric(predict.glm(logit2,type="response")>.5)
true<-Data2$DV1 #but its not the right length because of the nine or so omitted values from teh model so this could be all screwed up
correct<-as.numeric(predicted==true)
100*table(correct)/sum(table(correct))

# McFaddens R2
nullmod <- glm(DV1~1, data = Data2, family="binomial")
1-logLik(logit2)/logLik(nullmod)

# Odds Ratios
oddratios <- exp(coef(logit2))
print(oddratios)
round(exp(cbind(Estimate=coef(logit2), confint(logit2))), 2)

#another way to get odds ratios
exp(logit2$coefficients[-1])

# Diagnostics
head(fitted(logit2))
plot(logit2)

residualPlots(logit2) # none are significant (factors don't run) so no lack of fit evident from this test


influenceIndexPlot(logit2, vars+c("Cook", "hat"), id.n=3)
