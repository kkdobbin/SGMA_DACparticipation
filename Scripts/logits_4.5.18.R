# Logits 4.5.18

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

# add in new variable 0-1 zeros for zeros and one for everything else
Data$DV1 <- NA
Data$DV1 <- ifelse(Data$GSAMember == "Y" | Data$DecisionMaker == "Y" , 1, 0)
table(Data$DV1)

# Logit model (no subsetting of master 283 obs data) for 0 - 1 logit (zeros versus everything else)
logita <- glm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + HR, data = Data, family = "binomial")
summary(logita)

# Using rms package
logitb <- lrm(DV1 ~ GSAType + DAC_Population + DACP_MHI + Incorporated + HR, data = Data)
print(logitb)

# McFaddens R2
nullmod <- glm(DV1~1, data = Data, family="binomial")
1-logLik(logita)/logLik(nullmod)

# Odds Ratios
oddratios <- exp(coef(logita))
print(oddratios)
round(exp(cbind(Estimate=coef(logita), confint(logita))), 2)

# 95% CI
CI_AD <- exp(4.1615 + 1.96*0.8346)

# Diagnostics
head(fitted(logita))
plot(logita)

residualPlots(logita) # none are significant (factors don't run) so no lack of fit evident from this test

influenceIndexPlot(logita, vars+c("Cook", "hat"), id.n=3)

