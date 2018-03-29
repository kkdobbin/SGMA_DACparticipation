# Logit model testing 3.29.18

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

# Run logit on whole dataset (no subsetting)

#binary response variable is member Y or N with Surrogates set to N
table(Data$GSAMember)
Data$GSAMember[Data$GSAMember == "S"] <- "N"
table(Data$GSAMember)
Data$GSAMember <- factor(Data$GSAMember)
table(Data$GSAMember)
#predictor variables: GSA_Type, DACP population, MHI, Incoporated, GSA int-percent, HR
table(Data$GSAType)
table(is.na(Data$DACP_Population))
table(is.na(Data$DACP_MHI))
table(Data$Incorporated)
table(Data$HR)
# Consider adding in percent pop that is white just to see? need to add in IRWM pilot variable

#model 1
logit <- glm(GSAMember ~ GSAType + DAC_Population + DACP_MHI + Incorporated + HR, data = Data, family = "binomial")
summary(logit)

# same thing but with DM as binomial dependent variable
table(Data$DecisionMaker)
Data$DecisionMaker[Data$DecisionMaker == "S"] <- "N"
table(Data$DecisionMaker)
Data$DecisionMaker <- factor(Data$DecisionMaker)
table(Data$DecisionMaker)

logit2 <- glm(DecisionMaker ~ GSAType + DAC_Population + DACP_MHI + Incorporated + HR, data = Data, family = "binomial")
summary(logit2)

# AIC useful for comparing models (much like adjusted R squared but unlike adjusted R squared not interpretable on its own). In this case I want to go with the first logit

# Hosmer-Lemeshow goodness of fit test
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(Data$GSAMember, fitted(logit))
# Need to have same lengths so need dataframe with GSAmember without those that are removed
Data2 <- Data %>% select(GSAMember, GSAType, DACP_Population, DACP_MHI, Incorporated, HR)
Data2 <- na.omit(Data2)
# try again
hoslem.test(Data2$GSAMember, fitted(logit)) # sad it doesn't fit well. very significant difference between DV and fitted model values

# try eliminating a few IVs to see if fit improves
logit3 <- glm(GSAMember ~ GSAType + DAC_Population + DACP_MHI + Incorporated, data = Data, family = "binomial")
summary(logit3)

logit4 <- glm(GSAMember ~ DAC_Population + DACP_MHI, data = Data, family = "binomial")
summary(logit4) #nope doesn't help to only have continuous variables in the model

# Trying to figure out what the deviance means
pvalue <- 1 - pchisq(142.74, 259) # so can accept null hypothesis that the model provides adequate fit? 

# exploring residuals
library(car)
influenceIndexPlot(logit, vars=c("Cook", "Studentized", "hat"), id.n=5) # looks like most residuals are negative
Data2$fitted <- fitted(logit)
