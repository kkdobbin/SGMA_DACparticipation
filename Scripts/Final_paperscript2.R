# Final paper script 2

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

# Load data
Data <- read_csv("Data/DACP_GSApostings_8.15.18.csv")
GSApostings <- read_csv("Data/DACP_GSApostings_8.15.18.csv")
Regions <- read_csv("Data/hyrdoregions_DACP_int_3.27.18_dupsdeleted.csv")
IRWM <- read_csv("Data/IRWM_dupsdeleted_4.10.18.csv")
DEM <- read_csv("Data/Place_demographics_cutandedited_5_4_18.csv")
DEM2010 <- read_csv("Data/DEM2010.csv")
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

DEM2010$DACP_IDNumber <- as.factor(DEM2010$DACP_IDNumber)
DEM2010$ACS_HOL_epercent_2010 <- as.numeric(DEM2010$ACS_HOL_epercent_2010)

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
Data$DACP_IDNumber <- as.factor(Data$DACP_IDNumber)  # yay still 241

# Add in 2010 dem data
DEM2010Cut <- DEM2010 %>% select(DACP_IDNumber, ACS_HOL_epercent_2010)
Data <- left_join(Data, DEM2010Cut, by = "DACP_IDNumber")

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
facilitationphase1 <- facilitationphase1[c(-10, -17, -22, -25, -26),]
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

#Confirm that this is only two more cases than just looking at decision makers
summary(Data$DecisionMaker)
Data %>% filter(DecisionMaker == "N" & GSAMember == "Y")

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

# New dataset with only variables I care about
Data2 <- Data %>% select(DACP_IDNumber, DACP_Name, DV1, DACP_Population, Number_GSAs, Incorporated, AnyPilot, GSAType, GW, DACP_MHI, GSAcol, ACS_HOL_epercent_2010, GSA_eligible_entity, GSA_Int_Percent, Phase1, Number_DACs, Listed_IP, DecisionMaker, GSAMember)
sum(table(unique(Data2$DACP_IDNumber))) # still 281 AND 241 unique! 

# Make variable types right again
Data2$DACP_IDNumber <- as.factor(Data2$DACP_IDNumber)
Data2$DV1 <- as.factor(Data2$DV1)
Data2$GSAcol <- as.factor(Data2$GSAcol)

# inspect Data2
summary(Data2$DV1)

# subset data to only those DACs that use GW (but not exclusively GW necessarily)
Data3 <- Data2 %>% filter(GW == "Y") #brings it down to 264 observations
sum(table(unique(Data3$DACP_IDNumber))) #with 224 unique DACPs

#subset data to only those DACs that use GW and have GSA eligible entities
Data4 <-  Data3 %>% filter(GSA_eligible_entity == "Y") # brings down to 108 obs
sum(table(unique(Data4$DACP_IDNumber))) #91 unique DACPs

#subset to only GSA eligible entities
Data5 <- Data2 %>% filter(GSA_eligible_entity == "Y") # 125 obs
sum(table(unique(Data5$DACP_IDNumber))) # 101 unique DACPs

#Subset to only collaborative GSAs
Data6 <- Data2 %>% filter(GSAcol == 1) #126 unique, 134 total
sum(table(unique(Data6$DACP_IDNumber)))

# Subset to only those DACs that are listed interested parties
Data7 <- Data2 %>% filter(Listed_IP == "Y")
sum(table(unique(Data7$DACP_IDNumber))) #134 unique, 154 total

# subset to only those noncollaborative GSAs (single entity)
Data8 <- Data2 %>% filter(GSAcol == 0) #126 unique, 147 total (wierd that same number as collaboarative)
sum(table(unique(Data8$DACP_IDNumber)))
table(Data2$GSAcol) # but total numbers are correct

# bar graph of DV1
ggplot(data = Data2, aes(x = DV1)) + geom_bar() + theme_classic() + ylab("Number of small DACs") + xlab("") + scale_x_discrete(labels=c("0" = "GSA decision-makers", "1" = "Not decision-makers")) +
  theme(axis.text.x = element_text(size=12, color = "black")) + 
  theme(axis.title.x=element_text(size=12)) + 
  theme(axis.title.y=element_text(size=12)) + 
  theme(axis.text.y = element_text(size=14, color = "black")) + 
  theme(plot.title = element_text(hjust = 0.5, size=16)) + theme(plot.margin=unit   (c(1.5,1.5,1.5,1.5),"cm"))

# Descriptive stats and data exploration ----------------------------------

sum(table(unique(Data2$DACP_IDNumber))) #241 unique DACPs
sum(table(unique(Data$GSADWR_GSA_ID))) #109 unique GSAs

# Numbers that have GSA eligible agency
summary(Data2$GSA_eligible_entity)
GSA_eligible <- Data2[,c(1,13)]
GSA_eligible_unique <- GSA_eligible %>% unique(GSA_eligible$DACP_IDNumber, incomparables = F)
summary(GSA_eligible_unique)

# How many participated as members of decision makers but don't have GSA eligible entity?
Participating_butnot_eligible <- Data2 %>% filter(DV1 == 1) %>% filter(GSA_eligible_entity == "N")

# How many cities
summary(Data2$Incorporated) #42

Cities_single <- Data2 %>% filter(Incorporated == "Y") %>% filter(GSAType == "Single") #28

Cities_col <- Data2 %>% filter(Incorporated == "Y") %>% filter(GSAcol == 1) #14

#How many DAC DMs on average do GSAs with them have?
GSADMs <- as.data.frame(Data[ , c(7, 24, 47)])
GSADMs <- GSADMs %>% filter(DV1 == 1)
GSADMs_Y <- GSADMs %>% group_by(GSADWR_GSA_ID) %>% tally()
table(GSADMs_Y$n)

# who are members but not DMs?
Data2 %>% filter(DecisionMaker == "N" & GSAMember == "Y")

#export CSV for backup and review purposes
write.csv(Data, file = "Outputs/Data.csv", row.names = F)
write.csv(Data2, file = "Outputs/Data2.csv", row.names = F)

# average population
summary(Data2$DACP_Population)

# How may unique DACs are participating
tableunique <- as.data.frame(table(Data$DACP_IDNumber)) 
colnames(tableunique)[1] <- "DACP_IDNumber"
colnames(tableunique)[2] <- "Count_GSA_Int"
Data_test <- left_join(Data, tableunique, by = "DACP_IDNumber")
UniqueDACs <- Data_test %>% select(DACP_IDNumber, DACP_Name, GSA_Name, GSAMember, DecisionMaker, Listed_IP, Count_GSA_Int, Incorporated, Count_GSA_Int, DV1) %>% filter(DV1 == 1)

# summary of GSA type stats
summary(Data2$GSAType)

# number with facilitation
summary(Data2$Phase1)

# summary of pop
summary(Data2$DACP_Population)
sd(Data2$DACP_Population)

# summary of MHI
summary(Data2$DACP_MHI)
sd(na.omit(Data2$DACP_MHI))

# number incoporated
summary(Data2$Incorporated)

# summary of percent latino
summary(Data2$ACS_HOL_epercent_2010)
sd(na.omit(Data2$ACS_HOL_epercent_2010))

# number GSA eligible
summary(Data2$GSA_eligible_entity)

# average number DACs
summary(Data2$Number_DACs)
sd(na.omit(Data2$Number_DACs))

# summary GW reliant
summary(Data2$GW)

# summary percent intersection
summary(Data2$GSA_Int_Percent)
sd(na.omit(Data2$GSA_Int_Percent))

# LOGITS ------------------------------------------------------------------

#load only when needed once data is fully manipulated and ready for modeling
library(MASS)

# Logit 5 - All the variables expect Any Pilot, GSA Type instead of GSAcol. Using 2010 DEM
logit5 <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_HOL_epercent_2010 + Number_GSAs + Number_DACs + Phase1, data = Data2, family = binomial(link="logit"))
summary(logit5)
pR2(logit5)

# Logit 5 but with number GSAs removed to address colinearity issue between than and intersection percent
logit5 <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_HOL_epercent_2010 + Number_DACs + Phase1, data = Data2, family = binomial(link="logit"))
summary(logit5)
pR2 <- pR2(logit5); pR2

#Create null model
Data2_noNA <- na.omit(Data2)
Null <- glm(DV1 ~ 1, data = Data2_noNA, family = binomial(link="logit"))
summary(Null)

lrtest(logit5, Null)

# Tables for logit 5
logit5coefs <- coef(summary(logit5))
logit5coefs <- round(logit5coefs, 4)

oddratios <- round(exp(cbind(Estimate=coef(logit5), confint(logit5, level = 0.95))), 4)
print(oddratios)

exp(coef(logit5))

stargazer(logit5, single.row=TRUE, align=TRUE, rownames = TRUE, covariate.labels = c("Special Act District", "JPA", "MOU/MOA", "MHI", "Incorporated", "Percent intersected by GSA", "Population", "Groundwater reliant", "GSA eligible", "Percent Latino", "Number of DACs", "Facilitation"), title="Model One coefficients", out="Figures/fulllogitcoefs.htm", digits = 2)

stargazer(oddratios, align=TRUE, title="Odds ratios with 95% ci", out="Figures/fulllogitoddsratio.htm")

# predit
fitted(logit5)
round(fitted(logit5),0)
sum(round(fitted(logit5),0)) # overall 38 out of 273 (I think) are predicted to be participating
#attach this to Data2_noNA
Data2_noNA$Predicted <- round(fitted(logit5),0)
#how many in noncol GSAs predicted ones
Data2_noNA_nocol <- Data2_noNA %>% filter(GSAcol == 0)
sum(Data2_noNA_nocol$Predicted) # only 7 predicted to be ones, 7 divided by 143 is 5%
#how many in col GSAs predicted ones
Data2_noNA_col <- Data2_noNA %>% filter(GSAcol == 1)
sum(Data2_noNA_col$Predicted) # 31 predicted to be ones. 31 divided by 130 is 24%
# adds to 38 total which is good. 


# INSTITUTIONAL SUBSET ----------------------------------------------------

# load more libraries
library(ZeligChoice)
library(stargazer)
library(lmtest)

# re-order categories in Data6
# Re-order factor
relevel(Data6$GSAType, "MOU")
Data6 <- within(Data6, GSAType <- relevel(GSAType, ref = "MOU"))

# Logit five for only collaborative GSA subset
logit6 <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_HOL_epercent_2010 + Number_DACs + Phase1, data = Data6, family = binomial(link="logit"))
summary(logit6)
pR2(logit6)

oddratios6 <- round(exp(cbind(Estimate=coef(logit6), confint(logit6, level = 0.95))), 4)
print(oddratios6)

Logit6b <- glm(DV1 ~ GSAType + Phase1, data = Data6, family = binomial(link="logit"))
summary(Logit6b)
pR2(Logit6b)


# Recognition subset ----------------------------------------

# logit five for GSA eligible DACs subset only taking that variable away
logit7 <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + ACS_HOL_epercent_2010 + Number_DACs + Phase1, data = Data5, family = binomial(link="logit"))
summary(logit7)
pR2(logit7)

# Logit five for just those DACs lsited as interested parties
logit8 <- glm(DV1 ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + ACS_HOL_epercent_2010 + Number_GSAs + Number_DACs + Phase1, data = Data7, family = binomial(link="logit"))
summary(logit8)
pR2(logit8)

stargazer(logit5, logit6, logit7, single.row=TRUE, align=TRUE, rownames = TRUE, covariate.labels = c("Special Act District", "JPA", "MOU/MOA", "MHI", "Incorporated", "Percent intersected by GSA", "Population", "Groundwater reliant", "GSA eligible", "Percent Latino", "Number of DACs", "Facilitation"), column.labels = c("no subset", "Collaborative GSAs only", "GSA eligible DACs only"),title="Subset Model Coefficients", out="Figures/subsetcoefs.htm")

oddsratiosCol <- round(exp(cbind(Estimate=coef(logit6), confint(logit6, level = 0.95))), 4)
oddsratiosElg <- round(exp(cbind(Estimate=coef(logit7), confint(logit7, level = 0.95))), 4)

stargazer(oddratios, oddsratiosCol, oddsratiosElg, title=c("no subset", "Collaborative GSAs only", "GSA eligible DACs only"), out="Figures/subsetoddsratios.htm")


# Listed IP as DV ---------------------------------------------------------

logit9 <- glm(Listed_IP ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_HOL_epercent_2010 + Number_DACs + Phase1, data = Data2, family = binomial(link="logit"))
summary(logit9)
pR2(logit9)

stargazer(logit5, logit9, single.row=TRUE, align=TRUE, rownames = TRUE, column.labels = c("Model One", "Model Two"),title="Model Two coefficients", out="Figures/IPcoefs.htm")

oddsratiosIP <- round(exp(cbind(Estimate=coef(logit9), confint(logit9, level = 0.95))), 2)

exp(coef(logit9))

stargazer(oddratios, oddsratiosIP, title=c("Model One", "Model Two"), out="Figures/IPoddsratios.htm")

# Exploring multi-collinearity --------------------------------------------

#first get a dataset without any of the NAs shoudl amtch number of observations in model

Data2_noNA1 <- Data2_noNA
Data2_noNA1$GSA_eligible_entity <- as.numeric(Data2_noNA1$GSA_eligible_entity)

ggplot(data = Data2_noNA1, aes(y = GSA_eligible_entity, x = ACS_HOL_epercent_2010)) + geom_point() + geom_smooth(method=lm)

Fit <- lm(data = Data2_noNA1, GSA_eligible_entity~ACS_HOL_epercent_2010)
summary(Fit)

Fit2 <- lm(data = Data2_noNA1, GSA_eligible_entity~DACP_MHI)
summary(Fit2)

Fit3 <-  lm(data = Data2_noNA1, DACP_MHI~ACS_HOL_epercent_2010)
summary(Fit3)

ggplot(data = Data2_noNA1, aes(y = GSA_eligible_entity, x = ACS_HOL_epercent_2010)) + geom_point() + geom_smooth(method=lm)

ggplot(data = Data2_noNA1, aes(y = GSA_eligible_entity, x = DACP_MHI)) + geom_point() + geom_smooth(method=lm)

ggplot(data = Data2_noNA1, aes(y = ACS_HOL_epercent_2010, x = DACP_MHI)) + geom_point() + geom_smooth(method=lm)

summary(Data2_noNA$ACS_HOL_epercent_2010)
# I think 2010-2014 ACS estimates says CA overall was 38%. Mean for DACs is 57.5%, median is 63.3%
sd(Data2_noNA$ACS_HOL_epercent_2010)

#standard error
sd(Data2_noNA$ACS_HOL_epercent_2010)/(sqrt((length(Data2_noNA))))

# Hypothesis test for proportion
#z = (p - P) / σ
P<- .38 # hypothesized value for proportion, in this case state proporiton hispanic
p <- .575 # observed proportion
sd <- sqrt((.38*(1-0.38)/(length(Data2_noNA)))) # σ = sqrt[ P * ( 1 - P ) / n ]
z <- (p - P) / (sd); z # significant at P<.05

# Diagnostics -------------------------------------------------------------

# Pseudo R squared and AIC ------------------------------------------------

#pseudo R squared
pR2(logit5)

# LogLik
logLik(logit5)

nullmod <- glm(DV1~1, data = Data2_noNA, family="binomial")
1-logLik(logit5)/logLik(nullmod)

lrtest(logit5, nullmod)

print(logit5) #rms package

BIC(logit5)
BIC(nullmod)


# Odds ratios -------------------------------------------------------------

# Odds Ratios
oddratios <- exp(coef(logit5))
print(oddratios)
round(exp(cbind(Estimate=coef(logit5), confint(logit5))), 4)

#another way to get odds ratios
exp(logit5$coefficients[-1])

# VIF
vif(logit5) 

# CLassification tables and PRE -------------------------------------------

# CLassification table

#now classification table
classlogit5 <- data.frame(response = Data2_noNA, predicted = round(fitted(logit5),0))
xtabs(~ predicted + response.DV1, data = classlogit5)
# model sensitivity is how often correclty predicting success outcome so 28/45 so 62% (true positive rate)
# model specificity is how often predicting true negatives so 218/228 so 96% (true negative rate)
# total number correct is 246 out of 273 or 90%

# using DAMisc package
pre(logit5, sim=TRUE, R=1000) # simulated median PRE is .4 with a confidence interval of .244 to .489 so thats good I think! aalystical restul PRE is .400 same as what I calculated


# ROC curve ---------------------------------------------------------------

FinalModel <- zelig(DV1 ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_HOL_epercent_2010 + Number_DACs + Phase1, model="logit", data = Data2_noNA)
summary(FinalModel)

z.null <- zelig(DV1 ~ 1, model = "logit", data = Data2); z.null

rocplot(FinalModel, z.null, plot = F) # area under curve for model logit b is .95 excellent predictive power

# Predicted probabilities ------------------------------------------------------

summary(logit5)

# odds ratios
Odds <- exp(cbind("Odds ratio" = coef(logit5), confint.default(logit5, level = 0.95))); Odds

# percent change in odds from unincporated (base category) to incorporated
((exp(1.422*1) - exp(1.422*0))/exp(1.422*0))*100 # increases odds by nearly 314% matches odds ratio of 4.14

# glm(DV1 ~ GSAType + DACP_MHI + Incorporated + GSA_Int_Percent + DACP_Population + GW + GSA_eligible_entity + ACS_HOL_epercent_2010 + Number_DACs + Phase1, data = Data2, family = binomial(link="logit"))

summary(Data2_noNA$DACP_MHI)
summary(Data2_noNA$GSA_Int_Percent)
summary(Data2_noNA$DACP_Population)
summary(Data2_noNA$GW)
summary(Data2_noNA$ACS_HOL_epercent_2010)
summary(Data2_noNA$Number_DACs)
summary(Data2_noNA$Phase1)

# Noncollaborative (single entity) GSA scenarios

#Low resources, not GSA eligible using default zelig settings for everything but what I"m trying ot manipulate
set.seed(1990)
x.out_LR_no <- setx(FinalModel, GSAType="Single", DACP_MHI=27297, Incorporated="N", DACP_Population=424, GSA_eligible_entity="N")
s.out_LR_no <- sim(FinalModel, x=x.out_LR_no, num=100000)
summary(s.out_LR_no) #.00092 or 0.1%

#Make sure I know how it is setting other variables through default settings
set.seed(1990)
x.out_LR_no <- setx(FinalModel, GSAType="Single", DACP_MHI=27297, Incorporated="N", GSA_Int_Percent=0.8095, DACP_Population=424, GW="Y", GSA_eligible_entity="N", ACS_HOL_epercent_2010=57.5, Number_DACs=4.952, Phase1="N")
s.out_LR_no <- sim(FinalModel, x=x.out_LR_no, num=100000)
summary(s.out_LR_no) # is the same so thats what its doing 

#High resources not GSA eligible
set.seed(1990)
x.out_HR_no <- setx(FinalModel, GSAType="Single", DACP_MHI=40000, Incorporated="N", DACP_Population=3769, GSA_eligible_entity="N")
s.out_HR_no <- sim(FinalModel, x=x.out_HR_no, num=100000)
summary(s.out_HR_no) #0.00377 or 0.4%

#Low resources GSA eligible but not incoporated
set.seed(1990)
x.out_LR_GSA <- setx(FinalModel, GSAType="Single", DACP_MHI=27297, Incorporated="N", DACP_Population=424, GSA_eligible_entity="Y")
s.out_LR_GSA <- sim(FinalModel, x=x.out_LR_GSA, num = 100000)
summary(s.out_LR_GSA) # 0.04127 or 4.1%

# High resources GSA eligible but not incorporated
set.seed(1990)
x.out_HR_GSA <- setx(FinalModel, GSAType="Single", DACP_MHI=40000, Incorporated="N", DACP_Population=3769, GSA_eligible_entity="Y")
s.out_HR_GSA <- sim(FinalModel, x=x.out_HR_GSA, num = 100000)
summary(s.out_HR_GSA) # .16556 or 16.6%

#Low resources city
set.seed(1990)
x.out_LR_city <- setx(FinalModel, GSAType="Single", DACP_MHI=27297, Incorporated="Y", DACP_Population=424, GSA_eligible_entity="Y")
s.out_LR_city <- sim(FinalModel, x=x.out_LR_city, num = 100000)
summary(s.out_LR_city) # .14801 or 14.8%

# High resources city
set.seed(1990)
x.out_HR_city <- setx(FinalModel, GSAType="Single", DACP_MHI=40000, Incorporated="Y", DACP_Population=3769, GSA_eligible_entity="Y")
s.out_HR_city <- sim(FinalModel, x=x.out_HR_city, num = 100000)
summary(s.out_HR_city) #.4199 or 42%

# Collaborative GSA scenarios NOTE NEED TO AVERAGE THE THREE SIMULATIONS FOR EACH

#Low resources, not GSA eligible
set.seed(1990)
x.out_CLR_no <- setx(FinalModel, GSAType=c("MOU", "JPA", "AD"), DACP_MHI=27297, Incorporated="N", DACP_Population=424, GSA_eligible_entity="N")
s.out_CLR_no <- sim(FinalModel, x=x.out_CLR_no, num = 100000)
summary(s.out_CLR_no) # MOU = 0.0062, JPA = 0.00349, AD = 0.165 so average = 0.05823 or 5.8%
#weighted average
summary(Data2_noNA$GSAType)
#AD is 14/130, MOU is 31/130 and JPA is 85/130 so weighted average 2.2%

#High resources not GSA eligible
set.seed(1990)
x.out_CHR_no <- setx(FinalModel, GSAType=c("MOU", "JPA", "AD"), DACP_MHI=40000, Incorporated="N", DACP_Population=3769, GSA_eligible_entity="N")
s.out_CHR_no <- sim(FinalModel, x=x.out_CHR_no, num = 100000)
summary(s.out_CHR_no) #AD = 0.40043, JPA = 0.01421 and MOU = 0.02577 so average is = .1468 or 14.7%
#AD is 14/130, MOU is 31/130 and JPA is 85/130 so weighted average 4.3%

#Low resources GSA eligible but not incoporated
set.seed(1990)
x.out_CLR_GSA <- setx(FinalModel, GSAType=c("MOU", "JPA", "AD"), DACP_MHI=27297, Incorporated="N", DACP_Population=424, GSA_eligible_entity="Y")
s.out_CLR_GSA <- sim(FinalModel, x=x.out_CLR_GSA, num = 100000)
summary(s.out_CLR_GSA) # AD = 0.87183, JPA = 0.14895 MOU = 0.23657 so average = 0.4191 or 41.9%
#AD is 14/130, MOU is 31/130 and JPA is 85/130 so weighted average 24.8%

# High resources GSA eligible but not incorporated
set.seed(1990)
x.out_CHR_GSA <- setx(FinalModel, GSAType=c("MOU", "JPA", "AD"), DACP_MHI=40000, Incorporated="N", DACP_Population=3769, GSA_eligible_entity="Y")
s.out_CHR_GSA <- sim(FinalModel, x=x.out_CHR_GSA, num = 100000)
summary(s.out_CHR_GSA) #AD = .95712, JPA = .43748, MOU = .55268 so average = 0.64909 or 64.9%
#AD is 14/130, MOU is 31/130 and JPA is 85/130 so weighted average 52.1%

#Low resources city
set.seed(1990)
x.out_CLR_city <- setx(FinalModel, GSAType=c("MOU", "JPA", "AD"), DACP_MHI=27297, Incorporated="Y", DACP_Population=424, GSA_eligible_entity="Y")
s.out_CLR_city <- sim(FinalModel, x=x.out_CLR_city, num = 100000)
summary(s.out_CLR_city) #AD = .95247, JPA = .40641, MOU = .51576 so average = 0.624876 or 62.5%
#AD is 14/130, MOU is 31/130 and JPA is 85/130 so weighted average 49.1%

# High resources city
set.seed(1990)
x.out_CHR_city <- setx(FinalModel, GSAType=c("MOU", "JPA", "AD"), DACP_MHI=40000, Incorporated="Y", DACP_Population=3769, GSA_eligible_entity="Y")
s.out_CHR_city <- sim(FinalModel, x=x.out_CHR_city, num = 100000)
summary(s.out_CHR_city) #AD = 0.98674, JPA = .73751, MOU = .80635 so average = 0.8435 or 84.4%
#weighted average
#AD is 14/130, MOU is 31/130 and JPA is 85/130 so weighted average 78.1%

#Try graphing these
summary(Data2_noNA$DACP_MHI)
summary(Data2_noNA$DACP_Population)

set.seed(1990)
x.out_TEST <- setx(FinalModel, GSAType="Single", DACP_MHI=33500, Incorporated="N", GSA_Int_Percent=1, DACP_Population=c(33, 429, 1371, 2431, 3832, 9918), GW="Y", GSA_eligible_entity="N", ACS_HOL_epercent=66.5, Number_GSAs=1, Number_DACs=4, Phase1="N")
s.out_TEST <- sim(FinalModel, x=x.out_TEST)
summary(s.out_TEST)

jpeg(filename = "Figures/ppTEST.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_TEST, xlab = "Population", ylab = "Predicted Probability", ci=95) 

dev.off()

set.seed(1990)
x.out_TEST2 <- setx(FinalModel, GSAType="Single", DACP_MHI=33500, Incorporated="Y", GSA_Int_Percent=1, DACP_Population=c(33, 429, 1371, 2431, 3832, 9918), GW="Y", GSA_eligible_entity="Y", ACS_HOL_epercent=66.5, Number_GSAs=1, Number_DACs=4, Phase1="N")
s.out_TEST2 <- sim(FinalModel, x=x.out_TEST2)
summary(s.out_TEST2)

jpeg(filename = "Figures/ppTEST2.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_TEST2, xlab = "Population", ylab = "Predicted Probability", ci=95) 

dev.off()

# average not collaborative
set.seed(1990)
x.out_noA <- setx(FinalModel, GSAType="Single")
s.out_noA <- sim(FinalModel, x=x.out_noA, num=100000)
summary(s.out_noA) #0.2%

# average collaborative
set.seed(1990)
x.out_yesA <- setx(FinalModel, GSAType=c("MOU", "JPA", "AD"))
s.out_yesA <- sim(FinalModel, x=x.out_yesA, num = 100000)
summary(s.out_yesA) # AD = 27.6%, JPA = .69%, MOU = 1.3%

# Figures --------------------------------------------------------

#odds ratio plot

jpeg(filename = "Figures/oddsratio.jpg", width = 7, height = 6, units = "in", res = 100)

logit1coefs <- coef(summary(logit5))
or5 <- logit1coefs[,1]
or5b<-exp(logit1coefs[,1])
ci5a<-exp(logit1coefs[,1]-1.96*logit1coefs[,2])
ci5b<-exp(logit1coefs[,1]+1.96*logit1coefs[,2])


place<-c(0,1,2,3,4,5,6,7,8,9,10,11,12)
place1<-c(1,2,3,4,5,6,7,8,9,10,11,12)


par(mar=c(5,12.75,4,3), family="serif", cex=.8)
plot.new()
plot.window(xlim=c(0,400), ylim=c(0, 13))

points(x=exp(or5[2:13]), y=place1, pch=19)

for(i in 2:13){
  lines(x=c(ci5a[i], ci5b[i]), y=c(place[i],place[i]), lwd=1)
}
axis(1, cex.axis=1.2)
axis(2, labels=c("AD GSA***","JPA GSA**", "MOU GSA**", "MHI*", "Incorporated**","Intersection %***", "Population**","Groundwater reliant","GSA eligible***", "% Latino","Number DACs*", "Facilitation"), at=place1, las=2, cex.axis=1.6)
abline(v=1, lty=2)
box()


dev.off()

# bar chart for predicted probabilities 
value <- as.data.frame(c(0.1, 0.4, 4.1, 16.6, 14.8, 42, 2.1, 3.9, 20.7, 35.5, 34.3, 36.1))
Setting <- c("Non-Collaborative", "Non-Collaborative", "Non-Collaborative", "Non-Collaborative", "Non-Collaborative", "Non-Collaborative", "Collaborative", "Collaborative", "Collaborative", "Collaborative", "Collaborative", "Collaborative")
Resources <- c("Low Resourced", "High Resourced", "Low Resourced", "High Resourced", "Low Resourced", "High Resourced", "Low Resourced", "High Resourced", "Low Resourced", "High Resourced", "Low Resourced", "High Resourced")
type <- c("Not GSA eligible", "Not GSA eligible", "GSA eligible", "GSA eligible", "Incorporated", "Incorporated", "Not GSA eligible", "Not GSA eligible", "GSA eligible", "GSA eligible", "Incorporated", "Incorporated")
together2 <- cbind(value, Setting, Resources, type)
colnames(together2)[1] <- "value"

together2$type <- relevel(together2$type, ref = "Not GSA eligible")
together2$Resources <- relevel(together2$Resources, ref = "Low Resourced")

ggplot(data = together2, aes( x = type, y = value, fill = Setting )) +
  geom_bar( stat = 'identity' ) + facet_wrap(~Resources) + ylab("Predicted Probability of Participation") + ylim(0,100) + xlab("")

# old packages not sure I need
#library(nnet)
#library(reshape2)
#library(QRM)