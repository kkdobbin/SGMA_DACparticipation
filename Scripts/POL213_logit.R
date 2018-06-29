# POL 213 logit model

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

facilitation$Basin <- as.factor(facilitation$Basin)
facilitation$Phase1 <- as.factor(facilitation$Phase1)
facilitation <- select(facilitation, -8)


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

# subset data to only what will be used for model -------------------------

# update to only small DACs based on updated ACS pop and MHI data
table(Data$ACS_pop_e > 10000) # 1
table(Data$MHI_e > 54192) # 17 here that need to be discarded
Data$MHI_e[is.na(Data$MHI_e)] <- 0
Data$ACS_pop_e[is.na(Data$ACS_pop_e)] <- 0
Dataupdated <- Data 
Dataupdated <- Dataupdated %>% filter(MHI_e < 54192) %>% filter(ACS_pop_e < 10000)
sum(table(unique(Dataupdated$DACP_IDNumber)))  # down to 272 observations with 228 unique DACPs
Dataupdated$MHI_e[Dataupdated$MHI_e == 0] <- NA 
Data$MHI_e[Data$MHI_e == 0] <- NA 
Dataupdated$ACS_pop_e[Dataupdated$ACS_pop_e == 0] <- NA 
Data$ACS_pop_e[Data$ACS_pop_e == 0] <- NA

# New dataset with only variables I care about
Data2 <- Dataupdated %>% select(DACP_IDNumber, DACP_Name, DV1, ACS_pop_e, MHI_e, Number_GSAs, Incorporated, AnyPilot, GSAType, ACS_whitenotHOL_epercent, ACS_HOL_epercent, GW, DACP_MHI, DACP_Population, GSAcol, GSA_eligible_entity, GSA_Int_Percent, Phase1)
sum(table(unique(Data2$DACP_IDNumber))) # still 272 AND 228 unique!

# Make variable types right again
Data2$DACP_IDNumber <- as.factor(Data2$DACP_IDNumber)
Data2$DV1 <- as.factor(Data2$DV1)
Data2$GSAcol <- as.factor(Data2$GSAcol)

# subset data to only those DACs that use GW (but not exclusively GW necessarily)
Data3 <- Data2 %>% filter(GW == "Y") #brings it down to 210 observations
sum(table(unique(Data3$DACP_IDNumber))) #with 182 unique DACPs

#subset data to only those DACs that use GW and have GSA eligible entities
Data4 <-  Data3 %>% filter(GSA_eligible_entity == "Y") # brings down to 98 obs
sum(table(unique(Data4$DACP_IDNumber))) #83 unique DACPs

# Descriptive stats to compare dataset options --------------------------------------

table(Data2$DV1)
table(Data3$DV1)
table(Data4$DV1)

# bar graph of DV1
ggplot(data = Data2, aes(x = DV1)) + geom_bar() + theme_classic() + ylab("Number of small DACs") + xlab("") + scale_x_discrete(labels=c("0" = "Not Participating", "1" = "Participating")) +
  theme(axis.text.x = element_text(size=12, color = "black")) + 
  theme(axis.title.x=element_text(size=12)) + 
  theme(axis.title.y=element_text(size=12)) + 
  theme(axis.text.y = element_text(size=14, color = "black")) + 
  theme(plot.title = element_text(hjust = 0.5, size=16))

ggsave("Figures/fig1notitle.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

# Logits ------------------------------------------------------------------

# Logit model (no subsetting data) for 0 - 1 logit (zeros versus everything else), includes controls in DVs
logit1 <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit1) #33 obs deleted due to missingness mostly due to MHI_e variable

# try including GSA_Int_percent (makes it better)
logit11 <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + Number_GSAs + GSA_Int_Percent + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit11)

# Try getting rid of number of GSAs (do this for simplicity)
logit12 <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + GSA_Int_Percent + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit12)

# try logit 1 with GSAType instead of GSAcol
logit1b <- glm(DV1 ~ GSAType + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit1b)

# Logit model for just GW dependent DACs 
logit2 <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + ACS_whitenotHOL_epercent, data = Data3, family = binomial(link="logit"))
summary(logit2) 

# Logit 2 adding in GSA eligible entity as DV
logit2b <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data3, family = binomial(link="logit"))
summary(logit2b) 

# logit 2 with GSAtype instead of GSAcol
logit2c <- glm(DV1 ~ GSAType + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + ACS_whitenotHOL_epercent, data = Data3, family = binomial(link="logit"))
summary(logit2c) 

# logit 2b with GSAtype instead of GSAcol
logit2d <- glm(DV1 ~ GSAType + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data3, family = binomial(link="logit"))
summary(logit2d) 

# logit for DACs that use GW and have GSA eligible entities
logit3 <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + ACS_whitenotHOL_epercent, data = Data4, family = binomial(link="logit"))
summary(logit3) 

# logit 3 with GSAtype instead of GSAcol
logit3b <- glm(DV1 ~ GSAType + MHI_e + Incorporated + AnyPilot + Number_GSAs + ACS_pop_e + ACS_whitenotHOL_epercent, data = Data4, family = binomial(link="logit"))
summary(logit3b) 


# Compare logits ----------------------------------------------------------

# pseudo R2

pR2(logit1b) # pscl package
pR2(logit2d)
pR2(logit3b) 
pR2(logit3) 
pR2(logit1) 

pR2(logit12)


# odds ratio
#logit1b
oddratios <- exp(coef(logit1b))
print(oddratios)
round(exp(cbind(Estimate=coef(logit1b), confint(logit1b))), 2)
#logit2d
round(exp(cbind(Estimate=coef(logit2d), confint(logit2d))), 2) # pretty similar
#logit3b
round(exp(cbind(Estimate=coef(logit3b), confint(logit3b))), 2) # something goes wrong here
#logit3
round(exp(cbind(Estimate=coef(logit3), confint(logit3))), 2)
#logit1
round(exp(cbind(Estimate=coef(logit1), confint(logit1))), 2)
oddratios <- exp(coef(logit1))
print(oddratios)
#logit12
round(exp(cbind(Estimate=coef(logit12), confint(logit12))), 2)
oddratios <- exp(coef(logit12))
print(oddratios)


# final model -------------------------------------------------------------

# Logit 1! it is 

# logit 1
logit1 <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + GSA_Int_Percent + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logit1)

# Try getting rid of any pilot
logita <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + GSA_Int_Percent + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logita) # weak reason to do this

# try getting rid of GW
logitb <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + GSA_Int_percent + ACS_pop_e + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = Data2, family = binomial(link="logit"))
summary(logitb) # worse

# what about adding in number of GSAs? 
logitc <- glm(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + GSA_Int_Percent + ACS_pop_e + GSA_eligible_entity + ACS_whitenotHOL_epercent + Number_GSAs, data = Data2, family = binomial(link="logit"))
summary(logitc) # worse

# stay with full logit 1

# logit with just controls for comparison
logitcontrol <- glm(DV1 ~ GSA_Int_Percent + GW + GSA_eligible_entity, data = Data2, family = binomial(link="logit"))
summary(logitcontrol)

# Diagnostics -------------------------------------------------------------

# Pseudo R squared and AIC ------------------------------------------------

#pseudo R squared
pR2(logit1)  
pR2(logitcontrol)

# LogLik
logLik(logit1)
logLik(logitcontrol)

nullmod <- glm(DV1~1, data = Data2, family="binomial")
1-logLik(logit1)/logLik(nullmod)

lrtest(logit1, nullmod)
lrtest(logit1, logitcontrol)

print(logit1) #rms package

BIC(logit1)
BIC(nullmod)
BIC(logitcontrol)

# Odds ratios -------------------------------------------------------------

# Odds Ratios
oddratios <- exp(coef(logit1))
print(oddratios)
round(exp(cbind(Estimate=coef(logit1), confint(logit1))), 2)

#another way to get odds ratios
exp(logit1$coefficients[-1])

# CLassification tables and PRE -------------------------------------------

# CLassification table
classlogit1 <- data.frame(response = Data2_noNA, predicted = round(fitted(logit1),0))
xtabs(~ predicted + response.DV1, data = classlogit1)
# model sensitivity is how often correclty predicting success outcome so 30/38 so 79% (true positive rate)
# model specificity is how often predicting true negatives so 187/201 so 93% (true negative rate)
# total number correct is 217 out of 239 or 91%
nullmodel <- glm(DV1 ~ 1, data = Data2_noNA, family = binomial(link="logit"))
print(nullmodel)
classnullmodel <- data.frame(response = Data2_noNA$DV1, predicted = round(fitted(nullmodel),0))
xtabs(~ predicted + response, data = classnullmodel)
#PRE (proportion reduction in error) is number correctly classified in model (217) minus number correctly classified in null model (195) divided by total n (239) minus number correctly classified in null model (195) so .5 or 50%! 

# using DAMisc package
pre(logit1, sim=TRUE, R=1000) # simulated median PRE is .409 with a confidence interval of .186 to .442 so thats good I think! aalystical restul PRE is .5 same as what I calculated


# ROC curve ---------------------------------------------------------------

#Data2 without NAs
Data2_noNA <- Data2
Data2_noNA  <- select(Data2_noNA, -13)
Data2_noNA <- na.omit(Data2_noNA)

FinalModel <- zelig(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + GSA_Int_Percent + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, model="logit", data = Data2_noNA)
summary(FinalModel)

z.null <- zelig(DV1 ~ 1, model = "logit", data = Data2); z.null
z.controls <- zelig(DV1 ~ GSA_Int_Percent + GSA_eligible_entity + GW, model = "logit", data = Data2); z.controls

rocplot(FinalModel, z.null, plot = F) # area under curve for model logit b is .94 excellent predictive power
rocplot(FinalModel, z.null, plot = T)

rocplot(FinalModel, z.controls, plot = F) # improvement over just controls in .835 to .94



# Odds Ratio Plots --------------------------------------------------------

#odds ratio plot just non control variables

jpeg(filename = "Figures/oddsratio.jpg", width = 7, height = 6, units = "in", res = 100)

logit1coefs <- coef(summary(logit1))
logit1coefs2 <- logit1coefs[-6, ]
logit1coefs2 <- logit1coefs2[-7, ]
logit1coefs2 <- logit1coefs2[-7, ]
or5 <- logit1coefs2[,1]
or5b<-exp(logit1coefs2[,1])
ci5a<-exp(logit1coefs2[,1]-1.96*logit1coefs2[,2])
ci5b<-exp(logit1coefs2[,1]+1.96*logit1coefs2[,2])


place<-c(0,1,2,3,4,5,6,7,8,9,10,11,12)
place1<-c(1,2,3,4,5,6,7,8,9,10,11,12)


par(mar=c(5,12.75,4,3), family="serif", cex=.8)
plot.new()
plot.window(xlim=c(0,15), ylim=c(0, 6))

points(x=exp(or5[2:13]), y=place1, pch=19)

for(i in 2:13){
  lines(x=c(ci5a[i], ci5b[i]), y=c(place[i],place[i]), lwd=1)
}
axis(1)

#labels
axis(2, labels=c("Multi-agency GSA**","MHI**", "Incoporation*","DAC Pilot Study", "Population*","Percent White"), at=c(1,2,3,4,5,6), las=2, cex.axis=1.6)
axis(1, cex.axis=1.2)
abline(v=1, lty=2)
mtext("Odds Ratio Plot", font=2, side=3, line=2, cex=2)
mtext("Model of DAC Participation", font=1, side=3, line=.25, cex=1.5)
mtext("Odds Ratio", side=1, line=3, cex = 1.4)
box()


dev.off()


# Try to scale variables to improve interpretation --------------------------------------------------

#scale population and MHI
DataScaled <- Data2
DataScaled$MHI_e_scaled <- (DataScaled$MHI_e / 10000)
DataScaled$ACS_pop_e_scaled <- (DataScaled$ACS_pop_e / 1000)

# logit 1 but with scalled data
logitScaled <- glm(DV1 ~ GSAcol + MHI_e_scaled + Incorporated + AnyPilot + GSA_Int_Percent + ACS_pop_e_scaled + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent, data = DataScaled, family = binomial(link="logit")) 
summary(logitScaled)

#plot

jpeg(filename = "Figures/oddsratioScaled.jpg", width = 7, height = 6, units = "in", res = 100)

logitScoefs <- coef(summary(logitScaled))
logitScoefs2 <- logitScoefs[-6, ]
logitScoefs2 <- logitScoefs2[-7, ]
logitScoefs2 <- logitScoefs2[-7, ]
or5 <- logitScoefs2[,1]
or5b<-exp(logitScoefs2[,1])
ci5a<-exp(logitScoefs2[,1]-1.96*logitScoefs2[,2])
ci5b<-exp(logitScoefs2[,1]+1.96*logitScoefs2[,2])


place<-c(0,1,2,3,4,5,6,7,8,9,10,11,12)
place1<-c(1,2,3,4,5,6,7,8,9,10,11,12)


par(mar=c(5,18,4,3), family="serif", cex=.8)
plot.new()
plot.window(xlim=c(0,15), ylim=c(0, 6))

points(x=exp(or5[2:13]), y=place1, pch=19)

for(i in 2:13){
  lines(x=c(ci5a[i], ci5b[i]), y=c(place[i],place[i]), lwd=1)
}
axis(1)

#labels
axis(2, labels=c("Multi-agency GSA*","MHI (/$10,000)*", "Incoporation*","DAC Pilot Study", "Population (/1,000 people)*","Percent White"), at=c(1,2,3,4,5,6), las=2, cex.axis=1.6)
axis(1, cex.axis=1.2)
abline(v=1, lty=2)
mtext("Odds Ratio", side=1, line=3, cex = 1.4)
box()

dev.off()

# Model stats using scaled coefficients -----------------------------------

#odds ratios
exp(logitScaled$coefficients[-1])
exp(logit1$coefficients[-1])


# Look for interaction effect ---------------------------------------------

z.out2 <- zelig(DV1 ~ GSAcol + MHI_e + Incorporated + AnyPilot + GSA_Int_Percent + ACS_pop_e + GW + GSA_eligible_entity + ACS_whitenotHOL_epercent + ACS_whitenotHOL_epercent*Incorporated, model="logit", data=Data2)
summary(z.out2)

# Confirm if there is an interaction effect using graphing
summary(Data2$ACS_whitenotHOL_epercent)
x.out_interaction1 <- setx(z.out2, ACS_whitenotHOL_epercent = c(0, 8.675, 30.665, 48.85, 100), Incorporated = "N")
x.out_interaction2 <- setx(z.out2, ACS_whitenotHOL_epercent = c(0, 8.675, 30.665, 48.85, 100), Incorporated = "Y")

s.out_interaction <- sim(z.out2, x = x.out_interaction1, x1 = x.out_interaction2); s.out_interaction
ci.plot(s.out_interaction, xlab = "Percent White", ylab = "Predicted Probability of Relying on Surface Water", main = "Interaction Effect of Incoporation and Demographics", ci=c(95))


# Predicted probabilities ------------------------------------------------------

summary(logit1)

# odds ratios
Odds <- exp(cbind("Odds ratio" = coef(logit1), confint.default(logit1, level = 0.95))); Odds

# odds of participating if unincorporated 
exp(1.568*0)
# ods of participating if incorporated
exp(1.568*1)

# percent change in odds from unincporated (base category) to incorporated
((exp(1.568*1) - exp(1.568*0))/exp(1.568*1))*100 # increases odds by 79% (I think this is wrong)
((exp(1.568*1) - exp(1.568*0))/exp(1.568*0))*100 # increases odds by nearly 380%

# percent change in odds from noncollaborative (base category) to colaborative GSA
((exp(1.526*1) - exp(1.526*0))/exp(1.526*0))*100 # increases odds by 360%

# testing hand calculating predicted probs for continuous
exp(-1.290e+01 + (8.973e-05 * 28576) + 1.526 + 3.456 + (2.145e-04 * 1965) + 2.131 + 2.825 + (-1.680e-02 * 20.8))/ (1+exp(-1.290e+01 + (8.973e-05 * 28576) + 1.526 + 3.456 + (2.145e-04 * 1965) + 2.131 + 2.825 + (-1.680e-02 * 20.8))) # 0.4192573

# predicted probability plots

#MHI predicted probabilities
summary(Data2$MHI_e)
table(Data2$Incorporated)
table(Data2$GSAcol)
table(Data2$AnyPilot)
summary(Data2$GSA_Int_Percent)
summary(Data2$ACS_pop_e)
summary(Data2$ACS_whitenotHOL_epercent)

x.out_MHI <- setx(FinalModel, MHI_e=c(14803, 28842, 34939, 40568, 53250), Incorporated = "N", GSAcol = 1, AnyPilot = "N", GSA_Int_Percent = 1, ACS_pop_e = 1450, GW = "Y", GSA_eligible_entity = "Y",  ACS_whitenotHOL_epercent = 24.20)
s.out_MHI <-sim(FinalModel, x=x.out_MHI)
summary(s.out_MHI)
jpeg(filename = "Figures/ppMHI.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_MHI, xlab = "MHI", ylab = "Predicted Probability", main = "Effect of MHI", ci=95, ylim = (c(0,1)), xlim = c(0,54000), leg = 0)

dev.off()

# plot no title
jpeg(filename = "Figures/ppMHInotitle.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_MHI, xlab = "MHI", ylab = "Predicted Probability", ci=95, ylim = (c(0,1)), xlim = c(0,54000), leg = 0)

dev.off()

# percent change in odds of MHI Q1 to Q3

ppMHI_q1 <- .492
ppMHI_q3 <- .661

(ppMHI_q1 - ppMHI_q3)/ppMHI_q1
(ppMHI_q3 - ppMHI_q1)/ppMHI_q3

#MHI predicted probabilities for incorporated communities test # numbers not updated for data changes
summary(Data2$MHI_e)
table(Data2$Incorporated)
table(Data2$GSAcol)
table(Data2$AnyPilot)
summary(Data2$GSA_Int_Percent)
summary(Data2$ACS_pop_e)
summary(Data2$ACS_whitenotHOL_epercent)

x.out_MHI2 <- setx(FinalModel, MHI_e=c(14803, 28576, 34750, 40410, 53250), Incorporated = "Y", GSAcol = 1, AnyPilot = "N", GSA_Int_Percent = 1, ACS_pop_e = 1965, GW = "Y", GSA_eligible_entity = "Y",  ACS_whitenotHOL_epercent = 20.8)
s.out_MHI2 <-sim(FinalModel, x=x.out_MHI2)
summary(s.out_MHI)
jpeg(filename = "Figures/ppMHI2.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_MHI2, xlab = "MHI", ylab = "Predicted Probability", main = "Effect of MHI", ci=95, ylim = (c(0,1)), xlim = c(0,54000), leg = 0)

dev.off()

# pop
summary(Data2$MHI_e)
summary(Data2$ACS_pop_e)
x.out_pop <- setx(FinalModel, MHI_e=34046, Incorporated = "N", GSAcol = 1, AnyPilot = "N", GSA_Int_Percent = 1, ACS_pop_e = c(19, 433, 2431, 3821, 9566), GW = "Y", GSA_eligible_entity = "Y",  ACS_whitenotHOL_epercent = 24.20)
s.out_pop <-sim(FinalModel, x=x.out_pop)
summary(s.out_pop)

jpeg(filename = "Figures/ppPop.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_pop, xlab = "Population", ylab = "Predicted Probability", main = "Effect of Population Size", ci=95, ylim = (c(0,1)), xlim = c(0, 10000), leg = 0, cex.lab = 3) 

dev.off()

# without title
jpeg(filename = "Figures/ppPopnotitle.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_pop, xlab = "Population", ylab = "Predicted Probability", ci=95, ylim = (c(0,1)), xlim = c(0, 10000), leg = 0, cex.lab = 3) 

dev.off()

# percent change in odds of population Q1 to Q3
ppPop_q1 <- 0.507
ppPop_q3 <- 0.714

(ppPop_q1 - ppPop_q3)/ppPop_q1
(ppPop_q3 - ppPop_q1)/ppPop_q3


# incorporation
x.out_inc <- setx(FinalModel, MHI_e=34046, Incorporated = c("N", "Y"), GSAcol = 1, AnyPilot = "N", GSA_Int_Percent = 1, ACS_pop_e = 1450, GW = "Y", GSA_eligible_entity = "Y",  ACS_whitenotHOL_epercent = 24.20)
s.out_inc <-sim(FinalModel, x=x.out_inc)
summary(s.out_inc)

jpeg(filename = "Figures/ppinc.jpg", width = 5, height = 4, units = "in", res = 100)

par(mar=c(5,5,4,3), family="serif", cex=1.3)
ci.plot(s.out_inc, xlab = "Population", ylab = "Predicted Probability", main = "Effect of Population Size", ci=95) 

dev.off()

ppinc_n <- 0.563
ppinc_y <- 0.805

(ppinc_y - ppinc_n)/ppinc_y

# percent intersection
summary(Data2_noNA$GSA_Int_Percent)

x.out_int <- setx(FinalModel, MHI_e=34046, Incorporated = "N", GSAcol = 1, AnyPilot = "N", GSA_Int_Percent = c(0.10, 0.75, 0.82, 1), ACS_pop_e = 1450, GW = "Y", GSA_eligible_entity = "Y",  ACS_whitenotHOL_epercent = 24.20)
s.out_int <-sim(FinalModel, x=x.out_int)
summary(s.out_int)

ppint_q1 <- 0.39
ppint_q3 <- 0.574

(ppint_q3 - ppint_q1)/ppint_q3

# Goodness of fit test ----------------------------------------------------

# http://thestatsgeek.com/2014/02/16/the-hosmer-lemeshow-goodness-of-fit-test-for-logistic-regression/
#Hosmer- Lemeshow goodness of fit test

hl <- hoslem.test(Data2$DV1, fitted(logit1), g=10); hl
logit1$DV1
fitted(logit1)
fittedadjusted <- ifelse(fitted(logit1)<0.5, 0, 1)
hl2 <- hoslem.test(Data2$DV1, fittedadjusted, g=10); hl2

hl <- hoslem.test(Data2_noNA$DV1, fitted(logit1), g=10); hl
logit1$DV1
fitted(logit1)
fittedadjusted <- ifelse(fitted(logit1)<0.5, 0, 1)
hl2 <- hoslem.test(Data2$DV1, fittedadjusted, g=10); hl2


# Descriptive stats for paper ---------------------------------------------

# Figure out how many joined to more than one
tableunique <- as.data.frame(table(Data2$DACP_IDNumber)) #228
count(tableunique %>% filter(tableunique$Freq == 1)) #186
count(tableunique %>% filter(tableunique$Freq >1)) # 42 DACPs joined with more than one GSA. 
count(tableunique %>% filter(tableunique$Freq == 2)) #40 joined with two
count(tableunique %>% filter(tableunique$Freq == 3)) #2 joined with three
max(tableunique$Freq)

# Data exploration for paper ----------------------------------------------

table(Data2_noNA$DV1)
table(Data2_noNA$GSAcol)
summary(Data2_noNA$MHI_e)
plot(x=Data2_noNA$MHI_e, y=Data2_noNA$DV1)
table(Data2$Incorporated)
table(Data2$DV1)

# Combined plot for pp plots ----------------------------------------------

jpeg(filename = "Figures/ppcombined.jpg", width = 8, height = 4, units = "in", res = 100)

par(mar=c(5,3,4,3), family="serif", cex=1.3, mfrow=c(1,2))
ci.plot(s.out_MHI, xlab = "MHI", ylab = "Predicted Probability", ci=95, ylim = (c(0,1)), xlim = c(0,54000), leg = 0)


ci.plot(s.out_pop, xlab = "Population", ylab = "Predicted Probability", ci=95, ylim = (c(0,1)), xlim = c(0, 10000), leg = 0, cex.lab = 3) 

dev.off()
