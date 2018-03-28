# Final Project Process

#Set up
library(dplyr)
library(tidyverse)
library(foreign)
library(car)
library(MASS)
library(lmtest)
library(tseries)
library(broom)

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
# removign this because it is duplicating my GSA data when they are in more than one IRWM! Data <- left_join(Data, IRWM, by = c("DACP_IDNumber" = "DACPlace_ID_Number"))

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
# removing this because I took out IRWM Data$Any_Pilot <- as.factor(Data$Any_Pilot)
Data$AdvisoryCommittee <- as.factor(Data$AdvisoryCommittee)

# Make MOU and MOA the same
Data$GSAType[Data$GSAType == "MOA"] <- "MOU"
table(Data$GSAType)
Data$GSAType <- factor(Data$GSAType)
table(Data$GSAType)

# NEED TO DEAL WITH MHI zeros
Data$DACP_MHI[Data$DACP_MHI == 0] <- NA 

# NEED TO DEAL WITH POP zero
Data$DACP_Population[Data$DACP_Population == 0] <- NA

#Data has 286 observations total before limiting it any more than just small DACs and intersections ten percent or greater. Plus nine NAs. ANy other NAs or observations removed? what is the total universe here? narrowed it down to have just 9 NAs (one observation has NAs in two categories)

# Figure out how many unique DACP in this 286 master dataset
uniqueDACPData  <- unique(Data$DACP_IDNumber)
length(uniqueDACPData) #243

# Figure out how many joined to more than one
tableunique <- as.data.frame(table(Data$DACP_IDNumber))
count(tableunique %>% filter(tableunique$Freq >1)) # 39 DACPs joined with more than one GSA. 
count(tableunique %>% filter(tableunique$Freq == 2)) #35 joined with two
max(tableunique$Freq)

# Limit to those that have GSA eligible entities
Data <- Data %>% filter(Data$GSA_eligible_entity == "Y")

#Lets make a new dataframe with just the variables I care about to figure this out
Variables <- Data %>% dplyr::select(DACP_MHI, GSAType, DACP_Population, Percent_latino, Percent_white_not_hispanic, AREAofDACP_a_, GSA_Int_Percent, Listed_IP, Incorporated, AdvisoryCommittee)
Variables2 <- na.omit(Variables) # ugh there is tons of NAs in IP varaible due to decision makers 

# Change NA is Listed_IP variable to yes's
Data$Listed_IP[is.na(Data$Listed_IP)] <- "Y"

#now a lot fewer NAs but three in advisory committee column that should be Ns
Data$AdvisoryCommittee[is.na(Data$AdvisoryCommittee)] <- "N"
table(Data$AdvisoryCommittee)

# goign back to variables dataframe now and omitting NAs there is only nine lost which are the 9 with MHI of zero (another one had pop zero but also have MHI zero)

#Ends with final data set with 121 observations, one of which has an NA

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

#Deal with those DACPs not in IRWMs DONT NEED TO DO BECAUSE ELIMINATED IRWM
# Data$Any_Pilot[is.na(Data$Any_Pilot)] <- "N"
#table(Data$Any_Pilot)

# Data Visualization and Exploration --------------------------------------

# DV histogram
hist(Data$DV, breaks = 6, xlab = "6 Category Participation Variable", main = "Histogram of Dependent Variable", ylim = c(0, 100)) 

# IV individual Scatterplots and transformations

# MHI
scatterplot(Data$DV ~ Data$DACP_MHI) # scewed
Data$Trans_DACP_MHI <- (Data$DACP_MHI)^3
scatterplot(Data$DV ~ (Data$Trans_DACP_MHI))

# Population
scatterplot(Data$DV ~ Data$DACP_Population) # scewed
Data$Trans_DACP_Population <- (Data$DACP_Population)^2
scatterplot(Data$DV ~ Data$Trans_DACP_Population)
            
# GSA Type
scatterplot(Data$DV ~ Data$GSAType)

# DEM
scatterplot(Data$DV ~ Data$Percent_white_not_hispanic)
scatterplot(Data$DV ~ Data$Percent_latino)

# scatterplot(Data$DV ~ Data$Any_Pilot) eliminated IRWM
scatterplot(Data$DV ~ Data$AREAofDACP_a_) # scewed Only one that maybe isn't a monotone but close

# percent int
scatterplot(Data$DV ~ Data$GSA_Int_Percent)

# area of DACP
scatterplot(Data$DV ~ Data$AREAofDACP_a_) # major outlier
xoutData <- as.data.frame(Data %>% filter(Data$AREAofDACP_a_ < 20000)) # identify outlier
scatterplot(xoutData$DV ~ xoutData$AREAofDACP_a_)

# IP
scatterplot(Data$DV ~ Data$Listed_IP) 

#Incorproated
scatterplot(Data$DV ~ Data$Incorporated)

# Advisory committee
scatterplot(Data$DV ~ Data$AdvisoryCommittee)

# exploring pairwise graphs 
pairs(Variables) #dammit that is impossible to read

# DAC more social characteristics
VariablesDAC <- Data %>% select(DACP_MHI, DACP_Population, Percent_latino, Any_Pilot, AREAofDACP_a_, Incorporated)
pairs(VariablesDAC)

# More GSA level characteristics
VariablesGSA <- Data %>% select(GSAType, GSA_Int_Percent, Listed_IP, AdvisoryCommittee, GSA_eligible_entity)
pairs(VariablesGSA)

# scatterplot matrices
pairs(~ Data$GSA_Int_Percent + Data$DACP_MHI + Data$DACP_Population + Data$Percent_latino)
with(Data, pairs(~ GSA_Int_Percent + DACP_MHI + DACP_Population + Percent_latino))

with( Data, scatterplotMatrix(~ DV + GSA_Int_Percent + Trans_DACP_MHI + DACP_Population + Percent_latino + Data$AREAofDACP_a_, Data = Data, main = "Scatterplot Matrix")) # promising?

with( Data, scatterplotMatrix(~ DV + GSA_Int_Percent + Trans_DACP_MHI + DACP_Population + Percent_latino + Data$AREAofDACP_a_ + GSAType + Incorporated, Data = Data, main = "Scatterplot Matrix"))

# Spread level plots. Can't get this to work so ignore
spreadLevelPlot(DV + 1 ~ Incorporated, Data)
boxplot(DV ~ Incorporated, Data)
spreadLevelPlot(DV + 1 ~ Incorporated, Data)

spreadLevelPlot(DV + 1 ~ GSAType, Data)

table(Data$Incorporated)
table(Data$GSAType)

# OLS --------------------------------------------------

# DV
OLS <- lm(Data$DV ~ Data$GSAType + Data$GSA_Int_Percent + Data$Trans_DACP_MHI + Data$DAC_Population + Data$Incorporated + Data$Percent_latino + Data$AREAofDACP_a_)
summary(OLS)

OLS2 <- lm(Data$DV ~ Data$GSAType + Data$GSA_Int_Percent + Data$Trans_DACP_MHI + Data$DAC_Population + Data$Incorporated)
summary(OLS2)

#just to test function form against one without transformed MHI
OLS3 <- lm(Data$DV ~ Data$GSAType + Data$GSA_Int_Percent + Data$DACP_MHI + Data$DAC_Population + Data$Incorporated)
summary(OLS3)

tidy(OLS2)
write_csv(tidy(OLS2), "Outputs/coefs_OLS2.csv")

# F test to compare

var.test(OLS, OLS2)

# More Data Visualization and Exploration --------------------------------------

# Normality

# Distribution of residuals
sresid <- studres(OLS2) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=120) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) # normal

jarque.bera.test(OLS2$residuals) # because it is significant everythign is now okay not normally distributed

shapiro.test(OLS2$residuals) # Shapiro-wilk test. 

# Quantile comparison plots
qq.plot(OLS2, main = "Quantile-Comparison Plot")

#just for fun to see how this is different than the non transformed version
qq.plot(OLS3) # not much different. 

# Linearity: the expected mean value of residuals is 0

mean(sresid) # All looks well?

# reset test
Data2 <- Data %>% filter(!is.na(Data$DACP_MHI))

Data2$fit <- OLS2$fitted.values # in original data set save fitted values
reset <- lm(Data2$DV ~ Data2$GSAType + Data2$GSA_Int_Percent + Data2$DACP_MHI + Data2$DACP_Population + Data2$Incorporated + I(Data2$fit^2) + I(Data2$fit^3))
summary(reset)
anova(OLS2, reset) # compare original model to new model with added fitted vallues (squares and cubes) see if non-linear combinations of predictors explain response. if you can reject the null (0), model is misspecifieds. # if it is significant we are worried it isn't linear. In this case we are okay if we are using .05 as threshold # Not linear fuck

resettest(OLS2, power = 2:3, type = c("fitted", "regressor", "princomp"))
resettest(OLS3, power = 2:3, type = c("fitted", "regressor", "princomp")) # cool to see it is worse

# Nonstochastic regressors - x values are independent of the error term

residualPlots(OLS2, terms = ~ ., type = "rstudent", fitted = F) # Overall on the outlier front looking okay. doesn't seem like a ton more than 5% are falling outside of the plus or minus two range. Only significant result for lack of fit test is Percent_latino. For the distribution of residuals. Things look good except maybe not GSAType, incorporated or percent latino. Only percent latino is significant for lack of fit test. 

residualPlots(OLS2, terms = ~ 1, type = "rstudent", fitted = T) # I have no idea how to interpret this one. 

marginalModelPlot(OLS2)

avPlots(OLS2)

# homoscedacitity

# plot residuals on fitted values (if spread of residuals differs, may have heteroskedasticity)
plot(OLS2$residuals ~ OLS2$fitted.values, xlab="Fitted Values", ylab="Residuals") 

### heteroskedasticity ###

bptest(OLS2, studentize=F) # Breusch-Pagan test  if is is significant it is heteroskedastic 
bptest(OLS2) # isn't anymore when I take out the F studentized part....

# with(Data, bwplot(DV ~ GSAType))

# Multicolinearity
vif(OLS2) # NOT A PROBLEM! YAY!

# Outliers and influential observations

# studentized residuals
plot(rstudent(OLS2))
table(rstudent(OLS2) > 2)
table(rstudent(OLS2) < -2)

# Exploring leverage
head(hatvalues(OLS2))
influenceIndexPlot(OLS2, vars= "hat", id.n=4)
mean(hatvalues(OLS2))
table(hatvalues(OLS2) > mean(hatvalues(OLS2))*2)
table(hatvalues(OLS2) > mean(hatvalues(OLS2))*3) # looking good but just realized this isn't what they mean by average h bar.... h bar is equal to k plus 1 / n
# Grace gave a different cut off so here that is
abline(h = 2 * length(coef(OLS2)) / nrow(Data2)) # cutoff for hat values calculated based on # of estimates and observations. 

# exploring influence
head(cooks.distance(OLS2)) 
max(cooks.distance(OLS2))
influenceIndexPlot(OLS2, vars="Cook", id.n=5) 
cutoff <- 4/(120-5-1) # cutoff point = 4/(n-k-1). # Change k here
abline(h = 4/(120-5-1))
plot(OLS2, which=4, cook.levels=cutoff)
# cutoff for cook's generally 1 but can miss influential data about 17 above the cut off line here

# all together
influenceIndexPlot(OLS2, vars=c("Cook", "Studentized", "hat"), id.n=5) 
# cutoff for studentized residuals is generally abs(3) (absolute value)

influencePlot(OLS2, id.n=2, main = "Bubble Plot") # only one verticle dashed line because 3h cut off for hat values is outside of all observations. circle is proportional to cook's D

# Predict values
predicted <- predict(OLS2)
range(predicted)
table(predicted > 5)
table(predicted < 0)

# Transformed OLS ---------------------------------------------------------

#Trying to either transform Y down or X up to get rid of bulging
OLS2 <- lm( sqrt(Data$DV) ~ Data$GSAType + Data$GSA_Int_Percent + Data$DACP_MHI + Data$DAC_Population + Data$Incorporated + Data$Percent_latino + Data$Any_Pilot)
summary(OLS2)
qq.plot(OLS2) # That looks worse

OLS3 <- lm(Data$DV ~ Data$GSAType + Data$GSA_Int_Percent + Data$DACP_MHI^2 + Data$DAC_Population^2 + Data$Incorporated + Data$Percent_latino + Data$Any_Pilot)
summary(OLS3)
qq.plot(OLS3) # Doesn't seem to have done much 

var.test(OLS, OLS2) # F test to compare two variances - how to interpret? 

