
#######################################################################################################
################################ Geely Auto Market Research Assignment ##############################

##### Business Objective: To enter US Market

##### Strategy:  To understand the factors on which car price depends
#              -- Identify variables that are significant in predicting the price of a car
#              -- How well these variables describe the price of a car

#### Goal: To model the price of cars with the available independent variables.
#              -- Understand how price vary with the independent Variables
#              -- Manipulate Car Design , Business strategy accordingly to meet certain price levels
#              -- Understand the pricing dynamics of US market.


#### Data : CarPrice_Assignment.csv includes all the data required.
#           Data dictionary is also available

#---------------------------------------------------------------------

#Set the directory Path

setwd("C:/Users/Pritha/Desktop/DS C7 UPGRAD IIITB COURSE/Course 3/Assignment")
getwd()


#**** Load the required packages ****

library(dplyr)       # Manipulate data in dataframe
library(tidyr)       # Data tidying
library(DataExplorer)# Visualise data structure
library(ggplot2)     # Plotting the data
library(gridExtra)   # arrange the grids
library(stringr)     # Splitting the Data
library(corrplot)    # Correlation Plot
library(MASS)        # StepAIC function
library(car)         # For model Building

###################################### DATA UNDERSTANDING #######################################

# Import the input dataset file

car_price <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
str(car_price)
summary(car_price)
View(car_price)

# car_price dataset has 205 observation of 26 variables. 
# int, num,chr type variables are present in the dataset. No variable is stored as factors 
# Car_id is the unique or primary key in the dataset. 

###################################### DATA PREPARATION & EDA #######################################

#Checklist 1: Check for duplicate values 

sum(duplicated(car_price$Car_id)) #There are no duplicate values

#Checklist 2: Check for NA values 

sum(is.na(car_price))
# no NA values

# checklist 3: check for missing values (blanks)

sapply(car_price, function(x) length(which(x == ""))) 
# No blank values

# checklist 4: Removing car_ID: (unique ID) It is not required for modelling.

car_price$car_ID <- NULL

# checklist 5: Treating the variable CarName

# Variable "CarName" is split into "Company" and "ModelName"

car_price <- separate(car_price, CarName, into = "CompanyName",
                     sep = " ", extra = "drop") 
# model name is deleted from the dataset as it is not required for our modelling

# Correcting spelling of company name in the dataset.
car_price$CompanyName <- factor(gsub("maxda", "mazda", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("vw", "volkswagen", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("vokswagen", "volkswagen", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("porcshce", "porsche", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("toyouta", "toyota", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("Nissan", "nissan", car_price$CompanyName))

#to find out number of companies listed in the dataset
levels(as.factor(car_price$CompanyName))

# checklist 6: Treating the variable drivewheel

# Variable "drivewheel" has 4WD and FWD as inputs meaning same forward. Hence, needs correction

car_price$drivewheel <- factor(gsub("4wd", "fwd", car_price$drivewheel))
#to check levels of drivewheel:
levels(as.factor(car_price$drivewheel))

# Checklist 7: Converting all character variables to UPPER Case

char_v <- c("CompanyName", "fueltype", "aspiration", "doornumber", "carbody", "drivewheel", 
            "enginelocation", "enginetype", "cylindernumber", "fuelsystem" )
car_price[char_v] <- lapply(car_price[char_v], function(x) factor(toupper(x)))

#Visualising Data structure: 
plot_str(car_price)

# checklist 8: changing int variable 'symboling' to factors

car_price$symboling <- as.factor(car_price$symboling)
summary(car_price$symboling)

# 6 levels of factors: "-2","-1","0", "1", "2", "3"
# -2    -1    0     1   2     3 
#  3    22    67  54    32    27 

# creating 2 Bins of equal number of levels for symboling variable:
# "risky" and "safe" as mentioned in the data dictionary: 
# +3 indicates risky, -3 is probably pretty Safe. 
# Hence, considering the positive factors(risky) and negative factors(safe) as 2 bins

levels(car_price$symboling)[1:3] <- "SAFE"
levels(car_price$symboling)[2:4] <- "RISKY"

summary(car_price$symboling)
View(car_price)

# checklist9: Treating outliers in int/num variables

#1. wheelbase
ggplot(car_price, aes(x=1, y = wheelbase))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$wheelbase,seq(0,1,0.01))
# Outlier beyond 95% , 110.0000 can be treated 
car_price$wheelbase[which(car_price$wheelbase >114.200)]<- 114.200

#### the boxplot can be executed again after treating outliers to confirm 

#2. carlength
ggplot(car_price, aes(x=1, y = carlength))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$carlength,seq(0,1,0.01))
# Outlier below 3%, and above 99% can be treated
car_price$carlength[which(car_price$carlength < 155.900)] <- 155.900
car_price$carlength[which(car_price$carlength > 202.480)] <- 202.480 # this can be observed from quantile code results


# 3. carwidth
ggplot(car_price, aes(x=1, y = carwidth))+ # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$carwidth,seq(0,1,0.01))
#Outliers beyond 96%, 70.852 can be treated
car_price$carwidth[which(car_price$carwidth > 70.852)] <- 70.852

# 4. carheight
ggplot(car_price, aes(x=1, y = carheight))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$carheight,seq(0,1,0.01))
#No outliers observed

# 5. curbweight
ggplot(car_price, aes(x=1, y = curbweight))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$curbweight,seq(0,1,0.01))
# outlier beyond 1%: 1819.72 can be treated
car_price$curbweight[which(car_price$curbweight < 1819.72)] <- 1819.72

#6. enginesize
ggplot(car_price, aes(x=1, y = enginesize))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$enginesize,seq(0,1,0.01))
# outlier beyond 93%: 183.00 and below 3% : 90.00  can be treated
car_price$enginesize[which(car_price$enginesize < 90 )] <- 90.00
car_price$enginesize[which(car_price$enginesize > 183.00)] <- 183.00

#7. boreratio
ggplot(car_price, aes(x=1, y = boreratio))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$boreratio,seq(0,1,0.01))
#no outliers

# 8. stroke
ggplot(car_price, aes(x=1, y = stroke))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$stroke,seq(0,1,0.01))
car_price$stroke[which(car_price$stroke < 2.8820 )] <- 2.8820
car_price$stroke[which(car_price$stroke > 3.8600 )] <- 3.8600


# 9. compressionratio
ggplot(car_price, aes(x=1, y = compressionratio))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$compressionratio,seq(0,1,0.01))
# Outlier beyond 89%:10.0000 and below 4%: 7.5000 can be treated
car_price$compressionratio[which(car_price$compressionratio > 10.0000)] <- 10.0000
car_price$compressionratio[which(car_price$compressionratio < 7.5000)] <- 7.5000


# 10. horsepower
ggplot(car_price, aes(x=1, y = horsepower))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$horsepower,seq(0,1,0.01))
# Outlier can be clearly observed at 97%: >184.00
car_price$horsepower[which(car_price$horsepower > 184.00)] <- 184.00

# 11. peakrpm
ggplot(car_price, aes(x=1, y = peakrpm))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$peakrpm,seq(0,1,0.01))
#outliers not treated as jumps can be seen through out the data. 

# 12 .citympg
ggplot(car_price, aes(x=1, y = citympg))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$citympg,seq(0,1,0.01))
# Outlier can be clearly observed at 98%: >38.00
car_price$citympg[which(car_price$citympg > 38.00)] <- 38.00


#13. highwaympg
ggplot(car_price, aes(x=1, y = highwaympg))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(car_price$highwaympg,seq(0,1,0.01))
# Outlier can be clearly observed at 98%: >46.92
car_price$highwaympg[which(car_price$highwaympg > 46.92)] <- 46.92



# checklist 10: creating dummy variables: to convert the categorical variables to numerical

# 1. creating dummy variables for "symboling"

levels(car_price$symboling)<-c(1,0)
car_price$symboling <- as.numeric(levels(car_price$symboling))[car_price$symboling]
View(car_price)


# 2. to convert the categorical variable "fueltype" to numerical
levels(car_price$fueltype)<-c(1,0)
car_price$fueltype <- as.numeric(levels(car_price$fueltype))[car_price$fueltype]


# 3. to convert the categorical variable "aspiration" to numerical
levels(car_price$aspiration) <- c(1,0)
car_price$aspiration <- as.numeric(levels(car_price$aspiration))[car_price$aspiration]

# 4. to convert the categorical variable "doornumber" to numerical
levels(car_price$doornumber) <- c(1,0)
car_price$doornumber <- as.numeric(levels(car_price$doornumber))[car_price$doornumber]

# 5. creating dummy variables for "carbody"
dummy <- data.frame(model.matrix(~carbody,data = car_price))
dummy <- dummy[,-1]
car_price <- cbind(car_price[,-6],dummy)

# 6. creating dummy variables for "drivewheel"
levels(car_price$drivewheel) <- c(1,0)
car_price$drivewheel <- as.numeric(car_price$drivewheel)


# 7. creating dummy variables for "engineloc"
levels(car_price$enginelocation) <- c(1,0)
car_price$enginelocation <- as.numeric(car_price$enginelocation)


# 8. creating dummy variables for "enginetype"
dummy <- data.frame(model.matrix(~enginetype,data = car_price))
dummy <- dummy[,-1]
car_price <- cbind(car_price[,-13],dummy)

# 9. creating dummy variables for "cylindernumber"
dummy <- data.frame(model.matrix(~cylindernumber,data = car_price))
dummy <- dummy[,-1]
car_price <- cbind(car_price[,-13],dummy)


# 10. creating dummy variables for "fuelsystem"
dummy <- data.frame(model.matrix(~fuelsystem,data = car_price))
dummy <- dummy[,-1]
car_price <- cbind(car_price[,-14],dummy)


# 11. creating dummy variables for "CompanyName"
dummy <- data.frame(model.matrix(~CompanyName,data = car_price))
dummy <- dummy[,-1]
car_price <- cbind(car_price[,-2],dummy)

View(car_price)


 # checklist 11: derived Metrics:
 
 # 1. weightperarea:
 #Weight of car/ area can be considered as a variable for car pricing
 # deriving it by calculating curbweight/ (carlength* carwidth)
 
 car_price$weightperarea <- car_price$curbweight/(car_price$carlength*car_price$carwidth)
 
 # 2. weightperHP:
 # generally quoted as 'pounds per HP' is often quoted in road tests
 # it is simply weight of car/ horsepower of the engine
 
 car_price$weightperHP <- car_price$curbweight/car_price$horsepower
 
 
summary(car_price)


 ###################################### MODELING #######################################
 
set.seed(100)

 # TO CREATE TRAINING AND TEST DATASETS 
trainindices= sample(1:nrow(car_price), 0.7*nrow(car_price))
 
train = car_price[trainindices,]
test = car_price[-trainindices,]
 

# creating a linear model with all variables
#--------------------------------------------------------------------------------------
model_1 <- lm(price~.,data = train)
summary(model_1)
#Adjusted R-squared:  0.9688
#the  adjusted R-squared value is very high, but very few variables seem to be significant

# the correlation matrix 
 corrs = cor(car_price)
 View(corrs) 
 
 
#using Step AIC to identify insignificant columns 
step <- stepAIC(model_1, direction="both")

step

#model_2:------------------------------------------------------------------------------
model_2 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + carbodyHARDTOP + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + CompanyNameHONDA + CompanyNameISUZU + 
                CompanyNameMAZDA + CompanyNameMERCURY + CompanyNameMITSUBISHI + 
                CompanyNameNISSAN + CompanyNamePLYMOUTH + CompanyNameRENAULT + 
                CompanyNameSAAB + CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + 
                CompanyNameVOLVO + weightperarea, data = train)
summary(model_2)

# Multiple R-squared:  0.9798,	Adjusted R-squared:  0.9734 

# checking for multicolinearity
vif(model_2)

# Now Checking p values: CompanyNameISUZU has high p value. Deleting it


# Creating Model 3--------------------------------------------------------------------------
model_3 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + carbodyHARDTOP + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + CompanyNameHONDA + 
                CompanyNameMAZDA + CompanyNameMERCURY + CompanyNameMITSUBISHI + 
                CompanyNameNISSAN + CompanyNamePLYMOUTH + CompanyNameRENAULT + 
                CompanyNameSAAB + CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + 
                CompanyNameVOLVO + weightperarea, data = train)

summary(model_3)
# Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9731 

vif(model_3)
# Now Checking p values: CompanyNameMERCURY has high p value. Deleting it


# Creating Model 4--------------------------------------------------------------------------

model_4 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + carbodyHARDTOP + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + CompanyNameHONDA + 
                CompanyNameMAZDA + CompanyNameMITSUBISHI + CompanyNameNISSAN + 
                CompanyNamePLYMOUTH + CompanyNameRENAULT + CompanyNameSAAB + 
                CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + 
                CompanyNameVOLVO + weightperarea, data = train)

summary(model_4)
# Multiple R-squared:  0.9791,	Adjusted R-squared:  0.973

vif(model_4)
# Now Checking p values: CompanyNameVOLVO has high p value. Deleting it


# Creating Model 5--------------------------------------------------------------------------

model_5 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + carbodyHARDTOP + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + CompanyNameHONDA + 
                CompanyNameMAZDA + CompanyNameMITSUBISHI + CompanyNameNISSAN + 
                CompanyNamePLYMOUTH + CompanyNameRENAULT + CompanyNameSAAB + 
                CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + weightperarea, data = train)

summary(model_5)

# Multiple R-squared:  0.9788,	Adjusted R-squared:  0.9729

vif(model_5)
# Now Checking p values: carbodyHARDTOP has high p value. Deleting it

# Creating Model 6--------------------------------------------------------------------------


model_6 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + CompanyNameHONDA + 
                CompanyNameMAZDA + CompanyNameMITSUBISHI + CompanyNameNISSAN + 
                CompanyNamePLYMOUTH + CompanyNameRENAULT + CompanyNameSAAB + 
                CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + weightperarea, data = train)

summary(model_6)

# Multiple R-squared:  0.9782,	Adjusted R-squared:  0.9724

vif(model_6)
# Now Checking p values: CompanyNameSAAB has high p value. Deleting it


# Creating Model 7--------------------------------------------------------------------------

model_7 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + CompanyNameHONDA + 
                CompanyNameMAZDA + CompanyNameMITSUBISHI + CompanyNameNISSAN + 
                CompanyNamePLYMOUTH + CompanyNameRENAULT + 
                CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + weightperarea, data = train)

summary(model_7)

# Multiple R-squared:  0.9775,	Adjusted R-squared:  0.9717

vif(model_7)
# Now Checking p values: CompanyNameHONDA has high p value. Deleting it

# Creating Model 8--------------------------------------------------------------------------


model_8 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + 
                CompanyNameMAZDA + CompanyNameMITSUBISHI + CompanyNameNISSAN + 
                CompanyNamePLYMOUTH + CompanyNameRENAULT + 
                CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + weightperarea, data = train)

summary(model_8)

# Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9711

vif(model_8)
# Now Checking p values: CompanyNameNISSAN has high p value. Deleting it


# Creating Model 9--------------------------------------------------------------------------

model_9 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + 
                CompanyNameMAZDA + CompanyNameMITSUBISHI + 
                CompanyNamePLYMOUTH + CompanyNameRENAULT + 
                CompanyNameTOYOTA + CompanyNameVOLKSWAGEN + weightperarea, data = train)

summary(model_9)

# Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9708

vif(model_9)
# Now Checking p values: CompanyNameVOLKSWAGEN has high p value. Deleting it


# Creating Model 10--------------------------------------------------------------------------

model_10 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                carlength + carwidth + curbweight + citympg + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                CompanyNameBMW + CompanyNameDODGE + 
                CompanyNameMAZDA + CompanyNameMITSUBISHI + 
                CompanyNamePLYMOUTH + CompanyNameRENAULT + 
                CompanyNameTOYOTA + weightperarea, data = train)

summary(model_10)

# Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9705

vif(model_10)
# Now Checking p values: CompanyNamePLYMOUTH has high p value. Deleting it


# Creating Model 11--------------------------------------------------------------------------

model_11 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + CompanyNameDODGE + 
                 CompanyNameMAZDA + CompanyNameMITSUBISHI + CompanyNameRENAULT + 
                 CompanyNameTOYOTA + weightperarea, data = train)

summary(model_11)

# Multiple R-squared:  0.9753,	Adjusted R-squared:  0.9701

vif(model_11)
# Now Checking p values: CompanyNameDODGE has high p value. Deleting it


# Creating Model 12--------------------------------------------------------------------------

model_12 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW +
                 CompanyNameMAZDA + CompanyNameMITSUBISHI + CompanyNameRENAULT + 
                 CompanyNameTOYOTA + weightperarea, data = train)

summary(model_12)

# Multiple R-squared:  0.9749,	Adjusted R-squared:  0.9697

vif(model_12)
# Now Checking p values: CompanyNameMAZDA has high p value. Deleting it


# Creating Model 13--------------------------------------------------------------------------

model_13 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + CompanyNameMITSUBISHI + CompanyNameRENAULT + 
                 CompanyNameTOYOTA + weightperarea, data = train)

summary(model_13)

# Multiple R-squared:  0.9743,	Adjusted R-squared:  0.9693

vif(model_13)
# Now Checking p values: CompanyNameTOYOTA has high p value. Deleting it


# Creating Model 14--------------------------------------------------------------------------

model_14 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + CompanyNameMITSUBISHI + CompanyNameRENAULT + 
                 weightperarea, data = train)

summary(model_14)

# Multiple R-squared:  0.9738,	Adjusted R-squared:  0.969

vif(model_14)
# Now Checking p values: CompanyNameRENAULT has high p value. Deleting it


# Creating Model 15--------------------------------------------------------------------------

model_15 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + CompanyNameMITSUBISHI + 
                 weightperarea, data = train)

summary(model_15)

# Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9686

vif(model_15)
# Now Checking p values: carbodySEDAN has high p value. Deleting it


# Creating Model 16--------------------------------------------------------------------------

model_16 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + CompanyNameMITSUBISHI + 
                 weightperarea, data = train)

summary(model_16)

# Multiple R-squared:  0.9722,	Adjusted R-squared:  0.9676

sort(vif(model_16))
# Now Checking p values: drivewheel has higher p value compared to others. Deleting it


# Creating Model 17--------------------------------------------------------------------------

model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + CompanyNameMITSUBISHI + 
                 weightperarea, data = train)

summary(model_17)

# Multiple R-squared:  0.9713,	Adjusted R-squared:  0.9668

sort(vif(model_17))
# Now Checking p values: CompanyNameMITSUBISHI has higher p value compared to others. Deleting it


# Creating Model 18--------------------------------------------------------------------------

model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeOHCV + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)

summary(model_18)

# Multiple R-squared:  0.9702,	Adjusted R-squared:  0.9658

sort(vif(model_18))
# Now Checking p values: considering vif and p-value, next enginetypeOHCV can be deleted


# Creating Model 19--------------------------------------------------------------------------

model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyHATCHBACK + carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)

summary(model_19)

# Multiple R-squared:  0.9685,	Adjusted R-squared:  0.9643

sort(vif(model_19))
# Now Checking p values: considering vif and p-value, next carbodyHATCHBACK can be deleted


# Creating Model 20--------------------------------------------------------------------------

model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)

summary(model_20)

# Multiple R-squared:  0.9669,	Adjusted R-squared:  0.9626

sort(vif(model_20))
# Now Checking p values: aspiration has high p values. Deleting it


# Creating Model 21--------------------------------------------------------------------------


model_21 <- lm(formula = price ~ enginelocation + 
                 carlength + carwidth + curbweight + citympg + 
                 carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)

summary(model_21)

# Multiple R-squared:  0.966,	Adjusted R-squared:  0.962

sort(vif(model_21))
# Now Checking p values: citympg has high p values. Deleting it


# Creating Model 22--------------------------------------------------------------------------

model_22 <- lm(formula = price ~ enginelocation + 
                 carlength + carwidth + curbweight +
                 carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)

summary(model_22)

# Multiple R-squared:  0.9644,	Adjusted R-squared:  0.9605

sort(vif(model_22))

# Now Checking p values: carwidth has high p values. Deleting it


# Creating Model 23--------------------------------------------------------------------------

model_23 <- lm(formula = price ~ enginelocation + 
                 carlength + curbweight +
                 carbodyWAGON + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)

summary(model_23)
# Multiple R-squared:  0.9618,	Adjusted R-squared:  0.958

# Now Checking p values: carbodyWAGON has high p values. Deleting it

# Creating Model 24--------------------------------------------------------------------------


model_24 <- lm(formula = price ~ enginelocation + 
                 carlength + curbweight + enginetypeL + 
                 enginetypeOHC + enginetypeOHCF + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)

summary(model_24)
# Multiple R-squared:  0.9595,	Adjusted R-squared:  0.9558
# Now we have all variabels with low p-value. But still enginetypeOHCF can be deleted based on p-value
# let's try


# Creating Model 25--------------------------------------------------------------------------

model_25 <- lm(formula = price ~ enginelocation + 
                 carlength + curbweight + enginetypeL + 
                 enginetypeOHC + enginetypeROTOR + 
                 cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + 
                 CompanyNameBMW + weightperarea, data = train)


summary(model_25)
# Multiple R-squared:  0.9553,	Adjusted R-squared:  0.9515
# We will continue with this model
# Now Checking p values: enginetypeOHC has high p values. Deleting it


# Creating Model 26--------------------------------------------------------------------------

model_26 <- lm(formula = price ~ enginelocation + carlength + curbweight + enginetypeL + 
                 enginetypeROTOR + cylindernumberFIVE + cylindernumberFOUR +
                 cylindernumberSIX + CompanyNameBMW + weightperarea, data = train)


summary(model_26)
# Multiple R-squared:  0.9534,	Adjusted R-squared:  0.9499
# Now Checking p values: all variable has low p values. Deleting it

# Now Checking for multicolinearity
sort(vif(model_26))
#The variables weightperarea and curbweight has high vif values
# Finding correlation between the two
cor(train$curbweight, train$weightperarea)
#0.9387904
#based on the p values, deleting weightperarea and checking the adjusted R-squared value


# Creating Model 27--------------------------------------------------------------------------

model_27 <- lm(formula = price ~ enginelocation + carlength + curbweight + enginetypeL + 
                 enginetypeROTOR + cylindernumberFIVE + cylindernumberFOUR +
                 cylindernumberSIX + CompanyNameBMW, data = train)


summary(model_27)
# Multiple R-squared:  0.9274,	Adjusted R-squared:  0.9224
# we will continue with this model
# checking for p values: carlength has high p-value. Hence deleting it


# Creating Model 28--------------------------------------------------------------------------

model_28 <- lm(formula = price ~ enginelocation + curbweight + enginetypeL + 
                 enginetypeROTOR + cylindernumberFIVE + cylindernumberFOUR +
                 cylindernumberSIX + CompanyNameBMW, data = train)


summary(model_28)
# Multiple R-squared:  0.9262,	Adjusted R-squared:  0.9218 
# we will continue with this model further.
sort(vif(model_28))

cor(train$cylindernumberFOUR, train$cylindernumberSIX)
# -0.6480336
# deleting cylindernumberSIX based on p-value and checking for adjusted R-squared value


# Creating Model 29--------------------------------------------------------------------------

model_29 <- lm(formula = price ~ enginelocation + curbweight + enginetypeL + 
                 enginetypeROTOR + cylindernumberFIVE + cylindernumberFOUR +
                 CompanyNameBMW, data = train)

summary(model_29)
# Multiple R-squared:  0.8788,	Adjusted R-squared:  0.8725 
# continuing with this model
# checking for p values: deleting enginetypeROTOR for having high p-value


# Creating Model 30--------------------------------------------------------------------------

model_30 <- lm(formula = price ~ enginelocation + curbweight + enginetypeL + 
                 cylindernumberFIVE + cylindernumberFOUR +
                 CompanyNameBMW, data = train)

summary(model_30)
# Multiple R-squared:  0.8777,	Adjusted R-squared:  0.8723 
# checking for p values: deleting cylindernumberFIVE for having high p-value


# Creating Model 31--------------------------------------------------------------------------

model_31 <- lm(formula = price ~ enginelocation + curbweight + enginetypeL + 
                 cylindernumberFOUR + CompanyNameBMW, data = train)

summary(model_31)
# Multiple R-squared:  0.8769,	Adjusted R-squared:  0.8724 
# checking for p values: all variables having low p-value

sort(vif(model_31))
# All variables are having nearly vif values. 
#So, we can consider this as our final model.
# predicting price using model_31 


# Test Model--------------------------------------------------------------------------

predict_price <- predict(model_31, test[,-20])
test$test_price <- predict_price

# test the R-square between actual and predicted sample

r <- cor(test$price, test$test_price)
rsquared <- r^2
rsquared
# 0.8218207
# approx 5% (87.24% ~ 82.18%) difference in adjusted R-squared in the predicted and test model
# this is acceptable.


#--------------------------------------------------------------------------------
# So, that is our final predicted model. model_31.
# the significant 5 Variables for predicting price are :

# enginelocation having factors FRONT and REAR
# curbweight 
# enginetypeL
# cylindernumberFOUR 
# CompanyNameBMW

#Correlation of significant variables with car price

cor(car_price$price, car_price$enginelocation) # 0.3249733
cor(car_price$price, car_price$curbweight) # 0.8371057 : strong correlation
cor(car_price$price, car_price$enginetypeL) # 0.4226724
cor(car_price$price, car_price$cylindernumberFOUR) # -0.6977617 :negative correlation
cor(car_price$price, car_price$CompanyNameBMW) # 0.3247307

#--------------------------------------------------------------------------------------
