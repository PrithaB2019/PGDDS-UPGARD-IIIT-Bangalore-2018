                        
#-------------------------------- ASSIGNMENT: UBER SUPPLY-DEMAND GAP -----------------------------#


#----------------Getting Data-------------------#

# Creating & setting working directory

setwd("C:/Users/Pritha/Desktop/DS C7 UPGRAD IIITB COURSE/Course 2/UBER Case Study")
getwd()


# loading relevant packages required

library(stringr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)


# Importing 'Uber Request Data file.csv' in R

Uber.request.data <- read.csv("Uber request data.csv", stringsAsFactors = FALSE)

# Data Understanding

str(Uber.request.data)

View(Uber.request.data)

# The details of the dataset are as follows:
# Uber.request.data dataset has 6745 obs. of  6 variables (int or char type).
# 1. Request.id - int type: unique id for each record.
# 2. Pickup.point - chr type: 'Airport' or 'city'. 
# 3. Driver.id - int type.
# 4. Status - chr type: 'Trip completed' or 'Cancelled' or 'No cars Available'.
# 5. Request.timestamp - chr type: Data provided is for 11th - 15th Jul 2016; Date & time to be extracted.
# 6. Drop.timestamp - chr type: data available for completed trips; Time taken for trip completion can be calculated. 


summary(Uber.request.data)


# Points to be noted from the Summary of the dataset shows the following:
# Total number of records : 6745
# Driver.id has 2650 NA Values. Those are for the Status "No Cars Available".
# Time and date are stored as character value

# Converting into factors

Uber.request.data$Driver.id <- as.factor(Uber.request.data$Driver.id)
Uber.request.data$Pickup.point <- as.factor(Uber.request.data$Pickup.point)
Uber.request.data$Status <- as.factor(Uber.request.data$Status)
Uber.request.data$Request.timestamp <- as.factor(Uber.request.data$Request.timestamp)
Uber.request.data$Drop.timestamp <- as.factor(Uber.request.data$Drop.timestamp)

# After converting as factor, Drop.timestamp has 3914 NA Values. Those are for the Status "Cancelled" and "No Cars Available".

########################################################################################################


# ----------------- Data Cleaning & Manipulation ----------------------#


#Checklist 1: Check for duplicate values 

sum(duplicated(Uber.request.data$Request.id)) #There are no duplicate values

#Checklist 2: Check for NA values 

sum(is.na(Uber.request.data$Request.id))
sum(is.na(Uber.request.data$Pickup.point))
sum(is.na(Uber.request.data$Status))
sum(is.na(Uber.request.data$Request.timestamp))

# no data cleaning required for missing values 


#  Checklist 3: Format Date & Time (Required because the data is in different formats)


# Make the time separator consistent
Uber.request.data$Request.timestamp <- str_replace_all(Uber.request.data$Request.timestamp, "[/]",  "-")

Uber.request.data$Drop.timestamp <- str_replace_all(Uber.request.data$Drop.timestamp, "[/]",  "-")


#Make the date and time format same for all data

Uber.request.data$Request.timestamp <- parse_date_time(x = Uber.request.data$Request.timestamp, orders = c("%d-%m-%Y %H:%M","%d-%m-%Y %H:%M:%S"), locale = Sys.getlocale("LC_TIME"))

Uber.request.data$Drop.timestamp <- parse_date_time(x = Uber.request.data$Drop.timestamp, orders = c("%d-%m-%Y %H:%M","%d-%m-%Y %H:%M:%S"), locale = Sys.getlocale("LC_TIME"))


# Derived Metrics:

# Extract hour, date, Weekday from Request.timestamp

Uber.request.data$Reqday <- format(Uber.request.data$Request.timestamp, "%d")
Uber.request.data$ReqWday <- weekdays(Uber.request.data$Request.timestamp)
Uber.request.data$Reqhour <- format(Uber.request.data$Request.timestamp, "%H")


# Extract hour, date, Weekday of Drop.timestamp

Uber.request.data$Dropday <- format(Uber.request.data$Drop.timestamp, "%d")
Uber.request.data$DropWday <- weekdays(Uber.request.data$Drop.timestamp)
Uber.request.data$Drophour <- format(Uber.request.data$Drop.timestamp, "%H")


# Ordering ReqWday and DropWday column

Uber.request.data$ReqWday <- factor(Uber.request.data$ReqWday,levels =c("Monday","Tuesday","Wednesday", "Thursday","Friday"))

Uber.request.data$DropWday <- factor(Uber.request.data$DropWday,levels =c("Monday","Tuesday","Wednesday", "Thursday","Friday"))


# Assigning Time slots:
Uber.request.data$Reqhour <- as.numeric(Uber.request.data$Reqhour)
Uber.request.data$time_slot = ifelse(Uber.request.data$Reqhour > 0 & Uber.request.data$Reqhour <= 4, "Late_Night", 
                              ifelse(Uber.request.data$Reqhour > 4 & Uber.request.data$Reqhour <= 10, "Early_Morning",
                              ifelse(Uber.request.data$Reqhour > 10 & Uber.request.data$Reqhour <= 16, "Day_Time",
                              ifelse(Uber.request.data$Reqhour > 16 & Uber.request.data$Reqhour <= 23, "Late_Evening", "Midnight"))))

# Ordering Time_slot
Uber.request.data$time_slot <- factor(Uber.request.data$time_slot,levels =c("Early_Morning","Day_Time","Late_Evening", "Midnight","Late_Night"))


# calculating the Triptime

Uber.request.data$triptime<-as.numeric(Uber.request.data$Drop.timestamp-Uber.request.data$Request.timestamp)


####################################################################################################################
write.csv(Uber.request.data, "Uber Request Clean Data.csv", na = "") # for creating plots in Tableau simultaneously
####################################################################################################################

# ----------------- Data Analysis ----------------------#

#Step 1: Univariate Analysis: Analysing each variable separately


# Trip request Status: Finding number of trips completed, cancelled or No Cars available

summary(as.factor(Uber.request.data$Status))    
# 2831 trip Completed, 2650 No Cars available, 1264 Cancelled


#Finding number of trips per pickup.point

summary(as.factor(Uber.request.data$Pickup.point))   
# 3238 from Airport & 3507 from City

# Finding Average Trip time

Average_trip_time <- round(mean(!is.na(Uber.request.data$triptime))*60,2)
Average_trip_time         #25.18 mins

#finding the number of trips made in each slot

summary(as.factor(Uber.request.data$time_slot)) 

#Early_Morning: 2346; Day_Time: 981; Late_Evening: 2840; Midnight: 99; Late Night: 479;
# Clearly we can conclude that maximum trips are requested during 'early morning'(2346) or 'late evening'(2840)

###########################################################################################################


# Creating plots

# Plot1: Plot showing Trip status trend through out the day (for 5 days)

TripReq <- ggplot(Uber.request.data, aes(x = as.factor(Reqhour),fill = Status))+
            geom_bar(position = "dodge")+
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) + 
            facet_wrap( ~ Uber.request.data$ReqWday, nrow =5, ncol = 1) + 
            labs(title ="Number of Trip requests per day",x = "Hour of the Day", y = "Number of Trip Requests", fill = "Status" )
TripReq

# As already observed above, the plot also indicates that:
# maximum Trip requests were made during 'early morning' and 'late evening'
# further it can be observed:(detailed analysis regarding these follows below)
# maximum Trip requests were cancelled during the early_Morning Time and  
# maximum non-availability of cars can be observed during Late_Evening time 
# Similar trends are observed for all five weekdays. The number of requests made are nearly same on all the days

# Plot showing Trip Pickup.point trend through out the day time for 5 days

PickupTrend <- ggplot(Uber.request.data, aes(x = as.factor(Reqhour),fill = Pickup.point)) +
                geom_bar(position = "dodge") +
                facet_wrap( ~ Uber.request.data$ReqWday, nrow =5, ncol = 1) + 
                geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) +
                labs(title ="Pick up Point Trend plot", x = "Hour", y = "Number of Trip Requests", fill = "Pickup.point" )
PickupTrend

# It can be observed from the obtained plot that:
# maximum Trip requests were made during the early morning Time (approx 4:00 am to 10:00 am) from pickup.point: city  and  
# maximum Trip requests were made during the Late Evening time (approx 4:00 pm to 11:00 pm) from pickup.point: Airport


#Plot Showing number of trips by hour : to see the peak business hours

Tripsperhour <- ggplot(Uber.request.data, aes(x = as.factor(time_slot),fill = Pickup.point))+
  geom_bar(position = "dodge")+ 
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1))
  labs(title = "Number of trip requests Per Hour", x = "Time of the Day", y = "Number of Requests", fill = "Pickup Point" )
Tripsperhour

#Important point to note for Business developement: Early Morning hours and late evening hours can be identified as Peak business hours


#plotting and identifying the most critical problems for Uber

timeslot <- ggplot(Uber.request.data, aes(x = as.factor(time_slot), fill= Pickup.point)) + 
            geom_bar(position = "dodge")+
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) +
            facet_wrap( ~ Uber.request.data$Status, nrow =3, ncol = 1) + 
            labs(title = "Trip Status from city or airport at different time-slot", x = "Time Slot", y = "Number of Requests", fill = "Pickup.point")
timeslot


# the above plot shows us the major problems faced by Uber:

# Problem 1 - High cancellations in the early morning from City
# Problem 2 - Non-availability of Cars in the late evening from Airport



# Quantifying Problems 1 & 2 :Segmented analysis of time slots

# Early Morning Problem

Early_morning_data <- subset(Uber.request.data,time_slot=="Early_Morning")
nrow(subset(Early_morning_data, Early_morning_data$Status == "Cancelled" & Early_morning_data$Pickup.point == "Airport"))
nrow(subset(Early_morning_data, Early_morning_data$Status == "Cancelled" & Early_morning_data$Pickup.point == "City"))

 
# Number of Trips in early_morning Cancelled from Airport : 32
# Number of Trips in early_morning Cancelled from City : 873
# Hence, it can be concluded that during early_morning hours, maximum cancellation is in case of city to Airport trips
 
  
problem1 <- ggplot(Early_morning_data, aes(x = Pickup.point, fill= Status)) + 
            geom_bar() + coord_polar(theta = "y", start=0)+ 
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.1))
            labs(title ="Early Morning trips trend", y = "Number of Requests", x = "", fill = "Status")
problem1



# Late Evening Problem

Late_evening_data <- subset(Uber.request.data,time_slot=="Late_Evening")
nrow(subset(Late_evening_data, Late_evening_data$Status == "No Cars Available" & Late_evening_data$Pickup.point == "Airport"))
nrow(subset(Late_evening_data, Late_evening_data$Status == "No Cars Available" & Late_evening_data$Pickup.point == "City"))


# Number of Trips in late_evening failed due to non-availability of car from Airport : 1457
# Number of Trips in late_evening failed due to non-availability of car from City : 154
# Hence, it can be concluded that during late_evening hours, maximum non-availabilty of Cars is at Airport 


problem2 <- ggplot(Late_evening_data, aes(x = Pickup.point, fill= Status)) + 
            geom_bar() + coord_polar(theta = "y", start=0)+
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) + 
            labs(title = "Late_Evening trips trend ", y = "Number of Requests", x = "", fill = "Status")
problem2

#############################################################################################################
# ----------------------- Supply Demand Gap --------------------------#


# Airport Pick Up Trip requests


Airport_DSgap <- nrow(subset(Uber.request.data, Uber.request.data$Pickup.point == "Airport")) - 
                nrow(subset(Uber.request.data, Uber.request.data$Pickup.point == "Airport" & Uber.request.data$Status == "Trip Completed"))
Airport_DSgap                 
# For Airport Pick up trip Request, 1911 gap between demand and supply through out the day

# City Pick Up Trip requests

City_DSGap <- nrow(subset(Uber.request.data, Uber.request.data$Pickup.point == "City")) - 
  nrow(subset(Uber.request.data, Uber.request.data$Pickup.point == "City" & Uber.request.data$Status == "Trip Completed"))
City_DSGap 
# For City Pick up trip Request, 2003 gap between demand and supply through out the day

# The demand and supply gap is more for City to airport type of trip requests (2003 numbers) through out the day. 

# Finding Maximum Demand supply gap at which time slot of the day:

DSgap_latenight <- nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Late_Night")) - 
                   nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Late_Night" & Uber.request.data$Status =="Trip Completed"))
DSgap_latenight # 305

DSgap_earlymorning <- nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Early_Morning")) - 
  nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Early_Morning" & Uber.request.data$Status =="Trip Completed"))
DSgap_earlymorning #1376

DSgap_daytime <- nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Day_Time")) - 
  nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Day_Time" & Uber.request.data$Status =="Trip Completed"))
DSgap_daytime #375

DSgap_lateevening <- nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Late_Evening")) - 
  nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Late_Evening" & Uber.request.data$Status =="Trip Completed"))
DSgap_lateevening #1799

DSgap_midnight <- nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Midnight")) - 
  nrow(subset(Uber.request.data, Uber.request.data$time_slot == "Midnight" & Uber.request.data$Status =="Trip Completed"))
DSgap_midnight #59

# Highest Gap Exists during Late_Evening Hours, i.e. from 4:00 pm to 11:00 pm of the day.1799

# Finding the pickup point for which the gap is the most severe during Early_Morning Hours

# Late_evening_data : Subset created for late Evening Time slot
Airport_lateeve_DSgap <- nrow(subset(Late_evening_data, Late_evening_data$Pickup.point == "Airport")) - 
                         nrow(subset(Late_evening_data, Late_evening_data$Pickup.point == "Airport" & Late_evening_data$Status == "Trip Completed"))
Airport_lateeve_DSgap # 1566

City_lateeve_DSgap <- nrow(subset(Late_evening_data, Late_evening_data$Pickup.point == "City")) - 
                      nrow(subset(Late_evening_data, Late_evening_data$Pickup.point == "City" & Late_evening_data$Status == "Trip Completed"))
City_lateeve_DSgap # 233

# Highest Gap Exists during Late_Evening Hours, i.e. from 4:00 pm to 11:00 pm of the day for Airport to City Type of requests.
 

# Demand Supply Plots:

a_supply <- ggplot(subset(subset(Uber.request.data, Uber.request.data$Pickup.point == "Airport"),!is.na(as.factor(Uber.request.data$Drophour))), aes(Drophour))+
            geom_bar(fill = "red") + 
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) + 
            labs(title ="Supply at Airport(hours)", x = "Request Hours", y = "Number of Requests")

a_demand <- ggplot(subset(Uber.request.data, Uber.request.data$Pickup.point == "Airport"), aes(Reqhour))+
            geom_bar(fill = "blue") + 
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) +
            labs(title ="Demand at Airport(Requests)", x = "Request Hours", y = "Number of Requests")

c_supply <- ggplot(subset(subset(Uber.request.data, Uber.request.data$Pickup.point == "City"),!is.na(as.factor(Uber.request.data$Drophour))), aes(Drophour))+
            geom_bar(fill = "red") + 
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) + 
            labs(title ="Supply at City(hours)", x = "Request Hours", y = "Number of Requests")

c_demand <- ggplot(subset(Uber.request.data, Uber.request.data$Pickup.point == "City"), aes(Reqhour))+
            geom_bar(fill = "blue") + 
            geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1)) +
            labs(title ="Demand at City(Requests)", x = "Request Hours", y = "Number of Requests")


# Seeing overall demand and supply trend
grid.arrange(a_demand, a_supply, c_demand, c_supply, nrow = 2, ncol = 2)


# Seeing demand and supply gap from Airport to city during late_evening hours

DS_airport_eve <- ggplot(subset(Uber.request.data, Uber.request.data$Pickup.point == "Airport" & Uber.request.data$time_slot =="Late_Evening")) +
  geom_bar(aes(Reqhour), fill = c("blue"))+ 
  geom_bar(aes(as.numeric(Drophour)),fill = c("red"),position = "dodge") + 
  xlim(15,23) + labs(title = "Late Evening From Airport : Demand = Blue, Supply = Red")+ xlab("Hrs of the day") + ylab("Number of Requests")
DS_airport_eve

# Average Trip time During late Evening Hours
mean(as.numeric(Late_evening_data$triptime), na.rm = T)
# 52.04 mins , which is higher than average trip time through out the day

#####################################################################################################

# From the above observations, it can be concluded that Maximum Demand-Supply Gap is faced by Uber 
# at the Airport during late Evening Hours due to non-availability of Cars. 
# That can be due to less number of airport arrival. Main reasons behind this can be 
# drivers avoid trip to airport during this time due to higher average trip time (52mins)
# or less passengers travel to airport at this time. To improve, Uber can provide incentives to drivers
# for completing trip to airport. Another way of minimising the supply demand gap can be pool rides from Airport. 
# That can be promoted more.


###########################################################################################################################

###########################################################################################################################
