################################# GRAMENER CASE STUDY ##################################


#----------------Getting Data-------------------#

# Creating & setting working directory

setwd("C:/Users/Pritha/Desktop/DS C7 UPGRAD IIITB COURSE/Course 2/GRAMENER")
getwd()


# loading relevant packages required

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(ggthemes)
library(grid)
library(gridExtra)
library(dlookr)
library(DataExplorer)


# Importing 'loan.csv' in R

loan <- read.csv("loan.csv", header = TRUE, na.strings = c("", " ", "NA", "n/a"), stringsAsFactors = FALSE)  


#---------------- Data Understanding -------------------## 

str(loan)

View(loan)

# The dataset contains complete loan data for all loans issued through the time period 2007 to 2011.
# Rejected Loan data is not available in the dataset due to no transactional history of those applicants.
# There are 111 variables and total 39717 observations
# The variables are of chr, num, int and logical type.
# There are 115 variables described in data dictionary (excel file provided). hence 4 variables are missing in the records:
# 1) fico_range_high, 2) fico_range_low, 3) last_fico_range_high, 4) last_fico_range_low are missing as per observation.
# there are many columns with missing values, NA, and 0. 
# loan$id, loan$member_id are the unique field (or primary key) for each records.


#---------------- Data Cleaning -------------------## 

#step 1: checking duplicate rows

sum(duplicated(loan$id)) 

#0

#step 2: Get rid of all columns having only 1 unique value

loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

# This deletes all the columns with unique values, like the coulmns having only NA or 0 or f.
# now, loan dataset has 51 variables. Further cleaning can be done as below


# step 3: Delete columns not required for our analysis

# 1. The variables emp_title and url can be dropped due to many different character values not required for our analysis
# 2. The variable chargeoff_within_12_mths is of no use in our analysis. It has only 0 or NA Values.
# 3. The variable member_id can be dropped because all loans are individual loans,there is a 1 - 1
#    mapping from loan Id to member Id, hence just the loan Id will be enough for unique key.
# 4. The variable tax-lien has either 0 or NA values. Hence, it is also not required for our analysis.
# 5. the variable title can also be deleted since, it gives further details of purpose, which is not required for analysis.

loan <- loan[,-match(c("member_id", "chargeoff_within_12_mths","desc","url","emp_title", "tax_liens", "title"),names(loan))]


# now, loan dataset has 44 variables. Further cleaning can be done as below

# step 4: checking missing values

colSums(sapply(loan, is.na)) 

# this tells which columns has how many missing values

# removing columns for which the missing vales are greater than 50% of the observations(39717/2 = 19858)
clean <- c(names(loan[which(colSums(sapply(loan, is.na))>=19858)]))
loan <- loan %>% select(-clean)

# "mths_since_last_delinq"(25682 NAs'), "mths_since_last_record"(36931 NAs'), " next_pymnt_d"(38577 NAs') 
#columns got removed from the dataset
# now, loan dataset has 41 variables. Further cleaning can be done as below


#checking to see how much clean data we have
plot_missing(loan)
# this plot tells that we have data with acceptable amount of missing values to analyse further

# step 5: Data formatting:

# Converting specific columns to required format by seeing the structure of the data and 
# also reffering to data dictionary about various columns

plot_str(loan)

# term, int_rate and revol_util are of char format. We need to convert them to numbers

loan <- loan %>% 
  mutate(int_rate = as.numeric(gsub("%", "", int_rate)),
         term = as.numeric(gsub(" months", "", term)),
         revol_util = as.numeric(gsub("%","",loan$revol_util)))


# Convert issue_d, last_pymnt_d, last_credit_pull_d, earliest_cr_line to date format

loan <- loan %>%
  mutate(issue_d = parse_date_time(x = issue_d, orders = c("%d-%b-%Y", "%d-%b-%y", "%b-%y"), locale = Sys.getlocale("LC_TIME")),
         last_pymnt_d = parse_date_time(x = last_pymnt_d, orders = c("%d-%b-%Y", "%d-%b-%y", "%b-%y"), locale = Sys.getlocale("LC_TIME")),
         last_credit_pull_d = parse_date_time(x = last_credit_pull_d, orders = c("%d-%b-%Y", "%d-%b-%y", "%b-%y"), locale = Sys.getlocale("LC_TIME")),
         earliest_cr_line = parse_date_time(x = earliest_cr_line, orders = c("%d-%b-%Y", "%d-%b-%y", "%b-%y"), locale = Sys.getlocale("LC_TIME")))

# earliest_cr_line has date before 1970. Hence, it needs to be converted to proper date format separately.
# Clearly, earliest_cr_line can not be beyond 2011, hence it can be converted to 19th century date if found beyond

loan$earliest_cr_line <- ifelse(loan$earliest_cr_line > "2011-12-31", 
                                format(loan$earliest_cr_line, "19%y-%m-%d"), format(loan$earliest_cr_line))

# Step 6: Factorizing the character columns

loan <- loan %>%
  mutate(term = as.factor(term),
         grade=as.factor(grade),
         sub_grade=as.factor(sub_grade),
         emp_length = as.factor(emp_length),
         home_ownership = as.factor(home_ownership),
         verification_status = as.factor(verification_status),
         purpose = as.factor(purpose),
         loan_status = as.factor(loan_status))


#Step 7: Considering loan_status which is not "CURRENT"
# We need only 2 classes, FULLY PAID and CHARGED OFF in our analysis
# We dont need CURRENT since they might be FULLY PAID/ CHARGED OFF in future
# Get rid of all loan_status those are still on going i.e. loan_status = "CURRENT"

loan<-loan[-which(toupper(loan$loan_status) == "CURRENT"), ]

# loan dataset has 38577 observations of 41 Variables.


#Step 8: Treating Outliers in variables

summary(loan$annual_inc)
# As, it is clear from the summary, Annual Income has high number of outliers. 
# Min is 4000, median is 58868, mean is 68778. 3rd Qu is from 82000, but max is 6000000. 
# So, Ignoring values which are > Q3 + 1.5(IQR)


ggplot(loan, aes(x=1, y = annual_inc))+ geom_boxplot(outlier.size=1.5, outlier.shape=21)+
  scale_x_continuous(breaks=NULL) +
   theme_stata()+
  scale_y_log10()+
theme(axis.title.x = element_blank()) # For clear visualization of outliers

annual_income_limit <- quantile(as.numeric(loan$annual_inc))[4] + 1.5*IQR(as.numeric(loan$annual_inc))
annual_income_limit

# O/P = 75%  ~ 145000

# --------- Data Cleaning complete ---------- #


####################################################################################################################
write.csv(loan, "loan Clean Data.csv", na = "") # for creating plots in Tableau simultaneously
####################################################################################################################


#---------------- Data Analysis -------------------## 

#Step 1: Univariate Analysis: Analysing each variable separately

# 1. Loan Status:

summary(as.factor(loan$loan_status))

ggplot(loan, aes(x= loan_status, fill = loan_status)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = (..count..), vjust = -0.2)) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+
  labs(x="Loan Status", y="Count",title = "Loan Status Distribution") +theme_stata()

# CHARGED OFF: 5627
# FULLY PAID: 32950


# 2. Loan term

summary(as.factor(loan$term)) 

ggplot(loan, aes(x= term, fill = term)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = (..count..), vjust = -0.2)) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+
  labs(x="Loan term in Months", y="Count",title = "Loan Term Duration") +theme_stata()


# 36 months: 29096
# 60 months: 9481
# Only 60 month & 36 month term loans are available, of which maximum is for 60 months' term


# 3. Loan Grade:

summary(as.factor(loan$grade)) 

ggplot(loan, aes(x= grade, fill = grade)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+
  labs(x="Loan Grade", y="Count",title = "Loan Grade") + theme_stata()


# 7 grades are there:
# A: 10045, B: 11675, C: 7834, D: 5085, E: 2663, F: 976, G: 299
# for most loans, grade B is assigned whereas G grade is the minimum. 


# 4. Loan Sub_grade:
summary(as.factor(loan$sub_grade)) 

ggplot(loan, aes(x= sub_grade, fill = sub_grade)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  guides(fill = F)+
  labs(x="Sub Grade", y="Count",title = "Sub Grade") +theme_stata()

# to avoid warning message: n too large, allowed maximum for palette Pastel1 is 9"" , 
# scale_fill_brewer(palette = "Pastel1") is not used in this plot

# 35 sub-grades are there: 5 for each grades. 
# These sub-grades are uniformly assigned to entire grades.

# A1   A2   A3   A4   A5   B1   B2   B3   B4   B5   C1   C2   C3   C4   C5   D1   D2   D3   D4 
# 1139 1508 1810 2873 2715 1797 2001 2825 2437 2615 2055 1931 1488 1206 1154  931 1286 1116  918 
# D5   E1   E2   E3   E4   E5   F1   F2   F3   F4   F5   G1   G2   G3   G4   G5 
# 834  722  614  516  424  387  305  233  174  151  113   94   77   45   54   29 


# 5. Loan purpose:
summary(as.factor(toupper(loan$purpose)))

ggplot(loan, aes(x= purpose, fill = purpose)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  guides(fill = F)+
  labs(x="Purpose", y="Count",title = "Purpose For Which Loans Are Taken") +  theme_stata()+
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1),axis.text.y = element_text(angle=0))

# 14 loan purposes are recorded in the dataset.e.g. CAR ,CREDIT_CARD, DEBT_CONSOLIDATION, EDUCATIONAL, 
# HOME_IMPROVEMENT, HOUSE, MAJOR_PURCHASE, MEDICAL, MOVING, RENEWABLE_ENERGY, SMALL_BUSINESS,
# VACATION, WEDDING and OTHER ; of which:
# debt_consolidation is the purpose for maximum number of loans (18055 loans)


# 6. Verification Status:
summary(as.factor(loan$verification_status))


ggplot(loan, aes(x= verification_status, fill = verification_status)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+
  labs(x="Verification Status", y="Count",title = "Verification Status") +theme_stata()

# NOT VERIFIED: 16694
# SOURCE VERIFIED: 9677        
# VERIFIED: 12206


# 7. Home Ownership:
summary(as.factor(loan$home_ownership))

ggplot(loan, aes(x= home_ownership, fill = home_ownership)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+  
  labs(x="Home Ownership", y="Count",title = "Home Ownership") +theme_stata()

# MORTGAGE 17021     
#NONE       3  
#OTHER      98    
#OWN      2975 
#RENT     18480
#  maximum lenders are on rent or having mortgage property                         

# 8. issue Date:
summary(loan$issue_d)

ggplot(loan, aes(x= issue_d, fill = issue_d)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  labs(x="Loan Issue date", y="Count",title = "Loan Issue date") +theme_stata()

#we see an increasing no. of loans being disbursed with passing years
# 9. State

summary(as.factor(loan$addr_state))

ggplot(loan, aes(x= addr_state, fill = addr_state)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  guides(fill = F)+
  labs(x="State", y="Count",title = "Count Of Loans From Each State ") +  theme_stata()+
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1),axis.text.y = element_text(angle=0))

# Maximum loan applicants are from CA: 6949

# 10. emp_Length

summary(as.factor(loan$emp_length))

ggplot(loan, aes(x= emp_length, fill = emp_length)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  guides(fill = F)+
  labs(x="Employment length", y="Count",title = "Employment length of Applicants") +  theme_stata()+
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1),axis.text.y = element_text(angle=0))

# 10 + Years of Employment are maximum in numbers: 8488
# < 1 year    1 year   10+ years   2 years   3 years   4 years   5 years   6 years   7 years   8 years 
#   4508      3169        8488      4291      4012      3342      3194      2168      1711      1435 

# 9 years    NA's 
# 1226      1033 
# Data Missing for 1033 applications.


################# Segmented Univariate Analysis ##############

# 1. Interest Rate

summary(loan$int_rate)
# Since the interest rates lies between 5.42 to 24.59, segmenting it into bins in order to understand and analyse it 
# clearly with other variables. These bins are open to understanding of the analysts.
# Categorising interest rate between 5.42 and 9.00 as low, 9.00 and 12.00 as medium, 12.00 and 15.00 as high
# and 15.00 and 24.59 as very high.  

loan$int_rate_bin <- ifelse(
  loan$int_rate >= 5.42 &
    loan$int_rate <= 9.00,# 1st quantile just starts (round-off 8.94)
  "Low",
  ifelse(
    loan$int_rate > 9.00 &
      loan$int_rate <= 12.00,#( round-off mean 11.93)
    "Medium",
    ifelse(loan$int_rate > 12.00 &
             loan$int_rate <= 15.00, #( round-off 3rd quantile 11.93)
           "High",
           "Very High")
  )
)

loan %>% 
  filter(annual_inc<= annual_income_limit,loan_status == "Charged Off" | loan_status == "Fully Paid") %>%
  ggplot(aes(x = int_rate_bin, y = round(..count../sum(..count..)*100,2), fill = int_rate_bin))+ geom_bar(color = "black")+
  scale_fill_brewer(palette = "Pastel1")+ guides(fill =F) +
  scale_y_continuous(breaks = seq(0,60,10))+
  geom_text(stat = "count", aes(y =round((..count../sum(..count..))*100,2), label = scales::percent((..count..)/sum(..count..))) ,vjust =-0.2)+
  labs(title="Interest Rate Categories",x="Interest Rates",y="Percentage")+ theme_stata()+
  theme_stata()+theme(axis.text.y = element_text(angle=0))
summary(as.factor(loan$int_rate_bin))

# Medium Interest rate is applied on maximum Loan application


# 2. Annual Income - Derived Metrics

summary(loan$annual_inc)

loan$annual_inc_bin <- ifelse(
  loan$annual_inc >= 4000 & #minimum value
    loan$annual_inc <= 40000, # 1st quardant starts from this
  "Low",
  ifelse(
    loan$annual_inc > 40000 &
      loan$annual_inc <= 80000, # Approx 3rd quardant starts after this
    "Average",
    ifelse(loan$annual_inc > 80000 &
             loan$annual_inc <= 100000,
           "High","Very High") # Outliers in upper quantile fall in this bin
  )
)
summary(as.factor(loan$annual_inc_bin))

ggplot(loan, aes(x= annual_inc_bin, fill = annual_inc_bin)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+  
  labs(x="Annual Income", y="Count",title = "Annual Income") +theme_stata()
# Maximum loan applicants (count = 18884) have average annual Income ie. between 40000 to 80000.
# Average      High       Low     Very High 
#   18884      4514      9698      5481 


# 3. Loan Amount

summary(loan$loan_amnt)

loan$loan_amnt_bin <- ifelse(
  loan$loan_amnt >= 500 & #minimum value
    loan$loan_amnt <= 5500, # 1st quardant starts just before this (rounding off 5300)
  "Low",
  ifelse(
    loan$loan_amnt > 5500 &
      loan$loan_amnt <= 15000, # Approx 3rd quardant starts after this
    "Average",
    ifelse(loan$loan_amnt > 15000 &
             loan$loan_amnt <= 25000,
           "High","Very High") 
  )
)
summary(as.factor(loan$loan_amnt_bin))

# Average      High       Low   Very High 
# 19929       7185      10028      1435 

ggplot(loan, aes(x= loan_amnt_bin, fill = loan_amnt_bin)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+  
  labs(x="Loan Amount", y="Count",title = "Loan Amount") +theme_stata()

# Maximum Loan Application is for average amount ie. 5500 - 15000.

# 4. Funded Amount

summary(loan$funded_amnt)

loan$funded_amnt_bin <- ifelse(
  loan$funded_amnt >= 500 & #minimum value
    loan$funded_amnt <= 5500, # 1st quardant starts just before this
  "Low",
  ifelse(
    loan$funded_amnt > 5500 &
      loan$funded_amnt <= 15000, # 3rd quardant starts after this
    "Average",
    ifelse(loan$funded_amnt > 15000 &
             loan$funded_amnt <= 25000,
           "High","Very High")
  )
)
summary(as.factor(loan$funded_amnt_bin))

# Average      High     Low    Very High 
#  20269      6887     10153      1268 

ggplot(loan, aes(x= funded_amnt_bin, fill = funded_amnt_bin)) + geom_bar(color = "black") + 
  geom_text(stat = "count",aes(label = ..count..), position = position_stack(), vjust = -0.2) +
  scale_fill_brewer(palette = "Pastel1")+
  guides(fill = F)+  
  labs(x="Funded Amount", y="Count",title = "Funded Amount") +theme_stata()

# maximmum funded amount is average, i.e. 5500 - 15000



################# Bivariate Analysis ############

#1. let's find the relationship between home ownership and defaulted customers

loan %>%
  select(loan_status, home_ownership)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid", home_ownership != "NONE" ) %>%
  ggplot(aes(x = home_ownership,y = (..count../sum(..count..))*100, fill =loan_status)) + 
  geom_bar(color= "black", position = "dodge")+ 
  geom_text(stat = "count", aes(y =round((..count../sum(..count..))*100,2), label = scales::percent((..count..)/sum(..count..))) , 
            position = position_dodge(width = 1),vjust =-0.2)+
  scale_fill_brewer(palette = "Pastel1")+
  labs(x = "Home Ownership", y= "Percentage", title = "Home Ownership Vs Loan Status ",fill="Loan Status") +
  theme_stata()

#Borrowers with home ownership as "Rent" have defaulted the most with 7.4%
# Removing NONE, as there are only 3 rows.

#2. Loan status and purpose relationship

loan %>%
  select(loan_status,purpose)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid") %>%
  ggplot(aes(x= purpose, y = ..count.., fill = loan_status)) + geom_bar(color = "black",position = "fill")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(title="Purpose Vs Loan Status",x="Purpose",y="Percentage",fill="Loan Status")+
  theme_stata()+
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1),axis.text.y = element_text(angle=0))

#the graph shows that loans taken for the purpose of small business have the most defaulters.

#3. Loan Status and Grade Relationship

loan %>%
  select(loan_status,sub_grade,grade)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot(aes(x= grade, y = (..count../sum(..count..))*100,fill = loan_status)) + geom_bar(position = "dodge",color = "black") +
  scale_fill_brewer(palette = "Pastel1")+
  labs(x = "Grade", y= "Percentage", title = "Impact of Grades on Defaulted Loans",fill="Loan Status") +
  geom_text(stat = "count", aes(y =round((..count../sum(..count..))*100,2), label = scales::percent((..count..)/sum(..count..))) , position = position_dodge(width = 1),vjust =-0.2)+
  theme_stata()

#Most defaulted customers are from Grade B and C

#4. Loan term length and loan status

loan %>%
  select(loan_status, term)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot(aes(x = term,  y = (..count../sum(..count..))*100, fill = loan_status)) + geom_bar(color = "black")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(x = "Term", y= "Percentage", title = "Impact of Loan Term on Defaulted Loans",fill="Loan Status") +
  geom_text(stat = "count", aes(y =round((..count../sum(..count..))*100,2), label = scales::percent((..count..)/sum(..count..))) , position = position_stack(vjust =0.3),vjust =-0.2)+
  theme_stata()

#so we get that loans with 36 months term are more likely to be defaulted than with 60 months with 8.4%

#5. Employment length and loan status

loan %>%
  select(loan_status, emp_length)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot(aes(x = emp_length,  y = (..count../sum(..count..))*100,fill = loan_status)) + geom_bar(color = "black")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(x = "Employment Lenght", y= "Percentage", title = "Impact of Employment Length on Defaulted Loans",fill="Loan Status") +
  geom_text(stat = "count", aes(y =round((..count../sum(..count..))*100,2), label = scales::percent((..count..)/sum(..count..))) , position = position_stack(vjust =0.3),vjust =-0.2)+
  theme_stata()
# Employed for more than 10 years are more likely to default although there seems to be no specific correlation
#between laon status and employment length with 3.5%

#6. verification status and loan status
loan %>%
  select(loan_status, verification_status)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot(aes(x = verification_status,  y = (..count../sum(..count..))*100, fill = loan_status)) + geom_bar(color = "black")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(x = "Verification Status", y= "Percentage", title = "Impact of Verification Status on Defaulted Loans",fill="Loan Status") +
  geom_text(stat = "count", aes(y =round((..count../sum(..count..))*100,2), label = scales::percent((..count..)/sum(..count..))) , position = position_stack(vjust =0.3),vjust =-0.2)+
  theme_stata()

#it is surprising to see that verified loans have almost an equal amount of defaulters as not verified loans
# with 5.6% and 5.3% respectively

#7.Address State and loan status
loan %>%
  select(loan_status, addr_state)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>% ggplot(aes(x = addr_state, fill = loan_status)) +
  geom_bar(position = "dodge",color = "black")+
  scale_fill_brewer(palette = "Pastel1")+ 
  labs(x = "Term", y= "Count", title = "State Vs Loan Status",fill="Loan Status")+ theme_stata()


# we find that most defaults are from CA (California)


#8. Loan Amount vs loan status

loan %>%
  select(loan_status,loan_amnt_bin)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot(aes(x= loan_amnt_bin, fill = loan_status))+geom_bar(position ="dodge", color ="black")+
  labs(x = "Loan Amount", y = "Count", title="Loan Amount Vs Loan Status", fill="Loan Status")+
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(stat = "count", aes(y =(..count..), label = (..count..)) , position = position_dodge(width =1),vjust =-0.2)+
  theme_stata()+
  theme(axis.text.y = element_text(angle=0))

#we find that borrowers with average loan amount are more likely to default.

#9. Annual Income vs Loan Status
loan %>% 
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid") %>%
  ggplot(aes(x=annual_inc_bin,fill=loan_status))+ geom_bar( position="dodge",col="black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title="Annual Income Vs Loan Status",x="Annual Income",y="Count",fill="Loan Status")+
  geom_text(stat = "count", aes(y =(..count..), label = (..count..)) , position = position_dodge(width =1),vjust =-0.2)+
  theme_stata()+theme(axis.text.y = element_text(angle=0))

# we find that the borrowers with Average annual income are most likely to default


#10. Relationship between defaulters and public banckruptcy record

loan %>%
  select(loan_status,pub_rec_bankruptcies)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot(aes(x = pub_rec_bankruptcies, fill = loan_status))+geom_bar(position = "fill", color = "black")+
  scale_fill_brewer(palette = "Pastel1")+ 
  labs(title="Public Bankruptcy Record Vs Loan Status",x="No. of Records",y="Percentage",fill="Loan Status")+
  theme_stata()

#The borrowers having a higher number of public bankruptcy record default more than others
# Warning message due to 697 NA values


#11.Relationship between defaulters and Interest rate 
loan %>%
  select(loan_status,int_rate_bin)%>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot(aes(x= int_rate_bin, y = ..count.., fill = loan_status))+geom_bar(colour = "black", position = "dodge")+
  scale_fill_brewer(palette = "Pastel1")+ 
  labs(title="Interest Rate Vs Loan Status",x="Interest Rates",y="Count",fill="Loan Status")+
  geom_text(stat = "count", aes(y =(..count..), label = (..count..)) , position = position_dodge(width =1),vjust =-0.2)+
  theme_stata()
#This shows that borrowers with medium interest rate default more than any other rate  


#In CA, people are in maximum numbers applying for loan and they default the most. 
# Let's see, if there is any other factor working behind

# 12. Analysis of records having addr_state as CA:


loan %>%
  select(home_ownership, addr_state, loan_status)%>%
  filter(addr_state == "CA", home_ownership!="NONE")  %>%
  ggplot(aes(x = home_ownership, fill = loan_status)) + geom_bar(position = "dodge",color = "black") +
  scale_fill_brewer(palette = "Pastel1") + 
  geom_text(stat = "count", aes(y =(..count..), label = (..count..)) , position = position_dodge(width =1),vjust =-0.2)+ 
  labs(x = "Home Ownership", y= "Count", title = "Home ownership Vs Loan Status for CA Area",fill="Loan Status")+ theme_stata()

# For applicants in CA, Loan Staus charged off is more for Rented people.


################################## Derived Metrics ##############################

# Extract month of Issue Date

loan$issue_d_mnth <- months(loan$issue_d)

loan %>%
  select(issue_d_mnth, loan_status)%>%
   ggplot(aes(x = issue_d_mnth, fill = loan_status)) + geom_bar(position = "dodge",color = "black") +
  scale_fill_brewer(palette = "Pastel1") + 
  geom_text(stat = "count", aes(y =(..count..), label = (..count..)) , position = position_dodge(width =1),vjust =-0.2)+ 
  labs(x = "Issue Date Month", y= "Count", title = "Loan Status for each Months of Issue Date",fill="Loan Status")+ 
  theme_stata()+
  scale_x_discrete(limits=c("January","February","March","April","May", "June", "July", "August","September","October","November","December"))

# Max loans are defaulted when issued during months of Oct - Dec 


################## Multivariate Analysis #################

#Grades, interest rate and sub_grade

loan %>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid")  %>%
  ggplot()+geom_boxplot(aes(x=sub_grade,y = int_rate, fill = grade))+
  scale_fill_brewer(palette = "Pastel1")+
  labs( x = "Sub Grade", y = "Interest Rate", title = "Interest Rates Dependence on Grades and Sub Grades")+
  theme_stata()+
  theme(axis.text.y = element_text(angle=0))

#Interest rate shows a positive corelation with the sub-grades. Therefore giving the loan at a higher rate will 
#depend on the sub-grade of the employee

################### Multivariate ANalysis -Correlation ############

#Extractig values to correlate into matrix

lo_cor <- loan[, c("sub_grade",
                   "grade",
                   "int_rate",
                   "annual_inc",
                   "revol_util",
                   "dti",
                   "delinq_2yrs",
                   "home_ownership",
                   "loan_status",
                   "purpose",
                   "verification_status",
                   "loan_amnt")]

#converting the character type to numeric
lo_cor <- lo_cor %>%
  mutate(sub_grade = as.numeric(as.factor(sub_grade)),
         grade = as.numeric(as.factor(grade)),
         home_ownership = as.numeric(as.factor(home_ownership)),
         loan_status = as.numeric(as.factor(loan_status)),
         purpose = as.numeric(as.factor(purpose)),
         verification_status = as.numeric(as.factor(verification_status)))

# computing the correlation
lo_cor <- as.matrix(lo_cor)
col<- colorRampPalette(c("white", "black"))(20)
correlation <- round(cor(lo_cor, use = "complete.obs"),2)
head(correlation)
corrplot(correlation, method = "number", type = "upper", order = "FPC", col =col, bg = "lightblue", tl.col = "black", tl.srt = 45)

#It can be observed that 
#1. Sub grade has strong correlation with interest rate, revol_util and loan amount.
#2. interest rate and revol_util are moderately correlated
#3. revol_utli and dti are moderately correlated
#4. Loan amount, verification status and annual income also have correlation which is obvious
# Inference:
# Variables that effect the Defaulters are as follows
# 1. Purpose
# 2. Term
# 3. Home Ownership
# 4. Public Bankruptcy Records
# 5. State
# 6. Issue Month
# 7. revol_util_range
# 8. loan_amnt_range
