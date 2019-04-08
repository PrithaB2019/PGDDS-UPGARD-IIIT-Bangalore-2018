### Importing the packages to be used ###
setwd("D:/User/IIT-B/Investment Case Study Assignment")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
library(stringr)
library(dplyr)
library(tidyr)
library(countrycode)

####### Checkpoint 1- Data Cleaning #######

# reading companies and rounds2 datasets into companies and rounds2 dataframes respectively#

companies <- read.delim("companies.txt", header = TRUE, stringsAsFactors = FALSE)
View(companies)

rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
View(rounds2)


# Finding unique companies in rounds2 #

rounds2$company_permalink = tolower(rounds2$company_permalink)
unique_rounds2 <- n_distinct(rounds2$company_permalink)
unique_rounds2


# Finding unique companies in companies dataframe#

companies$permalink = tolower(companies$permalink)
unique_companies <- n_distinct(companies$permalink)
unique_companies


#Are there any companies in the rounds2 file which are not present in companies? Answer YES/NO

setdiff(rounds2$company_permalink, companies$permalink)

#     Ans: No

# Merging the two dataframes into a master_frame #

master_frame <-  merge(rounds2, companies,
        by.x = "company_permalink",
        by.y = "permalink",
        all = TRUE)

#How many observations are present in master_frame?

nrow(master_frame)

#     Ans. 114949



####### Checkpoint 2 - Funding Type Analysis #######

# Avergage funding of venture type, angel type, seed type and private_equity type #

avg_funding <- master_frame %>%
  filter(funding_round_type %in% c("venture", "angel", "seed", "private_equity")) %>%
  group_by(funding_round_type) %>%
  summarise_at(vars(raised_amount_usd), mean, na.rm = TRUE)

colnames(avg_funding) <- c("fund_type", "avg_raised_amt")
avg_funding

#based on the average investment amount calculated above, which investment type do you think is 
#the most suitable for SPARK FUNDS?


FT <- filter(avg_funding, avg_raised_amt >= 5000000 &
               avg_raised_amt <= 15000000)
FT

# Ans. - venture

####### Checkpoint -3 Country Analysis #######

##Finding the top 9 countries with the highest investment in decreasing order #

top9 <- master_frame %>%
  filter(funding_round_type == "venture" & country_code != "") %>%
  group_by(country_code) %>%
  summarise_at(vars(raised_amount_usd), sum, na.rm = T) %>%
  arrange(desc(raised_amount_usd)) %>%
  top_n(9)


## converting the country code to country name using countrycode package ##

code <- top9$country_code
top9[,1] <-  countrycode(code,'iso3c','country.name')

top9

#TOP THREE ENGLISH SPEAKING COUNTRIES:

#RANK 1: USA (USA)
#RANK 2: UNITED KINGDOMS (GBR)
#RANK 3: INDIA (IND)


####### Checkpoint - 4 Sector Analysis 1 #######

# import the mapping.csv file in mapping dataframe #

mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)

# Since Na values are replaced with 0 in excel we need to clean the data before starting with our analysis ##

mapping$category_list <- gsub("0", "Na", mapping$category_list)

# Converting mapping file to a long format #

mapping <-
  gather(mapping, "main_sector", "value", 2:10)

# removing the values with 0 to keep the cleaned data #

mapping <- mapping[!(mapping$value == 0), ]

# Removing the value column as it is of no significance anymore #

mapping <- mapping[,-3]

View(mapping)

# seperating the primary sector from the category list in the master_frame 

master_frame <- master_frame %>%
  mutate(primary_sector = as.character(str_extract_all(
    master_frame$category_list, "^([^\\|])+"
  )))

## coverting the category_list of mapping and master_frame to lower case so that there
#is no discrepensy within the data

mapping$category_list <- tolower(mapping$category_list)

master_frame$primary_sector <- tolower(master_frame$primary_sector)

# Merging the master_frame and mapping dataframe to map each primary sector to one of the eight main sectors 

master_frame <- merge(master_frame, mapping,
                 by.x = "primary_sector", 
                 by.y = "category_list",
                 all.x = T)
View(master_frame)


####### Checkpoint - 5 Sector Analysis 2 #######


#sector analysis for top 3 english speaking countries 

## Creating a dataframe from master frame for top 3 countries:
# Country 1: USA
# Country 2: UK
# Country 3: India
# FT = VENTURE
# Range = 5 to 15 million USD


USA_master_frame <- master_frame %>%
  filter(
    country_code == "USA",
    funding_round_type == "venture",
    raised_amount_usd >= 5000000 &
      raised_amount_usd <= 15000000
  )
  
GBR_master_frame <- master_frame %>%
  filter(
    country_code == "GBR",
    funding_round_type == "venture",
    raised_amount_usd >= 5000000 &
      raised_amount_usd <= 15000000
  )

IND_master_frame <- master_frame %>%
  filter(
    country_code == "IND",
    funding_round_type == "venture",
    raised_amount_usd >= 5000000 &
      raised_amount_usd <= 15000000
  )


# Creating a function to form a dataframe from the master frame to find the count of investments and amount of investments per sector
#create data frames with groupings on main sector

group_main_sector <- function(p)
{
  group_by(p, main_sector) %>%
    summarise(
      sum_rasied_amount_usd = sum(raised_amount_usd, na.rm = T),
      count_rasied_amount_usd = length(raised_amount_usd)
    )%>%
    arrange(desc(count_rasied_amount_usd))
}

USA <- group_main_sector(USA_master_frame)
GBR <- group_main_sector(GBR_master_frame)
IND <- group_main_sector(IND_master_frame)



# finding the total number and total amount of investments #

### USA ###

total_number_of_investments_USA <- sum(master_frame_USA$count_rasied_amount_usd)
total_amount_of_investment_USA <- sum(master_frame_USA$sum_rasied_amount_usd)

### GBR ###

total_number_of_investments_UK <- sum(master_frame_UK$count_rasied_amount_usd)
total_amount_of_investment_UK <- sum(master_frame_UK$sum_rasied_amount_usd)

### IND ###

total_number_of_investments_IND <- sum(master_frame_IND$count_rasied_amount_usd)
total_amount_of_investment_IND <- sum(master_frame_IND$sum_rasied_amount_usd)


## Merging the 2 dataframes on the basis of main_sector to have all the columns of the master_frame along with count and sum of investments

D1 <- merge(USA_master_frame, USA, by = "main_sector", all = T)

D2 <- merge(GBR_master_frame, GBR, by = "main_sector", all = T)

D3 <- merge(IND_master_frame, IND, by = "main_sector", all = T)



#finding the company which recieved highest and secong highest investment sector count-wise

highest_inv_company_by_sector <- function(r)
  {  r %>%
    filter(main_sector == "Others") %>%
    group_by(name) %>%
    summarise(total_inv = sum(raised_amount_usd)) %>%
    arrange(desc(total_inv)) %>%
    top_n(1)
}


second_highest_inv_company_by_sector <- function(r)
{ r %>%
    filter(main_sector == "Social..Finance..Analytics..Advertising") %>%
    group_by(name) %>%
    summarise(total_inv = sum(raised_amount_usd)) %>%
    arrange(desc(total_inv)) %>%
    top_n(1)
}

USA_highest_inv_company_by_sector <- highest_inv_company_by_sector(D1)

USA_second_highest_inv_company_by_sector <- second_highest_inv_company_by_sector(D1)

UK_highest_inv_company_by_sector <- highest_inv_company_by_sector(D2)

UK_second_highest_inv_company_by_sector <- second_highest_inv_company_by_sector(D2)

IND_highest_inv_company_by_sector <- highest_inv_company_by_sector(D3)

IND_second_highest_inv_company_by_sector <- second_highest_inv_company_by_sector(D3)

# Answers

USA_highest_inv_company_by_sector 
USA_second_highest_inv_company_by_sector

UK_highest_inv_company_by_sector
UK_second_highest_inv_company_by_sector 

IND_highest_inv_company_by_sector 
IND_second_highest_inv_company_by_sector 


###### Checkpoint -6 Tableau Plots ########

# exporting the master frame for creating visualisations in Tableau 

write.csv(master_frame,"final.csv")
