 
#	Import the Bollywood data set in Rstudio in a variable named bollywood

  bollywood <- read.csv(file.choose())
  View(bollywood)

#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)
	 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)
     
	last10subset <- tail(bollywood, n=10) #creating a subset of last 10 movie details
	last_10 <- c(last10subset$Movie)
print(last_10)
	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector

na_bollywood <- sum(is.na(bollywood))     #Counting the number of NA in bollywood dataset
na_bollywood                             # to check: is.vector(na_bollywood)	


#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 

top_collection <- subset(bollywood, Tcollection == max(bollywood$Tcollection), na.rm=TRUE)
top_collection
top_movie <- top_collection$Movie
print(top_movie)

#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

top_2_movieset <- subset(bollywood, !Tcollection == max(bollywood$Tcollection))   #Excluding the top collection movie from the dataset
str(top_2_movieset)              #To check it has 60 observation(1 less than bollywood)
top_2_collection <- subset(top_2_movieset, Tcollection == max(top_2_movieset$Tcollection))
top_2_collection                  #Creating a subset including the second highest Top collection movie
top_2_movie <- top_2_collection$Movie
print(top_2_movie)


	  
	
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
 shahrukh
	akshay <- subset(bollywood, Lead == "Akshay")
	akshay
	amitabh <- subset(bollywood, Lead  == "Amitabh")
amitabh
# You can view what the above data frames look like

		   
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
  shahrukh_collection <- sum(shahrukh$Tcollection, na.rm= TRUE)
  shahrukh_collection
	akshay_collection <- sum(akshay$Tcollection, na.rm= TRUE)
	akshay_collection
	amitabh_collection <- sum(amitabh$Tcollection, na.rm= TRUE)
	amitabh_collection
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.

summary(bollywood$Verdict)
   
#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  
max <- sapply(bollywood[c("Ocollection","Wcollection","Fwcollection","Tcollection")],max, na.rm = TRUE)
max
#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector
max_Ocollection <- subset(bollywood, Ocollection == max(bollywood$Ocollection, na.rm= TRUE))
max_Ocollection
max_Wcollection <- subset(bollywood, Wcollection == max(bollywood$Wcollection, na.rm= TRUE))
max_Wcollection
max_Fwcollection <- subset(bollywood, Fwcollection == max(bollywood$Fwcollection, na.rm= TRUE))
max_Fwcollection
top_collection         #Already created subset in Q2 for max Tcollection
                         
max_collection <- rbind(max_Ocollection, max_Wcollection, max_Fwcollection, top_collection)
movie_result <- c(max_collection$Movie)
print(movie_result)      # this gives result in the order required, as Rbind done in same order
    




    