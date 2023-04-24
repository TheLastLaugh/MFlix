#------------------------------------------------------------------
#(1) DATA GATHERING, AND BASIC SANITISATION AND TIDYING
#------------------------------------------------------------------

#INSTALL REQUIRED LIBRARIES
#install.packages("mongolite")
#install.packages("tidyverse")
#install.packages("dplyr")

#CALL REQUIRED LIBRARIES
library(mongolite)
library(tidyverse)
library(dplyr)

#DEFINE (ZEC'S) CONNECTION TO THE 'BILL NYE THE NAZI SPY' DATABASE
connection_string = 'mongodb+srv://Zec:PP@billnyethenazispy.w5zka2a.mongodb.net/?retryWrites=true&w=majority'

#COLLECT AND PULL ALL OF THE RELATED DATA FROM THE 'sample_mflix' MONGODB
db_comments_collection = mongo(collection="comments", db="sample_mflix", url=connection_string)
db_movies_collection = mongo(collection="movies", db="sample_mflix", url=connection_string)
db_sessions_collection = mongo(collection="sessions", db="sample_mflix", url=connection_string)
db_theatres_collection = mongo(collection="theaters", db="sample_mflix", url=connection_string)
db_users_collection = mongo(collection="users", db="sample_mflix", url=connection_string)

#CONVERT ALL OF THE DATA (PULLED FROM MONGO) INTO DATAFRAMES FOR USE
comments = as.data.frame(db_comments_collection$find())
movies = as.data.frame(db_movies_collection$find())
sessions = as.data.frame(db_sessions_collection$find())
theatres = as.data.frame(db_theatres_collection$find())
users = as.data.frame(db_users_collection$find())

#FIX AN ISSUE WITH THE CONVERTED 'USERS' DATAFRAME WHERE AN EXTRA COLUMN EXISTS THAT MAKES THE DATA UNUSABLE ("preferences" as a dataframe within the dataframe).
#THIS SELECTS ALL COLUMNS (EXCEPT THE BROKEN FRAME)
users = select(users, name, email, password)

#REMOVE THE OLD (NOW IRRELEVANT) DATA PULLED FROM MONGO
rm(db_comments_collection)
rm(db_movies_collection)
rm(db_sessions_collection)
rm(db_theatres_collection)
rm(db_users_collection)

#REMOVE SESSIONS DATAFRAME (IRRELEVANT)
rm(sessions)

#------------------------------------------------------------------
#(2) DATA TRANSFORMATION
#------------------------------------------------------------------

#THE TRANSFORMATIONS FROM PART 3 SHOULD BE MOVED HERE. - EG SUBSET CREATIONS, DATA CLEAN UP, ETC.
#THIS SHOULD BE SORTED SO IT CAN BE SEEN WHAT SECTION IT IS FOR - EG GRAPH 1.

#REMOVE BAD ENTRIES THAT HAVE YEAR ERRORS (e.g: 2014è, 2006è2007)
movies = movies[!grepl('è', movies$year),]

#DATAFRAME OF NUMBER OF MOVIES GROUPED BY YEAR
movieCount = movies %>% group_by(year) %>% count

#VISUAL REPRESENTATION OF MOVIECOUNT DATAFRAME
ggplot(data = movieCount) +
  geom_point(mapping= aes(x = year, y = n))




#(3) DATA ANALYSIS

#group by can be used, but is not at all needed for graphing
#===============================================================================
#movieRatings = group_by(movies, rated)
#summarise(movieRatings)
#===============================================================================


movieRatings = movies

#need to remove invalid entries (based on rating):
# NOT RATED
# Not Rated
# NA
movieRatings = subset(movieRatings, !(rated %in% c("NOT RATED","Not Rated",NA)))

#graph The Data
ggplot(data = movieRatings) + 
  geom_bar(mapping = aes(x = rated), fill = "#FF0000", colour = "#000000") + 
  ggtitle("Movie Rating Count(s)") + 
  xlab("Rating")



#===============================================================================
#CHECK THAT THESE RATING METHODS ARE THE CORRECT ONES
#select only the desired rating method(s) (for this graph)
# APPROVED
# G
# PASSED
# PG
# PG-13
# R
# UNRATED
primaryRatings = subset(movieRatings, rated %in% c("APPROVED","PASSED","G","PG","PG-13","R","UNRATED"))

#graph The Data of only the selected "primaryRatings"
ggplot(data = primaryRatings) + 
  geom_bar(mapping = aes(x = factor(rated, level = c("G", "PG", "PG-13", "R", "APPROVED", "PASSED", "UNRATED"))), fill = "#FF0000", colour = "#000000") + 
  ggtitle("Movie Rating Count(s)") +
  xlab("Rating")



#select only the desired rating method(s) (for this graph)
# TV-G
# TV-PG
# TV-14
# TV-MA
TVRatings = subset(movieRatings, rated %in% c("TV-G", "TV-PG", "TV-14", "TV-MA"))

#graph The Data
ggplot(data = TVRatings) + 
  geom_bar(mapping = aes(x = factor(rated, level = c("TV-G", "TV-PG", "TV-14", "TV-MA"))), fill = "#FF0000", colour = "#000000") + 
  ggtitle("Movie TV-Rating Count(s)") + 
  xlab("Rating")




#Can use the following to check what ratings exist
#summarise(movieRatings)



#===============================================================================
#I THINK AS WELL AS GRAPHS, WE NEED TO DO DATA SUMMARIES, STATISTICS, DISTRIBUTIONS, ETC...
#===============================================================================




#===============================================================================
#BELOW, COULD USE EMAIL TO REDUCE THE LIKELIHOOD OF 2 PEOPLE HAVING THE SAME NAME???
#===============================================================================
#PERHAPS :
# userComments = group_by(comments, email)

userComments = group_by(comments, name)



#===============================================================================
#Issues encountered (Maybe we can talk about, IDK)
#THIS IS NOT A METHOD TO BREAK UP THE DATASET

#userCommentRanges = split(userComments, cut(count(userComments)$n, seq(0,300,by=50)))
#userCommentsRanges = group_by(comments, gr = cut(count(userComments)$n, breaks = seq(0, 200, by = 50)) )
#===============================================================================


userCommentCounts = count(userComments)$n


#===============================================================================
#THIS CODE IS NOT NECESSARY WITH GEOM_HISTOGRAM, BUT IS WHEN CREATING GEOM_BAR

#userCommentRanges = c(sum(userCommentCounts >= 0 & userCommentCounts < 50), sum(userCommentCounts >= 50 & userCommentCounts < 100), sum(userCommentCounts >= 100 & userCommentCounts < 150), sum(userCommentCounts >= 150 & userCommentCounts < 200), sum(userCommentCounts >= 200 & userCommentCounts < 250), sum(userCommentCounts >= 250 & userCommentCounts < 300))

#userCommentRanges = data.frame(CommentCountRange = c("0 - 50", "50 - 100", "100 - 150", "150 - 200", "200-250", "250-300"), 
#                               Count = userCommentRanges)

#NOW OUTDATED CODE
#ggplot(userCommentRanges, aes(x = factor(CommentCountRange, levels = c("0 - 50", "50 - 100", "100 - 150", "150 - 200", "200-250", "250-300")), y = Count)) +
#  geom_bar(stat = "identity", fill = "#FF0000", colour = "#000000") + 
#  ggtitle("Number of User Comments within Specified Ranges") + 
#  xlab("Comments Created per User") +
#  ylab("Count of Users")

#
#===============================================================================

userCommentCounts = data.frame(count = userCommentCounts)

#this histogram shows the same information / data as the bar graph created in the 'bad' code above.
ggplot(data = userCommentCounts, aes(x = count)) +
  geom_histogram(bins = 6, fill = "#FF0000", colour = "#000000") + 
  ggtitle("Number of User Comments within Specified Ranges") + 
  xlab("Comments Created per User") +
  ylab("Count of Users")




#histogram with 30 bins (an outlier is present), but the right side of the data shows a normal / binomial distribution
ggplot(data = userCommentCounts, aes(x = count)) +
  geom_histogram(fill = "#FF0000", colour = "#000000") + 
  ggtitle("Number of User Comments within Specified Ranges") + 
  xlab("Comments Created per User") +
  ylab("Count of Users")



#remove all users with less than 5 comments (as outliers). Allows better viewing of the approximate normal distribution.
userCommentCountsSubset = subset(userCommentCounts, !(count < 5))

#histogram with 30 bins
ggplot(data = userCommentCountsSubset, aes(x = count)) +
  geom_histogram(fill = "#FF0000", colour = "#000000", bins = 30) + 
  ggtitle("Number of User Comments within Specified Ranges") + 
  xlab("Comments Created per User") +
  ylab("Count of Users")

#histogram with 10 bins (better approximates normal / binomial distribution)
ggplot(data = userCommentCountsSubset, aes(x = count)) +
  geom_histogram(fill = "#FF0000", colour = "#000000", bins = 10) + 
  ggtitle("Number of User Comments within Specified Ranges") + 
  xlab("Comments Created per User") +
  ylab("Count of Users")


#===============================================================================
#COULD DO THE SAME THING, BUT COMMENTS PER MOVIE INSTEAD
#===============================================================================









# A map of theatres

#==============================================================================
#IMPROVEMENT NEEDED HERE
#===============================================================================

Map = map_data("world")

USMap = map_data("usa")

theatreLocations = data.frame(t(sapply(theatres$location$geo$coordinates, c)))


ggplot() + 
  geom_polygon(data = Map, mapping = aes(long, lat, group = group), fill = "#ffffff", colour = "#000000") + 
  coord_quickmap() + 
  geom_point(theatreLocations, mapping = aes(X1, X2), colour = "#FF0000")

ggplot() + 
  geom_polygon(data = USMap, mapping = aes(long, lat, group = group), fill = "#ffffff", colour = "#000000") + 
  coord_quickmap() + 
  geom_point(theatreLocations, mapping = aes(X1, X2), colour = "#FF0000")

#do another map with the state lines shown?



# number of theatres per state or post code or somethings




#other data to compare could be:

#not sure if these are as good
#===============================================================================
# movies ratings / movies where the rating matches or something
# passwords - compare to a hash file (eg: rockyou.txt), and view security perhaps?
# line graph of total comments over time

# Movie writers / language / poster / tomatos info (dvd date / length, rating / meter, dot plot of rating vs no. of reviews)

#number of distinct years that movies (within this database) have been released in.
#length(db_movies_collection$distinct("year")) is the same as:
n_distinct(movies$year)


#potentially compare ratings by year? (like as years go on, how does the percentage of each type of rating change)
#IF THIS IS DONE, PUT IT NEAR THE OTHER 'RATING' DATA / GRAPHS






#(4) DATA MODELLING 


#===============================================================================
#
#===============================================================================

