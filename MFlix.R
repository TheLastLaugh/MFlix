#(1) DATA GATHERING, AND BASIC SANITISATION AND TIDYING

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


#===============================================================================
#SHOULD THE BELOW CODE BE PUT INTO SECTION 2?
#===============================================================================

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


#===============================================================================
#We can probably remove the sessions data completely. there is only one row... kinda useless.
#Maybe don't even need to import in the first place
#===============================================================================
rm(sessions)


#(2) DATA TRANSFORMATION

#THE TRANSFORMATIONS FROM PART 3 SHOULD BE MOVED HERE. - EG SUBSET CREATIONS, DATA CLEAN UP, ETC.
#THIS SHOULD BE SORTED SO IT CAN BE SEEN WHAT SECTION IT IS FOR - EG GRAPH 1.





#(3) DATA ANALYSIS


#number of distinct years that movies (within this database) have been released in.
#length(db_movies_collection$distinct("year")) is the same as:
n_distinct(movies$year)

movies %>% group_by(year) %>% count



#potentially compare ratings by year? (like as years go on, how does the percentage of each type of rating change)

movieRatings = group_by(movies, rated)


#need to remove entries with:
# NOT RATED
# Not Rated
# NA
movieRatings = subset(movieRatings, !(rated %in% c("NOT RATED","Not Rated",NA)))

#graph The Data
ggplot(data = movieRatings) + 
  geom_bar(mapping = aes(x = rated)) + 
  ggtitle("Movie Rating Count(s)")






#select only the desired rating method(s)
# APPROVED
# G
# PASSED
# PG
# PG-13
# R
# UNRATED

primaryRatings = subset(movieRatings, rated %in% c("APPROVED","PASSED","G","PG","PG-13","R","UNRATED"))

#graph The Data
ggplot(data = primaryRatings) + 
  geom_bar(mapping = aes(x = factor(rated, level = c("G", "PG", "PG-13", "R", "APPROVED", "PASSED", "UNRATED"))), fill = "#FF0000", colour = "#000000") + 
  ggtitle("Movie Rating Count(s)") +
  xlab("Rating")






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

#other data to compare could be:


# Comments Per User (perhaps top 10 commentors)
#===============================================================================
#BELOW, COULD USE EMAIL TO REDUCE THE LIKELIHOOD OF 2 PEOPLE HAVING THE SAME NAME???
#===============================================================================


userComments = group_by(comments, name)


#===============================================================================
#Issues encountered (Maybe can talk about, IDK)

#userCommentRanges = split(userComments, cut(count(userComments)$n, seq(0,300,by=50)))
#userCommentsRanges = group_by(comments, gr = cut(count(userComments)$n, breaks = seq(0, 200, by = 50)) )

#===============================================================================


userCommentCounts = count(userComments)$n

userCommentRanges = c(sum(userCommentCounts >= 0 & userCommentCounts < 50), sum(userCommentCounts >= 50 & userCommentCounts < 100), sum(userCommentCounts >= 100 & userCommentCounts < 150), sum(userCommentCounts >= 150 & userCommentCounts < 200), sum(userCommentCounts >= 200 & userCommentCounts < 250), sum(userCommentCounts >= 250 & userCommentCounts < 300))

userCommentRanges = data.frame(CommentCountRange = c("0 - 50", "50 - 100", "100 - 150", "150 - 200", "200-250", "250-300"), 
                               Count = userCommentRanges)



#summarise(userComments)
#print(summarise(userComments), n = 234)


#===============================================================================
#GROUP DATA BASED ON USERS COMMENT COUNT. EG:  0-50, 50 - 100, 100 - 150, 150 - 200, 200 - 250, 250 - 300
#===============================================================================


#===============================================================================
#COULD DO THE SAME THING, BUT COMMENTS PER MOVIE INSTEAD
#===============================================================================

ggplot(userCommentRanges, aes(x = factor(CommentCountRange, levels = c("0 - 50", "50 - 100", "100 - 150", "150 - 200", "200-250", "250-300")), y = Count)) +
  geom_bar(stat = "identity", fill = "#FF0000", colour = "#000000") + 
  ggtitle("Number of User Comments within Specified Ranges") + 
  xlab("Comments Created per User") +
  ylab("Count of Users")




# A map of theatres

#===============================================================================
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


# number of theatres per state or post code or somethings





#not sure if these are as good
#===============================================================================
# movies ratings / movies where the rating matches or something
# passwords - compare to a hash file (eg: rockyou.txt), and view security perhaps?
# line graph of total comments over time

# Movie writers / language / poster / tomatos info (dvd date / length, rating / meter, dot plot of rating vs no. of reviews)
#






#(4) DATA MODELLING 


#===============================================================================
#
#===============================================================================

