#(1) DATA GATHERING, AND BASIC SANITISATION AND TIDYING

#INSTALL REQUIRED LIBRARIES
#install.packages("mongolite")
#install.packages("tidyverse")
#install.packages("dplyr")

#CALL REQUIRED LIBRARIES
library(mongolite)
library(tidyverse)
library(dplyr)

#SET THE SEED FOR THE FILE TO MAKE 'RANDOM' RESULTS REPRODUCABLE 
set.seed(0)

#DEFINE (ZEC'S) CONNECTION TO THE 'BILL NYE THE NAZI SPY' DATABASE
connection_string = 'mongodb+srv://Zec:PP@billnyethenazispy.w5zka2a.mongodb.net/?retryWrites=true&w=majority'

#COLLECT AND PULL ALL OF THE RELATED DATA FROM THE 'sample_mflix' MONGODB
db_comments_collection = mongo(collection="comments", db="sample_mflix", url=connection_string)
db_movies_collection = mongo(collection="movies", db="sample_mflix", url=connection_string)
db_theatres_collection = mongo(collection="theaters", db="sample_mflix", url=connection_string)
db_users_collection = mongo(collection="users", db="sample_mflix", url=connection_string)


#CONVERT ALL OF THE DATA (PULLED FROM MONGO) INTO DATAFRAMES FOR USE
comments = as.data.frame(db_comments_collection$find())
movies = as.data.frame(db_movies_collection$find())
theatres = as.data.frame(db_theatres_collection$find())
users = as.data.frame(db_users_collection$find())

#FIX AN ISSUE WITH THE CONVERTED 'USERS' DATAFRAME WHERE AN EXTRA COLUMN EXISTS THAT MAKES THE DATA UNUSABLE ("preferences" as a dataframe within the dataframe).
#THIS SELECTS ALL COLUMNS (EXCEPT THE BROKEN FRAME)
users = select(users, name, email, password)

#REMOVE THE OLD (NOW IRRELEVANT) DATA PULLED FROM MONGO
rm(db_comments_collection)
rm(db_movies_collection)
rm(db_theatres_collection)
rm(db_users_collection)





#group by can be used, but is not at all needed for graphing - DISCUSS THIS IN THE REPORT
#===============================================================================
#movieRatings = group_by(movies, rated)
#print(summarise(movieRatings), n = 22)
#===============================================================================


#remove invalid entries (based on rating):
# NOT RATED
# Not Rated
# NA
movieRatings = subset(movies, !(rated %in% c("NOT RATED","Not Rated",NA)))

#graph The Data
ggplot(data = movieRatings) + 
  geom_bar(mapping = aes(x = rated), fill = "#FF0000", colour = "#000000") + 
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

#CLEANUP VARIABLES
rm(movieRatings)
rm(primaryRatings)
rm(TVRatings)


#===============================================================================
#BELOW, COULD USE EMAIL TO REDUCE THE LIKELIHOOD OF 2 PEOPLE HAVING THE SAME NAME???
#===============================================================================


#PERHAPS :

#dont use name as 2 people could have the same name, but not email. DISCUSS
# userComments = group_by(comments, name)

#group the user comments by the user's email
userComments = group_by(comments, email)


#===============================================================================
#Issues encountered (Maybe can talk about, IDK)

#userCommentRanges = split(userComments, cut(count(userComments)$n, seq(0,300,by=50)))
#userCommentsRanges = group_by(comments, gr = cut(count(userComments)$n, breaks = seq(0, 200, by = 50)) )
#===============================================================================

#count the number of comments per user. GROUP_BY IS REQUIRED TO ACHEIVE THIS - DISCUSS
userCommentCounts = count(userComments)$n


#===============================================================================
#
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

#convert the data to a dataframe to be graphed
userCommentCounts = data.frame(count = userCommentCounts)

#this histogram shows the same information / data as the bar graph created in the 'bad' code above.
ggplot(data = userCommentCounts, aes(x = count)) +
  geom_histogram(bins = 6, fill = "#FF0000", colour = "#000000") + 
  ggtitle("Number of User Comments within Specified Ranges") + 
  xlab("Comments Created per User") +
  ylab("Count of Users")



#summarise(userComments)
#print(summarise(userComments), n = 234)

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



#TO BE HONEST, IM NOT SURE HOW TO ANALYSE THIS.
#===============================================================================

#Convert the 'normal' dataset to a matrix for calculations
userCommentCountsSubset = as.matrix(userCommentCountsSubset)

#5 number summary
summary(userCommentCountsSubset)

#standard deviation
print(paste("Standard Deviation: ", sd(userCommentCountsSubset)))

#create data that follows a normal distribution
normal_data = rnorm(200)

#QQPlot compares true normal data to a sample set (in this case, the subset), and a
#straight line indicates the data is normal.
#This is seen here, hence the distribution of the subset of comments is normal
qqplot(normal_data, userCommentCountsSubset, 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
#===============================================================================


#CLEANUP VARIABLES
rm(userComments)
rm(userCommentCounts)
rm(userCommentCountsSubset)
rm(normal_data)


#===============================================================================
#COULD DO THE SAME THING, BUT COMMENTS PER MOVIE INSTEAD
#===============================================================================




# A map of theatre locations

#OBTAIN MAP INFO 
Map = map_data("world")

USMap = map_data("usa")


#CONVERT THE LIST TO A DATAFRAME FOR USE IN GGPLOT
#THE INITIAL FORMATTING OF THE COORDINATES WAS NOT FRIENDLY TO CONVERT
theatreLocations = data.frame(t(sapply(theatres$location$geo$coordinates, c)))

#PLOT THEATRES OVER A MAP OF THE US
ggplot() + 
  geom_polygon(data = USMap, mapping = aes(long, lat, group = group), fill = "#ffffff", colour = "#000000") + 
  coord_quickmap() + 
  geom_point(theatreLocations, mapping = aes(X1, X2), colour = "#FF0000")

#PLOT THE THEATERS OVER A MAP OF THE WORLD 
#AS THE THEATRES ARE NOT JUST CONTAINED TO THE MAIN US LAND MASS 
ggplot() + 
  geom_polygon(data = Map, mapping = aes(long, lat, group = group), fill = "#ffffff", colour = "#000000") + 
  coord_quickmap() + 
  geom_point(theatreLocations, mapping = aes(X1, X2), colour = "#FF0000")



#CLEANUP VARIABLES
rm(Map)
rm(USMap)
rm(theatreLocations)


# Movie Language

#SELECT RELEVANT DATA COLUMNS
movieLanguages = select(movies, languages)

#REMOVE NULL ENTRIES
movieLanguages = subset(movieLanguages, !(languages %in% "NULL"))

#ENTRIES WITH MORE THAN ONE LANGUAGE (CONTAINED WITHIN A LIST) ARE REMOVED FROM THE CONTAINED LIST(S), AND ALL LANGUAGES ARE PUT INTO SEPARATE ENTREIS
movieLanguages = data.frame(languages = unlist(movieLanguages$languages))


movieLanguages = group_by(movieLanguages, languages)



movieLanguages = data.frame(languages = summarise(movieLanguages),
                            count = count(movieLanguages)$n)

#OR THE FOLLOWING CODE CAN BE USED (But it doesnt really do it how is desired):
# count(movies, languages)

#ARRANGE MOVIES IN DESCENDING ORDER ('-' INDICATES THE DESCENDING AS DEFAULT IS ASCENDING)
movieLanguages = arrange(movieLanguages, -count)

#GRAPH ALL MOVIES
ggplot(data = movieLanguages) +
  geom_bar(stat = 'identity', mapping = aes(x = languages, y = count), fill = "#FF0000", colour = "#000000") +
  ggtitle("Count of Movies Utilising Each Language")


#ONLY CHOOSE LANGUAGES WITH A COUNT GREATER THAN 500
movieLanguages = subset(movieLanguages, count > 500)

ggplot(data = movieLanguages) +
  geom_bar(stat = 'identity', mapping = aes(x = languages, y = count), fill = "#FF0000", colour = "#000000") +
  ggtitle("Language count (Greater than 500) for the Sample Movies")


#ONLY GRAPH THE TOP 5 LANGUAGES
ggplot(data = head(movieLanguages, 5)) +
  geom_bar(stat = 'identity', mapping = aes(x = languages, y = count), fill = "#FF0000", colour = "#000000") +
  ggtitle("Count of Movies Utilising Each Language (Top 5 Languages)")



#CLEANUP VARIABLES
rm(movieLanguages)



#not sure if these are as good
#===============================================================================
# movies ratings / movies where the rating matches or something
# passwords - compare to a hash file (eg: rockyou.txt), and view security perhaps?
# line graph of total comments over time


# Movie writers / language / poster / tomatos info (dvd date / length, rating / meter, dot plot of rating vs no. of reviews)
# comment date? (compare to movie release too ?)

# Movie Genres
# Movie type
# Movie country


#number of distinct years that movies (within this database) have been released in.
#length(db_movies_collection$distinct("year")) is the same as:'


#n_distinct(movies$year)

#print(movies %>% group_by(year) %>% count, n = 133)

#movies = subset(movies, )


#potentially compare ratings by year? (like as years go on, how does the percentage of each type of rating change)
#IF THIS IS DONE, PUT IT NEAR THE OTHER 'RATING' DATA / GRAPHS






#(4) DATA MODELLING 


#===============================================================================



#===============================================================================

