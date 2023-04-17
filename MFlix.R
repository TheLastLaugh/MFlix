#(1) DATA GATHERING, AND BASIC SANITISATION AND TIDYING

#INSTALL REQUIRED LIBRARIES
#install.packages("mongolite")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("data.table")

#CALL REQUIRED LIBRARIES
library(mongolite)
library(tidyverse)
library(dplyr)
library(data.table)

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

rm(connection_string)



#group by can be used, but is not at all needed for graphing - DISCUSS THIS IN THE REPORT
#===============================================================================
#movieRatings = group_by(movies, rated)
#print(summarise(movieRatings), n = 22)
#===============================================================================


#REMOVE INVALID ENTRIES (BASED ON RATING)
# NOT RATED
# Not Rated
# NA
movieRatings = subset(movies, !(rated %in% c("NOT RATED","Not Rated",NA)))

#GENERATE A BAR GRAPH OF THE DATASET
ggplot(data = movieRatings) + 
  geom_bar(mapping = aes(x = rated), fill = "#FF0000", colour = "#000000") +
  xlab("Rating") + 
  ylab("Count") +
  ggtitle("Movie Rating Count(s)")



#SELECT ONLY THE DESIRED RATING METHOD(S) - CREATE A SUBSET OF THE DATA
# APPROVED
# G
# PASSED
# PG
# PG-13
# R
# UNRATED
primaryRatings = subset(movieRatings, rated %in% c("APPROVED","PASSED","G","PG","PG-13","R","UNRATED"))

#GENERATE A BAR GRAPH OF THE SUBSET OF THE DATA
ggplot(data = primaryRatings) + 
  geom_bar(mapping = aes(x = factor(rated, level = c("G", "PG", "PG-13", "R", "APPROVED", "PASSED", "UNRATED"))), fill = "#FF0000", colour = "#000000") + 
  xlab("Rating") + 
  ylab("Count") + 
  ggtitle("Movie Rating Count(s)")
  


#SELECT ONLY THE DESIRED RATING METHOD(S) - CREATE A SUBSET OF THE DATA
# TV-G
# TV-PG
# TV-14
# TV-MA
TVRatings = subset(movieRatings, rated %in% c("TV-G", "TV-PG", "TV-14", "TV-MA"))

#GENERATE A BAR GRAPH OF THE SUBSET OF THE DATA
ggplot(data = TVRatings) + 
  geom_bar(mapping = aes(x = factor(rated, level = c("TV-G", "TV-PG", "TV-14", "TV-MA"))), fill = "#FF0000", colour = "#000000") + 
  xlab("Rating") + 
  ylab("Count") + 
  ggtitle("Movie TV-Rating Count(s)")


#CLEANUP VARIABLES
rm(movieRatings)
rm(primaryRatings)
rm(TVRatings)




#dont use name as 2 people could have the same name, but not email. DISCUSS

#GROUP THE COMMENTS BY THE USER'S EMAIL
userComments = group_by(comments, email)


#===============================================================================
#Issues encountered (Maybe can talk about, IDK)
#REMOVE EVENTUALLY

#userCommentRanges = split(userComments, cut(count(userComments)$n, seq(0,300,by=50)))
#userCommentsRanges = group_by(comments, gr = cut(count(userComments)$n, breaks = seq(0, 200, by = 50)) )
#===============================================================================


#COUNT THE NUMBER OF COMMENTS PER USER
#GROUP_BY IS REQUIRED TO ACHIEVE THIS - DISCUSS ================================
userCommentCounts = count(userComments)$n


#===============================================================================
#REMOVE EVENTUALLY
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

#CONVERT THE DATA TO A DATARAME TO BE GRAPHED
userCommentCounts = data.frame(count = userCommentCounts)

#USING HISTOGRAMS AUTOMATICALLY SEPARATED THE DATA INTO BINS
#THIS MAKES THE MANUAL METHOD OF DOING THIS UNNECESSARY

#6 BIN HISTOGRAM OF ENTIRE DATASET
ggplot(data = userCommentCounts, aes(x = count)) +
  geom_histogram(bins = 6, fill = "#FF0000", colour = "#000000") + 
  xlab("Comments Created Per User") +
  ylab("Count of Users") +
  ggtitle("Number of User Comments within Specified Ranges")

#30 BIN HISTOGRAM OF ENTIRE DATASET
ggplot(data = userCommentCounts, aes(x = count)) +
  geom_histogram(fill = "#FF0000", colour = "#000000", bins = 30) + 
  xlab("Comments Created per User") +
  ylab("Count of Users") +
  ggtitle("Number of User Comments within Specified Ranges")


#CREATE A SUBSET OF THE DATA THAT REMOVES THE OUTLIERS WHERE THE COUNT IS LESS THAN 5
userCommentCountsSubset = subset(userCommentCounts, !(count < 5))

#30 BIN HISTOGRAM OF THE SUBSET OF DATA
ggplot(data = userCommentCountsSubset, aes(x = count)) +
  geom_histogram(fill = "#FF0000", colour = "#000000", bins = 30) + 
  xlab("Comments Created per User") +
  ylab("Count of Users") +
  ggtitle("Number of User Comments within Specified Ranges (Outliers Removed)")


#10 BIN HISTOGRAM OF THE SUBSET OF DATA
ggplot(data = userCommentCountsSubset, aes(x = count)) +
  geom_histogram(fill = "#FF0000", colour = "#000000", bins = 10) + 
  xlab("Comments Created per User") +
  ylab("Count of Users") +
  ggtitle("Number of User Comments within Specified Ranges (Outliers Removed)")



#A QQ GRAPH COMPARES A NORMAL DATASET TO A SAMPLE DATASET (THE SUBSET IN THIS CASE)
#FOLLOWING THE STRAIGHT LINE INDICATES NORMAL DATA (SEEN HERE)
ggplot(data = (userCommentCountsSubset), aes(sample = count)) + 
  geom_qq() + geom_qq_line() + 
  xlab("Theoretical Normal Distribution") + 
  ylab("Sample Data") +
  ggtitle("QQPlot of subset of Comment Count Distribution")

#BOX PLOT OF THE USER DATASET
ggplot(data = userCommentCounts) +
  geom_boxplot(mapping = aes(x = count), fill = "#FF0000", colour = "#000000") +
  xlab("Count") +
  ylab("Number of Comments Per User") +
  ggtitle("Box Plot showing Number of Comments Per User") + 
  coord_flip()

#BOX PLOT OF THE SUBSET OF THE DATA
ggplot(data = userCommentCountsSubset) +
  geom_boxplot(mapping = aes(x = count), fill = "#FF0000", colour = "#000000") +
  xlab("Count") +
  ylab("Number of Comments Per User") +
  ggtitle("Box Plot showing Number of Comments Per User (Outliers Removed)") + 
  coord_flip()

#CONVERT THE DATA SUBSET TO A MATRIX TO PERFORM SOME CALCULATIONS
userCommentCountsSubset = as.matrix(userCommentCountsSubset)

#5_NUMBER SUMMARY
summary(userCommentCountsSubset)

#STANDARD DEVIATION
print(paste("Standard Deviation: ", sd(userCommentCountsSubset)))


#CLEANUP VARIABLES
rm(userComments)
rm(userCommentCounts)
rm(userCommentCountsSubset)
rm(normal_data)



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

#GROUP MOVIES BASED ON LANGUAGE
movieLanguages = group_by(movieLanguages, languages)

#CREATE A DATAFRAME CONTAINING THE LANGUAGES AND COUNT OF OCCURENCES WITHIN MOVIES
movieLanguages = data.frame(languages = summarise(movieLanguages),
                            count = count(movieLanguages)$n)

#ARRANGE MOVIES IN DESCENDING ORDER ('-' INDICATES THE DESCENDING AS DEFAULT IS ASCENDING)
movieLanguages = arrange(movieLanguages, -count)

#GRAPH ALL Languages
ggplot(data = movieLanguages) +
  geom_bar(stat = 'identity', mapping = aes(x = languages, y = count), fill = "#FF0000", colour = "#000000") + 
  xlab("Languages") + 
  ylab("Count") + 
  ggtitle("Count of Movies Utilising Each Language")


#ONLY CHOOSE LANGUAGES WITH A COUNT GREATER THAN 500
movieLanguages = subset(movieLanguages, count > 500)

ggplot(data = movieLanguages) +
  geom_bar(stat = 'identity', mapping = aes(x = languages, y = count), fill = "#FF0000", colour = "#000000") +
  xlab("Languages") + 
  ylab("Count") + 
  ggtitle("Count of Movies Utilising Each Language (Greater than 500 Occurences)")


#ONLY GRAPH THE TOP 5 LANGUAGES
ggplot(data = head(movieLanguages, 5)) +
  geom_bar(stat = 'identity', mapping = aes(x = languages, y = count), fill = "#FF0000", colour = "#000000") + 
  xlab("Languages") + 
  ylab("Count") + 
  ggtitle("Count of Movies Utilising Each Language (Top 5 Languages)")


#CLEANUP VARIABLES
rm(movieLanguages)




# Movie type - ZEC

#SELECT RELEVANT DATA
movieType = select(movies, type, year)

#GROUP THE DATA BASED ON THE MOVIE TYPE
movieType = group_by(movieType, type)

#BAR GRAPH OF THE MOVIE TYPE DATA
ggplot(data = movieType) +
  geom_bar(mapping = aes(x = type), fill = "#FF0000", colour = "#000000") +
  xlab("Type") +
  ylab("Count") +
  ggtitle("Count of Movie Type vs Series Type")



#THIS IS A BAD WAY TO DO THIS - THERE IS A FAR BETTER WAY TO DO THIS BELOW
#===============================================================================

#CREATE DATAFRAME FOR MOVIES WITH TYPE "movie"
#movieTypeMovie = subset(movieType, type == "movie")

#ARRANGE BY ASCENDING YEAR
#movieTypeMovie = arrange(movieTypeMovie, year)

#CUMULATIVE COUNT OF MOVIES WITH TYPE "movie"
#movieTypeMovie$cum = rowid(movieTypeMovie$type)



#CREATE DATAFRAME FOR MOVIES WITH TYPE "series"
#movieTypeSeries = subset(movieType, type == "series")

#ARRANGE BY ASCENDING YEAR
#movieTypeSeries = arrange(movieTypeSeries, year)

#CUMULATIVE COUNT OF MOVIES WITH TYPE "series"
#movieTypeSeries$cum = rowid(movieTypeSeries$type)

#CONNECT BOTH DATAFRAMES BACK TOGETHER
#movieType = rbind(movieTypeMovie, movieTypeSeries)
#===============================================================================

#ARRANGE DATA IN ASCENDING YEAR
movieType = arrange(movieType, year)

#CREATE A COUNT BASED ON THE TYPE OF MOVIE - MORE EFFICIENT THAN THE CODE ABOVE
movieType = mutate(movieType, count = rowid(type))

#GREATE A DOT PLOT, AND SMOOTH LINE SHOWING THIS
ggplot(data = movieType, aes(x = as.numeric(year), y = count, group = type, colour = type)) +
  geom_point() + 
  geom_smooth() +
  scale_x_continuous(name = "Year", limits = NULL, n.breaks = 10) + 
  ylab("Cumulative Number of Movies") +
  ggtitle("Cumulative Movies Grouped by 'type'")



#CLEANUP VARIABLES
rm(movieType)

#rm(movieTypeMovie)
#rm(movieTypeSeries)
#perhaps movie type over time (Graph the cumulative sum over time)


# Movie Genres 

#COLLECT THE RELEVANT DATA
movieGenres = select(movies, genres)

#ENTRIES WITH MORE THAN ONE GENRE (CONTAINED WITHIN A LIST) ARE REMOVED FROM THE CONTAINED LIST(S), AND ALL GENRES ARE PUT INTO SEPARATE ENTREIS
movieGenres = data.frame(genres = unlist(movieGenres$genres))

#GROUP MOVIES BASED ON GENRE
movieGenres = group_by(movieGenres, genres)

#CREATE A DATAFRAME CONTAINING THE GENRES AND COUNT OF OCCURENCES WITHIN MOVIES
movieGenres = data.frame(genres = summarise(movieGenres),
                         count = count(movieGenres)$n)

#ARRANGE MOVIES IN DESCENDING ORDER
movieGenres = arrange(movieGenres, -count)

#BAR GRAPH OF DATA
ggplot(data = movieGenres) +
  geom_bar(stat = 'identity', mapping = aes(x = genres, y = count), fill = "#FF0000", colour = "#000000") + 
  xlab("Genres") + 
  ylab("Count") + 
  theme(axis.text.x =  element_text(angle = 315)) +
  ggtitle("Genre Occurence Count")

#BAR GRAPH OF TOP 5 GENRES
ggplot(data = head(movieGenres, 5)) +
  geom_bar(stat = 'identity', mapping = aes(x = genres, y = count), fill = "#FF0000", colour = "#000000") + 
  xlab("Genres") + 
  ylab("Count") + 
  ggtitle("Genre Occurence Count (Top 5 Genres)")


#CLEANUP VARIABLES
rm(movieGenres)






#not sure if these are as good
#===============================================================================
# movies ratings / movies where the rating matches or something
# passwords - compare to a hash file (eg: rockyou.txt), and view security perhaps?
# line graph of total comments over time


# Movie writers / language / poster / tomatos info (dvd date / length, rating / meter, dot plot of rating vs no. of reviews)
# comment date? (compare to movie release too ?)

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

