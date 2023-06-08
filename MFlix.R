#===============================================================================
#(1) DATA GATHERING, AND BASIC SANITISATION AND TIDYING

#INSTALL REQUIRED LIBRARIES
install.packages("mongolite")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("scales")
install.packages("data.table")
install.packages("maps")
install.packages("modelr")
install.packages("ggpubr")
install.packages("Metrics")
install.packages("factoextra")

#CALL REQUIRED LIBRARIES
library(mongolite)
library(tidyverse)
library(dplyr)
library(data.table)
library(scales)
library(maps)
library(modelr)
library(ggpubr)
library(Metrics)
library(factoextra)
options(na.action = na.warn)

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



#===============================================================================
#COMMENTS

#GROUP THE COMMENTS BY THE USER'S EMAIL
userCommentCounts = group_by(comments, email)


#COUNT THE NUMBER OF COMMENTS PER USER - GROUP_BY IS REQUIRED TO ACHIEVE THIS
userCommentCounts = data.frame(count = count(userCommentCounts)$n)

#USING HISTOGRAMS AUTOMATICALLY SEPARATED THE DATA INTO BINS
#THIS MAKES THE MANUAL METHOD OF PUTTING DATA INTO BINS UNNECESSARY

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


#CREATE A SUBSET OF THE DATA THAT REMOVES THE OUTLIERS
Q1 = quantile(userCommentCounts$count, 0.25)
Q3 = quantile(userCommentCounts$count, 0.75)
IQR = IQR(userCommentCounts$count)

userCommentCountsSubset = subset(userCommentCounts, (userCommentCounts > ((Q1) - 1.5 * (IQR))) &
                                   (userCommentCounts < ((Q3) + 1.5 * (IQR))))
#VARIABLE CLEANUP
rm(Q1)
rm(Q3)
rm(IQR)


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
  ggtitle("Box Plot showing Number of Comments Per User")

#BOX PLOT OF THE SUBSET OF THE DATA
ggplot(data = userCommentCountsSubset) +
  geom_boxplot(mapping = aes(x = count), fill = "#FF0000", colour = "#000000") +
  xlab("Count") +
  ylab("Number of Comments Per User") +
  ggtitle("Box Plot showing Number of Comments Per User (Outliers Removed)")

#ARRANGE COMMENTS BY DATE
dateOrganisedComments = arrange(comments, date)

#ADD COLUMN FOR CUMULATIVE COMMENTS WHEN COMMENT WAS CREATED
dateOrganisedComments = rowid_to_column(dateOrganisedComments, "count")

#SET DATA TYPE AS COMMENTS
dateOrganisedComments = mutate(dateOrganisedComments, type = "Comment")

#SELECT ONLY RELEVANT DATA
dateOrganisedComments = select(dateOrganisedComments, date, count, type)

#ORDER MOVIES BASED ON RELEASE DATE
movieTemp = arrange(movies, released)

#ADD COLUMN FOR CUMULATIVE NUMBER OF MOVIES WHEN MOVIE WAS RELEASED
movieTemp = rowid_to_column(movieTemp, "count")

#SET TYPE TO MOVIE, AND SET DATE COLUMN TO THE RELEASE DATE
movieTemp = mutate(movieTemp, type = "Movie", date = released)

#SELECT ONLY RELEVANT DATA
movieTemp = select(movieTemp, date, count, type)

#CONNECT DATA FRAME FOR MOVIES AND COMMENTS TOGETHER
moviesCommentsCumulative = rbind(dateOrganisedComments, movieTemp)

#CREATE A DOT PLOT, AND SMOOTH LINE SHOWING THIS
ggplot(data = moviesCommentsCumulative, aes(x = date, y = count, group = type, colour = type)) +
  geom_point() + 
  geom_line() +
  xlab("Year") + 
  ylab("Cumulative Count") +
  ggtitle("Cumulative Movies vs Comments")


#CLEANUP VARIABLES
rm(userCommentCounts)
rm(userCommentCountsSubset)
rm(dateOrganisedComments)
rm(movieTemp)
rm(moviesCommentsCumulative)



#===============================================================================
#THEATRES

#OBTAIN MAP INFO 
Map = map_data("world")
USMap = map_data("usa")


#CONVERT THE LIST TO A DATAFRAME FOR USE IN GGPLOT
#THE COORDINATES FROM MONGO IS A TABLE OF LISTS - NOT FRIENDLY TO CONVERT
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

#MODELLING

#LOADING THE CAPITAL CITIES INSIDE THE MAINLAND
capitals <- us.cities %>% 
  subset(capital == 2) %>% 
  subset(long > -130) %>% 
  subset(lat > 20)

#REDUCING DATA TO INSIDE USA'S MAINLAND
theatreLocations <- theatreLocations  %>% 
  mutate(state = theatres$location$address$state) %>% 
  subset(X1 > -140) %>% 
  subset(X2 > 20)

ggplot() + 
  geom_polygon(data = USMap, mapping = aes(long, lat, group = group), fill = "#ffffff", colour = "#000000") + 
  borders("state")+
  coord_quickmap() + 
  geom_point(theatreLocations, mapping = aes(X1, X2, colour = state))+
  geom_point(capitals, mapping = aes(long,lat))+
  theme(legend.position="none") +
  ggtitle("Theatres Grouped by State (State Capitals Included)")

#PERFORMING KMEANS CALCULATIONS ON GIVEN THEATRELOCATIONS

set.seed(500)
kMapTheatres <- kmeans(theatreLocations %>% select(1,2), 46, nstart=25)

#GRABBING THE LONGITUDE AND LATITUDE FROM CENTRES
kMapVals <- as.data.frame(kMapTheatres[["centers"]])

#DEFINING EACH DATA NODE INTO ONE OF THE 50 CLUSTERS
theatreLocations <- theatreLocations %>% 
  mutate(cluster = as.character(kMapTheatres[["cluster"]])) %>% 
  mutate(cluster = factor(cluster))

ggarrange(ggplot() + 
            geom_polygon(data = USMap, mapping = aes(long, lat, group = group), fill = "#ffffff", colour = "#000000") + 
            borders("state")+
            coord_quickmap() + 
            geom_point(theatreLocations, mapping = aes(X1, X2, colour = cluster))+
            geom_point(kMapVals, mapping = aes(X1, X2), shape = "square")+
            theme(legend.position="none") +
            ggtitle("Kmean Centres with Theatres (Grouped By Cluster Groups)"),
          ggplot() + 
            geom_polygon(data = USMap, mapping = aes(long, lat, group = group), fill = "#ffffff", colour = "#000000") + 
            borders("state")+
            coord_quickmap() + 
            geom_point(theatreLocations, mapping = aes(X1, X2, colour = state))+
            geom_point(kMapVals, mapping = aes(X1, X2), shape = "square") +
            geom_point(capitals, mapping = aes(long, lat), shape = "triangle") +
            theme(legend.position="none") +
            ggtitle("Kmean Centres with Theatres (Grouped By State)"),
          nrow = 2)


xtabs(~state+cluster,data = theatreLocations)

fviz_nbclust(theatreLocations[1:2],kmeans,method = "silhouette",k.max = 50)

#CLEANUP VARIABLES
rm(Map,USMap,theatreLocations,capitals,kMapTest,kMapVals,tlcluster,kMapTheatres,compare)

#===============================================================================
#MOVIES
################################################################################
#RATINGS

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
  theme(axis.text.x =  element_text(angle = 315)) +
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



################################################################################
#LANGUAGE

#SELECT RELEVANT DATA COLUMNS
movieLanguages = select(movies, languages)#

#REMOVE NULL ENTRIES
movieLanguages = subset(movieLanguages, !(languages %in% "NULL"))

#ENTRIES WITH MORE THAN ONE LANGUAGE (CONTAINED WITHIN A LIST) ARE REMOVED FROM THE CONTAINED LIST(S), AND ALL LANGUAGES ARE PUT INTO SEPARATE ENTREIS
movieLanguages = data.frame(languages = unlist(movieLanguages$languages)) #

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

################################################################################
#TYPE

#SELECT RELEVANT DATA
movieType = select(movies, type, released)

#GROUP THE DATA BASED ON THE MOVIE TYPE
movieType = group_by(movieType, type)

#BAR GRAPH OF THE MOVIE TYPE DATA
ggplot(data = movieType) +
  geom_bar(mapping = aes(x = type), fill = "#FF0000", colour = "#000000") +
  xlab("Type") +
  ylab("Count") +
  ggtitle("Count of Movie Type vs Series Type")


#ARRANGE DATA IN ASCENDING YEAR
movieType = arrange(movieType, released)

#CREATE A COUNT BASED ON THE TYPE OF MOVIE - MORE EFFICIENT THAN THE CODE ABOVE
movieType = mutate(movieType, count = rowid(type))

#CREATE A DOTPLOT, AND SMOOTH LINE SHOWING THIS
ggplot(data = movieType, aes(x = released, y = count, group = type, colour = type)) +
  geom_point() + 
  geom_line() +
  xlab("Year") +
  ylab("Cumulative Number of Movies") +
  ggtitle("Cumulative Movies Grouped by 'type'")


#CLEANUP VARIABLES
rm(movieType)



################################################################################
#GENRE

#COLLECT THE RELEVANT DATA
movieGenres = select(movies, released, genres)

#ENTRIES WITH MORE THAN ONE GENRE (CONTAINED WITHIN A LIST) ARE REMOVED FROM THE CONTAINED LIST(S), AND ALL GENRES ARE PUT INTO SEPARATE ENTRIES (EACH HAS A DATE ASSOCIATED)
movieGenres = unnest(movieGenres, genres)

#GROUP MOVIES BASED ON GENRE
movieGenres = group_by(movieGenres, genres)

#CREATE A DATAFRAME CONTAINING THE GENRES AND COUNT OF OCCURENCES WITHIN MOVIES
movieGenreSummary = data.frame(genres = summarise(movieGenres),
                         count = count(movieGenres)$n)

#ARRANGE MOVIES IN DESCENDING ORDER
movieGenreSummary = arrange(movieGenreSummary, -count)

#BAR GRAPH OF DATA
ggplot(data = movieGenreSummary) +
  geom_bar(stat = 'identity', mapping = aes(x = genres, y = count), fill = "#FF0000", colour = "#000000") + 
  xlab("Genres") + 
  ylab("Count") + 
  theme(axis.text.x =  element_text(angle = 270)) +
  ggtitle("Genre Occurence Count")

#BAR GRAPH OF TOP 5 GENRES
ggplot(data = head(movieGenreSummary, 5)) +
  geom_bar(stat = 'identity', mapping = aes(x = genres, y = count), fill = "#FF0000", colour = "#000000") + 
  xlab("Genres") + 
  ylab("Count") + 
  ggtitle("Genre Occurence Count (Top 5 Genres)")


#COLLECT TOP 5 GENRES (BASED ON OCCURENCES)
topFiveGenres = select(head(movieGenreSummary, 5), genres)

#CONVERT THESE TO AN ARRAY FOR USE
topFiveGenres = as.array(topFiveGenres$genres)

#CREATE A SUBSET OF THE MOVES, ONLY INCLUDING ENTRIES OF THE TOP 5 GENRES
movieGenres = subset(movieGenres, genres %in% topFiveGenres)

#ORDER BASED ON RELEASE
movieGenres = arrange(movieGenres, released)

#ADD A CUMULATIVE COUNT VARIABLE 
movieGenres = mutate(movieGenres, count = rowid(genres))

#GRAPH GENRES OVER TIME
ggplot(data = movieGenres, aes(x = released, y = count, group = genres, colour = genres)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Number of Movies") +
  ggtitle("Cumulative Count of Genre Usage (Top 5 Genres)")
  
#CLEANUP VARIABLES

rm(movieGenres)
rm(movieGenreSummary)
rm(topFiveGenres)


################################################################################
#Number Of Movies Released For Each Year

#DATAFRAME OF NUMBER OF MOVIES GROUPED BY YEAR
#REMOVING YEARS 2015,2016 AND ANYTHING BEFORE 1910

movieCount <- movies %>% 
  group_by(year) %>% 
  count %>%
  subset(year > 1910) %>%
  subset(year < 2015)

#VISUAL REPRESENTATION OF MOVIECOUNT DATAFRAME
ggplot(data = movieCount, aes(x = year, y = n)) +
  geom_point()+
  xlab("Year")+
  ylab("Movies Released That Year") +
  ggtitle("Movies Released Per Year")

#CONVERTING YEAR COLUMN TO A NUMERIC FORMAT AND REMOVING NAs
movieCount <- movieCount %>% 
  mutate(year = as.numeric(year)) %>%  
  na.omit(movieCount)

#VISUAL REPRESENTATION OF MOVIECOUNT DATAFRAME WITH CHANGES IN PLACE
ggplot(data = movieCount, aes(x = year, y = n)) +
  geom_point() +
  xlab("Year") +
  scale_x_continuous(n.breaks = 10) +
  ylab("Movies Released That Year") +
  ggtitle("Movies Released Per Year")

#MODELLING OF MOVIECOUNT

#CONVERTING n TO log(n) FOR A POTENTIAL LINEAR LOOK
movieCount <- movieCount %>% 
  mutate(logn = log(n))

#PLOT OF LOGARITHMIC GRAPH
ggplot(data = movieCount, aes(x = year, y = logn)) +
  geom_point() +
  xlab("Year") +
  ylab("Log(Movies Released That Year)") +
  ggtitle("Logarithm of Movies Released per Year")

#TESTING FOR CORRELATION COEFFICIENT
coefficient <- cor.test(movieCount$year,movieCount$logn)

#GATHERING DETAILS FOR LINEAR MODEL
#USE summary(model) FOR DETAILS
model <- lm(logn~year, data = movieCount)
modelSummary = summary(model)


#CREATING A GRID OF PREDICTIONS USING log(n)
grid <- movieCount %>% 
  data_grid(n = seq_range(n, 20)) %>% 
  mutate(logn = log(n)) %>% 
  add_predictions(model, "pred")

rmse(grid$logn, grid$pred)

#PLOT OF log(MOVIECOUNT) WITH MODEL
ggplot(data = movieCount, aes(x = year, y = logn)) +
  geom_point() +
  geom_line(data = grid , aes(x = year, y = pred), colour = "red", linewidth = 1) +
  scale_x_continuous(n.breaks = 10) +
  xlab("Year") +
  ylab("Log of Movies Released That Year") +
  ggtitle("Logarithm of Movies Released per Year (LOBF Attached)")


#PLOT OF MOVIECOUNT WITH MODEL (EXPONENTIAL)
ggplot(data = movieCount, aes(x = year, y = exp(1)^logn)) +
  geom_point() +
  geom_line(data = grid , aes(x = year, y = exp(1)^pred), colour = "red", linewidth = 1) +
  scale_x_continuous(n.breaks = 10) +
  xlab("Year") +
  ylab("Movies Released That Year") +
  ggtitle("Movies Released per Year (LOBF Attached)")

#DROPPING VARS
rm(movieCount,grid,model,modelSummary,coefficient)

################################################################################
#Number Of Movies Released For Each Country

#Un-listing the countries column
movieCountries = select(movies,countries)
movieCountries = data.frame(countries = unlist(movieCountries$countries))
movieCountries = movieCountries %>% group_by(countries) %>% count
movieCountries = group_by(movieCountries, countries)

#Creating the "Other" Entry, combining every entry besides the first 10
movieCountries = arrange(movieCountries,n)
sum = 0
max = nrow(movieCountries) - 9;
for (x in 1:max){
  sum = sum + movieCountries$n[x]
}
movieCountries = arrange(movieCountries,-n)
movieCountries = head(movieCountries,9)
movieCountries[nrow(movieCountries) + 1,] <- list('Other',sum)

#Calculating Percentages For Each Country
movieCountries = movieCountries %>% 
  summarise(n = sum(n)) %>%
  mutate(percentage = n/sum(n)*100)
  

#Plotting Pie Chart
ggplot(data=head(movieCountries,10),aes(x="", y=percentage, fill = countries)) +
  geom_col(color = "black", linewidth = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0((round(percentage*10))/10,'%'), x = 1.65), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Movies By Country") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))
#DROPPING VARS
rm(movieCountries,max,sum,x)

#EOF