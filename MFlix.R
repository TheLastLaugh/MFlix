#(1) DATA GATHERING, AND BASIC SANITISATION AND TIDYING

#INSTALL REQUIRED LIBRARIES
install.packages("mongolite")
install.packages("tidyverse")
install.packages("dplyr")

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


#===============================================================================
#We can probably remove the sessions data completely. there is only one row... kinda useless.
#===============================================================================
rm(sessions)


#(2) DATA TRANSFORMATION





#number of distinct years that movies (within this database) have been released in.
#length(db_movies_collection$distinct("year")) is the same as:
n_distinct(movies$year)

movies %>% group_by(year) %>% count


movies_collection$iterate()$one()

comments_collection$count()
movies_collection$count()
sessions_collection$count()
theatres_collection$count()
users_collection$count()




#(3) DATA ANALYSIS




#(4) DATA MODELLING 
