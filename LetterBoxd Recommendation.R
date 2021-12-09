library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

movies = read.csv('movie_data.csv', stringsAsFactors = FALSE)
ratings = read.csv("ratings_export.csv")
str(movies)


#EDA

summary(movies)
summary(ratings)

head(movies)
head(ratings)
