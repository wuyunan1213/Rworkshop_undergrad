####convert movie ID to movie names and truncate the size of dataset for the purpose of the workshop####
setwd('/Users/charleswu/Google Drive/HoltLab/Mentoring/Programming/ml-20m')
raw1 <- read.csv('movies.csv')
raw2 <- read.csv('ratings.csv')

raw3 <- raw2[19990000:20000263, 1:3]
write.csv(raw3, 'ratings2.csv')
raw <- read.csv('ratings2.csv')

head(raw)

raw <- raw[order(raw$movieId),]

Names <- NULL
Genre <- NULL
for (i in 1:length(unique(raw$movieId))){
  movID <- unique(raw$movieId)[i]
  movIndex <- raw1[raw1$movieId == movID,2:3]
  ntimes <- nrow(raw[raw$movieId == movID,])
  movieNames <- rep(as.character(movIndex[1,1]),ntimes)
  movieGenres <- rep(as.character(movIndex[1,2]),ntimes)
  Names <- c(Names, movieNames)
  Genre <- c(Genre, movieGenres)
  print(i)
}

raw$movieNames <- Names
raw$movieGenres <- Genre

library(dplyr)
raw <- select(raw, movieNames, movieGenres, userId, rating)
write.csv(raw, "ratings3.csv")
