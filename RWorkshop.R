###R workshop for Holt/Cunningham lab summer undergraduates
###Author: Charles Wu

###R is a very popular open-source data analysis tool
###especially in the statistics community--it was written by statisticians so a lot of the language is intuitive for people who want to do statistics
###First, we need to download R and Rstudio into your laptop.
###Go to https://www.r-project.org/ to download R, click on 'download R', look for USA and find the CMU version and find the approapriate version
###Go to https://www.rstudio.com/products/rstudio/download/ and download the Rstudio desktop

###Overview of the workspace

###we are gonna use R script today. But we can also use 

###Basic programming:

###Arithmetics:
###How to run each line. 
1 + 1
2 ^ 10

###logical statements
1 == 2
2 != 10

###variable assignment
a = 1
b = 4
###Variables can also be characters or other data types
a = 'apple'
##Note that when you assign a new value to a it overwrites the previous assignment
##You can do operations on the variables too
a = 1
c = a + b
a > c
##You can also create combination of characters.
a <- 'big'
b <- 'apple'
c = c(a, b)
c[1]
c[2]


b = 'big'
a = 'apple'
c = c(b, a)
c = paste(b, a, sep = ' ')
c = paste(b, a, sep = '-')

##Exercise: what happens when we perform operations on vectors? 
a <- c(1,2,3)  ##or a <- c(1,2,3)
b <- c(4,5,6)
c <- a * b

a <- 1
b <- 2

a == b
a != b

########################################################################################################################################################################
############Simulations to illustrate LLN and CLT###########################################################################################################################################################################################################

###LLN simulations
###What is the law of large numbers? 
n = 10000
a = rnorm(n, mean = 0, sd = 4)
hist(a, xlim = c(-16,16), breaks = 24)
##change the number of n---what happens? 
sp1 <- sample(a, size = 5, replace = TRUE)
hist(sp1, xlim = c(-12,12), ylim = c(0,10), main = 'Sample size = 5')

sp2 <- sample(a, size = 50, replace = TRUE)
hist(sp2, xlim = c(-12,12), ylim = c(0,40), main = 'Sample size = 50')

sp3 <- sample(a, size = 200, replace = TRUE)
hist(sp3, xlim = c(-12,12), ylim = c(0,40), main = 'Sample size = 200')

sp4 <- sample(a, size = 600, replace = TRUE)
hist(sp4, xlim = c(-12,12), main = 'Sample size = 600')


####You don't need to implement this part, but basically I'm writing a function to take a certain number of sample from the population with different sample sizes for our experiment
sampleMeans <- function(population, nsamples, sampleSize){
  SM <- NULL
  for (i in 1:nsamples) {
    sp = sample(population, size = sampleSize, replace = TRUE)
    mean <- mean(sp)
    SM <- c(SM, mean(sp))
  }
  return(SM)
}

###Use the function to create samples. We can think of them as the experiments that we are doing
experiment1 <- sampleMeans(a, 10, 5)

experiment2 <- sampleMeans(a, 10, 20)

experiment3 <- sampleMeans(a, 10, 80)

experiment4 <- sampleMeans(a, 10, 500)

###We can see that as the sample size increases, the mean of all the samples clusters around the true mean of the population
plot(experiment1, ylim = c(-4,4), xlab = 'Ordered Sample', ylab = 'Mean')
lines(experiment1)
points(jitter(experiment2), col = 'green')
lines(jitter(experiment2), col = 'green')
points(jitter(experiment3), col = 'blue')
lines(jitter(experiment3), col = 'blue')
points(jitter(experiment4), col = 'red')
lines(jitter(experiment4), col = 'red', lwd = 2)

#####simulation of Central Limit Theorem
####What is the central limit theorem?
###Let's draw samples from an arbitrary distribution
b <- sample(-5:5,1000,replace=T)
mean(b)
hist(b, xlim = c(-5,5), breaks = 10, main = 'Histogram of a random distribution')
experiment1 <- sampleMeans(b, 100, 1)
experiment2 <- sampleMeans(b, 100, 5)
experiment3 <- sampleMeans(b, 100, 50)
experiment4 <- sampleMeans(b, 100, 500)

###we can see that as the sample size increases, the distribution of the sample means approaches a normal distribution. Also, the mean approaches the true population mean. 
hist(experiment1, breaks = 10)
hist(experiment2, breaks = 10)
hist(experiment3, breaks = 10)
hist(experiment4, breaks = 10)



########################################################################################################################################################################
############   Working with real-life data sets  ###########################################################################################################################################################################################################
### go to this website to download the dataset https://github.com/wuyunan1213/Rworkshop

setwd('/Users/charleswu/Google Drive/HoltLab/Mentoring/Programming') ##change the working directory to where the file is stored
###then load the dataset into the program
ratings <- read.csv('MovieRatings.csv')

##data is read into a data structure called data frame. In r, we mostly work with data frames because of its flexibilit and convenience.

##examine the dataset by looking at the first few rows
head(ratings)

## indexing variables(columns) of the data
ratings$movieNames
ratings$userId

## querying parts of the dataset by using logical statements
## we start with indexing the dataset ratings[]
## since this is a two-dimensional dataset, we use a comma to separate row index and column index ratings[ , ]
## so for example, if we want the first column, which is the movienames column, we index it like this
ratings[, 1]

## exercise: how do we query movies that have a 5-star rating? Hint: remember how we make logical statements? 
## ???

## exercise: how do we find all the ratings by userId = 138484? 
## ???

###next we are going to learn about a function called ddply. It's a very powerful function especially for analyzing experimental data because it can group datasets by variables and then perform desired functions separated for these variables. More about using ddply, see the reading materials: 
install.packages('plyr')
library('plyr')
## for example, if we want to find the average ratings for all movies across all users
ave_ratings <- ddply(ratings, ~movieNames + movieGenres, summarise, mean = mean(rating))

## exercise: how do we find the average ratings for all users across all movies? 

##exercise: which movie is the most popular? Hint: use max function to find the maximum


popular_movies <- ave_ratings[ave_ratings$mean == max(ave_ratings$mean),]


##next we try to perform a t-test on two movies, Men in Black (a.k.a. MIB) (1997) and Braveheart (1995), and see if people like one movie more than the other. 
FirstMovie <- ratings[ratings$movieNames == 'Men in Black (a.k.a. MIB) (1997)', ]
SecondMovie <- ratings[ratings$movieNames == 'Braveheart (1995)', ]

t.test(FirstMovie$rating, SecondMovie$rating)


## building linear models to see if the movie ratings are correlated with each other. First we find two movies within the same genre that we think might have similar ratings.
genre <- ddply(ratings, ~movieGenres + userId, summarise, mean = mean(rating))

###we transform the matrix from long format into wide format and get a matrix called user
library(reshape2)
user <- dcast(genre, userId ~ movieGenres, value.var = 'mean')

###with the user matrix, we can have each subjects' rating (rows) of all the movies (columns). Note that this matrix will mostly be empty because users never rate all of the movies in the dataset.
###first check if the relationship is linear
plot(user$`Mystery|Sci-Fi|Thriller`, user$`Children|Comedy`)
plot(user$`Mystery|Sci-Fi|Thriller`, user$`Action|Crime|Drama|Thriller`)

###next fit the model, we can examine the model information by using the function summary and plot. More on lm function in the reading materials https://www.r-bloggers.com/r-tutorial-series-simple-linear-regression/
model1 <- lm(user$`Mystery|Sci-Fi|Thriller` ~ user$`Children|Comedy`, na.action=na.omit)
model2 <- lm(user$`Mystery|Sci-Fi|Thriller` ~ user$`Action|Crime|Drama|Thriller`, na.action=na.omit)
summary(model1)
plot(model1)

###we don't have time to get into this today but you can
###learn about ggplot on your own to create beautiful plots! 
