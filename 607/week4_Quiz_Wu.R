# Week 4 Quiz 

# Read in movie data

movie <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="", stringsAsFactors = FALSE)
summary(movie)
str(movie)

##### 1. Visualization of total number of movies for each decade #####
# creat x.axis lable
label1 <- as.character(seq(1890, 2000, 10))
label2 <- as.character(seq(1899, 2009, 10))
label <- paste(label1, label2, sep ="-")

# Cut years into decades and create a factor column 
movie$year.decade <- factor(cut(movie$year, breaks = seq(1889, 2009, 10)), labels = label)
# nlevels(movie$year.decade)
# levels(movie$year.decade)
# table(movie$year.decade)

# Aggregation
a <- aggregate(year~year.decade, movie, length)

# Plot
require('ggplot2')

ggplot(a, aes(x = year.decade, y = year)) + geom_boxplot()

# qplot(a[,1], a[,2], xlab= 'year', ylab = 'count', main = 'Total movie per decade')

# plot(a, xlab= 'year', ylab = 'count', main = 'Total movie per decade')

##### 2 #####
require('dplyr')
require('reshape2')

# Reshape the dataset

a <- select(movie, year, rating, Action, Animation, Comedy, Drama, Documentary, Romance, Short)
m <- melt(a, id.vars = c("year", "rating"), variable.name = "Genres", value.name = 'filter')
m1 <- filter(m, filter == 1)
m2 <- group_by(m1, Genres)

# Average rating for different genres
summarise(m2, avg.rating = mean(rating)) 

# Genres avg.rating
# 1      Action   5.292022
# 2   Animation   6.583686
# 3      Comedy   5.955492
# 4       Drama   6.153684
# 5 Documentary   6.650576
# 6     Romance   6.163997
# 7       Short   6.481423

# plot rating on different genres

ggplot(data = m1, aes(x = Genres, y = rating)) + geom_boxplot() 

# plot rating change over the years, separated by different genres
# dataset>1000, use default method 'gam' for geom_smooth
ggplot(data = m1, aes(x = year, y = rating)) + geom_smooth(method = 'gam') + facet_wrap(~Genres) 

# Rating for Action, Comedy and Romance decrease over time. Rating for Documentary and Short increase over time.
# Rating for Animation and Drama stay constant over time. 

##### 3 #####

ggplot(data = movie, aes(x = length, y = rating)) + geom_point()

# There are several long movies. summary: 
summary(movie$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   74.00   90.00   82.34  100.00 5220.00 

# Separate the length into three groups: 1st quantile, 1-3 quantiles, and 3 quantiles (exclue the extra long movies):

q1 <- movie[movie$length <74, ]
q3 <- movie[movie$length >= 74 & movie$length <= 100, ]
q4 <- movie[movie$length >= 100 & movie$length < 500, ]


# Creat plots for each category
l1 <- ggplot(data = q1, aes(x = length, y = rating)) 
l1 + geom_point() + geom_smooth(method = 'gam') + ggtitle("Movies shorter than 74 Minutes")

l2 <- ggplot(data = q3, aes(x = length, y = rating)) 
l2 + geom_point() + geom_smooth(method = 'gam') + ggtitle("Movies between 74 and 100 Minutes")

l3 <- ggplot(data = q4, aes(x = length, y = rating)) 
l3 + geom_point() + geom_smooth(method = 'gam') + ggtitle("Movies longer than 100 minutes")

# There is a trend that for movie longer than 100 minutes increase of the length would increase the rating.

##### 4 #####
require('dplyr')
require('reshape2')

# Reshape the dataset
m.len <- select(movie, length, Action, Animation, Comedy, Drama, Documentary, Romance, Short)
m.gen <- melt(m.len, id.vars = "length", variable.name = "Genres", value.name = 'filter')
len.gen <- filter(m.gen, filter == 1) 

# Plot movie length by different genres
g <- ggplot(data = len.gen, aes(x = Genres, y = length)) 
g1 <- g + geom_boxplot()

# Change the y scale to show the main area.
g1 + scale_y_continuous(limit = c(0, 300))

# In average, Animation and Short are short in length and all other genres are closed to 100 minutes.   

##### 5 #####

year.plot <- ggplot(data = movie, aes(x = year, y = votes)) + geom_point() + geom_smooth(method = "gam") 
year.plot + ggtitle("year vs. votes")

budget.plot <- ggplot(data = movie, aes(x = budget, y = votes)) + geom_point() + geom_smooth(method = "gam")
budget.plot + ggtitle("budget vs. votes")

l500 <- movie[movie$length < 500, ]
length.plot <- ggplot(data = l500, aes(x = length, y = votes)) + geom_point() + geom_smooth(method = "gam")
length.plot + ggtitle("length vs. votes")

rating.plot <- ggplot(data = movie, aes(x = rating, y = votes)) + geom_point() + geom_smooth(method = "gam")
rating.plot + ggtitle("rating vs. votes")

mpaa.plot <- ggplot(data = movie, aes(x = mpaa, y = votes)) + geom_boxplot() 
mpaa.plot + ggtitle("mpaa vs. votes")

v <- select(movie, votes, Action, Animation, Comedy, Drama, Documentary, Romance, Short)
gen <- melt(v, id.vars = "votes", variable.name = "Genres", value.name = 'filter')
v.gen <- filter(gen, filter == 1) 

genres.plot <- ggplot(data = v.gen, aes(x = Genres, y = votes)) + geom_boxplot()
genres.plot + ggtitle("genres vs. votes")

# It seems that movies with higher budget tend to get higher votes. 