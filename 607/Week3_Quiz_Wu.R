# Week3 Quiz

##### 1 #####

mean1 <- function(numVec){
    mean <- sum(numVec)/(length(numVec))
    return (mean)
}
numVec <- c(1, 2, 3, 4, 5)
mean1(numVec) 

# [1] 3

##### 2 #####

mean.wNA <- function(numVec){
    sum <- sum(numVec, na.rm = TRUE)
    count <- length(na.omit(numVec))
    return (sum/count)
} 

numVec.wNA <- c(1, 2, 3, NA, NA, 4, 5)

mean1(numVec.wNA) 
# [1] NA
# The function built in question 1 can't handle NA value. Returned 'NA'

mean.wNA(numVec.wNA) 
# [1] 3
# Return the mean value of 3 with NA omitted.

##### 3 #####

gcd <- function(a, b){
  if (a == 0 || b == 0){
    return (0)
  }  else {
    Divisor = 1
    for (i in 1:min(abs(a), abs(b))){
      if (abs(a)%%i == 0 & abs(b)%%i ==0){
        Divisor <- i
      }
    }
    return (Divisor)
  }
} 

gcd(1, 0) # 0
gcd(21, 7) # 7
gcd(21, 11) # 1
gcd(210, 45) # 15
gcd(210, -45) # 15

##### 4 #####

EuclidFun <- function(a, b){
  if (a == 0 || b == 0){
    return (0)
  } else {
    m <- max(abs(a), abs(b))
    n <- min(abs(a), abs(b))
    r <- m %% n
    while (r != 0){
      m <- n
      n <- r
      r <- m %% n
    }
    return (n)
  }
}

EuclidFun(1, 0) # 0
EuclidFun(21, 7) # 7
EuclidFun(21, 11) # 1
EuclidFun(210, 45) # 15
EuclidFun(210, -45) # 15


##### 5 #####

func1 <- function(x, y){
    value <- (x^2)*y + 2*x*y -x*y^2
    return (value)
}

func1(1, 2)

# [1] 2

##### 6 #####
price <- read.csv('week-3-price-data.csv', header = TRUE, stringsAsFactors = FALSE)
model <- read.csv('week-3-make-model-data.csv', header = TRUE, stringsAsFactors = FALSE)
merge1 <- merge(price, model, by = 'ModelNumber')
str(merge1) # 27 observations in the results. Since price has 28 observations, one observation is missing by such merge. 

##### 7 #####

merge2 <-merge(price, model, by = 'ModelNumber', all.x = TRUE)
str(merge2) # 28 obs. of  8 variables 

##### 8 #####

(Year2010 <- subset(merge2, Year == 2010))
str(Year2010)

# alternative way: 
Year2010.alt <- na.omit(merge2[merge2$Year== 2010,])
str(Year2010.alt)


##### 9 #####

Red.10000 <- subset(merge2, Color == 'Red' & Price > 10000)
str(Red.10000)

##### 10 #####

remove <- subset(Red.10000, select = -c(ModelNumber, Color))
str(remove)

##### 11 #####

charCount <- function(charVect){     
    return(nchar(charVect))
}

charCount(c('theatre', 'movie', 'ticket')) 
# [1] 7 5 6

##### 12 #####

concFun <- function(cvec1, cvec2){
    if (length(cvec1) != length(cvec2)){
        stop('Two character vectors are not the same length.')
    } else {
        concVec <- paste(cvec1, cvec2)
    }
    return (concVec)
}

char<- c('theatre', 'movie', 'ticket')
char2 <- c('AMC', 'Guardians Of The Galaxy', 'One')
concFun(char, char2)
# [1] "theatre AMC"                   "movie Guardians Of The Galaxy"
# [3] "ticket One"  

char3 <- c('AMC', 'Guardians Of The Galaxy', 'Begin Again' ,'One')
concFun(char, char3)
# Error in concFun(char, char3) : 
# Two character vectors are not the same length.

##### 13 #####

require("stringr")
vow <- function(charVec){
  loc <- str_locate(charVec, "[AEIOUaeiou]{1}")
  three.char <- str_sub(charVec, start = loc[,1], end = loc[,1]+2)
  for (i in 1:length(three.char)){
      if (is.na(three.char[i]) == TRUE){
          print(sprintf("Note: The No. %d character doesn't contain vowel.", i))
      } else if (nchar(three.char[i]) < 3){
          print(sprintf("Note: The No. %d character only returns %d character(s).", i, nchar(three.char[i])))
      }
  }
  return(three.char)
}

charVec<-c("Apple", "Pear", "dye", "dy")
vow(charVec)

# [1] "Note: The No. 3 character only returns 1 character(s)."
# [1] "Note: The No. 4 character doesn't contain vowel."
# [1] "App" "ana" "ear" "ine" "eac"

##### 14 #####
date <- data.frame(month = seq(1, 3), day = seq(4, 10, 3), year = rep(2014, 3))
date$date <- as.Date(paste(date$year, date$month, date$day, sep='/'))
head(date)

#   month day year       date
# 1     1   4 2014 2014-01-04
# 2     2   7 2014 2014-02-07
# 3     3  10 2014 2014-03-10

##### 15 #####

convert.date <- function(string){
  return (as.Date(string, format = "%m-%d-%Y"))
}

string <- "02-03-2014"
convert.date(string)

# [1] "2014-02-03"

##### 16 #####
require("stringr")
extract.month <- function(date){
  return (str_split_fixed(date, pattern = "-", n=3)[,2])
}

date1 <- as.Date("2014-02-03")
extract.month(date1)

# [1] "02"

##### 17 #####
date.seq <- seq(as.Date("2005-01-01"), as.Date("2014-12-31"), by = "day")

head(date.seq)
# [1] "2005-01-01" "2005-01-02" "2005-01-03" "2005-01-04" "2005-01-05" "2005-01-06"

tail(date.seq)
# [1] "2014-12-26" "2014-12-27" "2014-12-28" "2014-12-29" "2014-12-30" "2014-12-31"