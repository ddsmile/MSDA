# Week 3 Assignment

##### 1 #####

count.na <- function(vec){
  return (sum(is.na(vec)))
}

vec1 <- c(1, 2, 3, NA, 4, 5, NA, 8, NA, 10)

count.na(vec1)
# [1] 3


##### 2 #####

count.df.na <- function(df){
  return (sapply(df, count.na))
}


vec2 <- c('one', 'two', 'three', 'four', NA, NA, 'five', 'six', NA, NA)
df <- data.frame(NumCol = vec1, charCol = vec2)

count.df.na(df)

# NumCol charCol 
#      3       4 


##### 3 #####

# Create a sorting function to sort the numeric vector in ascending order
# Omit the NA value in the vector

sorting <- function(NumVec){
  NumVec <- NumVec[!is.na(NumVec)] # Omit the NA value in the vector
  start <- 1
  while (start < (length(NumVec)+1)){
    for (i in start:(length(NumVec))){
      # print(NumVec[i])
      # print(NumVec[start])
      if (NumVec[i] < NumVec[start]){
        NumVec[c(i, start)] <- NumVec[c(start, i)]
      }
    }
    start <- start + 1
  }
  return (NumVec)
}

numVec <- c(3, 4, 5, 3, NA, 5, 13, 6, NA)
sorting(numVec)
# [1]  3  3  4  5  5  6 13

numFunc <- function(numVec){
  num.of.missing <- count.na(numVec)
  numVec <- sorting(numVec)
  len <- length(numVec)
  min <- numVec[1]
  max <- numVec[len]
  mean <- sum(numVec)/len
  if ((len-1)%%2 ==0){
    median <- numVec[(len-1)/2+1]
  } else {
    median <- (numVec[ceiling((len-1)/2)] + numVec[ceiling((len-1)/2) + 1])/2
  }
  if ((len-1)%%4 ==0){
    first.quant <- numVec[(len-1)/4+1]
    third.quant <- numVec[3*(len-1)/4+1]
  } else {
    first.quant <- (numVec[ceiling((len-1)/4)] + numVec[ceiling((len-1)/4) + 1])/2
    third.quant <- (numVec[ceiling(3*(len-1)/4)] + numVec[ceiling(3*(len-1)/4) + 1])/2
  }
  SD <-sqrt(sum((numVec - mean)^2)/(len-1))
  st.list <- list(minimum = min, first.quantile = first.quant, median = median, mean = mean, 
                  third.quantile = third.quant, maximum = max, num.of.NA = num.of.missing,                    
                  standard.deviation = SD) 
  return(st.list)
  
}

numVec <- c(3, 4, 5, 3, NA, 5, 13, 6, NA)
numFunc(numVec)

# $minimum
# [1] 3
# 
# $first.quantile
# [1] 3.5
# 
# $median
# [1] 5
# 
# $mean
# [1] 5.571429
# 
# $third.quantile
# [1] 5.5
# 
# $maximum
# [1] 13
# 
# $num.of.NA
# [1] 2
# 
# $standard.deviation
# [1] 3.457222

# Using build-in-function to test

summary(numVec)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   3.000   3.500   5.000   5.571   5.500  13.000       2 

sd(numVec, na.rm = TRUE)
# [1] 3.457222



##### 4 #####

charFunc <- function(charVec){
  distinct <- length(unique(charVec)) # Count all NA as a distinct element
  charTable <- table(charVec)
  most.common <- max(charTable)
  element <- names(charTable[charTable == most.common])
  num.of.missing <- sum(is.na(charVec))
  charlist <- list(number.of.distince.elements = distinct, most.common.element = element, 
                   number.of.times = most.common, number.of.missing = num.of.missing)
  return(charlist)
}

charVec <- c("Apple", "Banana", "Apple", "Pear", NA, "Apple", NA, "Pineapple", 
             "Pear", "Peach", "Pear")

charFunc(charVec)

# $number.of.distince.elements
# [1] 6
# 
# $most.common.element
# [1] "Apple" "Pear" 
# 
# $number.of.times
# [1] 3
# 
# $number.of.missing
# [1] 2



##### 5 #####

logFunc <- function(logVec){
  len <- length(logVec)
  num.of.missing <- sum(is.na(logVec))
  newlogVec <- logVec[!is.na(logVec)]
  truevalue <- length(newlogVec[newlogVec == TRUE])
  falsevalue <- length(newlogVec[newlogVec == FALSE])
  portion <- truevalue/len
  loglist <- list(number.of.TRUE.value = truevalue, proportion.of.TRUE.value = portion,
                  number.of.FALSE.value = falsevalue, number.of.missing.value = num.of.missing)
  return(loglist)
}


logVec <- c(TRUE, FALSE, NA, FALSE, TRUE, FALSE, TRUE, FALSE, NA, FALSE)

logFunc(logVec)

# $number.of.TRUE.value
# [1] 3
# 
# $proportion.of.TRUE.value
# [1] 0.3
# 
# $number.of.FALSE.value
# [1] 5
# 
# $number.of.missing.value
# [1] 2


##### 6 #####

df.summary <- function(df){
    ncol <- ncol(df)
    summary <- list()
    sum.name <- c()
    for (i in 1:ncol){
        name <- colnames(df)[i]
      if (is.numeric(df[ ,i])){
          result <- list(numFunc(df[ ,i]))
      } else if (is.character(df[ ,i]) | is.factor(df[ ,i])){
          result <- list(charFunc(df[ ,i]))
      } else if (is.logical(df[ ,i])){
          result <- list(logFunc(df[ ,i]))
      }
      sum.name <- append(sum.name, name)  
      summary <- append(summary, result)
    }
    names(summary) <- sum.name
    return (summary)
}

vec1 <- c(1, 2, 3, NA, 4, 5, NA, 8, NA, 10)
vec2 <- c('one', 'two', 'three', 'four', NA, NA, 'five', 'six', NA, NA)
vec3 <- c(TRUE, FALSE, NA, FALSE, TRUE, FALSE, TRUE, FALSE, NA, FALSE)
vec4 <- as.factor(c('one', 'two', 'three', 'four', NA, NA, 'five', 'six', NA, NA))
df <- data.frame(numCol = vec1, charCol = vec2, logCol = vec3, facCol= vec4, stringsAsFactors = FALSE)

df.summary(df)
df.summary(df) -> df.summary
str(summary)

# List of 4
# $ Numeric  :List of 8
# ..$ Mean   : num 14.4
# ..$ Minimum: num -4
# ..$ Q1     : num 3
# ..$ Median : num 6.5
# ..$ Q3     : num 32
# ..$ Maximum: num 42
# ..$ SD     : num 17.5
# ..$ Missing: int 5
# $ Character:List of 4
# ..$ Distinct: int 7
# ..$ Mode    : chr "Bob"
# ..$ Occurs  : int 2
# ..$ Missing : int 5
# $ Factor   :List of 4
# ..$ Distinct: int 7
# ..$ Mode    : chr "Bob"
# ..$ Occurs  : int 2
# ..$ Missing : int 5
# $ Logical  :List of 4
# ..$ True          : int 7
# ..$ False         : int 5
# ..$ TrueProportion: num 0.583
# ..$ Missing       : int 3
