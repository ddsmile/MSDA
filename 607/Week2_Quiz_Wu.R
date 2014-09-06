##### 1 #####
vec <- c(1, 3, 5, 4, 3, 6, 7, 5, 8, 10, 20, 11, 13, 14, 12, 15, 16, 17, 18, 19)

##### 2 #####
charVec <- as.character(vec)

##### 3 #####
facVec <- as.factor(vec)

##### 4 #####
facVec # showing there is 18 levels

levels(facVec) # showing the levels

##### 5 #####
newVec <- 3*vec^2-4*vec+1
newVec

##### 6 #####
(X <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 5, 4, 6, 2, 3, 2, 7, 8, 8, 9, 4, 7, 4, 9, 6, 4), nrow = 8))
(y <- matrix(c(45.2, 46.9, 31.0, 35.3, 25.0, 43.1, 41.0, 35.1), nrow = 8))
(beta <- solve((t(X)%*%X))%*%t(X)%*%y)

##### 7 #####
namedList <- list(a = 2, b = 3, c= 4, d =5)
namedList$a # To refer one element by name


##### 8 #####

charCol <- c("apple", "tomato", "strawberry", "cherry", "watermelon", "broccoli", "pepper", "lemon", "pineapple", "banana")
factCol <- factor(c("red", "red", "red", "red", "red", "green", "green", "yellow", "yellow", "yellow"))
numCol <- c(seq(1, 10, 1.0))
dateCol <- seq(as.Date("2014-09-01"), by=1, len=10)
df <- data.frame (charCol, factCol, numCol, dateCol, stringsAsFactors = FALSE)
str(df)

##### 9 #####

levels(df$factCol) <- c(levels(df$factCol), "purple")
newRow <- data.frame(charCol = "grape", factCol = "purple", numCol = 11, dateCol = as.Date("2014-09-11"))
df1 <- rbind(df, newRow)
str(df1)


##### 10 #####
csvFile <- read.csv('temperatures.csv')


##### 11 #####
    
tsvFile <- read.table('C:\Users\Sandra\Downloads\measurements.txt', sep = "\t")


##### 12 #####

pipeFile <- read.delim(file = 'https://gateway.ifionline.org/public_download/nonGovEntities.txt', header = T, sep = "|")


##### 13 #####
fact=1
for (i in 1:12){
    fact <- fact*i
}
fact #479001600

##### 14 #####
balance <- 1500
Mon.Int <- 0.0324/12
for (i in 1:(6*12)){
    balance <- balance*(1 + Mon.Int)
}
format(round(balance, 2), nsmall = 2) # 1821.40

##### 15 #####
numVec <- seq(1, 20)
sum <- 0
for (i in 1:length(numVec)){
    if (i%%3 == 0){
        sum <- sum + numVec[i]
    }
}
sum 

##### 16 #####
sum1 <- 0
x <- 2
for (i in 1:10){
    sum1 <- sum1 + x^i
}
sum1 # 2046

##### 17 #####
count = 1
sum2 <- 0
x <- 2
while (count <= 10){
    sum2 <- sum2 + x^count
    count <- count + 1
}
sum2 # 2046

##### 18 #####
sum(2^seq(1,10)) # 2046

##### 19 #####
numVec5 <- seq(20, 50, 5)
numVec5 # 20 25 30 35 40 45 50

##### 20 #####
charVec <- rep('example', 10)
charVec

##### 21 #####

quadFun <- function (a, b, c){
    x1 <- (- b + sqrt(b^2 - 4*a*c) )/(2*a)
    x2 <- (- b - sqrt(b^2 - 4*a*c))/(2*a)
    return(c(x1, x2))
}

quadFun(1, -1, -2)
