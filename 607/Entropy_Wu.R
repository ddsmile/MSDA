# Entropy project 

##### 1 entropy() #####

entropy <- function(dvector){
  k <- length(unique(dvector))
  total <- length(dvector)
  entropy <- 0
  for (i in 1: k){
    pi <- length(dvector[dvector == unique(dvector)[i]])/total
    entropy <- entropy - pi*log2(pi)
  } 
  return(entropy)
}

dataset <- read.csv('entropy-test-file.csv', header = TRUE, stringsAsFactors = FALSE) # Read in table.
str(dataset) # structure of the table: 1000 obs. of 4 variables 
summary(dataset) # No NA values in the tables
entropy(dataset$answer) # Test the entropy() function

# [1] 0.9832692
      

##### 2 infogain() #####

infogain <- function (dvector, avector){
  m <- length(unique(avector))
  # print (m)
  n <- length(avector)
  entro.j <- 0
  for (j in 1:m){
    nj <- length(dvector[avector == unique(avector)[j]])
    # print(nj)
    entro.j <- entro.j + nj*entropy(dvector[avector == unique(avector)[j]])/n
  }
  return (entropy(dvector) - entro.j)
}

infogain(dataset$answer, dataset$attr1) # [1] 2.411565e-05

infogain(dataset$answer, dataset$attr2) # [1] 0.2599038

infogain(dataset$answer, dataset$attr3) # [1] 0.002432707


##### 3 decide() #####
decide <- function(dataset, col){
    gains <- c()
    gain.name <- c()
    for (i in 1:ncol(dataset)){
        if (i != col){
            gains <- append(gains, infogain(dataset[,col], dataset[,i]))
            gain.name <- append(gain.name, names(dataset)[i])
        }
    }
    names(gains) <- gain.name
    max.gain <- grep(names(gains[gains == max(gains)]), colnames(dataset))
    decide <- list(max = max.gain, gains = gains)
    return(decide)
}

decide(dataset, 4)

# $max
# [1] 2
# 
# $gains
# attr1        attr2        attr3 
# 2.411565e-05 2.599038e-01 2.432707e-03 

