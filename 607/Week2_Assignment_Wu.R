# Week 2 Assignment

##### 1 #####
# a queue with five person
(queue <- c("James", "Mary", "Steve", "Alex", "Patricia"))

# b Harold arrived
(queue <- append(queue, "Harold"))

# c James checked out
(queue <- queue[-1])

# d Pam in front of Steve
(queue <- append(queue, "Pam", after = 1))

# e Harold left
(queue <- queue[-6])

# f Alex left
(queue <- queue[queue != "Alex"])

# g Identify the position of Patricia
match("Patricia", queue) # 4, the fourth in the queue

# h Count the number in the queue
length(queue)  # 4


##### 2 #####

quadFun <- function (a, b, c){
    if (a == 0){
        stop("a must be non-zero.")
    }
    dis <- b^2 - 4*a*c
    if (dis > 0){
        x1 <- (- b + sqrt(dis) )/(2*a)
        x2 <- (- b - sqrt(dis))/(2*a)
        print (paste('There are two solutions: x1 = ', x1, ', x2 = ', x2, '.', sep=""))
    } else if (dis == 0){
        x <- (- b)/(2*a)
        print (paste('There is one solution: x = ', x, '.', sep=""))
    } else {
        x1.complex <- (- b + sqrt(as.complex(dis)))/(2*a)
        x2.complex <- (- b - sqrt(as.complex(dis)))/(2*a)
        print (paste('There are two complex solutions: x1 = ', x1.complex, ', x2 = ', x2.complex, '.', sep=""))
    }
    
}

quadFun(0, 1, 2) # raise an error, 'Error in quadFun(0, 1, 2) : a must be non-zero.'
quadFun(1, -1, -2) # "There are two solutions: x1 = 2, x2 = -1."
quadFun(1, 2, 1)   # "There is one solution: x = -1."
quadFun(1, 2, 4)   "There are two complex solutions: x1 = -1+1.73205080756888i, x2 = -1-1.73205080756888i."

##### 3 #####
count = 0
for (i in 1:1000){
    if (i %% 3 == 0|| i%%7 == 0 || i%%11 == 0){
        count = count + 1
    }
}
1000-count # 520

##### 4 #####
PyTriple <- function(f, g, h){
    triple <- c(f, g, h)
    max <- max(triple)
    double <- triple[triple != max]
    if (double[1]^2 + double[2]^2 == max^2){
        print ("The constants form a Pythagorean Triple.")
    } else {
        print ("The constants don't form a Pythagorean Triple.")
    }
}
PyTriple(3, 4, 5) # "The constants form a Pythagorean Triple."
PyTriple(6, 3, 4) # "The constants don't form a Pythagorean Triple."
