# Week 6 Quiz -- Wu

# Provided code
install.packages("XML")
require(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE, stringsAsFactors = FALSE)
bowlPool

##### 1. data type #####
class(bowlPool)
# bowlPool is a dataframe.

##### 2. data type #####
theURL <- "http://www.w3schools.com/html/html_tables.asp"
hvalues <-readHTMLTable(theURL)
class(hvalues)
str(hvalues)

# hvalues is a list of 7 elements. Two of the elements are dataframe type.

##### 3. How many HTML tables? #####
table.count <- function(list){
  count <- 0
  for (i in 1:length(list)){
    if (class(list[[i]]) == "data.frame"){
      count <- count + 1
    }
  }
  return(count)
}
table.count(hvalues)

# Alternative way
length(hvalues[!sapply(hvalues, is.null)]) 

# [1] 2

##### 4. Extract specific table #####
(hvalues <- readHTMLTable(theURL, which = 1, stringsAsFactors = FALSE))


##### 5. Show specific columns #####

hvalues[, c("Last Name", "Points")]

##### 6. Identify another page with html table values #####
# http://www.movieinsider.com/c194/pixar-animation-studios/

##### 7. How many tables? #####

url.pixar <- "http://www.movieinsider.com/c194/pixar-animation-studios/"
(pixar <- readHTMLTable(url.pixar, header = F, stringsAsFactors = F))
table.count(pixar)
# [1] 1
# One table in this website.

##### 8. Web browser #####

# I use Firefox in Windows, which contains Open Web Developer tools (Ctrl+Shift+I). 
# Under the Developer tab, I can choose Page Source (Ctrl+U) to show the source code.
