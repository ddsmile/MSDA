# Week6 Assignment - Wu

# Install rvest package from github

install.packages("devtools")
devtools::install_github("hadley/rvest")

# New website: http://www.disneymovieslist.com/pixar-movies.asp
# extract the table of the pixar movie list
library(rvest)
website <- html("http://www.disneymovieslist.com/pixar-movies.asp")

head <- website %>%
  html_nodes("#myTable font")%>%
  html_text()

content <- website %>%
  html_nodes("#myTable .content a")%>%
  html_text()

content2 <- website %>%
  html_nodes("#myTable .content2")%>%
  html_text()

content2.df <- data.frame(matrix(content2, ncol =3, byrow = TRUE))

table <- cbind(content, content2.df)
names(table) <- head

table


# Optional
# Using XML to pull down the same table is much easier. I have to use skip.rows option to get the same information.
require(XML)
url <- "http://www.disneymovieslist.com/pixar-movies.asp"
table.xml <- readHTMLTable(url, which = 4, skip.rows = 1:34, stringsAsFactors = FALSE, header=TRUE)
table2 <- table.xml[complete.cases(table.xml), ]

table2

# The layout of the website may affect which method would be easier to pull out the data. 
# For current comparison, it seems the XML package did better job than rvest. 