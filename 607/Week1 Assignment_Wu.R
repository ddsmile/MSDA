# Week 1 Assignment

##### 1 versions of R and RStudio #####

# R version 3.1.1 (2014-07-10) -- "Sock it to Me"
# RStudio Version 0.98.1028


##### 2 version of PostgreSQL #####

# PostgreSQL 9.3 version 1.18.1 (Jul 22 2014, rev: REL-1_18_1)


##### 3 package DMwR, dataset sales#####

# Install and load the R package DMwR

install.packages('DMwR')
require('DMwR')

# Load the dataset sales
data(sales)
dim(sales) # 401146 obs. of 5 variables
