##### 1. three questions #####

# Question 1, Did the overall voters preferred Cullen skink over Partan bree?
# Question 2, Did the voters aged 16-24 preferred Cullen skink over Partan bree?
# Question 3, Did the voters in Edinburgh preferred Cullen skink over Partan bree?

##### 2. Messy data #####
messy <- data.frame(decision = c('Yes', 'No'), Edinburgh.16to24 = c(80100, 35900), 
                   Edinburgh.25plus=c(143000, 214800), Glasgow.16to24 = c(99400, 43000), 
                   Glasgow.25plus=c(150400, 207000))
messy

##### 3. tidyr #####
require('tidyr')
require('dplyr')

tidy <- messy %>%
  gather(location, count, -decision)%>%
  separate(location, into = c ('city', 'age'), sep = "\\.")%>%
  arrange(city, age, decision, count)

##### 4. plyr/dplyr #####
Q1 <- tidy %>%
  select(decision, count)%>%
  group_by(decision)%>%
  summarise(total.count = sum(count))
Q1
# decision total.count
# 1       No      500700
# 2      Yes      472900

# Overall voters didn't prefer Cullen skink over Partan bree. 


Q2 <- tidy %>%
  select(decision, age, count)%>%
  filter(age == '16to24')%>%
  group_by(decision)%>%
  summarise(total.count = sum(count))
Q2
# decision total.count
# 1       No       78900
# 2      Yes      179500

# The voters aged 16-24 preferred Cullen skink over Partan bree. 

Q3 <- tidy %>%
  select(decision, city, count)%>%
  filter(city == 'Edinburgh')%>%
  group_by(decision)%>%
  summarise(total.count = sum(count))
Q3

# decision total.count
# 1       No      250700
# 2      Yes      223100

# The voters in Edinburgh didin't prefer Cullen skink over Partan bree.

##### 5 #####
# The tidy data made it easier to answer all the questions I had.
# The process was straightforward using dplyr papckage.
