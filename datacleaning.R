###################
# 0. Preliminaries
###################

# Install the tidyverse package by going to the
# "Packages" tab in the Plotting pane.

# Load the package
library(tidyverse)


####################
# 1. Importing Data
####################

# Import the data. This can be done via the "Files" tab
# in your plotting window pane in RStudio. Or, it can be
# one manually as follows.

# Because the data are in csv format, we will use read_csv()
dat <- read_csv(file = paste0("~/Desktop/Academics/2021 Fall/Linear Models and Experimental Designs /Lab/00_smokingRCT.csv"))
names(dat) # What are the names of the data columns?
dat # View the portion of the data that will fit in your console
print(dat, n = 20) # print 20 rows
print(dat, n = 10, width = 10) # print 10 rows and 10 cols

#OR
dat <- file.choose() #when you press run it will allow you to manually select 

#OR if a Strata file
install.packages("haven")
library(haven)
dat <- read_dta(file.choose())




####################
# 2. Data Cleaning
####################


### Overview of the data set ###

dim(dat) #check for how many variables or "dimensions"

str(dat2) # Check structure of data to see class of variables


### Selecting Variables ###

dat2 <- dat %>% 
  select(Condition, Age, Sex, 
         cigarettes_day_baseline, bdi_baseline, 
         bdi_EOT, Cigarettes_day_EOT) # Let's create a new data set that only has the variables 
# we are interested in working with
head(dat2) # Examine the first 6 rows after cleaning

# If there are a lot of variables, we will only focus on the 
# categorical treatment status variable, called, "treatmentstatus" and
# the continuous outcome variable called, 
names(dat)
names(dat)[1000:1430]

options()$max.print #tells us that it will print maximum 1000
options(max.print = 2000) #change it to print 2000 names instead of just 1000
options()$max.print
names(dat)

grep(pattern = "treatmentstatus", x = names(dat)) # The grep() function searches for a character pattern in x.
# The output from the grep() call above tells us that the 
# treatment status variable is the 798th variable in the data
# set.

# The argument value = TRUE will cause grep() to print the value 
# of the pattern match found instead of its position.
grep(pattern = "know_std_prev", x = names(dat), value = FALSE)
grep(pattern = "know_std_prev", x = names(dat), value = TRUE)

# This returns any patterns that *contain* the string "know_std_prev", 
# but we only want those that match the string *exactly*. One solution
# is to use regular expressions (https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)
# to specify that the pattern should begin and end at the first and
# last characteres in our string. ^ specifies the start of a string and
# $ specifies the end. The strings we want, however, begin with r1_, r2_, 
# or r3_. Here, we can use the period to denote "any character".
grep(pattern = "^r._know_std_prev$", x = names(dat), value = TRUE)

# Ok, now that we know we have the three variables we need, let's get
# their positions.
grep(pattern = "^r._know_std_prev$", x = names(dat), value = FALSE)


### Changing Type ###

table(dat2$Sex)
dat2$SexF <- factor(dat2$Sex, # Create factor versions of categorical variables
                    levels = c(1,2),
                    labels = c("Male", "Female")) 
dat2
table(dat2$SexF)


### Other Cleaning ###

# Rename some of the variables to have shorter names
# You can use the grep function above to find its location
names(dat2)
names(dat2)[4] <- "cig_base"
names(dat2)[5] <- "bdi_base"
names(dat2)[7] <- "cig_EOT"
names(dat2)
head(dat2)






