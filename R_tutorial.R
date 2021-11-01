# Purpose: To practice some new skills in R
# Date created: September 22, 2021
# Created by: Joseph Baafi

# Clear workspace
rm(list = ls())

# Working directory
setwd("/Users/jbaafi/Google Drive/My Drive")

# Install packages eg. gapminder
#install.packages("gapminder")

#Import useful packages
library(gapminder)
data("gapminder")

# To view a summary of the data
summary(gapminder)

# compute the mean of any quantitative columnn
mean(gapminder$gdpPercap)

# To use the columns without having to call the dataframe and any dollar sign, we use the following
attach(gapminder)

# Now doing any operation of the columns works right away by the help of the attach function
median(pop)
mean(gdpPercap)
hist(lifeExp)
boxplot(lifeExp ~ continent)

# fitting a gamma distribution in r
#install 'fitdistrplus' package if not already installed
#install.packages('fitdistrplus')

#load package
library(fitdistrplus)

#generate 50 random values that follow a gamma distribution with shape parameter = 3
#and shape parameter = 10 combined with some gaussian noise
z <- rgamma(50, 3, 10) + rnorm(50, 0, .02)

#fit our dataset to a gamma distribution using mle
fit <- fitdist(z, distr = "gamma", method = "mle")

#view the summary of the fit
summary(fit)

#produce plots to visualize the fit
plot(fit)

# Using the dplyr package in r
# This is a practice session from online material that I found very useful. 
library(readr) # package to effectively read rectangular data
library(dplyr) # is a new package that has set of tools to help effectively manipulate data 

# Load data of pfaizer payment to doctors and fda warning letters to doctors
pfizer <- read_csv("/Users/jbaafi/Google Drive/My Drive/week7/pfizer.csv")
fda <- read_csv("/Users/jbaafi/Google Drive/My Drive/week7/fda.csv")

# view the structure of data
str(pfizer)
str(fda)

# use attach() function on pfizer
attach(pfizer)

# convert total column in pfizer to a numeric variable
total <- as.numeric(total)

#summary of pfizer data
summary(pfizer)

# Useful functions in dplyr
# 1. select(): select which columns to include from a data set. 
# 2. filter(): filters the data according to a defined subset you wanna include.
# 3. arrange(): sort the data in a particular order eg. ascending, descending, alphabetical etc
# 4. group_by(): group the data by a categorical variable 
# 5. summarize(): summarize for each group if following group_by. Used with functions as n() (count), mean(), median() etc
# 6. mutate(): creates new column or change existing column in a data frame
# 7. rename(): rename columns of a data frame
# 8. bind_rows(): merges data frames into one, both must have same column names. 

# Now let us learn how to use these functions

# Filter and sort data

# Questions?? 
# 1. Find doctors in California who were paid $10,000 or more by Pfizer to run “Expert-Led Forums.”
ca_doctors_10000 <- pfizer %>% 
  filter(state == "CA" & total >= 10000 & category == "Expert-Led Forums") %>% 
  arrange(desc(total))

# 2. Find doctors in California or New York who were paid $10,000 or more by Pfizer to run “Expert-Led Forums.”

ca_ny_doctors_10000 <- pfizer %>% 
  filter((state == "CA" | state == "NY") & total >= 10000 & category == "Expert-Led Forums") %>% 
  arrange(desc(total))

# 3. Find doctors in states other than California who were paid $10,000 or more by Pfizer to run “Expert-Led Forums.”

not_ca_doctors_10000 <- pfizer %>% 
  filter(state != "CA" & total >= 10000 & category == "Expert-Led Forums") %>% 
  arrange(desc(total))

# 4. Find the 20 doctors across the four largest states (CA, TX, FL, NY) who were paid the most for professional advice.
ca_tx_fl_ny_doctors_top20 <- pfizer %>% 
  filter((state == "CA" | state == "TX" | state == "FL" | state == "NY") & category == "Professional Advising") %>% 
  arrange(desc(total)) %>% 
  head(20)

# 5. Filter the data for all payments for running Expert-Led Forums or for Professional Advising, and arrange alphabetically by doctor (last name, then first name)
all_payments_ELF_PA <- pfizer %>% 
  filter(category == "Expert-Led Forums" | category == "Professional Advising") %>% 
  arrange(last_name, first_name)

# Use pattern matching to filter text using the grepl() function

expert_advice <- pfizer %>%
  filter(grepl("Expert|Professional", category)) %>%
  arrange(last_name, first_name)

not_expert_advice <- pfizer %>%
  filter(!grepl("Expert|Professional", category)) %>%
  arrange(last_name, first_name)

expert_led_adcising <- pfizer %>% 
  filter(grepl("Expert", category) & grepl("C", state)) %>% 
  arrange(desc(total))

# Append one data frame to another
pfizer2 <- bind_rows(expert_advice, not_expert_advice)

# Write data to a CSV file
# readr package can write data to CSV and other text files.
# write expert_advice data to a csv file
write_csv(expert_advice, "expert_advice.csv", na = "")

# Group and summarize data
# Questions??
# 1. Calculate the total payments, by state

state_total_pay <- pfizer %>% 
  group_by(state) %>% 
  summarize(sum = sum(total)) %>% 
  arrange(desc(sum))

# 2. Calculate the total payments by categories
category_total_pay <- pfizer %>% 
  group_by(category) %>% summarize(sum = sum(total)) %>% 
  arrange(desc(sum))

# As above, but for each state also calculate the median payment, and the number of payments

state_summary <- pfizer %>% 
  group_by(state) %>%
  summarize(sum = sum(total), median = median(total), count = n()) %>%
  arrange(desc(count))

# as above, but group by state and category
# This is able to tell how many doctors that gave Expert-Led Advising in each state and others
state_category_summary <- pfizer %>% 
  group_by(state, category) %>% 
  summarize(sum = sum(total), median = median(total), mean = mean(total), count = n()) %>% 
  arrange(state, category)

# Working with dates
#Questions??
# 1. Filter the FDA warning letters data for letters sent from the start of 2005 onward

post2005 <- fda %>% 
  filter(issued >= "2005-01-01") %>% 
  arrange(issued)

# Count the number of letters issued by year
count_letters_by_year <- fda %>% 
  mutate(year = format(issued, "%Y")) %>% 
  group_by(year) %>% 
  summarize(letters = n())

# Add columns giving the number of days and weeks that have elapsed since each letter was sent

days_weeks_elapsed <- fda %>% 
  mutate(days_elapsed = Sys.Date() - issued, weeks_elapsed = difftime(Sys.Date(), issued, units = "weeks"))


# Join data from two data frames.

# join to identify doctors paid to run Expert-led forums who also received a warning letter
doctors_warned <- inner_join(pfizer, fda, by = c("first_name" = "name_first", "last_name" = "name_last")) %>% 
  filter(category == "Expert-Led Forums")

# semi_join does same work as inner_join in this circumstance. inner_join() contains all the columns for both data frame while semi_join has for only the first data set
# After using inner_join() you may use select() to select the required columns of interest. 
doctors_warned_semi <- semi_join(pfizer, fda, by=c("first_name" = "name_first", "last_name" = "name_last")) %>%
  filter(category=="Expert-Led Forums")
 
# join to identify doctors paid to give meals who also received a warning letter
# as above, but select desired columns from data

doctors_warned_meals <- inner_join(pfizer, fda, by = c("first_name" = "name_first", "last_name" = "name_last")) %>% 
  filter(category == "Meals") %>% 
  select(3:6)

join <- anti_join(pfizer, fda, by = c())






