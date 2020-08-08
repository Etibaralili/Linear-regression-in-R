#===================================================================


# Import the csv file (Boston_house_prices.csv) and explore it using str and summary functions.
#====================== Write R code HERE ==========================

prc <- read.csv("Boston_house_prices.csv")

str(prc)
summary(prc)

#===================================================================
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stringr")
library(stringr)
#======= Question 1 (2 Point) =======
# Q1-1. Spread the data out to multiple columns, with the shop type (Starbucks vs. Dunkin Donuts) being the column name (key) and the shops variable the value (Hint: tidyr).
# Q1-2. Delete observations that contain at least one missing value or invalud value (e.g. negative income).

#===================== Write R code HERE ==========================
#Q1-1
prc2 <- spread(prc, shop_type, shops)

#Q1-2
is.na(prc2)
sum(is.na(prc2))
prc3<- drop_na(prc2)

sum(is.na(prc3))
summary(prc3)
View(prc3)
#===================================================================


#======= Question 2 (2 Point) =======
# Q2-1. To examine the relationship between house prices and Starbucks, make a scatter plot using ggplot2.
# Q2-2. Repeat Q2-1 using Dunkin Donuts instead of Starbucks.

#====================== Write R code HERE ==========================
#Q2-1
install.packages("ggplot2")
library(ggplot2)

relationship <- ggplot(prc3, aes(starbucks, house_price_index, color=starbucks))+
  geom_point()
relationship
#Q2-2

relationship2 <- ggplot(prc3, aes(dunkin_donuts, house_price_index, color = dunkin_donuts))+
  geom_point()
relationship2

#===================================================================



#======= Question 3 (1 Point) =======
# Q3. Make a linear regression model to predict house prices based on the number of Starbucks and Dunkin Donuts.

#====================== Write R code HERE ==========================


my_lm <- lm(house_price_index ~ starbucks + dunkin_donuts, prc3)
summary(my_lm)


#===================================================================



#======= Question 4 (2 Point) =======
# One might argue that neighborhoods where Starbucks are located are relatively rich. 
# Does Starbucks have a predictive power for house prices, even after controlling for household incomes and population?
# Q4-1. Make new variables by taking log10 to median_income and population.
# Q4-2. Make a linear regression model to predict house prices based on the number of Starbucks and Dunkin Donuts, as well as (logarithm of) household incomes and population.

#====================== Write R code HERE ==========================
prc3 <- prc3 %>%
  mutate(income = log10(median_income), pop = log10(population))

View(prc3)

my_lm2<- lm(house_price_index ~ starbucks + dunkin_donuts+ income + pop, prc3)

summary(my_lm2)

#===================================================================



#======= Question 5 (2 Point) =======
# The dynamics of house prices might vary across counties in Boston.
# Q5-1. Separate the plot resulting from Question 2 by county.
# Q5-2. Add county variable to the previous linear regression model (from Question 4).

#====================== Write R code HERE ==========================
relationship <- ggplot(prc3, aes(starbucks, house_price_index, color=starbucks))+
  geom_point()+ 
  facet_wrap(.~ county)
relationship


relationship2 <- ggplot(prc3, aes(dunkin_donuts, house_price_index, color = dunkin_donuts))+
  geom_point()+ 
  facet_wrap(.~ county)
relationship2

final <- lm(house_price_index ~ starbucks + dunkin_donuts+ income + pop + county, prc3)
final

#===================================================================



#======= Question 6 (1 Point) =======
# Q6. Based on your analysis, do you agree or disagree that Starbucks is the bellwether of rise in house prices?
# Use comments to write your opinion (#).

#====================== Write R code HERE ==========================
summary(final)


#From residuals it can be seen that that the distribution of the residuals do not appear to be strongly symmetrical. 
#That means that the model predicts certain points that fall far away from the actual observed points.
#From Multiple R-squared we can claim results are not statistically significant.
#The intercept, in our example, is essentially the expected value required.
#Here starbucks are not closer to the expected value. Additionally, standard error is not quite high.
#At the same time p value is quite higher for starbucks which indeed shows relationship.
#On the other hand we can see from plot that housing_price_index is not quite scattered for starbucks.
#In conclusion I disagree with the idea that Starbucks is the bellwether.


#===================================================================
