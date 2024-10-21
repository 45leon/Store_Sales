library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)


train <- read.csv("Data/train.csv")
transactions <- read.csv("Data/transactions.csv")
stores <- read.csv("Data/stores.csv")
holiday_events <- read.csv("Data/holidays_events.csv")
oil <- read.csv("Data/oil.csv")


train <- train %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

oil$date <- as.Date(oil$date, format = "%Y-%m-%d")

transactions <- transactions %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

stores <- stores %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

str(oil)
str(train)
str(transactions)
families <- train %>% 
  distinct(family)

families %>% 
  print()

electronics_with_oil <- train %>% 
  filter(family == "PLAYERS AND ELECTRONICS") %>% 
  group_by(date) %>% 
  summarise(sales = sum(sales)) %>% 
  merge(oil) %>% 
  na.omit() %>% 
  filter(sales != 0) %>% 
  rename(oil_price = dcoilwtico) %>% 
  gather(key = "Variable", value = "Value", -date)

str(electronics_with_oil)

ggplot(electronics_with_oil, aes(x = date, y = Value, colour = Variable, group = Variable)) +
  geom_line() 


str(oil)
str(electronics_with_oil)  

#asdfasdfasdf
