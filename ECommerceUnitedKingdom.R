# Analyzing trends in e-commerce in the United Kingdom, 2018-2019
## Sara E. Hansen, hanse2s
## Modified October 9, 2023

library(tidyverse)
library(lubridate)
library(janitor)

options(max.print = 1e9)
options(dplyr.print_max = 1e9)

# Data obtained from Kaggle: https://www.kaggle.com/datasets/gabrielramos87/an-online-shop-business
# Thanks, Gabriel Ramos @gabrielramos87 !

# Let's get into it

dat <- read.csv("data/Sales Transaction v.4a.csv", fileEncoding = "UTF-8-BOM")

str(dat)

# This dataset represents sales of products by an e-commerce company
# based in the United Kingdom

# How big is this dataset?
dat %>% count() # 536,350 sales

# How many different products are sold by this company?
dat %>% 
  distinct(ProductName) %>%
  count() # 3,768 products

# When did these sales occur?
dat %>%
  count(Date)

dat %>%
  separate(Date, into = c("month", "day", "year")) %>%
  mutate(newDate = lubridate::ymd(paste(year, month, day, sep="-"))) %>%
  summarize(minDate = min(newDate),
            maxDate = max(newDate))
# sales were recorded between 2018-12-01 and 2019-12-09, so about one year

# Are there any discrepancies with ProductNo and ProductName?
dat %>%
  count(ProductNo, ProductName) %>%
  get_dupes(ProductNo) # 0

dat %>%
  count(ProductNo, ProductName) %>%
  get_dupes(ProductName) # 0
# No issues with either field

# Which products are sold most often?
dat %>%
  count(ProductName) %>%
  arrange(desc(n)) %>%
  head(100)
# Looks like party supplies are super popular!

# Which months have the most sales?
dat %>%
  separate(Date, into = c("month", "day", "year")) %>%
  count(month) %>%
  arrange(desc(n))
# November (11), followed by December (12), and October (10)
# They must sell some good Christmas presents

# What is the top selling product each month?
dat %>%
  separate(Date, into = c("month", "day", "year")) %>%
  group_by(month) %>%
  count(ProductName) %>%
  filter(n == max(n))
# Customers really love to party in the spring and summer

# Prep data for our uses
dat2 <- dat %>%
  separate(Date, into = c("month", "day", "year")) %>%
  mutate(Date = lubridate::ymd(paste(year, month, day, sep="-"))) %>%
  filter(Date < as.Date("2019-12-01"))
# We are not including the final month of data because it is not complete
# and we don't want to incorrectly calculate any summary statistics

# More in-depth analysis
# So far, we have only looked at how often a given product is sold,
# not the actual quantity sold in these transactions

# Which products had the most units sold?
dat2 %>%
  group_by(ProductName) %>%
  summarize(totalUnitsSold = sum(Quantity)) %>%
  arrange(desc(totalUnitsSold))
# Wow! The products sold most often are not the same as the products with the most units sold.

# How many unique items (and their quantities) are sold in each transaction?
dat2 %>%
  group_by(TransactionNo, ProductName) %>%
  summarize(unitSold = sum(Quantity)) %>%
  arrange(TransactionNo) %>%
  head(100)

dat2 %>%
  group_by(TransactionNo) %>%
  summarize(unitSold = sum(Quantity)) %>%
  head(100)
# Many customers are purchasing hundreds of items at a time

# How much money is made on each product each month?
# Let's just say Price is the same as profit, because we don't know the production cost
profitByMonth <- dat2 %>%
  mutate(profit = Price * Quantity) %>%
  mutate(month = recode(month,
                        "1" = "January", "2" = "February", "3" = "March",
                        "4" = "April", "5" = "May", "6" = "June", "7" = "July",
                        "8" = "August", "9" = "September", "10" = "October",
                        "11" = "November", "12" = "December")) %>%
  mutate(month = factor(month, level = c("January", "February", "March", 
                                         "April", "May", "June", "July",
                                         "August", "September", "October",
                                         "November", "December"))) %>%
  group_by(ProductName, month) %>%
  summarize(profit = sum(profit)) %>%
  pivot_wider(id_cols = ProductName,
              names_from = month,
              values_from = profit)

# What is the average profit made per transaction?
profitByTransaction <- dat2 %>%
  mutate(profit = Price * Quantity) %>%
  group_by(TransactionNo) %>%
  summarize(profit = sum(profit)) %>%
  arrange(desc(profit)) # with this table we can see the total profit for each transaction

summary(profitByTransaction$profit)
# Average profit per transaction is $2,619.60
# The largest profit in a single transaction was over $840,000 !

# What is the average profit per customer?
profitByCustomer <- dat2 %>%
  mutate(profit = Price * Quantity) %>%
  group_by(CustomerNo) %>%
  summarize(profit = sum(profit)) %>%
  arrange(desc(profit)) # with this table we can see the total profit for each customer

summary(profitByCustomer$profit)
# Average profit per customer is $12,422
# The largest profit made form a single customer is over $2 million !

# How many customers have bought from this business over the year?
count(distinct(profitByCustomer)) #4,692

# How does the total profit per customer distribute?
# From the summary, we know the Max is way above3rd Quartile
# so we will filter the results a little to get a picture of the majority of data
ggplot(profitByCustomer %>%
         filter(profit < 15000)) + 
  geom_histogram(aes(x = profit))
# Many customers tend to fall into the $1-2000 range, plenty of left skew
# Note there are also some customers who profit the company < 0,
# which just means their returns were higher in value than their purchases

# What is the total profit over the past year?
dat2 %>%
  mutate(profit = Price * Quantity) %>%
  plyr::summarise(sum(profit))
# Over $58 million

# What proportion of profit is each customer responsible for?
profitByCustomer2 <- profitByCustomer %>%
  mutate(prop = profit/sum(profit),
         percent = prop * 100,
         cumulative = cumsum(percent)) %>%
  arrange(desc(percent))

head(profitByCustomer2)  
# The top customer contributed over 3.5% of total profit this year!

# How many customers make up 20% of profit?
# (meaning the smallest number of customers, so those at the top)

profitByCustomer2 %>%
  filter(cumulative < 21)
# Wow! Only 21 customers make up 20.8% of profit this year!

topCustomers <- profitByCustomer2 %>%
  filter(cumulative < 21) %>%
  pull(CustomerNo)

# Which products are most popular among these top customers?
dat2 %>%
  filter(CustomerNo %in% topCustomers) %>%
  count(ProductName) %>%
  arrange(desc(n)) %>%
  head(50)
# These top customers really like the Regency Cakestand 3 Tier, 
# Jumbo Bag Red Retrospot, and Popcorn Holder!
# How about loyalty discounts on their favorite products?

# Are any products returned more frequently than they are purchased?
profitByProduct <- dat2 %>%
  mutate(profit = Price * Quantity) %>%
  group_by(ProductName) %>%
  summarize(profit = sum(profit)) %>%
  arrange(profit)

head(profitByProduct, 50)
# Travel Care Wallen Dotcomgift shop netted -$2000
# Maybe it's time to retire that one!
# How many times was it purchased?
dat2 %>%
  filter(ProductName == "Travel Card Wallet Dotcomgiftshop") %>%
  count(Quantity) # It was only once, someone returned 200 at a time
# Depending on the circumstances of each return and purchase,
# some of these products may not be worth selling anymore!

# How much did profit increase each month?
profitIncreaseByMonth <- dat2 %>%
  mutate(profit = Price * Quantity) %>%
  mutate(month = recode(month,
                        "1" = "January", "2" = "February", "3" = "March",
                        "4" = "April", "5" = "May", "6" = "June", "7" = "July",
                        "8" = "August", "9" = "September", "10" = "October",
                        "11" = "November", "12" = "December")) %>%
  mutate(month = factor(month, level = c("December", "January", "February",
                                         "March", "April", "May", 
                                         "June", "July", "August",
                                         "September", "October", "November"))) %>%
  group_by(month) %>%
  summarize(profit = sum(profit)) %>%
  mutate(prop = profit/sum(profit),
         percent = prop * 100,
         cumulative = cumsum(percent),
         increase = profit - lag(profit))

profitIncreaseByMonth

profitIncreaseByMonth %>%
  filter(str_detect(month, "December|September|October|November")) %>%
  plyr::summarise(sum(percent))
# The last four months of the year make up almost 44% of all profit

profitIncreaseByMonth %>%
  filter(increase < 0)
# The months of January, February, April, and June experienced negative growth this year
# Discounts and post-holiday promotions could help bump up profit in those months

# Will profit increase in future months, based on the current trend?
ggplot(data = dat2 %>%
         mutate(profit = Price * Quantity) %>%
         group_by(Date) %>%
         summarize(sumProfit = sum(profit))) +
  geom_point(aes(x = Date, y = sumProfit)) +
  geom_smooth(aes(x = Date, y = sumProfit))
# This company appears to be on track for a profitable December!

# Customer segmentation
# In order to more effectively market to and retain current customers,
# we want to understand their behavior.
# We will place customers into groups based on two factors:
# 1) how frequently they make a purchase, and 2) how much they spend per purchase
profitByCustomer3 <- dat2 %>%
  group_by(CustomerNo) %>%
  summarize(PurchasesPerMonth = n()/12) %>%
  left_join(dat2 %>%
              mutate(profit = Price * Quantity) %>%
              group_by(CustomerNo) %>%
              summarize(MeanPurchaseAmount = mean(profit)),
            by = "CustomerNo") %>%
  mutate(normPurchasesPerMonth = c(scale(PurchasesPerMonth)),
         normMeanPurchaseAmount = c(scale(MeanPurchaseAmount)))

summary(profitByCustomer3)
# You can see the means of the normalized ("norm") columns are 0

ggplot(profitByCustomer3) +
  geom_point(aes(x = normPurchasesPerMonth, y = normMeanPurchaseAmount))
# Both metrics have a lot of left skewness
# These two components do not create clear separation in our data

# However, we can create our own groups by creating some additional metrics
profitByCustomer4 <- profitByCustomer3 %>%
  mutate(newMetric = MeanPurchaseAmount/PurchasesPerMonth)
# Customers with a high positive value for newMetric buy infrequently, but
# when they do buy the purchase amount if very large

summary(profitByCustomer4$newMetric)
  
profitByCustomer4 %>%
  filter(newMetric > 0 & newMetric < 100) %>%
  ggplot() +
  geom_histogram(aes(x = newMetric), bins = 4)
# Just one way to visualize a subset of these data

# What could we do with this information?
# Customers with very high values of newMetric are infrequent buyers who spend a lot at once
# Offering a limited-time discount could bring them back in

# Customers with low (non-negative) values of newMetric are frequent buys who spend a little at once
# Offering free shipping above a certain purchase price could increase their spending
# These are our "loyal" customers, so we want to retain them

# Customers with moderate values of newMetric don't fall into either category.
# They may be more affected by seasonal trends, opting to buy during certain times
# Let's diver further into those customers

summary(profitByCustomer4$newMetric)
# Our "moderate" customers are those that fall between the 1st and 3rd quartile
# Are these customers more or less influenced by seasonal trends than the overall body of customers?

# Building on monthly calculations:
profitAndPurchasesByMonth <- dat2 %>%
  mutate(month = recode(month,
                        "1" = "January", "2" = "February", "3" = "March",
                        "4" = "April", "5" = "May", "6" = "June", "7" = "July",
                        "8" = "August", "9" = "September", "10" = "October",
                        "11" = "November", "12" = "December")) %>%
  mutate(month = factor(month, level = c("December", "January", "February",
                                         "March", "April", "May", 
                                         "June", "July", "August",
                                         "September", "October", "November"))) %>%
  group_by(month) %>%
  summarize(purchases = n()) %>%
  left_join(profitIncreaseByMonth %>%
              select(month, profit, propProfit = prop),
            by = "month") %>%
  mutate(propPurchases = purchases/sum(purchases)) %>%
  relocate(propPurchases, .after = purchases)

# Now do the same for our subset of "moderate" customers
first <- quantile(profitByCustomer4$newMetric, probs = c(0.25))
third <- quantile(profitByCustomer4$newMetric, probs = c(0.75))

mod_profitAndPurchasesByMonth <- profitByCustomer4 %>%
  filter(newMetric >= first & newMetric <= third) %>%
  left_join(dat2, by = c("CustomerNo")) %>%
  mutate(profit = Price * Quantity) %>%
  mutate(month = recode(month,
                        "1" = "January", "2" = "February", "3" = "March",
                        "4" = "April", "5" = "May", "6" = "June", "7" = "July",
                        "8" = "August", "9" = "September", "10" = "October",
                        "11" = "November", "12" = "December")) %>%
  mutate(month = factor(month, level = c("December", "January", "February",
                                         "March", "April", "May", 
                                         "June", "July", "August",
                                         "September", "October", "November"))) %>%
  group_by(month) %>%
  summarize(profit = sum(profit)) %>%
  mutate(propProfit = profit/sum(profit)) %>%
  left_join(profitByCustomer4 %>%
              filter(newMetric >= first & newMetric <= third) %>%
              left_join(dat2, by = c("CustomerNo")) %>%
              mutate(profit = Price * Quantity) %>%
              mutate(month = recode(month,
                                    "1" = "January", "2" = "February", "3" = "March",
                                    "4" = "April", "5" = "May", "6" = "June", "7" = "July",
                                    "8" = "August", "9" = "September", "10" = "October",
                                    "11" = "November", "12" = "December")) %>%
              mutate(month = factor(month, level = c("December", "January", "February",
                                                     "March", "April", "May", 
                                                     "June", "July", "August",
                                                     "September", "October", "November"))) %>%
              group_by(month) %>%
              summarize(purchases = n()), by = "month") %>%
  mutate(propPurchases = purchases/sum(purchases)) %>%
  relocate(purchases, .after = month) %>%
  relocate(propPurchases, .after = purchases)

# Let's compare these customers to the general population
profitAndPurchasesByMonth
mod_profitAndPurchasesByMonth

ggplot(data = bind_rows(profitAndPurchasesByMonth %>% mutate(customerType = "all"), 
                        mod_profitAndPurchasesByMonth %>% mutate(customerType = "mod"))) +
  geom_bar(aes(x = month, y = propPurchases, fill = customerType),
           stat = "identity", position = position_dodge())

ggplot(data = bind_rows(profitAndPurchasesByMonth %>% mutate(customerType = "all"), 
                        mod_profitAndPurchasesByMonth %>% mutate(customerType = "mod"))) +
  geom_bar(aes(x = month, y = propProfit, fill = customerType),
           stat = "identity", position = position_dodge())

# Though the exact proportions differ, the "moderate" subset of customers
# is affected similar by monthly trends as the overall population
# Which makes sense, because they make up the central 50% of the population

# Market to increase purchases in slow months with returning customer discounts
# In months where purchases by the moderate population are especially low,
# consider targeted marketing to them. Or increase their spending in high-purchase months
# with discounts, conditional coupons, free shipping, etc.

# There is a lot we can learn from the behavior of our customers over the past year!
# In the future, we could collect more information about each of them
# in order to predict which demographic factors influence transactions and spending.

rm(list = ls())

