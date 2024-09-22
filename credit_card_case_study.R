setwd("C:/Users/ADMIN/Documents/Credit card case study")
customer_acquisition <- read.csv("Customer Acqusition.csv")
repayment_data <- read.csv("Repayment.csv")
spending_data <- read.csv("spend.csv")

summary(customer_acquisition)
str(customer_acquisition)
head(customer_acquisition)

# Rename the "Amount" column to "repay_amount"
colnames(repayment_data)[colnames(repayment_data) == "Amount"] <- "repay_amount"
# Rename the "Amount" column to "spend_amount"
colnames(spending_data)[colnames(spending_data) == "Amount"] <- "spend_amount"
# Rename the "Month" column to "repay_month"
colnames(repayment_data)[colnames(repayment_data) == "Month"] <- "repay_month"
# Rename the "Amount" column to "spend_month"
colnames(spending_data)[colnames(spending_data) == "Month"] <- "spend_month"

#1 a.Incase age is less than 18, replace it with mean value of age.
# Calculate the mean age
mean_age <- mean(customer_acquisition$Age)

# Replace ages less than 18 with the mean age
customer_acquisition$Age[customer_acquisition$Age < 18] <- mean_age

merged_data <- merge(customer_acquisition, repayment_data, by = "Customer")
merged_data <- merge(merged_data, spending_data, by = "Customer")

#1 b.	Incase spend amount is more than the limit, replace it with 50% of that customer's limit.
# Check if spend amount is greater than the limit
over_limit <- merged_data$spend_amount > merged_data$Limit

# Replace spend amount with 50% of the limit for transactions over the limit
merged_data$spend_amount[over_limit] <- merged_data$Limit[over_limit] * 0.5

#1 c.	Incase the repayment amount is more than the limit, replace the repayment with the limit.
# Check if repayment amount is greater than the limit
over_limit <- merged_data$repay_amount > merged_data$Limit

# Replace repayment amount with the limit for transactions over the limit
merged_data$repay_amount[over_limit] <- merged_data$Limit[over_limit]

#2 a.	How many distinct customers exist?
distinct_customers_count <- length(unique(merged_data$Customer))

cat("The number of distinct customers:", distinct_customers_count)

#2 b.	How many distinct categories exist?
distinct_categories_count <- length(unique(merged_data$Type))

cat("The number of distinct categories:", distinct_categories_count)

#2 c.	What is the average monthly spend by customers?
average_monthly_spend <- aggregate(spend_amount ~ Customer, data = merged_data, FUN = mean)

cat("Average Monthly Spend by Customers:\n")
print(average_monthly_spend)

#2d.	What is the average monthly repayment by customers?
average_monthly_repayment <- aggregate(repay_amount ~ Customer, data = merged_data, FUN = mean)

cat("Average Monthly Repayment by Customers:\n")
print(average_monthly_repayment)

#2e.	If the monthly rate of interest is 2.9%, what is the profit for the bank for each month?
# Calculate the monthly profit for each customer
merged_data$Monthly_Profit <- merged_data$repay_amount - merged_data$spend_amount

# Define the interest rate
interest_rate <- 2.9

# Calculate the interest earned
merged_data$Interest_Earned <- ifelse(merged_data$Monthly_Profit > 0, (merged_data$Monthly_Profit * interest_rate) / 100, 0)

# Calculate the total profit by grouping by the month
total_profit <- aggregate(Interest_Earned ~ repay_month, data = merged_data, FUN = sum)

cat("Total Profit for the Bank by Month:\n")
print(total_profit)

#2f.	What are the top 5 product types?
product_counts <- table(merged_data$Type)

# Sort the product counts in descending order
sorted_product_counts <- sort(product_counts, decreasing = TRUE)

# Select the top 5 product types
top_5_products <- names(sorted_product_counts)[1:5]

cat("Top 5 Product Types:\n")
print(top_5_products)

#2g.	Which city is having maximum spend?
city_spend <- aggregate(spend_amount ~ City, data = merged_data, FUN = sum)

# Find the city with the maximum spend
max_spend_city <- city_spend[which.max(city_spend$spend_amount), ]

cat("City with Maximum Spend:\n")
print(max_spend_city)

#2h.	Which age group is spending more money?
# Create age groups
merged_data$Age_Group <- cut(merged_data$Age, breaks = c(0, 18, 30, 45, 60, Inf),
                             labels = c("Under 18", "18-30", "31-45", "46-60", "Over 60"))

# Calculate total spending by age group
age_group_spend <- aggregate(spend_amount ~ Age_Group, data = merged_data, FUN = sum)

# Find the age group spending the most
max_spend_age_group <- age_group_spend[which.max(age_group_spend$spend_amount), ]

cat("Age Group Spending the Most:\n")
print(max_spend_age_group)

#2i.	Who are the top 10 customers in terms of repayment?
customer_repayment <- aggregate(repay_amount ~ Customer, data = merged_data, FUN = sum)

# Sort the customer repayments in descending order
sorted_customer_repayments <- customer_repayment[order(-customer_repayment$repay_amount), ]

# Select the top 10 customers
top_10_customers <- head(sorted_customer_repayments, 10)

cat("Top 10 Customers in Terms of Repayment:\n")
print(top_10_customers)

#3.	Calculate the city wise spend on each product on yearly basis. Also include a graphical representation for the same.
merged_data <- subset(merged_data, select = -c(No, SL.No., Sl.No.))
head(merged_data)
# Convert "spend_month" and "repay_month" columns to date format
merged_data$spend_month <- as.Date(merged_data$spend_month, format = "%d-%b-%y")
merged_data$repay_month <- as.Date(merged_data$repay_month, format = "%d-%b-%y")
# Create a new column for the year
merged_data$spend_year <- format(merged_data$spend_month, format = "%Y")
head(merged_data)
# Calculate the city-wise spend on each product on a yearly basis
library(dplyr)

city_product_spend <- merged_data %>%
  group_by(spend_year, City, Type) %>%
  summarise(Total_Spend = sum(spend_amount))

# Create a graphical representation (bar plot) with the "spend_year" column
library(ggplot2)

ggplot(city_product_spend, aes(x = spend_year, y = Total_Spend, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~City) +
  labs(title = "City-wise Spend on Each Product (Yearly)",
       x = "Year",
       y = "Total Spend",
       fill = "Product Type") +
  theme_minimal()

#4.	Create graphs for a.	Monthly comparison of total spends, city wise

# Create a plot of monthly comparison of total spends, city-wise
ggplot(merged_data, aes(x = spend_month, y = spend_amount, fill = City)) +
  geom_bar(stat = "sum") +
  labs(
    title = "Monthly Comparison of Total Spends (City-wise)",
    x = "Month",
    y = "Total Spend",
    fill = "City"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set1")  

#4 b.	Comparison of yearly spend on air tickets

# Filter the dataset to only include "AIR TICKET" Type
air_ticket_data <- subset(merged_data, Type == "AIR TICKET")

# Create a plot to compare yearly spend on air tickets
ggplot(air_ticket_data, aes(x = spend_year, y = spend_amount)) +
  geom_bar(stat = "sum", fill = "skyblue") +
  labs(
    title = "Yearly Spend on Air Tickets",
    x = "Year",
    y = "Total Spend",
    fill = NULL
  ) +
  theme_minimal()

#4 c.	Comparison of monthly spend for each product.

# Create a plot to compare monthly spend for each product
ggplot(merged_data, aes(x = spend_month, y = spend_amount, fill = Type)) +
  geom_bar(stat = "sum", position = "dodge") +
  labs(
    title = "Comparison of Monthly Spend for Each Product",
    x = "Month",
    y = "Total Spend",
    fill = "Product"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#5 5.	Write user defined R function to perform the following analysis:
#You need to find top 10 customers for each city in terms of their repayment amount by different products and by different time periods i.e. year or month. The user should be able to specify the product (Gold/Silver/Platinum) and time period (yearly or monthly) and the function should automatically take these inputs while identifying the top 10 customers.
# User-defined function to find top 10 customers by city, product, and time period
find_top_customers <- function(data, city, product, time_period) {
  # Filter the data based on the specified city and product
  filtered_data <- subset(data, City == city & Product == product)
  
  # Define the time period column based on user input (yearly or monthly)
  if (time_period == "yearly") {
    filtered_data$Time_Period <- format(filtered_data$repay_month, format = "%Y")
  } else if (time_period == "monthly") {
    filtered_data$Time_Period <- format(filtered_data$repay_month, format = "%Y-%m")
  } else {
    stop("Invalid time_period. Use 'yearly' or 'monthly'.")
  }
  
  # Calculate the total repayment amount for each customer and time period
  repay_summary <- aggregate(repay_amount ~ Customer + Time_Period, data = filtered_data, sum)
  
  # Find the top 10 customers for each time period
  top_10_customers <- by(repay_summary, repay_summary$Time_Period, function(x) {
    top_10 <- head(arrange(x, desc(repay_amount)), 10)
    return(top_10)
  })
  
  # Return the top 10 customers for each time period
  return(top_10_customers)
}


# Find the top 10 customers in BANGALORE for Gold products on a yearly basis
top_customers <- find_top_customers(merged_data, city = "BANGALORE", product = "Gold", time_period = "yearly")

# Print the top customers for each time period
print(top_customers)


