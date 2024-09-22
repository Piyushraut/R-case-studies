setwd("C:/Users/ADMIN/Documents/Retail case study_R")
Customers <- read.csv("Customer.csv")
Products <- read.csv("prod_cat_info.csv")
Transactions <- read.csv("Transactions.csv")
tail(Customers)
any(is.na(Transactions$tran_date))

# Load the lubridate package
library(lubridate)
# Rename the column
colnames(Transactions)[colnames(Transactions) == "prod_subcat_code"] <- "prod_sub_cat_code"
# Rename the column
colnames(Transactions)[colnames(Transactions) == "cust_id"] <- "customer_Id"

# Define the date formats you expect in your data
date_formats <- c("%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y")

# Parse the tran_date column
Transactions$tran_date <- parse_date_time(Transactions$tran_date, orders = date_formats)
any(is.na(Transactions$tran_date))

# Parse the tran_date column
Customers$DOB <- parse_date_time(Customers$DOB, orders = date_formats)

any(is.na(Transactions$tran_date))
# Check which rows have missing values in the tran_date column
rows_with_na <- Transactions[is.na(Transactions$tran_date), ]

# Print the rows with missing values
print(rows_with_na)

# Merge Customers and Transactions based on customer_Id
Customer_merged <- merge(Customers, Transactions, by.x = "customer_Id")

# Merge customer_merged and Products based on  prod_cat_code
Customer_Final <- merge(Customer_merged, Products, by.x = "prod_cat_code", by.y = "prod_cat_code")
tail(Customer_Final)

# Check for missing values in the 'tran_date' column
any(is.na(Customer_Final$tran_date))

# Load the dplyr library if not already loaded
library(dplyr)
# Merge Customers and Transactions based on customer_Id and cust_id
Customer_merged_dplyr <- Customers %>%
  left_join(Transactions, by = "customer_Id")

Customer_Final_dplyr <- Customer_merged_dplyr %>%
 left_join(Products, by = "prod_cat_code")
head(Customer_Final_dplyr)
any(is.na(Customer_Final_dplyr$tran_date))
#2 	Prepare a summary report for the merged data set.
#a.	Get the column names and their corresponding data types
# Get column names and data types
column_info <- sapply(Customer_Final, function(x) paste(class(x), collapse = ", "))

# Create a data frame with column names and data types
column_data_types <- data.frame(Column_Name = names(Customer_Final), Data_Type = column_info)

# Print the summary report
print(column_data_types)

#2 # Display the top 10 observations
top_10 <- head(Customer_Final, 10)
print(top_10)
# Display the bottom 10 observations
bottom_10 <- tail(Customer_Final, 10)
print(bottom_10)

#2c.		"Five-number summary" for continuous variables (min, Q1, median, Q3 and max)
# Calculate the Five-number summary for each continuous variable
continuous_vars <- c("Qty", "Rate", "Tax", "total_amt")  
five_num_summary <- sapply(Customer_Final[, continuous_vars], fivenum)

# Create a data frame for the summary
summary_df <- data.frame(
  Variable = continuous_vars,
  Minimum = five_num_summary[1, ],
  Q1 = five_num_summary[2, ],
  Median = five_num_summary[3, ],
  Q3 = five_num_summary[4, ],
  Maximum = five_num_summary[5, ]
)

# Print the Five-number summary
print(summary_df)

#2d.	Frequency tables for all the categorical variables
# Get a list of column names with categorical variables
categorical_vars <- sapply(Customer_Final, function(x) is.factor(x) || is.character(x))

# Create frequency tables for each categorical variable
frequency_tables <- lapply(Customer_Final[, categorical_vars], function(x) table(x))

# Print the frequency tables
for (i in seq_along(frequency_tables)) {
  cat("Frequency Table for", names(frequency_tables)[i], ":\n")
  print(frequency_tables[[i]])
  cat("\n")
}

#3.	Generate histograms for all continuous variables and frequency bars for categorical variables.
# Load the necessary libraries for plotting
library(ggplot2)

# Generate histograms for continuous variables with custom binwidths and titles
continuous_plots <- lapply(continuous_vars, function(var_name) {
  ggplot(Customer_Final, aes(x = .data[[var_name]])) +
    geom_histogram(binwidth = ifelse(var_name == "Qty", 1, 100), fill = "blue", color = "black") +
    labs(title = paste0("Histogram of ", var_name)) +
    theme_minimal()
})

# Generate frequency bar plots for categorical variables
categorical_plots <- lapply(names(Customer_Final[, categorical_vars]), function(var_name) {
  ggplot(Customer_Final, aes(x = .data[[var_name]])) +
    geom_bar(fill = "green") +
    labs(title = paste("Frequency Bar Plot of", var_name)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})


# Display the plots
print(continuous_plots)
print(categorical_plots)

#4.	Calculate the following information using the merged dataset :
#a.	Time period of the available transaction data

# Calculate the minimum and maximum dates in the 'tran_date' column
min_date <- min(Customer_Final$tran_date)
max_date <- max(Customer_Final$tran_date)

# Convert Unix timestamps to POSIXct format
start_date <- as.POSIXct(min_date, origin = "1970-01-01", tz = "UTC")
end_date <- as.POSIXct(max_date, origin = "1970-01-01", tz = "UTC")

# Print the results in a human-readable format
cat("Time Period of Available Transaction Data:\n")
cat("Start Date:", format(start_date, "%Y-%m-%d"), "\n")
cat("End Date:", format(end_date, "%Y-%m-%d"), "\n")

#4 b.	Count of transactions where the total amount of transaction was negative
# Filter transactions with negative total amount
negative_transactions <- Customer_Final[Customer_Final$total_amt < 0, ]

# Count of negative transactions
count_negative_transactions <- nrow(negative_transactions)

# Display the count of negative transactions
cat("Count of Negative Transactions:", count_negative_transactions, "\n")

# Display the first few rows of the resulting data frame with the calculated 'amount'
head(negative_transactions)

#5.	Analyze which product categories are more popular among females vs male customers.

# Group the data by 'Gender' and 'prod_cat', and calculate the count of transactions in each group
gender_product_popularity <- Customer_Final %>%
  group_by(Gender, prod_cat) %>%
  summarise(Count = n())

# Filter for female customers
female_popularity <- gender_product_popularity %>%
  filter(Gender == "F") %>%
  arrange(desc(Count))

# Filter for male customers
male_popularity <- gender_product_popularity %>%
  filter(Gender == "M") %>%
  arrange(desc(Count))

# Display the product categories that are more popular among females
cat("Product Categories More Popular Among Females:\n")
print(female_popularity)

# Display the product categories that are more popular among males
cat("\nProduct Categories More Popular Among Males:\n")
print(male_popularity)

#6 Which City code has the maximum customers and what was the percentage of customers from that city?

# Group the data by 'city_code' and calculate the count of unique customers in each city
city_customer_counts <- Customers %>%
  group_by(city_code) %>%
  summarise(CustomerCount = n_distinct(customer_Id))

# Find the city code with the maximum number of customers
max_customers_city <- city_customer_counts %>%
  filter(CustomerCount == max(CustomerCount))

# Calculate the total number of customers
total_customers <- sum(city_customer_counts$CustomerCount)

# Calculate the percentage of customers from the city with the maximum customers
percentage_max_city_customers <- (max_customers_city$CustomerCount / total_customers) * 100

# Display the results
cat("City Code with Maximum Customers:", max_customers_city$city_code, "\n")
cat("Percentage of Customers from that City:", percentage_max_city_customers, "%\n")

#7.	Which store type sells the maximum products by value and by quantity?

# Group the data by 'Store_type' and calculate the total quantity and total value of products sold in each store type
store_sales_summary <- Customer_Final %>%
  group_by(Store_type) %>%
  summarise(Total_Quantity = sum(Qty),
            Total_Value = sum(total_amt))

# Find the store type that sells the maximum products by quantity
max_quantity_store <- store_sales_summary %>%
  filter(Total_Quantity == max(Total_Quantity))

# Find the store type that sells the maximum products by value
max_value_store <- store_sales_summary %>%
  filter(Total_Value == max(Total_Value))

# Display the results
cat("Store Type Selling Maximum Products by Quantity:", max_quantity_store$Store_type, "\n")
cat("Store Type Selling Maximum Products by Value:", max_value_store$Store_type, "\n")

#8.	What was the total amount earned from the "Electronics" and "Clothing" categories from Flagship Stores?

# Filter the data for transactions in "Flagship store" and "Electronics" or "Clothing" categories
flagship_electronics_clothing <- Customer_Final %>%
  filter(Store_type == "Flagship store" & (prod_cat == "Electronics" | prod_cat == "Clothing"))

# Calculate the total amount earned from these transactions
total_amount_earned <- sum(flagship_electronics_clothing$total_amt)

# Display the result
cat("Total Amount Earned from Electronics and Clothing Categories in Flagship Stores:", total_amount_earned, "\n")

#9.	What was the total amount earned from "Male" customers under the "Electronics" category?
# Filter the data for transactions by "Male" customers and in the "Electronics" category
male_electronics <- Customer_Final %>%
  filter(Gender == "M" & prod_cat == "Electronics")

# Calculate the total amount earned from these transactions
total_amount_earned <- sum(male_electronics$total_amt)

# Display the result
cat("Total Amount Earned from Electronics Category by Male Customers:", total_amount_earned, "\n")

#10.	How many customers have more than 10 unique transactions, after removing all transactions which have any negative amounts?

# Remove transactions with negative amounts
filtered_data <- Customer_Final %>%
  filter(total_amt >= 0)

# Count the number of unique transactions per customer
unique_transactions <- filtered_data %>%
  group_by(customer_Id) %>%
  summarise(unique_count = n_distinct(transaction_id))

# Filter customers with more than 10 unique transactions
customers_with_more_than_10_transactions <- unique_transactions %>%
  filter(unique_count > 10)

# Count the number of customers
num_customers_with_more_than_10_transactions <- nrow(customers_with_more_than_10_transactions)

# Display the result
cat("Number of customers with more than 10 unique transactions after removing negative amounts:", num_customers_with_more_than_10_transactions, "\n")

#11.	For all customers aged between 25 - 35, find out:
#a.	What was the total amount spent for "Electronics" and "Books" product categories?
# Calculate age from DOB
Customer_Final$DOB <- as.Date(Customer_Final$DOB, format = "%d-%m-%Y")
Customer_Final$Age <- as.integer(difftime(Sys.Date(), Customer_Final$DOB, units = "days")/365)

# Filter customers aged between 25 and 35
filtered_customers <- Customer_Final %>%
  filter(between(Age, 25, 35))

# Calculate the total amount for "Electronics" and "Books" categories
category_totals <- filtered_customers %>%
  filter(prod_cat %in% c("Electronics", "Books")) %>%
  group_by(prod_cat) %>%
  summarise(total_amount = sum(total_amt))

# Display the results
head(category_totals)

#11 b.	What was the total amount spent by these customers between 1st Jan, 2014 to 1st Mar, 2014?
# Define the start and end dates for the time period
start_date <- as.Date("2014-01-01")
end_date <- as.Date("2014-03-01")

# Filter transactions within the specified date range
filtered_transactions <- filtered_customers %>%
  filter(tran_date >= start_date & tran_date <= end_date)

# Calculate the total amount spent during the specified time period
total_amount_spent <- sum(filtered_transactions$total_amt)

# Display the total amount spent
cat("Total Amount Spent between 1st Jan, 2014 and 1st Mar, 2014:", total_amount_spent, "\n")

summary(Customer_Final$Qty)

