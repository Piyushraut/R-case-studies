# Load the necessary libraries
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("C:/Users/ADMIN/Documents/CaseStudy_R_Visualizations")

SalesData <- read.csv("SalesData.csv")
#1
# Step 1: Aggregate the data by region and year
sales_by_region <- SalesData %>%
  group_by(Region) %>%
  summarise(Sales2015 = sum(Sales2015),
            Sales2016 = sum(Sales2016))

# Step 2: Reshape the data from wide to long
melted_sales_data <- melt(sales_by_region, id.vars = "Region")

# Step 3: Create a bar chart
ggplot(melted_sales_data, aes(x = Region, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Sales by Region (2015 vs. 2016)",
       x = "Region",
       y = "Total Sales",
       fill = "Year") +
  theme_minimal()

#2

# Group data by region
sales_by_region <- SalesData %>%
  group_by(Region)

# Summarize total sales for each region in 2016
sales_summary <- sales_by_region %>%
  summarise(
    TotalSales = sum(Sales2016)
  )

# Create a data frame for the pie chart
pie_data <- sales_summary

# Create the pie chart for total sales in 2016 by region
pie_chart <- pie_data %>%
  ggplot(aes(x = "", y = TotalSales, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Total Sales (2016) by Region") +
  theme_void() +
  geom_text(aes(label = scales::percent(TotalSales/sum(TotalSales), scale = 1), vjust = 0.5), 
            position = position_stack(vjust = 0.5))  # Add percentage labels inside their areas

# Display the pie chart
print(pie_chart)


#3.
# Step 1: Aggregate the data by Tier, Region, and Year
sales_by_tier_region <- SalesData %>%
  group_by(Tier, Region) %>%
  summarise(TotalSales2015 = sum(Sales2015),
            TotalSales2016 = sum(Sales2016))

# Step 2: Reshape the data from wide to long
melted_sales_data <- melt(sales_by_tier_region, id.vars = c("Tier", "Region"))

# Step 3: Create a grouped bar chart
ggplot(melted_sales_data, aes(x = Tier, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_grid(. ~ Region) +  # Facet by Region
  labs(title = "Comparison of Total Sales by Tier and Region (2015 vs. 2016)",
       x = "Tier",
       y = "Total Sales",
       fill = "Year") +
  theme_minimal()


#4

# Filter the data for the East region
east_region_data <- subset(SalesData, Region == "East")

# Calculate the total sales for each state in 2015 and 2016
state_sales_summary <- east_region_data %>%
  group_by(State) %>%
  summarise(Total_Sales_2015 = sum(Sales2015),
            Total_Sales_2016 = sum(Sales2016))

# Identify states with a decline in sales in 2016 compared to 2015
states_with_decline <- state_sales_summary[state_sales_summary$Total_Sales_2016 < state_sales_summary$Total_Sales_2015, ]

# Create a bar plot for total sales in 2015 and 2016 for each state
melted_state_sales <- melt(state_sales_summary, id.vars = "State")

# Bar plot
ggplot(melted_state_sales, aes(x = State, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales (2015 vs. 2016) by State in the East Region",
       x = "State",
       y = "Total Sales",
       fill = "Year") +
  scale_fill_manual(values = c("Total_Sales_2015" = "blue", "Total_Sales_2016" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#5

# Filter the data for High tier and calculate the total units sold for each Division in 2015 and 2016
high_tier_data <- subset(SalesData, Tier == "High")
unit_summary <- high_tier_data %>%
  group_by(Division) %>%
  summarise(Total_Units_2015 = sum(Units2015),
            Total_Units_2016 = sum(Units2016))

# Reshape the data from wide to long using melt
unit_summary_long <- melt(unit_summary, id.vars = "Division")

# Create a bar plot for unit_summary_long
ggplot(unit_summary_long, aes(x = Division, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Units Sold (2015 vs. 2016) by Division in High Tier",
       x = "Division",
       y = "Total Units Sold",
       fill = "Year") +
  scale_fill_manual(values = c("Total_Units_2015" = "blue", "Total_Units_2016" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#6

# Define the quarters based on months
SalesData <- SalesData %>%
  mutate(Qtr = ifelse(Month %in% c("Jan", "Feb", "Mar"), "Q1",
                      ifelse(Month %in% c("Apr", "May", "Jun"), "Q2",
                             ifelse(Month %in% c("Jul", "Aug", "Sep"), "Q3",
                                    ifelse(Month %in% c("Oct", "Nov", "Dec"), "Q4", NA)))))

# View the updated dataset with the "Qtr" column
head(SalesData)

#7
# Group data by Qtr and Year (2015 or 2016) and calculate total sales
sales_summary <- SalesData %>%
  group_by(Qtr) %>%
  summarise(Total_Sales_2015 = sum(Sales2015),
            Total_Sales_2016 = sum(Sales2016))

print(sales_summary)
# Reshape the data to long format
sales_summary_long <- melt(sales_summary, id.vars = "Qtr")
print(sales_summary_long)
# Create a bar plot to compare total sales in 2015 and 2016 by quarter
ggplot(sales_summary_long, aes(x = Qtr, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Qtr-wise Sales (2015 vs. 2016)",
       x = "Quarter (Qtr)",
       y = "Total Sales",
       fill = "Year") +
  scale_fill_manual(values = c("Total_Sales_2015" = "blue", "Total_Sales_2016" = "green")) +
  theme_minimal()

#8

# Group data by Quarter and Tier
sales_by_qtr_tier <- SalesData %>%
  group_by(Qtr,Tier) %>%
  summarise(TotalSales = sum(Sales2016))

# Create pie charts for each Quarter and Tier
pie_charts <- ggplot(sales_by_qtr_tier, aes(x = "", y = TotalSales, fill = Tier)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~Qtr, ncol = 2) +
  labs(title = "Composition of Quarterly Sales (2016) by Tier", fill = "Tier") +
  theme_void()

# Display the pie charts
print(pie_charts)
