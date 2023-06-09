install.packages("plolty")
# Load required libraries
library(tidyverse)
library(lattice)
library(caret)
library(randomForest)
library(e1071)
library(ROSE)
library(ggplot2)
library(lubridate)
library(data.table)
library(tibble)


# Read the dataset
data <- read.csv("BankChurners.csv")

# Check the dimensions of the dataset
dim(data)

#check the column names
colnames(data)

# check the structure to learn about the data type of each column
str(data)

# Check for missing values in each column
missing_values = colSums(is.na(data))
missing_values_table = tibble(Column = names(missing_values), Count = missing_values)
missing_values_table %>%
  print(n = 23)

# remove rows with NA values
data <- na.omit(data)

# remove duplicate rows
data <- distinct(data)

# remove the uneccessary columnn
data <- data %>%
  select(-c(Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1,
            Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))

# save the new data as a .csv file in the files directory so as to use to make visualizations on tableau
fwrite(data,"data_tableau.csv")

# customer_age
quantile(data$Customer_Age, 0.25)
quantile(data$Customer_Age, 0.75)
median(data$Customer_Age)
IQR(data$Customer_Age)
lower_bound <- quantile(data$Customer_Age, 0.25) - 1.5 * IQR(data$Customer_Age)
upper_bound <- quantile(data$Customer_Age, 0.75) + 1.5 * IQR(data$Customer_Age)
lower_bound
upper_bound
outlier_rows <- data$Customer_Age < lower_bound | data$Customer_Age > upper_bound
data$Customer_Age[outlier_rows]

# dependent_count
quantile(data$Dependent_count, 0.25)
quantile(data$Dependent_count, 0.75)
median(data$Dependent_count)
IQR(data$Dependent_count)
lower_bound <- quantile(data$Dependent_count, 0.25) - 1.5 * IQR(data$Dependent_count)
upper_bound <- quantile(data$Dependent_count, 0.75) + 1.5 * IQR(data$Dependent_count)
lower_bound
upper_bound
outlier_rows <- data$Dependent_count < lower_bound | data$Dependent_count > upper_bound
data$Dependent_count[outlier_rows]

#  months in book
quantile(data$Months_on_book, 0.25)
quantile(data$Months_on_book, 0.75)
median(data$Months_on_book)
IQR(data$Months_on_book)
lower_bound <- quantile(data$Months_on_book, 0.25) - 1.5 * IQR(data$Months_on_book)
upper_bound <- quantile(data$Months_on_book, 0.75) + 1.5 * IQR(data$Months_on_book)
lower_bound
upper_bound
outlier_rows <- data$Months_on_book < lower_bound | data$Months_on_book > upper_bound
data$Months_on_book[outlier_rows]

# products
quantile(data$Total_Relationship_Count, 0.25)
quantile(data$Total_Relationship_Count, 0.75)
median(data$Total_Relationship_Count)
IQR(data$Total_Relationship_Count)
lower_bound <- quantile(data$Total_Relationship_Count, 0.25) - 1.5 * IQR(data$Total_Relationship_Count)
upper_bound <- quantile(data$Total_Relationship_Count, 0.75) + 1.5 * IQR(data$Total_Relationship_Count)
lower_bound
upper_bound
outlier_rows <- data$Total_Relationship_Count < lower_bound | data$Total_Relationship_Count > upper_bound
data$Total_Relationship_Count[outlier_rows]

# inactive months
quantile(data$Months_Inactive_12_mon, 0.25)
quantile(data$Months_Inactive_12_mon, 0.75)
median(data$Months_Inactive_12_mon)
IQR(data$Months_Inactive_12_mon)
lower_bound <- quantile(data$Months_Inactive_12_mon, 0.25) - 1.5 * IQR(data$Months_Inactive_12_mon)
upper_bound <- quantile(data$Months_Inactive_12_mon, 0.75) + 1.5 * IQR(data$Months_Inactive_12_mon)
lower_bound
upper_bound
outlier_rows <- data$Months_Inactive_12_mon < lower_bound | data$Months_Inactive_12_mon > upper_bound
data$Months_Inactive_12_mon[outlier_rows]

# credit limit
quantile(data$Credit_Limit, 0.25)
quantile(data$Credit_Limit, 0.75)
median(data$Credit_Limit)
IQR(data$Credit_Limit)
lower_bound <- quantile(data$Credit_Limit, 0.25) - 1.5 * IQR(data$Credit_Limit)
upper_bound <- quantile(data$Credit_Limit, 0.75) + 1.5 * IQR(data$Credit_Limit)
lower_bound
upper_bound
outlier_rows <- data$Credit_Limit < lower_bound | data$Credit_Limit > upper_bound
data$Credit_Limit[outlier_rows]

# total transaction amount
quantile(data$Total_Trans_Amt, 0.25)
quantile(data$Total_Trans_Amt, 0.75)
median(data$Total_Trans_Amt)
IQR(data$Total_Trans_Amt)
lower_bound <- quantile(data$Total_Trans_Amt, 0.25) - 1.5 * IQR(data$Total_Trans_Amt)
upper_bound <- quantile(data$Total_Trans_Amt, 0.75) + 1.5 * IQR(data$Total_Trans_Amt)
lower_bound
upper_bound
outlier_rows <- data$Total_Trans_Amt < lower_bound | data$Total_Trans_Amt > upper_bound
data$Total_Trans_Amt[outlier_rows]



