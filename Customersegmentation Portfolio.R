
pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS", "testthat",  "caret",
               "RSQLite", "class", "babynames", "nasaweather", "pls",
               "fueleconomy", "viridis", "boot", "devtools", "tree", "leaps",
               "glmnet", "gam", "akima", "factoextra", "randomForest", "gbm", 
               "ggrepel", "GGally", "fmsb", "sjPlot", "rcompanion", "DT")
# Installer nødvendige pakker, hvis du ikke allerede har dem

library(ggplot2)
library(pROC)
library(caret)
library(MASS)
library(gbm)
library(readxl)



#CRISP DM


#Business understanding



# Data Understanding ------------------------------------------------------



#Loading dataset
retaildata <- read_excel("Online Retail.xlsx")
head(retaildata)

#Explorative analysis

summary(retaildata)
str(retaildata)


sum(is.na(retaildata))
colSums(is.na(retaildata))

na_rows <- retaildata[!complete.cases(retaildata), ]
na_rows

subset(retaildata, UnitPrice == 0)

length(unique(retaildata$Country))
retaildata %>%
  summarise(across(where(is.numeric), list(min = ~min(.x, na.rm = TRUE),
                                           max = ~max(.x, na.rm = TRUE))))


# Data Preparation --------------------------------------------------------



#managering NA values

#removing all rows where CustomerID is missing

retaildata <- subset(retaildata, !is.na(CustomerID))


library(caret)

#dmy <- dummyVars(" ~ .", data = retaildata)
#retaildata_dummies <- data.frame(predict(dmy, newdata = retaildata))

retaildata$Country <- as.factor(retaildata$Country)


retaildata <- retaildata %>%
  group_by(CustomerID) %>%
  mutate(total_distinct_stockcodes = n_distinct(StockCode)) %>%
  ungroup()

retaildata <- retaildata %>%
  group_by(CustomerID) %>%
  mutate(total_quantity_purchased = sum(Quantity, na.rm = TRUE)) %>%
  ungroup()



#avg days betwwen orders
retaildata <- retaildata %>%
  group_by(CustomerID) %>%
  mutate(
    first_order_date = min(InvoiceDate, na.rm = TRUE),
    last_order_date = max(InvoiceDate, na.rm = TRUE),
    customer_active_days = as.numeric(difftime(last_order_date, first_order_date, units = "days"))
  ) %>%
  ungroup()

retaildata <- retaildata %>%
  group_by(CustomerID) %>%
  arrange(InvoiceDate) %>%
  mutate(
    days_since_last_order = as.numeric(difftime(InvoiceDate, lag(InvoiceDate), units = "days")),
    avg_days_between_orders = mean(days_since_last_order, na.rm = TRUE),
    days_since_last_order = ifelse(is.na(days_since_last_order), 0, days_since_last_order)  # Replace NA with 0
  ) %>%
  ungroup()

# No of orders
retaildata <- retaildata %>%
  group_by(CustomerID) %>%  # Group data by CustomerID
  mutate(
    total_orders = n_distinct(InvoiceNo)  # Count unique InvoiceNo per customer
  ) %>%
  ungroup()  # Remove grouping to maintain normal dataframe structure

retaildata <- retaildata %>%
  group_by(CustomerID) %>%  # Group data by CustomerID
  mutate(
    total_quantity = sum(Quantity, na.rm = TRUE)  # Sum all Quantity values per customer
  ) %>%
  ungroup()  # Remove grouping to restore normal dataframe structure

retaildata <- retaildata %>%
  group_by(CustomerID) %>%  # Group by Customer
  mutate(
    total_unique_stockcodes = n_distinct(StockCode)  # Count unique StockCodes per customer
  ) %>%
  ungroup()

retaildata <- retaildata %>%
  group_by(CustomerID) %>%  # Group by CustomerID
  mutate(
    total_spent = sum(Quantity * UnitPrice, na.rm = TRUE)  # Sum total amount spent per customer
  ) %>%
  ungroup()  # Remove grouping to restore normal dataframe structure

retaildata <- retaildata %>%
  group_by(CustomerID) %>%  # Group by CustomerID
  mutate(
    total_spent = sum(Quantity * UnitPrice, na.rm = TRUE),  # Total amount spent per customer
    total_unique_stockcodes = n_distinct(StockCode),  # Count of unique products purchased
    avg_spent_per_product = total_spent / total_unique_stockcodes  # Average spend per product
  ) %>%
  ungroup()  # Remove grouping to restore normal dataframe structure

library(dplyr)  # Load dplyr for efficient data manipulation

retaildata <- retaildata %>%
  mutate(avg_days_between_orders = ifelse(is.na(avg_days_between_orders), 0, avg_days_between_orders))

#customer_summary <- retaildata %>%
#  group_by(CustomerID) %>%
#  summarise(
#    total_spent = sum(Quantity * UnitPrice, na.rm = TRUE),  # Total spend
#    total_unique_stockcodes = n_distinct(StockCode),  # Unique products purchased
#    avg_spent_per_product = total_spent / total_unique_stockcodes  # Avg spend per product
#  )

library(dplyr)  # Load dplyr for efficient data manipulation

retaildata <- retaildata %>%
  group_by(CustomerID) %>%  # Group by CustomerID
  mutate(
    total_quantity = sum(Quantity, na.rm = TRUE),  # Total quantity purchased per customer
    total_orders = n_distinct(InvoiceNo),  # Count unique orders per customer
    avg_quantity_per_order = total_quantity / total_orders  # Compute average quantity per order
  ) %>%
  ungroup()  # Remove grouping to restore normal dataframe structure

dummy_model <- dummyVars("~ Country", data = retaildata)
country_dummies <- data.frame(predict(dummy_model, newdata = retaildata))

# Combine dummy columns back into the data set
retaildata <- cbind(retaildata, country_dummies)

retaildata_scaled <- retaildata %>%
  # 1. Remove the specified columns
  dplyr::select(-Description, -Country, -Quantity, -UnitPrice, -InvoiceDate, -StockCode, -first_order_date, -last_order_date, -InvoiceNo)

str(retaildata_scaled)
library(dplyr)  # Load dplyr for efficient data manipulation

retaildata_scaled <- retaildata_scaled %>%
  mutate_if(is.integer, as.numeric)  # Convert all integer columns to numeric


retaildata_scaled <- retaildata_scaled %>%
  distinct()  # Removes completely identical rows (duplicates across all columns)

customerIDs <- retaildata_scaled$CustomerID


library(dplyr)  # Load dplyr for data manipulation

# Identify columns that have more than two unique values
cols_to_scale <- retaildata_scaled %>%
  select_if(is.numeric) %>%  # Select only numeric columns
  summarise_all(n_distinct) %>%  # Count distinct values per column
  gather(variable, unique_values) %>%  # Convert to long format
  filter(unique_values > 2) %>%  # Keep only columns with more than 2 unique values
  pull(variable)  # Extract column names

# Fjern "CustomerID" fra listen, så den ikke standardiseres
cols_to_scale <- setdiff(cols_to_scale, "CustomerID")

retaildata_scaled <- retaildata_scaled %>%
  mutate_at(vars(cols_to_scale), scale)

# Tilføj CustomerID tilbage til datasættet
retaildata_scaled$CustomerID <- customerIDs


sum(is.na(retaildata_scaled))
colSums(is.na(retaildata_scaled))

str(retaildata_scaled)






# Modelling ---------------------------------------------------------------



str(retaildata_scaled)

sum(is.na(retaildata_scaled))
colSums(is.na(retaildata_scaled))
# Opret et datasæt til clustering, hvor CustomerID udelukkes
clustering_data <- retaildata_scaled %>% dplyr::select(-CustomerID)

#K-means

# Use the Elbow Method to choose the optimal number of clusters
set.seed(123)  # Ensures reproducible results
wss <- numeric(20)  # Vector to store total within-cluster sum of squares (WSS) for k=1..20

for (k in 1:20) {
  model <- kmeans(clustering_data, centers = k, nstart = 5, iter.max = 15)
  wss[k] <- model$tot.withinss
}

# 5. Plot the results and look for the “elbow”
plot(1:20, wss, type = "b",
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Retail Data")


library(ggplot2)
library(dplyr)
library(reshape2)  # For reshaping data for visualization

set.seed(123)  # Ensures reproducibility

k_optimal <- 7  # Set the optimal number of clusters to 4

# Train the K-means model with the chosen number of clusters
kmeans_model <- kmeans(clustering_data, centers = k_optimal, nstart = 25)

# Convert the cluster centers to a dataframe for visualization
centers_df <- as.data.frame(kmeans_model$centers)

# Add a column to label each cluster
centers_df$Cluster <- as.factor(1:nrow(centers_df))

# Transform to 'long' format for ggplot2 visualization
centers_long <- melt(centers_df, id.vars = "Cluster")

# Create a bar plot to visualize cluster centroids
ggplot(centers_long, aes(x = variable, y = value, fill = Cluster)) + 
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot for centroid values
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Feature", y = "Centroid Value", fill = "Cluster") +  # Axis labels and legend
  ggtitle("Centroid Values for Each Cluster from K-Means")  # Plot title

# Assign cluster labels to the original dataset
retaildata_scaled$clusterKmeans <- as.factor(kmeans_model$cluster)

# Print the number of observations in each cluster
table(retaildata_scaled$clusterKmeans)

# Join 'clusterKmeans' from retaildata_scaled to retaildata based on CustomerID
retaildata_with_segments <- retaildata %>%
  dplyr::left_join(retaildata_scaled %>% dplyr::select(CustomerID, clusterKmeans), by = "CustomerID")



# Remove columns
retaildata_with_segments <- retaildata_with_segments %>% dplyr::select(-InvoiceNo, -StockCode, -Description, -InvoiceDate, -UnitPrice, -Country)
# Remove columns
retaildata_with_segments <- retaildata_with_segments %>% dplyr::select(-Quantity)

# Remove all duplicate rows from retaildata
retaildata_with_segments <- retaildata_with_segments %>% distinct()



# Evalation ---------------------------------------------------------------


# Convert K-Means centroids to a readable dataframe
centroid_values <- as.data.frame(kmeans_model$centers)

# Add a cluster identifier column
centroid_values$Cluster <- 1:nrow(centroid_values)

# Print the table with all cluster centroid values
print(centroid_values, digits = 3)  # Show 3 decimal places for readability

library(fmsb)  # Load for radar charts
library(dplyr)  # Load for data manipulation
library(tidyr)  # Load for data reshaping

# Function to plot only the most defining features for a cluster
plot_top_features_radar <- function(cluster_num, top_n = 5) {
  
  # Extract the centroid values for the selected cluster
  cluster_data <- as.data.frame(kmeans_model$centers)[cluster_num, , drop = FALSE]
  
  # Find the top N most influential features (highest absolute values)
  top_features <- cluster_data %>%
    gather(key = "Feature", value = "CentroidValue") %>%
    arrange(desc(abs(CentroidValue))) %>%
    slice(1:top_n) %>%  # Select only the top N features
    pull(Feature)  # Extract feature names
  
  # Subset the centroid data to keep only the selected features
  cluster_data <- cluster_data[, top_features, drop = FALSE]
  
  # Find min/max values for the selected features for scaling
  max_vals <- apply(as.data.frame(kmeans_model$centers)[, top_features, drop = FALSE], 2, max)
  min_vals <- apply(as.data.frame(kmeans_model$centers)[, top_features, drop = FALSE], 2, min)
  
  # Add min/max rows for normalization
  cluster_data <- rbind(max_vals, min_vals, cluster_data)
  
  # Define colors for visualization
  cluster_color <- "blue"
  fill_color <- adjustcolor(cluster_color, alpha.f = 0.3)  # Lightened color
  
  # Generate the radar chart
  radarchart(cluster_data,
             axistype = 2,  # Show axis labels
             pcol = cluster_color,  # Border color for the cluster
             pfcol = fill_color,  # Fill color
             plwd = 2,  # Line width
             cglcol = "gray", cglty = 1, cglwd = 0.8,  # Grid settings
             axislabcol = "black",  # Axis label color
             vlcex = 0.8)  # Size of variable labels
  
  # Add a title
  title(paste("Top", top_n, "Features for Cluster", cluster_num), cex.main = 1.5)
}

plot_top_features_radar(1, top_n = 6)  # View Cluster 1
plot_top_features_radar(2, top_n = 6)  # View Cluster 2
plot_top_features_radar(3, top_n = 6)  # View Cluster 3
plot_top_features_radar(4, top_n = 6)  # View Cluster 4
plot_top_features_radar(5, top_n = 6)  # View Cluster 5
plot_top_features_radar(6, top_n = 6)  # View Cluster 6
plot_top_features_radar(7, top_n = 6)  # View Cluster 7



cluster_summary <- retaildata_scaled %>%
  group_by(clusterKmeans) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  print(n = Inf)  # Show all cluster mean values



#Step 1: Understanding Each Cluster’s Characteristics
#Looking at the cluster_summary, we analyze how each cluster behaves compared to others:
#  
#  Cluster	Unique Products Bought	Total Quantity Purchased	Active Days	Possible Label
#1	-0.315	-0.291	0.154	"Average Customers"
#2	-0.390	-0.309	-1.36	"Churn Risk Customers"
#3	-0.0460	-0.163	0.576	"Occasional Buyers"
#4	-0.573	-0.275	0.0359	"Inactive Customers"
#5	0.733	6.70	0.826	"High-Value Buyers"
#6	0.534	1.82	0.811	"Frequent Buyers"
#7	5.40	2.52	1.01	"Bulk Buyers"



#Now, let's create meaningful customer segment labels:

retaildata_scaled$customer_segment <- factor(retaildata_scaled$clusterKmeans,
                                             levels = 1:7,
                                             labels = c("Average Customers", 
                                                        "Churn Risk Customers",
                                                        "Occasional Buyers",
                                                        "Inactive Customers",
                                                        "High-Value Buyers",
                                                        "Frequent Buyers",
                                                        "Bulk Buyers"))

retaildata_with_segments$customer_segment <- factor(retaildata_with_segments$clusterKmeans,
                                             levels = 1:7,
                                             labels = c("Average Customers", 
                                                        "Churn Risk Customers",
                                                        "Occasional Buyers",
                                                        "Inactive Customers",
                                                        "High-Value Buyers",
                                                        "Frequent Buyers",
                                                        "Bulk Buyers"))

retaildata_with_segmentsnoID <- retaildata_with_segments  %>% dplyr::select(-CustomerID)

write.csv(retaildata_with_segmentsnoID, "retaildata_with_segmentsNOID.csv", row.names = FALSE)

cluster_sizes <- table(retaildata_scaled$customer_segment)
library(ggplot2)
barplot(cluster_sizes, main = "Number of Customers in Each Segment", col = "steelblue")

ggplot(retaildata_scaled, aes(x = customer_segment, y = total_quantity_purchased, fill = customer_segment)) +
  geom_boxplot() +
  labs(title = "Quantity Purchased by Customer Segment",
       x = "Customer Segment", 
       y = "Total Quantity Purchased") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(retaildata_scaled, aes(x = customer_segment, y = customer_active_days, fill = customer_segment)) +
  geom_boxplot() +
  labs(title = "Active Days by Customer Segment",
       x = "Customer Segment", 
       y = "Customer Active Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Deployment


#Step 3: Turning Clusters into Business Strategies
#Now that we know what each customer segment represents, we can suggest marketing strategies:
#  
#  Segment	Key Insights	Actionable Strategies
#1️⃣ Average Customers	Moderate quantity & active days	Offer loyalty rewards to increase engagement
#2️⃣ Churn Risk Customers	Very low quantity & low active days	Send re-engagement emails & discounts
#3️⃣ Occasional Buyers	Buy infrequently but active	Send personalized recommendations
#4️⃣ Inactive Customers	Low activity, few items bought	Run win-back campaigns
#5️⃣ High-Value Buyers	Buy in large amounts frequently	Offer exclusive VIP perks
#6️⃣ Frequent Buyers	Buy often but smaller amounts	Introduce subscription models
#7️⃣ Bulk Buyers	Buy large quantities, less often	Offer bulk discounts & business accounts


#Step 5: How to Use These Insights
#💡 For Marketing Teams:
#  
#  VIP Customers → Exclusive offers, early access, loyalty programs
#Bulk Buyers → Wholesale pricing, custom deals
#Frequent Buyers → Subscription services, membership discounts
#Churn Risk Customers → Re-engagement emails, special discounts
#💡 For Product Teams:
#  
#  See which products attract which segments
#Optimize pricing based on spending patterns
#Improve customer retention strategies




retaildata



