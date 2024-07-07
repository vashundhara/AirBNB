# **Data Importing:** #

airbnb_data <- read.csv("C:/Users/user/Downloads/ABUS.csv")
options(max.print = 1000000)
View(airbnb_data)


# **Data Cleaning and Transformation:** #

library(dplyr)
library(tidyr)

# Handling missing values
airbnb_data <- airbnb_data %>% 
  drop_na()  # Drop rows with any missing values

# Handling outliers
airbnb_data <- airbnb_data %>% 
  filter(price <= 1000)  # Keep only listings with price <= 1000



# **Exploratory Data Analysis:** #

library(ggplot2)

# Summary statistics
summary(airbnb_data)

# Distributions
ggplot(airbnb_data, aes(x = price)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Price", y = "Frequency") +
  ggtitle("Distribution of Airbnb Listing Prices")

#histogram
hist(airbnb_data$price)

# Boxplot #

boxplot(airbnb_data$price ~ airbnb_data$neighbourhood_group)

# Correlations
colnames(airbnb_data)

correlation_matrix <- cor(airbnb_data[, c("price", "reviews_per_month", "number_of_reviews","availability_365")])
print(correlation_matrix)

# Scatter plot
ggplot(airbnb_data, aes(x = reviews_per_month, y = price)) +
  geom_point() +
  labs(x = "reviews_per_month", y = "Price") +
  ggtitle("Scatter Plot of Price vs.reviews_per_month ")


# **Feature Engineering:** #

# Example: Calculate the distance from a popular landmark (e.g., Empire State Building)
landmark_latitude <- 40.748817  # Replace with the actual latitude of the landmark
landmark_longitude <- -73.985428  # Replace with the actual longitude of the landmark

# Calculate the distance using Haversine formula or other distance functions

# Function to calculate distance using Haversine formula
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Earth's radius in kilometers
  radius <- 6371
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- radius * c
  
  return(distance)
}

# Replace landmark_latitude and landmark_longitude with the actual values for the landmark
landmark_latitude <- 0
landmark_longitude <- 0

# Calculate distance from landmark for each Airbnb listing
airbnb_data$distance_from_landmark <- calculate_distance(airbnb_data$latitude, airbnb_data$longitude, landmark_latitude, landmark_longitude)

# Add the new feature to the dataset
airbnb_data$distance_from_landmark <- calculate_distance(airbnb_data$latitude, airbnb_data$longitude, landmark_latitude, landmark_longitude)


# **Modeling:** #

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(airbnb_data), 0.7 * nrow(airbnb_data))
train_data <- airbnb_data[train_indices, ]
test_data <- airbnb_data[-train_indices, ]

# Build a linear regression model
model <- lm(price ~ reviews_per_month + number_of_reviews + distance_from_landmark, data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model
rmse <- sqrt(mean((test_data$price - predictions)^2))


# **Model Evaluation:** #

# Predict on test data
test_data$predicted_price <- predict(model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$price - test_data$predicted_price)^2))

# Calculate R-squared
r_squared <- summary(model)$r.squared

# Print evaluation metrics
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")


