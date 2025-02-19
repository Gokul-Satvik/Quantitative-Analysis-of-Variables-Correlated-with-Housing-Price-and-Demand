# If you are using a version of R that is newer than version 3.5.3, execute the following line of code:
suppressWarnings(RNGversion("3.5.3"))

# Linear Regression
# Load the dataset for House Prices
House_Price <- read.csv("House_Price.csv")

# Display the dataset
View(House_Price)

# Attach the dataset for easier variable reference
attach(House_Price)

# Display summary statistics for the dataset
summary(House_Price)

# Fit a linear regression model using the number of bedrooms (Beds) as the predictor variable
model <- lm(Sale_amount ~ Beds, House_Price)

# Display summary statistics for the linear regression model
summary(model)

# Fit a more complex linear regression model using multiple predictor variables
model <- lm(Sale_amount ~ Beds + Baths + Sqft_home + Sqft_lot + Type + Build_year, House_Price)

# Display summary statistics for the updated linear regression model
summary(model)

###############
# Logistic Regression
# Load necessary libraries for logistic regression
library(dplyr)
library(caret)

# Load the dataset for House Prices
df <- read.csv("House_Price.csv")

# Set a seed for reproducibility
set.seed(123)

# Set a threshold for Sale_amount to classify as above or below median
sale_amt_median <- median(df$Sale_amount)
threshold <- sale_amt_median
df$Above_Threshold <- ifelse(df$Sale_amount > threshold, 1, 0)

# Split the data into training and test sets
train_index <- createDataPartition(df$Above_Threshold, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Fit logistic regression model on the training data
model <- glm(Above_Threshold ~ Beds + Baths + Sqft_home + Sqft_lot + Build_year, 
             data = train_data, family = binomial)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data, type = "response")

# Create a confusion matrix
conf_matrix <- table(observed = test_data$Above_Threshold, predicted = ifelse(predictions > 0.5, 1, 0))

# Display the confusion matrix
print(conf_matrix)

# Calculate accuracy, sensitivity, and specificity
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

# Display the results
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

# Create ROC curve
roc_curve <- roc(test_data$Above_Threshold, predictions)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

