# Set the seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation
num_folds <- 5

# Create the control parameters for cross-validation
ctrl <- trainControl(method = "cv",   # K-fold cross-validation method
                     number = num_folds)  # Number of folds

# Train the linear regression model with K-fold cross-validation
lm_model <- train(REVENUE ~ .,  # Predict Sepal.Length using other variables
                  data = data_set1,       # Using the iris dataset
                  method = "lm",    # Linear regression method
                  trControl = ctrl) # Cross-validation control parameters

# Print the cross-validation results
print(lm_model)

# Access the mean performance metrics across folds
print(lm_model$results)

# Extract variables of importance from the linear regression model
vars_importance <- varImp(lm_model, scale = FALSE)

# Print the variables of importance
print(vars_importance)

Adata_set1 <- data_set1 %>%
  select(-CHURN) %>%
  mutate_all(as.numeric)

predictions1 <- predict(lm_model, newdata = new_data)


# Evaluate Model Performance1
mse <- mse(new_data$REVENUE, predictions1)
rmse <- rmse(new_data$REVENUE, predictions1)
mae <- mae(new_data$REVENUE, predictions1)
r_squared <- R2(new_data$REVENUE, predictions1)


# Display Evaluation Metrics1
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r_squared, "\n")


#######Testing Assumptions############

#####Assumption 1: Linear relationship between target vraiable and predictor variables
# Calculate Pearson's correlation coefficients
correlation_matrix <- cor(data_set1[, -which(names(data_set1) == "REVENUE")])

# Set a correlation threshold (adjust as needed)
correlation_threshold <- 0.7

# Find features with high correlation with the target variable
highly_correlated_features <- names(which(abs(correlation_matrix[, which(names(data_set1) == "REVENUE")]) > correlation_threshold))

# Display highly correlated features
print(highly_correlated_features)

#####Assumption 2: Homoscedasticity: The residuals have constant variance at every level of x.

# Extract residuals from the linear regression model
residuals <- residuals(lm_model)

# Ensure that both data sets have the same number of rows
n_rows <- min(nrow(data_set1), nrow(new_data))

# Create a data_set2aframe with predictions and residuals
residual_df <- data.frame(
  predictions = predictions1[1:n_rows],
  residuals = residuals[1:n_rows]
)

# Plot residuals vs. fitted values
plot(residual_df$predictions, residual_df$residuals,
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")

#####Assumption 3: Residuals are normally distributed.
# Plot a histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

# Plot a Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals)

#####Assumption 4: Test if there is correlation in the residuals
# Install and load the 'car' package
install.packages("car")
library(car)

# Perform the Durbin-Watson test
durbinWatsonTest(lm_model)



