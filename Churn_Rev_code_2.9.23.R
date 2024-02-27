# Load train and test datsets
data_set1 <- read.csv("C:/Users/loraa/OneDrive/Family/Desktop/Desktop/SNHU/DAT-690 Capstone in Data Analytics/Final Project/CustomerRevenue_Data.csv")
new_data <- read.csv("C:/Users/loraa/OneDrive/Family/Desktop/Desktop/SNHU/DAT-690 Capstone in Data Analytics/Final Project/CustomerRevenue_Verify.csv")


# Load necessary libraries
library(dplyr)
library(Metrics)  # For evaluation metrics
library(caret) 
install.packages("corrplot")
library(corrplot)
install.packages("VIM")
library(VIM)

#Variables to be rounded
# Select variables to round up
variables_to_round <- c("DROPVCE", "BLCKVCE", "UNANSVCE", "CUSTCARE", "THREEWAY", "OUTCALLS", "INCALLS", "PEAKVCE", "OPEAKVCE", "DROPBLK", "CALLFWDV", "CALLWAIT")

# Round up selected variables
data_set1[, variables_to_round] <- lapply(data_set1[, variables_to_round], function(x) ceiling(x))
new_data[, variables_to_round] <- lapply(new_data[, variables_to_round], function(x) ceiling(x))

# View the modified data_set
print(data_set1)
print(new_data)

#View data type
str(data_set1)
str(new_data)

#convert variables to numerical and remove categorical variables from both test and train dataset
data_set1 <- data_set1 %>% select(-CUSTOMER, -PhoneNumber) %>% mutate_all(as.numeric) 
new_data <- new_data %>% select(-CUSTOMER, -PhoneNumber) %>% mutate_all(as.numeric)
#data_set1 <- data_set1[,3:39]
#new_data <- new_data[,3:38]
#data_set1 <- mutate_all(data_set1, as.numeric)
#new_data <- mutate_all(new_data, as.numeric)

str(data_set1)
str(new_data)


# Replace zeros with NA in SETPRC in both test and train dataset
data_set1$SETPRC[data_set1$SETPRC == 0] <- NA
new_data$SETPRC[new_data$SETPRC == 0] <- NA

#######################Missing Value Analysis
# Check for missing values using colSums
print(colSums(is.na(data_set1)))
print(colSums(is.na(new_data)))

##############Missingness Analysis

#install.packages("VIM")
#library(VIM)
missing_pattern <- aggr(data_set1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data_set1), cex.axis=.7, gap=3)

#what is the percentage of missing data in SETPRC for different values of CHANGER?
spineMiss(data_set1[, c("CHANGER","SETPRC")])
#what is the percentage of missing data in SETPRC for different values of DIRECTAS?
spineMiss(data_set1[, c("DIRECTAS", "SETPRC")])

spineMiss(data_set1[, c("ROAM", "SETPRC")])
spineMiss(data_set1[, c("MOU", "SETPRC")])
spineMiss(data_set1[, c("OVERAGE", "SETPRC")])
spineMiss(data_set1[, c("CHANGEM", "SETPRC")])

#Patterns in missingness
matrixplot(data_set1, sortby = c('CHANGEM'))
matrixplot(data_set1, sortby = c('CHANGER'))
matrixplot(data_set1, sortby = c('MOU'))
matrixplot(data_set1, sortby = c('DIRECTAS'))
matrixplot(data_set1, sortby = c('OVERAGE'))
matrixplot(data_set1, sortby = c('ROAM'))
matrixplot(data_set1, sortby = c('SETPRC'))
matrixplot(data_set1, sortby = c('REVENUE'))
matrixplot(data_set1, sortby = c('DROPVCE'))

#impute missing values in the data_set1 (train dataset) using K nearest neighbour.
# Impute missing values using k-nearest neighbors (KNN)
data_set1 <- kNN(data_set1, k = 5)
# Delete the first 36 columns
data_set1 <- data_set1[, -(38:74)]
#check that values were imputed
print(colSums(is.na(data_set1)))

# Impute missing values in new_data (test dataset) using k-nearest neighbors (KNN)
new_data <- kNN(new_data, k = 5)
# Delete the first 36 columns
new_data <- new_data[, -(37:72)]
#check that values were imputed
print(colSums(is.na(new_data)))



###Principal Component Analysis
str(data_set1)# check if all variables are numeric
mean(cor(data_set1[,2:36])) #mean correlation of the dataset is less than 0.3 hence there are no strongly corelated variables and the PCA test can be avoided
pca_result <- prcomp(data_set1[,2:36])
summary(pca_result)
loadings <- pca_result$rotation
print(loadings)
PC = pca_result$x
View(PC)
mean(cor(PC))

# Extract eigenvalues
eigenvalues <- (pca_result$sdev)^2

# Plot the scree plot
plot(1:length(eigenvalues), eigenvalues, type = "b", 
     xlab = "Principal Component", ylab = "Eigenvalue",
     main = "Scree Plot")

# Create a biplot
biplot(pca_result, cex = 1.9) 

########Check alternative method to find correlated variables
# Install and load the corrplot package (if not already installed)
# install.packages("corrplot")
library(corrplot)

#######Correlation Analysis 
# Calculate Pearson's correlation coefficients
correlation_matrix <- cor(data_set1[, -which(names(data_set1) == "REVENUE")])

# Set a correlation threshold (adjust as needed)
correlation_threshold <- 0.7

# Find features with high correlation with the target variable
highly_correlated_features <- names(which(abs(correlation_matrix[, which(names(data_set1) == "REVENUE")]) > correlation_threshold))

# Display highly correlated features
print(highly_correlated_features)

# Install and load the corrplot package (if not already installed)
# install.packages("corrplot")
library(corrplot)

# Subset the dataset with correlated variables
subset_data <- data_set1[, highly_correlated_features]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data) #Why weren't these detected by the PCA analysis in previous step?

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# Compute the correlation matrix of independent variables
correlation_matrix_indep <- cor(data_set1[, -which(names(data_set1) == "REVENUE")])

# Example correlation matrix (replace with your actual correlation matrix)
correlation_matrix <- cor(data_set1)

# Set correlation threshold
correlation_threshold <- 0.8

# Find highly correlated variable pairs
highly_correlated_pairs <- which(abs(correlation_matrix) > correlation_threshold & upper.tri(correlation_matrix, diag = FALSE), arr.ind = TRUE)

# Print highly correlated variable pairs
for (i in 1:nrow(highly_correlated_pairs)) {
  row_idx <- highly_correlated_pairs[i, 1]
  col_idx <- highly_correlated_pairs[i, 2]
  
  var1 <- colnames(correlation_matrix)[row_idx]
  var2 <- colnames(correlation_matrix)[col_idx]
  
  cat("Variables", var1, "and", var2, "are highly correlated.\n")
}

# create linear regression model
statistical_sig <- lm(REVENUE ~ ., data = data_set1)
summary(statistical_sig)

#Remove correlated variables, keeping variable that was most statistically significant "MOU"
library(dplyr)

data_set1 <- data_set1 %>%
  select(-DROPVCE, -MOUREC, -PHONES, -NumMinStreamVideo) %>%
  mutate_all(as.numeric)

new_data <- new_data %>%
  select(-DROPVCE, -MOUREC, -PHONES, -NumMinStreamVideo) %>%
  mutate_all(as.numeric) 

###-OUTCALLS, -MOUREC, -PEAKVCE, -OPEAKVCE

##########Outlier Analysis##############
#create box plots for all variables in the dataset to see which data points are outliers.
boxplot(data_set1) 
#test model before outliers are handle
statistical_sig <- lm(REVENUE ~ ., data = data_set1)
summary(statistical_sig)

#Replace outliers using cap/floor technique
# Define the function
adjust_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  caps <- quantile(x, probs=c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

# Apply the function to each column of the dataset
adjusted_data <- lapply(data_set1, adjust_outliers)
adjusted_data <- as.data.frame(adjusted_data)

#Create boxplot to check if outliers were replaced properly
boxplot(adjusted_data)

#model performance after outliers 
statistical_sig <- lm(REVENUE ~ ., data = adjusted_data)
summary(statistical_sig)


######Feature Engineering###########
#estimate the customer's lifetime value
adjusted_data$CLV <- adjusted_data$REVENUE * adjusted_data$MONTHS
new_data$CLV <- new_data$REVENUE * new_data$MONTHS
View(adjusted_data)

#remove churn variable
adjusted_data <- adjusted_data %>%
  select(-CHURN) %>%
  mutate_all(as.numeric)


#model performance after CLV added 
# create linear regression model
model <- lm(REVENUE ~ ., data = adjusted_data)
summary(model)

# Make Predictions on Test data_set1
predictions <- predict(model, newdata = new_data)

# Evaluate Model Performance
mse <- mse(new_data$REVENUE, predictions)
rmse <- rmse(new_data$REVENUE, predictions)
mae <- mae(new_data$REVENUE, predictions)
r_squared <- R2(new_data$REVENUE, predictions)


# Display Evaluation Metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r_squared, "\n")

# Statistical Tests
anova_result <- anova(model)  # Analysis of Variance (ANOVA)
summary(model)  # Display detailed summary of the linear regression model



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
residuals <- residuals(model)

# Ensure that both data sets have the same number of rows
n_rows <- min(nrow(adjusted_data), nrow(new_data))

# Create a data_set2aframe with predictions and residuals
residual_df <- data.frame(
  predictions = predictions[1:n_rows],
  residuals = residuals[1:n_rows]
)

# Plot residuals vs. fitted values
plot(residual_df$predictions, residual_df$residuals,
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")

#####Assumption 3: Residuals are normally distributed.
# Plot a histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

S# Plot a Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals)

#####Assumption 4: Test if there is correlation in the residuals
# Install and load the 'car' package
install.packages("car")
library(car)

# Perform the Durbin-Watson test
durbinWatsonTest(model)

