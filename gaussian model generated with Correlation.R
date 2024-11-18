# Load necessary libraries
if (!require(dplyr)) install.packages("dplyr")
if (!require(caTools)) install.packages("caTools")
if (!require(kernlab)) install.packages("kernlab")
if (!require(ggplot2)) install.packages("ggplot2")

library(dplyr)
library(caTools)
library(kernlab)
library(ggplot2)

# Importing the dataset
dataset <- read.csv('Data set.csv') 

# Install and load necessary libraries
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Select the relevant columns for correlation
data_for_corr <- dataset %>% 
  select(V, Time, Dye.amount, Ce, Qe)
# Calculate the correlation matrix
correlation_matrix <- cor(data_for_corr, use = "complete.obs")

# Plot the correlation matrix

par(mar = c(0.2, 0.2, 0.2, 0.2)) # Adjusting margins
corrplot(correlation_matrix, method = "ellipse", type = "upper", 
         tl.col = "black", tl.srt = 90,
         addCoef.col = "black", number.cex = 1, tl.cex = 1.45, 
         cex.main = 1) # Using tl.cex instead of cex


# Plot the scatter matrix plot using correlation  matrix

pairs(correlation_matrix,
      main = "Scatter Plot Matrix",
      col = "blue", pch = 19, cex = 0.6)



library(GGally)
library(ggplot2)

ggpairs(as.data.frame(correlation_matrix),
        title = "",
        upper = list(continuous = wrap("cor", size = 4, color = "maroon")),  # Red correlations in upper triangle
        lower = list(continuous = wrap("smooth", color = "darkgreen", size = 2)), # Smooth lines to connect points in the lower triangle
        diag = list(continuous = wrap("barDiag", fill = "blue", size = 3, color = "darkblue")), # Light blue density bars with black outlines on diagonal
        axisLabels = "show") + 
  theme_minimal() + # Minimalistic and clean theme
  theme(panel.grid.major = element_line(color = "skyblue", size = 0.2), # Faint grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines for a cleaner look
        panel.background = element_rect(fill = "white"), # White background for a clear contrast
        plot.title = element_text(color = "darkblue", size = 18, face = "bold"), # Customize title
        axis.text = element_text(size = 7, family = "Roboto"), # Roboto font for axis text
        axis.title = element_text(size = 10, face = "bold", family = "Roboto"), # Roboto font for axis title
        strip.text = element_text(size = 10, face = "bold", family = "Roboto")) # Roboto font for variable labels


library(GGally)
library(ggplot2)

# Custom function to display correlation coefficients
my_custom_cor <- function(data, mapping, color = I("black"), size = 4, ...){
  # Calculate correlation coefficient
  cor_val <- round(cor(data[[as.character(mapping$x)]], data[[as.character(mapping$y)]]), 2)
  # Use ggally_text to display it
  ggally_text(
    label = paste("Corr:", cor_val),
    mapping = mapping,
    color = "red",
    size = size,
    ...
  )
}

# Generate ggpairs plot
ggpairs(as.data.frame(correlation_matrix),
        title = "Correlation Matrix",
        upper = list(continuous = my_custom_cor), # Display correlations only on straight-line graphs
        lower = list(continuous = wrap("smooth", color = "blue", size = 0.5)), # Smooth line in lower triangle
        diag = list(continuous = wrap("barDiag", fill = "lightblue", color = "black")), # Diagonal with bar chart
        axisLabels = "show") + 
  theme_minimal() + # Minimalistic and clean theme
  theme(panel.grid.major = element_line(color = "gray90", size = 0.2), # Faint grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines for a cleaner look
        panel.background = element_rect(fill = "white"), # White background for a clear contrast
        plot.title = element_text(color = "darkblue", size = 14, face = "bold"), # Customize title
        axis.text = element_text(size = 7, family = "Roboto"), # Roboto font for axis text
        axis.title = element_text(size = 9, face = "bold", family = "Roboto"), # Roboto font for axis title
        strip.text = element_text(size = 9, face = "bold", family = "Roboto")) # Roboto font for variable labels

#applying gaussian process model+
# Load required libraries
library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)

# Import the dataset
dataset <- read.csv("Data set.csv")

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(dataset$Qe, p = 0.8, list = FALSE)
trainData <- dataset[trainIndex, ]
testData <- dataset[-trainIndex, ]

# Set up the training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train the model using Gaussian Process
model <- train(
  Qe ~ Co + V + Time + Dye.amount + Mass + Flow.rate + Ce,
  data = trainData,
  method = "gaussprRadial",
  trControl = train_control
)

# Make predictions on the test data
predicted_Qe <- predict(model, testData)

# Calculate R-squared for model performance
r_squared <- R2(predicted_Qe, testData$Qe)
cat("R² =", r_squared, "\n")

# Calculate test errors
test_error_1 <- mean(abs(predicted_Qe - testData$Qe)) # MAE
test_error_2 <- mean((predicted_Qe - testData$Qe)^2)  # MSE

cat("Test Error 1 (MAE):", test_error_1, "\n")
cat("Test Error 2 (MSE):", test_error_2, "\n")

# Plot the results
# (A) Predicted vs Experimental Qe with error bars
plot_A <- ggplot(testData, aes(x = 1:nrow(testData))) +
  geom_line(aes(y = predicted_Qe, color = "Predicted Qe"), size = 0.8) +
  geom_point(aes(y = testData$Qe, color = "Experimental Qe"), shape = 8, size = 2) +
  labs(x = "Instances", y = "Qe (mg/g)") +
  scale_color_manual(values = c("Predicted Qe" = "red", "Experimental Qe" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("A) Predicted vs Experimental Qe")

# (B) Predicted Qe vs Experimental Qe with R-squared
plot_B <- ggplot(testData, aes(x = Qe, y = predicted_Qe)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  annotate("text", x = max(testData$Qe) * 0.8, y = max(predicted_Qe) * 0.8, 
           label = paste("R² =", round(r_squared, 2)), hjust = 1, color = "black") +
  labs(x = "Experimental Qe (mg/g)", y = "Predicted Qe (mg/g)") +
  theme_minimal() +
  ggtitle("B) Predicted vs Experimental Qe with R²")

# Display plots side by side
grid.arrange(plot_A, plot_B, ncol = 2)




#applying gausian process and improving the model via hypermeter tuning
# Load required libraries
library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)

# Import the dataset
dataset <- read.csv("Data set.csv")

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(dataset$Qe, p = 0.8, list = FALSE)
trainData <- dataset[trainIndex, ]
testData <- dataset[-trainIndex, ]

# Set up the training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Define the grid of hyperparameters to search
# Only 'sigma' is needed for the "gaussprRadial" method
tune_grid <- expand.grid(
  sigma = seq(0.01, 1, length = 5)   # range of RBF kernel parameter
)

# Train the model using Gaussian Process with hyperparameter tuning
model <- train(
  Qe ~ Co + V + Time + Dye.amount + Mass + Flow.rate + Ce,
  data = trainData,
  method = "gaussprRadial",
  trControl = train_control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale")   # Standardize the features
)

# Make predictions on the test data
predicted_Qe <- predict(model, testData)

# Calculate R-squared for model performance
r_squared <- R2(predicted_Qe, testData$Qe)
cat("R² =", r_squared, "\n")

# Calculate test errors
test_error_1 <- mean(abs(predicted_Qe - testData$Qe)) # MAE
test_error_2 <- mean((predicted_Qe - testData$Qe)^2)  # MSE

cat("Test Error 1 (MAE):", test_error_1, "\n")
cat("Test Error 2 (MSE):", test_error_2, "\n")

# Plot the results
# (A) Predicted vs Experimental Qe with error bars
plot_A <- ggplot(testData, aes(x = 1:nrow(testData))) +
  geom_line(aes(y = predicted_Qe, color = "Predicted Qe"), size = 0.8) +
  geom_point(aes(y = testData$Qe, color = "Experimental Qe"), shape = 8, size = 2) +
  labs(x = "Instances", y = "Qe (mg/g)") +
  scale_color_manual(values = c("Predicted Qe" = "red", "Experimental Qe" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("A) Predicted vs Experimental Qe")

# (B) Predicted Qe vs Experimental Qe with R-squared
plot_B <- ggplot(testData, aes(x = Qe, y = predicted_Qe)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  annotate("text", x = max(testData$Qe) * 0.8, y = max(predicted_Qe) * 0.8, 
           label = paste("R² =", round(r_squared, 2)), hjust = 1, color = "black") +
  labs(x = "Experimental Qe (mg/g)", y = "Predicted Qe (mg/g)") +
  theme_minimal() +
  ggtitle("B) Predicted vs Experimental Qe with R²")

# Display plots side by side
grid.arrange(plot_A, plot_B, ncol = 2)



#applying gaussian
#ce as input then qe prediction
# Load required libraries
library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)

# Import the dataset
dataset <- read.csv("Data set.csv")

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(dataset$Qe, p = 0.8, list = FALSE)
trainData <- dataset[trainIndex, ]
testData <- dataset[-trainIndex, ]

# Set up the training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Define the grid of hyperparameters to search
tune_grid <- expand.grid(
  sigma = seq(0.01, 1, length = 5)   # Range of RBF kernel parameter for Gaussian Process
)

library(caret)
install.packages("kernlab")
library(kernlab)
# Required for Gaussian Process method

# Check if trainData and testData are data.frames
is.data.frame(trainData)  # Should return TRUE
is.data.frame(testData)   # Should return TRUE

# Verify column names in the dataset
colnames(trainData)

ce_model <- train(
  Ce ~ Co + V + Time + Dye.amount + Mass + Flow.rate,
  data = trainData,
  method = "gaussprRadial",
  trControl = train_control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale")
)


# Predict with Gaussian Process model

predicted_Ce <- predict(ce_model, as.data.frame(scale(testData)))


# Stage 1: Train a model to predict Ce
ce_model <- train(
  Ce ~ Co + V + Time + Dye.amount + Mass + Flow.rate,
  data = trainData,
  method = "gaussprRadial",
  trControl = train_control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale")
)

# Make predictions on the test data for Ce
predicted_Ce <- predict(ce_model, testData)

# Calculate R-squared and MSE for Ce prediction
ce_r_squared <- R2(predicted_Ce, testData$Ce)
ce_mse <- mean((predicted_Ce - testData$Ce)^2)

cat("Stage 1 - Ce Prediction\n")
cat("R² =", ce_r_squared, "\n")
cat("MSE:", ce_mse, "\n\n")

# Update testData with predicted Ce for Stage 2
testData$Predicted_Ce <- predicted_Ce
trainData$Predicted_Ce <- predict(ce_model, trainData)  # Adding predicted Ce to training data as well

# Stage 2: Train a model to predict Qe using predicted Ce and other variables
qe_model <- train(
  Qe ~ Co + V + Time + Dye.amount + Mass + Flow.rate + Predicted_Ce,
  data = trainData,
  method = "gaussprRadial",
  trControl = train_control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale")
)

# Make predictions on the test data for Qe
predicted_Qe <- predict(qe_model, testData)

# Calculate R-squared and MSE for Qe prediction
qe_r_squared <- R2(predicted_Qe, testData$Qe)
qe_mse <- mean((predicted_Qe - testData$Qe)^2)

cat("Stage 2 - Qe Prediction\n")
cat("R² =", qe_r_squared, "\n")
cat("MSE:", qe_mse, "\n")

# Plotting results
# (A) Predicted vs Experimental Qe
plot_A <- ggplot(testData, aes(x = 1:nrow(testData))) +
  geom_line(aes(y = predicted_Qe, color = "Predicted Qe"), size = 0.8) +
  geom_point(aes(y = testData$Qe, color = "Experimental Qe"), shape = 8, size = 2) +
  labs(x = "Instances", y = "Qe (mg/g)") +
  scale_color_manual(values = c("Predicted Qe" = "red", "Experimental Qe" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("A) Predicted vs Experimental Qe")

# (B) Predicted Qe vs Experimental Qe with R-squared
plot_B <- ggplot(testData, aes(x = Qe, y = predicted_Qe)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  annotate("text", x = max(testData$Qe) * 0.8, y = max(predicted_Qe) * 0.8, 
           label = paste("R² =", round(qe_r_squared, 2)), hjust = 1, color = "black") +
  labs(x = "Experimental Qe (mg/g)", y = "Predicted Qe (mg/g)") +
  theme_minimal() +
  ggtitle("B) Predicted vs Experimental Qe with R²")

# Display plots side by side
grid.arrange(plot_A, plot_B, ncol = 2)


# Install the iml package if not already installed
if (!requireNamespace("iml", quietly = TRUE)) {
  install.packages("iml")
}

#shapely plot

install.packages("shapr")
library(caret)
library(kernlab)
library(ggplot2)
library(Metrics)
library(gridExtra)
library(shapr)  # For Shapley value calculation

# Prepare a model explainer using the trained qe_model
explainer <- shapr(trainData[, -which(names(trainData) == "Qe")], model = qe_model$finalModel)

# Select some query points from the testData (for example, the first four)
queryPoints <- testData[1:4, ]

# Calculate Shapley values for the selected query points
shap_values_1 <- explain(explainer, x = queryPoints[1, -which(names(queryPoints) == "Qe")])
shap_values_2 <- explain(explainer, x = queryPoints[2, -which(names(queryPoints) == "Qe")])
shap_values_3 <- explain(explainer, x = queryPoints[3, -which(names(queryPoints) == "Qe")])
shap_values_4 <- explain(explainer, x = queryPoints[4, -which(names(queryPoints) == "Qe")])


# Set up the layout for the tiled plot
library(gridExtra)
library(grid)

grid.arrange(
  plot(shap_values_1), plot(shap_values_2),
  plot(shap_values_3), plot(shap_values_4),
  ncol = 2
)

