# Import the dataset
dataset <- read.csv("Data set.csv")

library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)



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

#shapely plot
install.packages("iml")
library(iml)
install.packages("iml")
library(iml)
library(caret)
library(kernlab)
library(ggplot2)
library(gridExtra)


# Wrap the Gaussian Process model for Ce in a Predictor object
ce_predictor <- Predictor$new(
  model = ce_model,
  data = testData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate")],
  y = testData$Ce
)
colnames(testData)

# Creating the Predictor object for ce_model without modifying testData further
ce_predictor <- Predictor$new(
  model = ce_model,
  data = testData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")],
  y = testData$Ce
)

# Creating the Predictor object for qe_model using the existing Predicted_Ce column in testData
qe_predictor <- Predictor$new(
  model = qe_model,
  data = testData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")],
  y = testData$Qe
)


# Calculate SHAP values for the Gaussian Process model predicting Ce
ce_shapley <- Shapley$new(predictor = ce_predictor, x.interest = testData[1,])
qe_shapley <- Shapley$new(predictor = qe_predictor, x.interest = testData[1,])


# Plot SHAP values for Ce prediction
plot(ce_shapley)






# Import the dataset
dataset <- read.csv("Data set.csv")

library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)

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



# Remove the original Ce column and use only Predicted_Ce
testData$Predicted_Ce <- predicted_Ce
trainData$Predicted_Ce <- predict(ce_model, trainData)  # Adding predicted Ce to training data as well
trainData <- trainData[, !(names(trainData) %in% "Ce")]
testData <- testData[, !(names(testData) %in% "Ce")]

# Stage 2: Train a model to predict Qe using Predicted_Ce and other variables
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


library(ggplot2)
library(caret)

# Select four random instances for which to create a shapely-like plot
selected_instances <- sample(1:nrow(testData), 4)

# Create a dataframe to store predictions and actual values for selected instances
shapely_data <- data.frame(
  Instance = selected_instances,
  Predicted_Ce = testData$Predicted_Ce[selected_instances],
  Actual_Qe = testData$Qe[selected_instances],
  Predicted_Qe = predicted_Qe[selected_instances]
)

# Melt the data to long format for ggplot
library(reshape2)
shapely_data_long <- melt(shapely_data, id.vars = "Instance")

# Plotting the contributions of each feature for these four instances
ggplot(shapely_data_long, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Instance) +
  labs(title = "Feature Contributions for Selected Instances", 
       x = "Feature", y = "Contribution Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



install.packages("iml")  # Install iml package if not already installed
library(iml)  # Load iml package

# For Stage 1 model (Ce prediction)
ce_predictor <- Predictor$new(ce_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate")], y = trainData$Ce)

# For Stage 2 model (Qe prediction)
qe_predictor <- Predictor$new(qe_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")], y = trainData$Qe)

# For Ce model (Stage 1)
ce_instance <- testData[1:4, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate")]
ce_shapley <- lapply(1:nrow(ce_instance), function(i) {
  Shapley$new(ce_predictor, x.interest = ce_instance[i, ])
})

# For Qe model (Stage 2)
qe_instance <- testData[1:4, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")]
qe_shapley <- lapply(1:nrow(qe_instance), function(i) {
  Shapley$new(qe_predictor, x.interest = qe_instance[i, ])
})


# Create Shapley plots for the first four instances (Ce and Qe)
ce_plots <- lapply(ce_shapley, function(shap) {
  shap$plot()
})

qe_plots <- lapply(qe_shapley, function(shap) {
  shap$plot()
})

# Display the Shapley plots side by side
grid.arrange(ce_plots[[1]], ce_plots[[2]], ce_plots[[3]], ce_plots[[4]], ncol = 2)  # For Stage 1

grid.arrange(qe_plots[[1]], qe_plots[[2]], qe_plots[[3]], qe_plots[[4]], ncol = 2)  # For Stage 2


# Import necessary libraries
library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)
library(iml)        # For SHAP
library(reshape2)

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

# Update the predicted_Ce values to 0.03 up to two decimals in the test data
testData$Predicted_Ce <- round(predicted_Ce, 2)  # Round to 2 decimals for consistency

# Remove the original Ce column and use only Predicted_Ce
trainData$Predicted_Ce <- predict(ce_model, trainData)  # Adding predicted Ce to training data as well
trainData <- trainData[, !(names(trainData) %in% "Ce")]
testData <- testData[, !(names(testData) %in% "Ce")]

# Stage 2: Train a model to predict Qe using Predicted_Ce and other variables
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

# Create Shapley values for Qe (Stage 2) predictions
# For Stage 1 model (Ce prediction)
ce_predictor <- Predictor$new(ce_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate")], y = trainData$Ce)

# For Stage 2 model (Qe prediction)
qe_predictor <- Predictor$new(qe_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")], y = trainData$Qe)

# For Stage 2 (Qe) - Adjust Predicted_Ce to 0.03 for all test instances for consistency in Shapley plots
testData$Predicted_Ce[testData$Predicted_Ce != 0.03] <- 0.03

# For Qe model (Stage 2) create Shapley values for the first 4 instances
qe_instance <- testData[1:4, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")]
qe_shapley <- lapply(1:nrow(qe_instance), function(i) {
  Shapley$new(qe_predictor, x.interest = qe_instance[i, ])
})

# Create Shapley plots for Qe (Stage 2) with the adjusted Predicted_Ce
qe_plots <- lapply(qe_shapley, function(shap) {
  shap$plot() + ggtitle(paste("Stage 2: Qe - Instance", shap$instance)) + theme_minimal()
})

# Display the Shapley plots side by side
grid.arrange(qe_plots[[1]], qe_plots[[2]], qe_plots[[3]], qe_plots[[4]], ncol = 2, 
             top = "Shapley Plots for Stage 2: Qe Prediction")


#black and white shapely plot. sirf shapely ban nai rha tha to pura model phle apply kiya
# Import necessary libraries
library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)
library(iml)        # For SHAP
library(reshape2)

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

# Update the predicted_Ce values to 0.03 up to two decimals in the test data
testData$Predicted_Ce <- round(predicted_Ce, 2)  # Round to 2 decimals for consistency

# Remove the original Ce column and use only Predicted_Ce
trainData$Predicted_Ce <- predict(ce_model, trainData)  # Adding predicted Ce to training data as well
trainData <- trainData[, !(names(trainData) %in% "Ce")]
testData <- testData[, !(names(testData) %in% "Ce")]

# Stage 2: Train a model to predict Qe using Predicted_Ce and other variables
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

# Create Shapley values for Qe (Stage 2) predictions
# For Stage 1 model (Ce prediction)
ce_predictor <- Predictor$new(ce_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate")], y = trainData$Ce)

# For Stage 2 model (Qe prediction)
qe_predictor <- Predictor$new(qe_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")], y = trainData$Qe)

# For Stage 2 (Qe) - Adjust Predicted_Ce to 0.03 for all test instances for consistency in Shapley plots
testData$Predicted_Ce[testData$Predicted_Ce != 0.03] <- 0.03

# For Qe model (Stage 2) create Shapley values for the first 4 instances
qe_instance <- testData[1:4, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")]
qe_shapley <- lapply(1:nrow(qe_instance), function(i) {
  Shapley$new(qe_predictor, x.interest = qe_instance[i, ])
})

# Create Shapley plots for Qe (Stage 2) with the adjusted Predicted_Ce
# Colorful theme with color scale for better visualization
qe_plots <- lapply(qe_shapley, function(shap) {
  shap$plot() + 
    ggtitle(paste("Stage 2: Qe - Instance", shap$instance)) + 
    theme_minimal() +
    scale_fill_viridis_c() +    # Apply a colorful fill scale (you can try other palettes too)
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
})

# Display the Shapley plots side by side with a colorful palette
grid.arrange(qe_plots[[1]], qe_plots[[2]], qe_plots[[3]], qe_plots[[4]], ncol = 2, 
             top = "Shapley Plots for Stage 2: Qe Prediction")



#colorful shapely plot
library(caret)
library(kernlab)   # For Gaussian Process
library(ggplot2)
library(Metrics)
library(gridExtra)
library(iml)        # For SHAP
library(reshape2)
library(RColorBrewer) # For a wider range of color palettes

# Create Shapley values for Qe (Stage 2) predictions
# For Stage 1 model (Ce prediction)
ce_predictor <- Predictor$new(ce_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate")], y = trainData$Ce)

# For Stage 2 model (Qe prediction)
qe_predictor <- Predictor$new(qe_model, data = trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")], y = trainData$Qe)

# For Stage 2 (Qe) - Adjust Predicted_Ce to 0.03 for all test instances for consistency in Shapley plots
testData$Predicted_Ce[testData$Predicted_Ce != 0.03] <- 0.03

# For Qe model (Stage 2) create Shapley values for the first 4 instances
qe_instance <- testData[1:4, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")]
qe_shapley <- lapply(1:nrow(qe_instance), function(i) {
  Shapley$new(qe_predictor, x.interest = qe_instance[i, ])
})

# Create Shapley plots for Qe (Stage 2) with the adjusted Predicted_Ce
# Colorful theme with color scale for better visualization
qe_plots <- lapply(qe_shapley, function(shap) {
  ggplot(shap$results, aes(x = feature, y = phi, fill = phi)) +   # Use 'phi' for Shapley values
    geom_bar(stat = "identity") +
    ggtitle(paste("Stage 2: Qe - Instance", shap$instance)) + 
    theme_minimal() +
    scale_fill_viridis_c(option = "D") +  # Color scale using 'viridis'
    theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "darkblue", size = 12),  # Rotate x-axis labels and color
          axis.text.y = element_text(color = "darkred", size = 12),   # Change y-axis label color
          axis.title.x = element_text(color = "black", size = 14),      # Change x-axis title color
          axis.title.y = element_text(color = "red", size = 14),       # Change y-axis title color
          plot.title = element_text(hjust = 0.5, size = 16, color = "darkgreen"))  # Center and color the title
})

# Display the Shapley plots side by side with the colorful palette
grid.arrange(qe_plots[[1]], qe_plots[[2]], qe_plots[[3]], qe_plots[[4]], ncol = 2, 
             top = "Shapley Plots for Stage 2: Qe Prediction")



#without phi values written on bar and exes changed
# Create Shapley plots for Qe (Stage 2) with the adjusted Predicted_Ce
# Swap axes: 'phi' on the x-axis and 'feature' on the y-axis
qe_plots <- lapply(qe_shapley, function(shap) {
  ggplot(shap$results, aes(x = phi, y = feature, fill = phi)) +   # Swap axes, use 'phi' on x-axis and 'feature' on y-axis
    geom_bar(stat = "identity") +
    ggtitle(paste("Stage 2: Qe - Instance", shap$instance)) + 
    theme_minimal() +
    scale_fill_viridis_c(option = "D") +  # Color scale using 'viridis'
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "darkblue", size = 10),  # Rotate x-axis labels and color
      axis.text.y = element_text(color = "darkred", size = 10),   # Change y-axis label color
      axis.title.x = element_text(color = "black", size = 14),      # Change x-axis title color
      axis.title.y = element_text(color = "red", size = 14),       # Change y-axis title color
      plot.title = element_text(hjust = 0.5, size = 16, color = "darkgreen")  # Center and color the title
    )
})

# Display the Shapley plots side by side with the colorful palette
grid.arrange(qe_plots[[1]], qe_plots[[2]], qe_plots[[3]], qe_plots[[4]], ncol = 2, 
             top = "Shapley Plots for Stage 2: Qe Prediction")


# Extract the phi values for each instance and feature
phi_values <- lapply(qe_shapley, function(shap) {
  shap$results[, c("feature", "phi")]  # Extract feature and corresponding phi values
})

# Print the Shapley values for the first instance
print(phi_values[[1]])

# If you want to view the Shapley values for all instances in the plot
for(i in 1:length(phi_values)) {
  cat("Shapley values for Instance", i, ":\n")
  print(phi_values[[i]])
  cat("\n")
}


#with phi values written on bars
# Create Shapley plots for Qe (Stage 2) with the adjusted Predicted_Ce
# Swap axes: 'phi' on the x-axis and 'feature' on the y-axis
qe_plots <- lapply(qe_shapley, function(shap) {
  ggplot(shap$results, aes(x = phi, y = feature, fill = phi)) +   # Swap axes, use 'phi' on x-axis and 'feature' on y-axis
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(phi, 3)),  # Add Shapley values as text on bars (round to 3 decimal places)
              position = position_stack(vjust = 0.5), size = 3, color = "white") +  # Position the text inside the bars
    ggtitle(paste("Stage 2: Qe - Instance", shap$instance)) + 
    theme_minimal() +
    scale_fill_viridis_c(option = "D") +  # Color scale using 'viridis'
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "darkblue", size = 10),  # Rotate x-axis labels and color
      axis.text.y = element_text(color = "darkred", size = 10),   # Change y-axis label color
      axis.title.x = element_text(color = "black", size = 14),      # Change x-axis title color
      axis.title.y = element_text(color = "red", size = 14),       # Change y-axis title color
      plot.title = element_text(hjust = 0.5, size = 16, color = "darkgreen")  # Center and color the title
    )
})

# Display the Shapley plots side by side with the colorful palette
grid.arrange(qe_plots[[1]], qe_plots[[2]], qe_plots[[3]], qe_plots[[4]], ncol = 2, 
             top = "Shapley Plots for Stage 2: Qe Prediction")


#for capitalization of Feature
# Create Shapley plots for Qe (Stage 2) with the adjusted Predicted_Ce
# Swap axes: 'phi' on the x-axis and 'feature' on the y-axis
qe_plots <- lapply(qe_shapley, function(shap) {
  ggplot(shap$results, aes(x = phi, y = feature, fill = phi)) +   # Swap axes, use 'phi' on x-axis and 'feature' on y-axis
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(phi, 3)),  # Add Shapley values as text on bars (round to 3 decimal places)
              position = position_stack(vjust = 0.5), size = 3, color = "white") +  # Position the text inside the bars
    ggtitle(paste("Stage 2: Qe - Instance", shap$instance)) + 
    theme_minimal() +
    scale_fill_viridis_c(option = "D") +  # Color scale using 'viridis'
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "darkblue", size = 10),  # Rotate x-axis labels and color
      axis.text.y = element_text(color = "darkred", size = 10),   # Change y-axis label color
      axis.title.x = element_text(color = "black", size = 14),      # Change x-axis title color
      axis.title.y = element_text(color = "red", size = 14),       # Change y-axis title color
      plot.title = element_text(hjust = 0.5, size = 16, color = "darkgreen")  # Center and color the title
    ) +
    ylab("Feature")  # Change the y-axis label to "Feature"
})

# Display the Shapley plots side by side with the colorful palette
grid.arrange(qe_plots[[1]], qe_plots[[2]], qe_plots[[3]], qe_plots[[4]], ncol = 2, 
             top = "Shapley Plots for Stage 2: Qe Prediction")




# Load necessary libraries
library(iml)
library(ggplot2)
library(gridExtra)

# Define the list of features for ICE plots
features_to_plot <- c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")

# Remove any constant features from the ICE plot
constant_features <- sapply(trainData[, features_to_plot], function(x) length(unique(x)) == 1)
variable_features <- names(constant_features[!constant_features])  # Exclude constant features

# Print any constant features to know which ones are excluded
if (any(constant_features)) {
  cat("Excluding constant features from ICE:", names(constant_features[constant_features]), "\n")
}

# Generate ICE plots for each variable feature
ice_plots <- lapply(variable_features, function(feature) {
  ice <- FeatureEffect$new(qe_predictor, feature = feature, method = "ice")
  
  # Plot ICE with customization
  ice_plot <- ice$plot() + 
    ggtitle(paste("Individual Conditional Expectation for", feature, "on Qe")) +
    xlab(feature) +
    ylab("Predicted Qe") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, color = "darkgreen", size = 10),
      axis.text.x = element_text(color = "darkblue", size = 10),
      axis.text.y = element_text(color = "darkred", size = 10),
      axis.title.x = element_text(color = "black", size = 14),
      axis.title.y = element_text(color = "red", size = 14)
    )
  
  ice_plot
})

# Display the ICE plots side by side
do.call(grid.arrange, c(ice_plots, ncol = 2, top = "Individual Conditional Expectation (ICE) Plots for Qe Prediction"))



#ALE PLOT
# Check for constant features in the training data
constant_features <- sapply(trainData[, c("Co", "V", "Time", "Dye.amount", "Mass", "Flow.rate", "Predicted_Ce")], function(x) length(unique(x)) == 1)
variable_features <- names(constant_features[!constant_features])  # Exclude constant features

# Print any constant features to know which ones are excluded
if (any(constant_features)) {
  cat("Excluding constant features from ALE plots:", names(constant_features[constant_features]), "\n")
}

# Create ALE plots for each variable feature in Stage 2 Qe model
ale_plots <- lapply(variable_features, function(feature) {
  ale <- FeatureEffect$new(qe_predictor, feature = feature, method = "ale")  # Create ALE effect for each feature
  
  # Plot with customization
  ale_plot <- ale$plot() + 
    ggtitle(paste("ALE of", feature, "on Qe")) +
    xlab(feature) +
    ylab("ALE (Accumulated Local Effect)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, color = "darkgreen", size = 14),
      axis.text.x = element_text(color = "darkblue", size = 10),
      axis.text.y = element_text(color = "darkred", size = 10),
      axis.title.x = element_text(color = "black", size = 14),
      axis.title.y = element_text(color = "red", size = 14)
    )
  
  ale_plot
})

# Display the ALE Plots side by side
do.call(grid.arrange, c(ale_plots, ncol = 2, top = "Accumulated Local Effects (ALE) Plots for Qe Prediction"))



