EXTERNAL VALIDATION EXCLUDING MISSING VALUES

# Load required libraries
library(glmnet)
library(caret)
library(pROC)

#---------------------------------------------
# STEP 1: Set up variables and datasets
#---------------------------------------------

# Define predictors
predictor_vars <- c("Age", "Gender", "SympMon", "Tumorsite", "LDH", "VAF", "ctDNA",
                    "i1mut", "e2mut", "EBER1", "EBER2", "EBNA2", "EBVmax", 
                    "EBVSR", "EBVP", "EBVent", "autoent", "Translocation")

# Create training and external datasets with predictors + outcome
train_data <- table3dataset[, c(predictor_vars, "Diagnosis")]
ext_data   <- external_data[, c(predictor_vars, "Diagnosis")]

# Remove rows with missing values
train_data <- train_data[complete.cases(train_data), ]
ext_data   <- ext_data[complete.cases(ext_data), ]

# Combine predictor data for consistent dummy variable encoding
combined_predictors <- rbind(train_data[, predictor_vars],
                             ext_data[, predictor_vars])

#---------------------------------------------
# STEP 2: Create aligned model matrix
#---------------------------------------------

# Use model.matrix to convert to numeric (dummy encode factors)
full_model_matrix <- model.matrix(~ ., data = combined_predictors)[, -1]

# Split back into aligned training and external matrices
x_train_aligned <- full_model_matrix[1:nrow(train_data), ]
x_ext_aligned   <- full_model_matrix[(nrow(train_data) + 1):nrow(full_model_matrix), ]

# Create factor outcomes
y_train <- factor(train_data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))
y_ext   <- factor(ext_data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

#---------------------------------------------
# STEP 3: Train LASSO model on aligned training data
#---------------------------------------------

set.seed(123)
cv_lasso_aligned <- cv.glmnet(x_train_aligned, y_train,
                              family = "binomial",
                              alpha = 1,
                              type.measure = "class")

# View best lambda
best_lambda <- cv_lasso_aligned$lambda.min
print(paste("Best lambda:", best_lambda))

#---------------------------------------------
# STEP 4: External validation
#---------------------------------------------

# Predict probabilities on external data
predicted_probs_ext <- predict(cv_lasso_aligned, newx = x_ext_aligned, s = "lambda.min", type = "response")

# Convert probabilities to predicted class
predicted_classes_ext <- ifelse(predicted_probs_ext > 0.5, "BL", "Non_BL")
predicted_classes_ext <- factor(predicted_classes_ext, levels = c("Non_BL", "BL"))

# Confusion matrix
conf_matrix_ext <- confusionMatrix(predicted_classes_ext, y_ext, positive = "BL")

# Print metrics
sensitivity_ext <- conf_matrix_ext$byClass["Sensitivity"]
specificity_ext <- conf_matrix_ext$byClass["Specificity"]
accuracy_ext    <- conf_matrix_ext$overall["Accuracy"]

print(conf_matrix_ext)
print(paste("External Sensitivity:", round(sensitivity_ext, 2)))
print(paste("External Specificity:", round(specificity_ext, 2)))
print(paste("External Accuracy:", round(accuracy_ext, 2)))

# ROC and AUC with 95% CI via bootstrap
library(pROC)

# Compute AUC
roc_ext <- roc(y_ext, as.numeric(predicted_probs_ext))
auc_ext <- auc(roc_ext)

# Bootstrap AUC CI (DeLong method also available via pROC)
ci_auc <- ci.auc(roc_ext, conf.level = 0.95)
ci_auc_low <- round(ci_auc[1], 3)
ci_auc_mid <- round(ci_auc[2], 3)
ci_auc_high <- round(ci_auc[3], 3)

# Print AUC and CI
print(paste("External AUC:", ci_auc_mid))
print(paste("95% CI:", ci_auc_low, "-", ci_auc_high))

# Plot ROC with AUC and 95% CI in label
plot(roc_ext, main = "ROC Curve - External Validation (n=44)", col = "darkgreen", lwd = 2)
text(x = 0.6, y = 0.2, 
     labels = paste0("AUC = ", ci_auc_mid, 
                     " [", ci_auc_low, ", ", ci_auc_high, "]"), 
     col = "darkgreen", cex = 1.2)

# Create confusion matrix plot
library(ggplot2)
library(reshape2)

# Convert confusion matrix to data frame
cm_df <- as.data.frame(conf_matrix_ext$table)

# Plot with ggplot2
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix - External Validation (n=44)",
       x = "Actual Diagnosis", y = "Predicted Diagnosis") +
  theme_minimal(base_size = 14)



#EXTERNAL VALIDATION WITH IMPUTATION OF MISSING VALUES

Load required libraries
library(mice)
library(glmnet)
library(caret)
library(pROC)
library(ggplot2)
library(reshape2)

#------------------------------------------------
# STEP 1: Define predictor variables
#------------------------------------------------
predictor_vars <- c("Age", "Gender", "SympMon", "Tumorsite", "LDH", "VAF", "ctDNA",
                    "i1mut", "e2mut", "EBER1", "EBER2", "EBNA2", "EBVmax", 
                    "EBVSR", "EBVP", "EBVent", "autoent", "Translocation")

#------------------------------------------------
# STEP 2: Prepare training and external datasets
#------------------------------------------------
train_data <- table3dataset[, c(predictor_vars, "Diagnosis")]
ext_data   <- external_data[, c(predictor_vars, "Diagnosis")]

# Combine training and external data for imputation
combined_data <- rbind(train_data, ext_data)

#------------------------------------------------
# STEP 3: Impute missing values using mice
#------------------------------------------------
# Use predictive mean matching for numeric variables (you can adjust method if needed)
imputed <- mice(combined_data, m = 1, method = "pmm", seed = 123)

# Extract completed dataset
combined_imputed <- complete(imputed, 1)

#------------------------------------------------
# STEP 4: Split back into training and external sets
#------------------------------------------------
train_data_imputed <- combined_imputed[1:nrow(train_data), ]
ext_data_imputed   <- combined_imputed[(nrow(train_data) + 1):nrow(combined_data), ]

#------------------------------------------------
# STEP 5: Create aligned design matrix using model.matrix
#------------------------------------------------
combined_predictors <- rbind(train_data_imputed[, predictor_vars],
                             ext_data_imputed[, predictor_vars])

full_model_matrix <- model.matrix(~ ., data = combined_predictors)[, -1]

# Split matrices
x_train_aligned <- full_model_matrix[1:nrow(train_data_imputed), ]
x_ext_aligned   <- full_model_matrix[(nrow(train_data_imputed) + 1):nrow(full_model_matrix), ]

# Create outcome variables
y_train <- factor(train_data_imputed$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))
y_ext   <- factor(ext_data_imputed$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

#------------------------------------------------
# STEP 6: Train LASSO model using 10-fold cross-validation
#------------------------------------------------
set.seed(123)
cv_lasso_aligned <- cv.glmnet(x_train_aligned, y_train,
                              family = "binomial",
                              alpha = 1,
                              type.measure = "class")

best_lambda <- cv_lasso_aligned$lambda.min
print(paste("Best lambda:", best_lambda))

#------------------------------------------------
# STEP 7: Predict and evaluate on external data
#------------------------------------------------
# Predict probabilities
predicted_probs_ext <- predict(cv_lasso_aligned, newx = x_ext_aligned, s = "lambda.min", type = "response")

# Convert to binary classes
predicted_classes_ext <- ifelse(predicted_probs_ext > 0.5, "BL", "Non_BL")
predicted_classes_ext <- factor(predicted_classes_ext, levels = c("Non_BL", "BL"))

# Confusion matrix
conf_matrix_ext <- confusionMatrix(predicted_classes_ext, y_ext, positive = "BL")

# Extract performance metrics
sensitivity_ext <- conf_matrix_ext$byClass["Sensitivity"]
specificity_ext <- conf_matrix_ext$byClass["Specificity"]
accuracy_ext    <- conf_matrix_ext$overall["Accuracy"]

# ROC and AUC with 95% CI via bootstrap
library(pROC)

# Compute AUC
roc_ext <- roc(y_ext, as.numeric(predicted_probs_ext))
auc_ext <- auc(roc_ext)

# Bootstrap AUC CI (DeLong method also available via pROC)
ci_auc <- ci.auc(roc_ext, conf.level = 0.95)
ci_auc_low <- round(ci_auc[1], 3)
ci_auc_mid <- round(ci_auc[2], 3)
ci_auc_high <- round(ci_auc[3], 3)

# Print AUC and CI
print(paste("External AUC:", ci_auc_mid))
print(paste("95% CI:", ci_auc_low, "-", ci_auc_high))

# Plot ROC with AUC and 95% CI in label
plot(roc_ext, main = "ROC Curve - External Validation (n=56)", col = "darkgreen", lwd = 2)
text(x = 0.6, y = 0.2, 
     labels = paste0("AUC = ", ci_auc_mid, 
                     " [", ci_auc_low, ", ", ci_auc_high, "]"), 
     col = "darkgreen", cex = 1.2)

# Create confusion matrix plot
library(ggplot2)
library(reshape2)

# Convert confusion matrix to data frame
cm_df <- as.data.frame(conf_matrix_ext$table)

# Plot with ggplot2
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix - External Validation (n=44)",
       x = "Actual Diagnosis", y = "Predicted Diagnosis") +
  theme_minimal(base_size = 14)
