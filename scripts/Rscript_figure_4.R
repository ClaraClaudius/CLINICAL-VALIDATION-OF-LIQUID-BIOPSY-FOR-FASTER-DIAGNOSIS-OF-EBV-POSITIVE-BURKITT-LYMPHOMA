#COMPREHENSIVE MODEL
# Load required libraries
library(mice)
library(glmnet)
library(pROC)
library(caret)
library(flextable)
library(officer)
library(dplyr)

# Step 1: Prepare data for imputation
impute_data <- table3dataset[, c("Age", "Gender", "SympMon", "Tumorsite", "LDH", "VAF", "ctDNA", "i1mut",
                                 "e2mut", "EBER1", "EBER2", "EBNA2", "EBVmax", "EBVSR", "EBVP", "EBVent",
                                 "autoent", "Translocation", "Diagnosis")]

# Convert character variables to factors if any
impute_data <- impute_data %>%
  mutate(across(where(is.character), as.factor))

# Run multiple imputation (m = 5)
set.seed(123)
imputed <- mice(impute_data, m = 5, method = "pmm", seed = 123)

# Use the first imputed dataset for modeling
imputed_data <- complete(imputed, 1)

# Step 2: Prepare predictor matrix and outcome
x <- model.matrix(Diagnosis ~ ., data = imputed_data)[, -1]  # Remove intercept
y <- factor(imputed_data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

# Step 3: LASSO with cross-validation
set.seed(123)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
print(paste("Optimal Lambda: ", best_lambda))

# View selected coefficients
print(coef(cv_lasso, s = "lambda.min"))

# Step 4: Predict probabilities and classes
predicted_probs <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "BL", "Non_BL")
predicted_classes <- factor(predicted_classes, levels = c("Non_BL", "BL"))

# Step 5: Evaluate performance
conf_matrix <- confusionMatrix(predicted_classes, y, positive = "BL")
sensitivity_value <- conf_matrix$byClass["Sensitivity"]
specificity_value <- conf_matrix$byClass["Specificity"]
accuracy_value <- conf_matrix$overall["Accuracy"]
roc_curve <- roc(y, as.numeric(predicted_probs))
auc_value <- auc(roc_curve)

# Print metrics
print(paste("Sensitivity:", round(sensitivity_value, 3)))
print(paste("Specificity:", round(specificity_value, 3)))
print(paste("AUC:", round(auc_value, 3)))


#CLINICAL MODEL
# Load required libraries
library(mice)
library(glmnet)
library(pROC)
library(caret)
library(flextable)
library(officer)
library(dplyr)

# Step 1: Prepare data for imputation
impute_data <- table3dataset[, c("Age", "Gender", "SympMon", "Tumorsite", "LDH", "Diagnosis")]

# Convert character variables to factors if any
impute_data <- impute_data %>%
  mutate(across(where(is.character), as.factor))

# Run multiple imputation (m = 5)
set.seed(123)
imputed <- mice(impute_data, m = 5, method = "pmm", seed = 123)

# Use the first imputed dataset for modeling
imputed_data <- complete(imputed, 1)

# Step 2: Prepare predictor matrix and outcome
x <- model.matrix(Diagnosis ~ ., data = imputed_data)[, -1]  # Remove intercept
y <- factor(imputed_data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

# Step 3: LASSO with cross-validation
set.seed(123)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
print(paste("Optimal Lambda: ", best_lambda))

# View selected coefficients
print(coef(cv_lasso, s = "lambda.min"))

# Step 4: Predict probabilities and classes
predicted_probs <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "BL", "Non_BL")
predicted_classes <- factor(predicted_classes, levels = c("Non_BL", "BL"))

# Step 5: Evaluate performance
conf_matrix <- confusionMatrix(predicted_classes, y, positive = "BL")
sensitivity_value <- conf_matrix$byClass["Sensitivity"]
specificity_value <- conf_matrix$byClass["Specificity"]
accuracy_value <- conf_matrix$overall["Accuracy"]
roc_curve <- roc(y, as.numeric(predicted_probs))
auc_value <- auc(roc_curve)

# Print metrics
print(paste("Sensitivity:", round(sensitivity_value, 3)))
print(paste("Specificity:", round(specificity_value, 3)))
print(paste("AUC:", round(auc_value, 3)))



#EBV QUANTITAIVE MODEL
# Load required libraries
library(mice)
library(glmnet)
library(pROC)
library(caret)
library(flextable)
library(officer)
library(dplyr)

# Step 1: Prepare data for imputation
impute_data <- table3dataset[, c("EBER1", "EBER2", "EBNA2", "EBVmax", "Diagnosis")]

# Convert character variables to factors if any
impute_data <- impute_data %>%
  mutate(across(where(is.character), as.factor))

# Run multiple imputation (m = 5)
set.seed(123)
imputed <- mice(impute_data, m = 5, method = "pmm", seed = 123)

# Use the first imputed dataset for modeling
imputed_data <- complete(imputed, 1)

# Step 2: Prepare predictor matrix and outcome
x <- model.matrix(Diagnosis ~ ., data = imputed_data)[, -1]  # Remove intercept
y <- factor(imputed_data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

# Step 3: LASSO with cross-validation
set.seed(123)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
print(paste("Optimal Lambda: ", best_lambda))

# View selected coefficients
print(coef(cv_lasso, s = "lambda.min"))

# Step 4: Predict probabilities and classes
predicted_probs <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "BL", "Non_BL")
predicted_classes <- factor(predicted_classes, levels = c("Non_BL", "BL"))

# Step 5: Evaluate performance
conf_matrix <- confusionMatrix(predicted_classes, y, positive = "BL")
sensitivity_value <- conf_matrix$byClass["Sensitivity"]
specificity_value <- conf_matrix$byClass["Specificity"]
accuracy_value <- conf_matrix$overall["Accuracy"]
roc_curve <- roc(y, as.numeric(predicted_probs))
auc_value <- auc(roc_curve)

# Print metrics
print(paste("Sensitivity:", round(sensitivity_value, 3)))
print(paste("Specificity:", round(specificity_value, 3)))
print(paste("AUC:", round(auc_value, 3)))



#EBV MODEL
# Load required libraries
library(mice)
library(glmnet)
library(pROC)
library(caret)
library(flextable)
library(officer)
library(dplyr)

# Step 1: Prepare data for imputation
impute_data <- table3dataset[, c("EBER1", "EBER2", "EBNA2", "EBVmax", "EBVSR", "EBVP", "EBVent", "Diagnosis")]

# Convert character variables to factors if any
impute_data <- impute_data %>%
  mutate(across(where(is.character), as.factor))

# Run multiple imputation (m = 5)
set.seed(123)
imputed <- mice(impute_data, m = 5, method = "pmm", seed = 123)

# Use the first imputed dataset for modeling
imputed_data <- complete(imputed, 1)

# Step 2: Prepare predictor matrix and outcome
x <- model.matrix(Diagnosis ~ ., data = imputed_data)[, -1]  # Remove intercept
y <- factor(imputed_data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

# Step 3: LASSO with cross-validation
set.seed(123)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
print(paste("Optimal Lambda: ", best_lambda))

# View selected coefficients
print(coef(cv_lasso, s = "lambda.min"))

# Step 4: Predict probabilities and classes
predicted_probs <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "BL", "Non_BL")
predicted_classes <- factor(predicted_classes, levels = c("Non_BL", "BL"))

# Step 5: Evaluate performance
conf_matrix <- confusionMatrix(predicted_classes, y, positive = "BL")
sensitivity_value <- conf_matrix$byClass["Sensitivity"]
specificity_value <- conf_matrix$byClass["Specificity"]
accuracy_value <- conf_matrix$overall["Accuracy"]
roc_curve <- roc(y, as.numeric(predicted_probs))
auc_value <- auc(roc_curve)

# Print metrics
print(paste("Sensitivity:", round(sensitivity_value, 3)))
print(paste("Specificity:", round(specificity_value, 3)))
print(paste("AUC:", round(auc_value, 3)))



#LIQUID BIOPSY MODEL
# Load required libraries
library(mice)
library(glmnet)
library(pROC)
library(caret)
library(flextable)
library(officer)
library(dplyr)

# Step 1: Prepare data for imputation
impute_data <- table3dataset[, c("VAF", "ctDNA", "i1mut",
                                 "e2mut", "EBER1", "EBER2", "EBNA2", "EBVmax", "EBVSR", "EBVP", "EBVent",
                                 "autoent", "Translocation", "Diagnosis")]

# Convert character variables to factors if any
impute_data <- impute_data %>%
  mutate(across(where(is.character), as.factor))

# Run multiple imputation (m = 5)
set.seed(123)
imputed <- mice(impute_data, m = 5, method = "pmm", seed = 123)

# Use the first imputed dataset for modeling
imputed_data <- complete(imputed, 1)

# Step 2: Prepare predictor matrix and outcome
x <- model.matrix(Diagnosis ~ ., data = imputed_data)[, -1]  # Remove intercept
y <- factor(imputed_data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

# Step 3: LASSO with cross-validation
set.seed(123)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
print(paste("Optimal Lambda: ", best_lambda))

# View selected coefficients
print(coef(cv_lasso, s = "lambda.min"))

# Step 4: Predict probabilities and classes
predicted_probs <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "BL", "Non_BL")
predicted_classes <- factor(predicted_classes, levels = c("Non_BL", "BL"))

# Step 5: Evaluate performance
conf_matrix <- confusionMatrix(predicted_classes, y, positive = "BL")
sensitivity_value <- conf_matrix$byClass["Sensitivity"]
specificity_value <- conf_matrix$byClass["Specificity"]
accuracy_value <- conf_matrix$overall["Accuracy"]
roc_curve <- roc(y, as.numeric(predicted_probs))
auc_value <- auc(roc_curve)

# Print metrics
print(paste("Sensitivity:", round(sensitivity_value, 3)))
print(paste("Specificity:", round(specificity_value, 3)))
print(paste("AUC:", round(auc_value, 3)))




#ROC CURVE FOR ALL FIVE MODELS WITH THEIR AUC

## Load required libraries
library(pROC)
library(ggplot2)
library(dplyr)

# Recode outcome variable
table3dataset$Diagnosis <- factor(table3dataset$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))

# Imputation settings
library(mice)
get_imputed_data <- function(vars) {
  data <- table3dataset[, c(vars, "Diagnosis")]
  data <- mice(data, m = 5, method = "pmm", seed = 123)
  complete(data, 1)
}

# Function to build model and extract ROC
get_model_roc <- function(data, model_name) {
  x <- model.matrix(Diagnosis ~ ., data = data)[, -1]
  y <- data$Diagnosis
  cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")
  probs <- predict(cv_fit, newx = x, s = "lambda.min", type = "response")
  roc_obj <- roc(y, as.numeric(probs))
  return(data.frame(
    specificity = rev(roc_obj$specificities),
    sensitivity = rev(roc_obj$sensitivities),
    model = paste0(ifelse(model_name == "Complete", "Comprehensive", model_name), 
                   " (AUC: ", round(auc(roc_obj), 2), ")")
  ))
}

# Define models and predictors
model_vars <- list(
  Clinical = c("Age", "Gender", "SympMon", "Tumorsite", "LDH"),
  Comprehensive = c("Age", "Gender", "SympMon", "Tumorsite", "LDH", "VAF", "ctDNA", "i1mut", "e2mut",
                    "EBER1", "EBER2", "EBNA2", "EBVmax", "EBVSR", "EBVP", "EBVent", "autoent", "Translocation"),
  EBV_Quantitative = c("EBER1", "EBER2", "EBNA2", "EBVmax"),
  EBV = c("EBER1", "EBER2", "EBNA2", "EBVmax", "EBVSR", "EBVP", "EBVent"),
  Liquid_Biopsy = c("VAF", "ctDNA", "i1mut", "e2mut", "EBER1", "EBER2", "EBNA2", "EBVmax",
                    "EBVSR", "EBVP", "EBVent", "autoent", "Translocation")
)

# Loop over models
roc_df <- bind_rows(lapply(names(model_vars), function(name) {
  data <- get_imputed_data(model_vars[[name]])
  get_model_roc(data, name)
}))

# Plot
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    title = "ROC Curves for Different Models",
    x = "1 - Specificity",
    y = "Sensitivity",
    color = "Models"
  ) +
  theme(legend.position = "right")

#IMPORTANCE CHARTS BASED ON COEFFICIENTS
coef_df <- as.data.frame(as.matrix(coef(cv_lasso, s = "lambda.min")))
coef_df$Variable <- rownames(coef_df)
colnames(coef_df)[1] <- "Coefficient"

# Filter out zero coefficients and intercept
coef_df_filtered <- coef_df[coef_df$Coefficient != 0 & coef_df$Variable != "(Intercept)", ]

# Add a column for absolute value of the coefficients
coef_df_filtered$AbsCoefficient <- abs(coef_df_filtered$Coefficient)

# Reorder variables by absolute value (most important on top)
coef_df_filtered$Variable <- factor(coef_df_filtered$Variable,
                                    levels = coef_df_filtered$Variable[order(coef_df_filtered$AbsCoefficient, decreasing = TRUE)])

# Plot with reordered variables using absolute values
ggplot(coef_df_filtered, aes(x = Variable, y = AbsCoefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most Important Predictors from LASSO Model",
    x = "Predictor",
    y = "Absolute Coefficient"
  )


#DELONG TEST FOR MODEL ROC CURVES

# Required libraries
library(pROC)
library(dplyr)
library(mice)
library(tibble)

# Your model definitions (already defined)
model_vars <- list(
  Clinical = c("Age", "Gender", "SympMon", "Tumorsite", "LDH"),
  Comprehensive = c("Age", "Gender", "SympMon", "Tumorsite", "LDH", "VAF", "ctDNA", "i1mut", "e2mut",
                    "EBER1", "EBER2", "EBNA2", "EBVmax", "EBVSR", "EBVP", "EBVent", "autoent", "Translocation"),
  EBV_Quantitative = c("EBER1", "EBER2", "EBNA2", "EBVmax"),
  EBV = c("EBER1", "EBER2", "EBNA2", "EBVmax", "EBVSR", "EBVP", "EBVent"),
  Liquid_Biopsy = c("VAF", "ctDNA", "i1mut", "e2mut", "EBER1", "EBER2", "EBNA2", "EBVmax",
                    "EBVSR", "EBVP", "EBVent", "autoent", "Translocation")
)

# Helper function: get imputed data
get_imputed_data <- function(vars) {
  data <- table3dataset[, c(vars, "Diagnosis")]
  data <- mice(data, m = 5, method = "pmm", seed = 123)
  complete(data, 1)
}

# Helper function: fit model and return ROC
get_model_roc <- function(data) {
  x <- model.matrix(Diagnosis ~ ., data = data)[, -1]
  y <- factor(data$Diagnosis, levels = c(0, 1), labels = c("Non_BL", "BL"))
  cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")
  probs <- predict(cv_fit, newx = x, s = "lambda.min", type = "response")
  roc(y, as.numeric(probs), ci = TRUE)
}

# Loop over models and store ROC objects
roc_list <- list()
for (model_name in names(model_vars)) {
  dat <- get_imputed_data(model_vars[[model_name]])
  roc_list[[model_name]] <- get_model_roc(dat)
}

# Get AUC + 95% CI for all models
auc_table <- lapply(names(roc_list), function(name) {
  auc_ci <- ci.auc(roc_list[[name]])
  data.frame(
    Model = name,
    AUC = as.numeric(auc_ci[2]),
    CI_Lower = as.numeric(auc_ci[1]),
    CI_Upper = as.numeric(auc_ci[3])
  )
}) %>% bind_rows()

# DeLong test vs Clinical model
ref_model <- "Clinical"
delong_results <- lapply(names(roc_list), function(name) {
  if (name != ref_model) {
    test <- roc.test(roc_list[[ref_model]], roc_list[[name]], method = "delong")
    data.frame(
      Model = name,
      p_value = test$p.value
    )
  } else {
    data.frame(Model = name, p_value = NA)
  }
}) %>% bind_rows()

# Combine AUC and DeLong results
final_table <- left_join(auc_table, delong_results, by = "Model") %>%
  mutate(
    AUC_CI = paste0(round(AUC, 3), " [", round(CI_Lower, 3), "–", round(CI_Upper, 3), "]"),
    p_value = ifelse(is.na(p_value), "-", formatC(p_value, format = "e", digits = 2))
  ) %>%
  select(Model, AUC_CI, p_value)

# Display table
print(final_table)

# Load required libraries
library(flextable)
library(officer)

# Create flextable
ft <- flextable(final_table) %>%
  autofit() %>%
  set_caption("AUCs with 95% Confidence Intervals and DeLong Test P-values Compared to Clinical Model") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  padding(padding = 6, part = "all")

# Save to Word
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal") # optional spacing

print(doc, target = "AUC_DeLong_Comparison_Table.docx")
