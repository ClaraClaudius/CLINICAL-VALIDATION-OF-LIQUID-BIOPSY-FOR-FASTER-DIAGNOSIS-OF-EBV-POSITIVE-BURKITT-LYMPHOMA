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
impute_data <- table3dataset[, c("Age", "Gender", "SympMon", "Tumorsite", "LDH","i1mut",
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


###############################################
# Liquid Biopsy Model: False-Negative Analysis
# End-to-end script (train model, evaluate, FN/TP tables, export to Word)
###############################################

## 0) Packages ---------------------------------------------------------------
# List of required packages for your pipeline
pkgs <- c(
  "mice", "glmnet", "pROC", "caret", "flextable", "officer",
  "dplyr", "gtsummary", "glue", "stringr", "cardx"
)

# Install any that are missing
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

# Load them all
invisible(lapply(pkgs, library, character.only = TRUE))


pkgs <- c("mice","glmnet","pROC","caret","flextable","officer",
          "dplyr","gtsummary","glue","stringr")
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

## 1) Output location (EDIT THIS PATH) --------------------------------------
# Use forward slashes on Windows.
OUT_DIR <- "C:/Users/hp/Documents/PhD/PhD/Overarching manuscript/Manuscript2/Data"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_FILE <- file.path(OUT_DIR, "LB_false_negative_analysis.docx")

## 2) Check data is available -----------------------------------------------
stopifnot(exists("table3dataset"))
stopifnot("Diagnosis" %in% names(table3dataset))

## 3) Select variables & prep for imputation --------------------------------
impute_data <- table3dataset[, c("Age","Gender","SympMon","Tumorsite","LDH","i1mut",
                                 "e2mut","EBER1","EBER2","EBNA2","EBVmax","EBVSR","EBVP","EBVent",
                                 "autoent","Translocation","Diagnosis")]

# Coerce character -> factor; keep Diagnosis numeric 0/1
impute_data <- impute_data |>
  dplyr::mutate(dplyr::across(where(is.character), as.factor))

if (is.factor(impute_data$Diagnosis)) {
  # Convert factor to 0/1 if needed (expects "Non_BL"/"BL" or similar)
  impute_data$Diagnosis <- as.integer(impute_data$Diagnosis %in% c("BL","1","Yes","Positive"))
}

## 4) Multiple imputation (m=5) ---------------------------------------------
set.seed(123)
# Let mice choose sensible defaults per variable type
imp <- mice::mice(impute_data, m = 5, seed = 123, printFlag = FALSE)
data_imp <- mice::complete(imp, 1)

## 5) Modeling with LASSO (glmnet) ------------------------------------------
# Model matrix (no intercept)
x <- model.matrix(Diagnosis ~ ., data = data_imp)[, -1]
# Gold standard as factor for evaluation
y_fac <- factor(ifelse(data_imp$Diagnosis == 1, "BL","Non_BL"), levels = c("Non_BL","BL"))

set.seed(123)
cvfit <- glmnet::cv.glmnet(x, y_fac, family = "binomial", alpha = 1, type.measure = "class")
best_lambda <- cvfit$lambda.min
message("Optimal lambda: ", signif(best_lambda, 4))

# Predict probabilities & classes
pred_prob <- as.numeric(predict(cvfit, newx = x, s = "lambda.min", type = "response"))
pred_class <- factor(ifelse(pred_prob > 0.5, "BL","Non_BL"), levels = c("Non_BL","BL"))

## 6) Performance summary ----------------------------------------------------
cm <- caret::confusionMatrix(pred_class, y_fac, positive = "BL")
sens <- unname(cm$byClass["Sensitivity"])
spec <- unname(cm$byClass["Specificity"])
acc  <- unname(cm$overall["Accuracy"])

roc_obj <- pROC::roc(response = y_fac, predictor = pred_prob, levels = c("Non_BL","BL"))
auc_val <- pROC::auc(roc_obj)

message(sprintf("Accuracy=%.3f  Sensitivity=%.3f  Specificity=%.3f  AUC=%.3f", acc, sens, spec, auc_val))

## 7) Bind predictions to data ----------------------------------------------
analysis_df <- data_imp |>
  dplyr::mutate(
    Diagnosis_lbl = y_fac,
    Predicted = pred_class,
    Pred_Prob_BL = pred_prob
  )

## 8) Identify false negatives (FN) & true positives (TP) -------------------
FN <- analysis_df |> dplyr::filter(Diagnosis_lbl == "BL", Predicted == "Non_BL")
TP <- analysis_df |> dplyr::filter(Diagnosis_lbl == "BL", Predicted == "BL")

n_total_BL <- sum(analysis_df$Diagnosis_lbl == "BL", na.rm = TRUE)
n_FN <- nrow(FN); n_TP <- nrow(TP)

## 9) Variables to summarize in tables --------------------------------------
vars_to_describe <- c("Age","Gender","SympMon","Tumorsite","LDH","VAF","ctDNA",
                      "i1mut","e2mut","EBER1","EBER2","EBNA2","EBVmax","EBVSR",
                      "EBVP","EBVent","autoent","Translocation","Pred_Prob_BL")
vars_to_describe <- intersect(vars_to_describe, names(analysis_df))

## 10) Table A: False negatives only ----------------------------------------
tbl_fn <- FN |>
  dplyr::select(dplyr::all_of(vars_to_describe)) |>
  gtsummary::tbl_summary(
    statistic = list(
      gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
      gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    missing = "no"
  ) |>
  gtsummary::modify_header(label ~ "**Characteristic**") |>
  gtsummary::modify_caption("**Table A. Clinical characteristics of false-negative BL cases (gold standard BL, model predicted Non_BL).**")

## 11) Table B: FN vs TP among BL cases -------------------------------------
BL_cases <- dplyr::bind_rows(
  FN |> dplyr::mutate(Group = "False Negative"),
  TP |> dplyr::mutate(Group = "True Positive")
)

tbl_fn_vs_tp <- BL_cases |>
  dplyr::select(dplyr::all_of(c("Group", vars_to_describe))) |>
  gtsummary::tbl_summary(
    by = Group,
    statistic = list(
      gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
      gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    missing = "no"
  ) |>
  gtsummary::add_p(test = list(
    gtsummary::all_continuous() ~ "wilcox.test",
    gtsummary::all_categorical() ~ "fisher.test"
  )) |>
  gtsummary::modify_header(label ~ "**Characteristic**") |>
  gtsummary::bold_labels() |>
  gtsummary::modify_caption("**Table B. False-negative vs true-positive BL cases (comparison among BL).**")

2
library(cardx)
## 12) Summary line for your Results ----------------------------------------
summary_line <- glue::glue(
  "Among {n_total_BL} patients with BL by the gold standard, ",
  "{n_FN} ({round(100*n_FN/n_total_BL,1)}%) were not detected by the liquid biopsy model (false negatives), ",
  "while {n_TP} ({round(100*n_TP/n_total_BL,1)}%) were correctly identified (true positives)."
)

library(officer)
library(flextable)

## 13) Export to Word --------------------------------------------------------
doc <- read_docx() |>
  body_add_par("Liquid Biopsy Model: False-Negative Analysis", style = "heading 1") |>
  body_add_par(summary_line, style = "Normal") |>
  body_add_par(" ", style = "Normal") |>
  body_add_par("Table A", style = "heading 2") |>
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_fn)) |>
  body_add_par(" ", style = "Normal") |>
  body_add_par("Table B", style = "heading 2") |>
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_fn_vs_tp)) |>
  body_add_par(" ", style = "Normal") |>
  body_add_par(
    sprintf("Accuracy=%.3f  Sensitivity=%.3f  Specificity=%.3f  AUC=%.3f",
            acc, sens, spec, auc_val),
    style = "Normal"
  )

print(doc, target = OUT_FILE)
message("✅ Word file created: ", OUT_FILE)
getwd()

library(officer)
library(flextable)
library(gtsummary)

# check your gtsummary version
packageVersion("gtsummary")



## 14) Verify save location --------------------------------------------------
nf <- normalizePath(OUT_FILE, winslash = "/", mustWork = FALSE)
if (file.exists(nf)) {
  message("✅ Saved: ", nf)
} else {
  fallback <- file.path(getwd(), "LB_false_negative_analysis.docx")
  print(doc, target = fallback)
  message("⚠️ Could not save to desired path. Saved instead to working directory: ",
          normalizePath(fallback, winslash = "/"))
  message("Working directory was: ", getwd())
}
getwd()
## 15) Console recap ---------------------------------------------------------
cat("\n", summary_line, "\n")
cat("FN cases:", n_FN, " | TP cases:", n_TP, " | Total BL:", n_total_BL, "\n")
cat("Saved file:", nf, "\n")
