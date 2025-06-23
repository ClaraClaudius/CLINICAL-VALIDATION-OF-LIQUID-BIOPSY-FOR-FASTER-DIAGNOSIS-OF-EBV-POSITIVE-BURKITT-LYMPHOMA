# Load required packages
library(gtsummary)
library(dplyr)

# Rename dataset (R doesn't allow variable names to start with numbers)
data <- X377_description

# Generate summary table
summary_table <- data %>%
  tbl_summary(
    by = phase,  # Stratify by 'phase'
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",  # Use median and IQR
      all_categorical() ~ "{n} ({p}%)"               # n (%)
    ),
    missing = "no"  # Exclude missing values from summaries
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%  # Format headers
  modify_caption("**Table: Summary of Variables Stratified by Phase**") %>%
  bold_labels()

# View the table
summary_table



