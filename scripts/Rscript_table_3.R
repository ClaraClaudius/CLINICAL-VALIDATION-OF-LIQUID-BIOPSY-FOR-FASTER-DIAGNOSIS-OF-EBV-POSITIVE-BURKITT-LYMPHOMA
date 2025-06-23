## Load required packages
library(tidyverse)
library(rstatix)
library(gtsummary)
library(gt)
library(flextable)

# Load and prepare dataset
df <- table3dataset

# Recode diagnosis
df <- df %>%
  mutate(Group = factor(Diagnosis, levels = c(1, 0), labels = c("BL", "Non-BL")))

# Vectorized p-value formatter for exact display
p_format <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) return(NA)
    if (val < 0.001) {
      formatC(val, format = "e", digits = 2)
    } else {
      formatC(val, format = "f", digits = 4)
    }
  })
}

# Effect size for Wilcoxon (rank biserial correlation)
my_wilcox_effsize <- function(data, variable, by, ...) {
  df <- data[!is.na(data[[variable]]), , drop = FALSE]
  if (length(unique(df[[by]])) < 2) return(NA)
  if (length(unique(df[[variable]])) < 2) return(0)
  result <- rstatix::wilcox_effsize(df, formula(paste(variable, "~", by)))
  result$effsize
}

# Effect size for Chi-squared (Cramér's V)
my_cramer_v <- function(data, variable, by, ...) {
  df <- data[!is.na(data[[variable]]), , drop = FALSE]
  if (length(unique(df[[by]])) < 2) return(NA)
  if (length(unique(df[[variable]])) < 2) return(0)
  tbl <- table(df[[variable]], df[[by]])
  rstatix::cramer_v(tbl)
}

# Determine test name
my_test_name <- function(data, variable, by, ...) {
  if (is.numeric(data[[variable]])) {
    "Wilcoxon"
  } else {
    "Chi-squared"
  }
}

# Explicitly convert known categorical variables to factors
categorical_vars <- c("Gender", "Tumorsite", "Translocation")
df <- df %>%
  mutate(across(all_of(categorical_vars), ~ as.factor(.)))

# Create gtsummary table
summary_table <- df %>%
  tbl_summary(
    by = Group,
    include = c(Age, Gender, SympMon, Tumorsite, LDH, VAF, ctDNA, cfDNA,
                i1mut, e2mut, EBER1, EBER2, EBNA2, EBVmax, EBVSR,
                EBVP, EBVent, autoent, Translocation),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ "wilcox.test",
      all_categorical() ~ "chisq.test"
    ),
    test.args = list(
      all_continuous() ~ list(exact = FALSE),
      all_categorical() ~ list(correct = FALSE)
    ),
    pvalue_fun = identity  # defer formatting
  ) %>%
  add_q(method = "fdr", pvalue_fun = identity)  # defer formatting

# Apply exact formatting to p-values and FDR q-values
summary_table$table_body <- summary_table$table_body %>%
  mutate(
    p.value = p_format(p.value),
    q.value = p_format(q.value)
  )

# Add test name and effect size
summary_table <- summary_table %>%
  add_stat(fns = list(
    all_continuous() ~ my_test_name,
    all_categorical() ~ my_test_name
  )) %>%
  add_stat(fns = list(
    all_continuous() ~ my_wilcox_effsize,
    all_categorical() ~ my_cramer_v
  )) %>%
  modify_header(
    label ~ "**Variable**",
    stat_0 ~ "**Overall**",
    stat_1 ~ "**BL**",
    stat_2 ~ "**Non-BL**",
    p.value ~ "**p-value**",
    q.value ~ "**FDR p-value**",
    add_stat_1 ~ "**Test**",
    add_stat_2 ~ "**Effect Size**"
  ) %>%
  modify_spanning_header(c("stat_0", "stat_1", "stat_2") ~ "**Summary Statistics**") %>%
  modify_footnote(
    update = list(
      stat_0 ~ "Median (Q1, Q3) for continuous; n (%) for categorical",
      p.value ~ "Exact p-values from Wilcoxon rank-sum or Chi-squared tests",
      q.value ~ "FDR-adjusted p-values (Benjamini-Hochberg)",
      add_stat_2 ~ "r for Wilcoxon; Cramér’s V for Chi-squared"
    )
  )

# Export the final table to Word
summary_table %>%
  as_flex_table() %>%
  save_as_docx(path = "summary_table.docx")
