# Load required libraries
library(dplyr)
library(readr)

# Step 1: Read original data
original_data <- read_csv("external_data")

# Step 2: Get summary of data structure
summary_info <- external_data %>%
  summarise(across(everything(), list(
    class = ~ class(.),
    min = ~ if(is.numeric(.)) min(., na.rm = TRUE) else NA,
    max = ~ if(is.numeric(.)) max(., na.rm = TRUE) else NA,
    n_unique = ~ n_distinct(.)
  ), .names = "{.col}_{.fn}"))

print("Variable summary:")
print(summary_info)

# Step 3: Manually inspect and define rules for mock data creation
set.seed(123)
n <- nrow(external_data)  # same number of observations

mock_data <- tibble(
  Age = sample(1:18, n, replace = TRUE),  # Pediatric ages
  Gender = sample(c("Male", "Female"), n, replace = TRUE),
  SympMon = sample(1:12, n, replace = TRUE),
  Tumorsite = sample(c("Abdomen", "Jaw", "CNS", "Chest"), n, replace = TRUE),
  LDH = round(rnorm(n, mean = 500, sd = 300), 1),
  VAF = round(runif(n, 0.001, 0.1), 3),
  ctDNA = round(runif(n, 0, 100), 2),
  i1mut = sample(c(0, 1), n, replace = TRUE),
  e2mut = sample(c(0, 1), n, replace = TRUE),
  EBER1 = round(rnorm(n, mean = 1000, sd = 300), 1),
  EBER2 = round(rnorm(n, mean = 950, sd = 280), 1),
  EBNA2 = round(rnorm(n, mean = 850, sd = 250), 1),
  EBVmax = round(runif(n, 1e3, 1e6), 0),
  EBVSR = round(runif(n, 1e1, 1e3), 1),
  EBVP = round(runif(n, 1e1, 1e3), 1),
  EBVent = round(runif(n, 0.1, 2), 2),
  autoent = round(runif(n, 0.1, 2), 2),
  Translocation = sample(c("Yes", "No"), n, replace = TRUE),
  Diagnosis = sample(c(0, 1), n, replace = TRUE)  # 0 = Non-BL, 1 = BL
)

# Step 4: Save to CSV
write_csv(mock_data, "mock_external_data.csv")

print("✅ Mock dataset created and saved as 'mock_external_data.csv'")
