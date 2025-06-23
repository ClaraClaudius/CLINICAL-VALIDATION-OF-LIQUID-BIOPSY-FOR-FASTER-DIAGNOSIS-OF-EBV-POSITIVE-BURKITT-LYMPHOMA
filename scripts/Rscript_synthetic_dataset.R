# Load the synthpop package
library(synthpop)

# Use the full dataset including the outcome variable 'Diagnosis'
data <- table3dataset

# Generate the synthetic data (including the outcome variable)
synthetic_result <- syn(data)

# Extract the synthetic dataset
synthetic_data <- synthetic_result$syn

# Save the synthetic dataset to CSV
write.csv(synthetic_data, "synthetic_data_with_outcome.csv", row.names = FALSE)

# Optional: View the first few rows
head(synthetic_data)


library(synthpop)
library(dplyr)

# Recode factors
data <- external_data %>%
  mutate(
    Gender = as.factor(Gender),
    Tumorsite = as.factor(Tumorsite),
    Translocation = as.factor(Translocation),
    Diagnosis = as.factor(Diagnosis)
  )

# Try a faster synthesis method
synthetic_result <- syn(data, method = "cart")

# Extract and save
synthetic_data <- synthetic_result$syn
write.csv(synthetic_data, "synthetic_data_with_outcome.csv", row.names = FALSE)

