#merge the diagnosis of non-BL samples
library(dplyr)
library(openxlsx)

# Join diagnosis info into variants
merged <- variants %>%
  left_join(diagnosis, by = "Sample")

# Save to Excel
write.xlsx(merged, "MYCintron1variants_with_diagnosis.xlsx")




#Number of MYC mutations Non-BL samples
library(dplyr)
library(ggplot2)

# --- 1. Count MYC mutations per sample ---
myc_counts <- dataset %>%
  filter(Gene == "MYC") %>%
  group_by(Sample, GSDiagnosis) %>%
  summarise(n_mutations = n(), .groups = "drop")

# --- 2. Calculate overall median ---
overall_median <- median(myc_counts$n_mutations)

# --- 3. Plot ---
ggplot(myc_counts, aes(x = factor(Sample), y = n_mutations, fill = as.factor(GSDiagnosis))) +
  geom_col() +
  geom_hline(yintercept = overall_median, linetype = "dotted", color = "black", size = 1) +
  scale_x_discrete(labels = NULL) +  # hide sample names
  theme_classic(base_size = 14) +
  labs(
    title = "Number of MYC intron 1 mutations in non-BL samples by Diagnosis",
    x = "Samples (each bar = one sample)",
    y = "Number of MYC mutations",
    fill = "Diagnosis"
  )

#With sample names

library(dplyr)
library(ggplot2)

# --- 1. Count MYC mutations per sample ---
myc_counts <- dataset %>%
  filter(Gene == "MYC") %>%
  group_by(Sample, GSDiagnosis) %>%
  summarise(n_mutations = n(), .groups = "drop")

# --- 2. Calculate overall median ---
overall_median <- median(myc_counts$n_mutations)

# --- 3. Plot with sample names on x-axis ---
ggplot(myc_counts, aes(x = Sample, y = n_mutations, fill = as.factor(GSDiagnosis))) +
  geom_col() +
  geom_hline(yintercept = overall_median, linetype = "dotted", color = "black", size = 1) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6) # rotate to avoid overlap
  ) +
  labs(
    title = "Number of MYC mutations per sample",
    x = "Sample",
    y = "Number of MYC mutations",
    fill = "GSDiagnosis"
  )

