# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Reshape the data into long format for ggplot
long_data <- my_data %>%
  pivot_longer(cols = c(SR2GSP, TotalLB), 
               names_to = "Test", 
               values_to = "Time")

# Update the labels for the Test variable
long_data$Test <- recode(long_data$Test, 
                         SR2GSP = "Tissue Biopsy", 
                         TotalLB = "Liquid Biopsy")

# Calculate medians, IQR, and thresholds for filtering
stats <- long_data %>%
  group_by(Test) %>%
  summarize(
    Median = median(Time, na.rm = TRUE),
    Q1 = quantile(Time, 0.25, na.rm = TRUE),
    Q3 = quantile(Time, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    Upper_Threshold = Q3 + 1.5 * (Q3 - Q1),  # Upper limit for outliers
    Lower_Threshold = Q1 - 1.5 * (Q3 - Q1),  # Lower limit for outliers
    Label = paste0("Median: ", round(Median, 2), 
                   ", IQR: ", round(Q1, 2), "-", round(Q3, 2))
  )

# Filter out outliers beyond thresholds
filtered_data <- long_data %>%
  left_join(stats, by = "Test") %>%
  filter(Time <= Upper_Threshold & Time >= Lower_Threshold)

# Wilcoxon test (on original paired data)
wilcox_result <- wilcox.test(my_data$SR2GSP, my_data$TotalLB, 
                             paired = TRUE, alternative = "two.sided")
p_value <- signif(wilcox_result$p.value, digits = 3)

# Your original Wilcoxon test
result <- wilcox.test(my_data$SR2GSP, my_data$TotalLB, paired = TRUE)

# Load the effectsize package
library(effectsize)

# Load package
library(effectsize)

# Calculate rank-biserial correlation for paired data
r_result <- rank_biserial(my_data$SR2GSP, my_data$TotalLB, paired = TRUE)
r_result
effect_size_label <- "Effect size (r) = 0.93 [0.89, 0.96]"

# Add it to your existing ggplot (add below the p-value)
ggplot(filtered_data, aes(x = Test, y = Time, fill = Test)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  labs(
    title = "Comparison of Median Time to Diagnosis\nBetween Liquid Biopsy and Gold Standard Pathology",
    x = "Test", y = "Time to Diagnosis (Days)"
  ) +
  scale_fill_manual(values = c("Tissue Biopsy" = "#66c2a5", "Liquid Biopsy" = "#fc8d62")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  # Add median + IQR text
  geom_text(
    data = stats,
    aes(x = Test, y = Median * 2, label = Label),
    inherit.aes = FALSE, size = 3.8, color = "black"
  ) +
  # Add p-value
  annotate("text", 
           x = 1.2, 
           y = max(filtered_data$Time, na.rm = TRUE) * 0.9, 
           label = paste0("P-value: ", signif(result$p.value, 3)),
           fontface = "bold", size = 5, color = "black") +
  # Add effect size
  annotate("text", 
           x = 1.2, 
           y = max(filtered_data$Time, na.rm = TRUE) * 0.7, 
           label = effect_size_label,
           fontface = "italic", size = 4.5, color = "black")









