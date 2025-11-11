# Load necessary libraries 
library(dplyr)
library(tidyr)
library(ggplot2)

# Reshape the dataset into long format
my_data_long <- my_data %>%
  pivot_longer(cols = c(BI_Dx, seq, LP),  # Specify process columns
               names_to = "Phase", values_to = "Time")

# Calculate median and IQR for each phase
stats <- my_data_long %>%
  group_by(Phase) %>%
  summarize(
    Median = median(Time, na.rm = TRUE),
    Q1 = quantile(Time, 0.25, na.rm = TRUE),
    Q3 = quantile(Time, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Label = paste0("Median: ", round(Median, 2), ", IQR: ", round(Q1, 2), "-", round(Q3, 2)))

# Reorder the Phase levels for proper stacking (LP on top, BI_Dx at bottom)
my_data_long$Phase <- factor(my_data_long$Phase, levels = c("BI_Dx", "seq", "LP"))


# Step 4: Filter samples with total turnaround time ≤ 30 days
filtered_data <- my_data_long %>%
  group_by(Sample) %>%
  filter(sum(Time, na.rm = TRUE) <= 30)

# Step 5: Calculate total time per sample and get overall median and IQR
total_time <- filtered_data %>%
  group_by(Sample) %>%
  summarize(Total_Time = sum(Time, na.rm = TRUE))

overall_median <- median(total_time$Total_Time, na.rm = TRUE)
overall_q1 <- quantile(total_time$Total_Time, 0.25, na.rm = TRUE)
overall_q3 <- quantile(total_time$Total_Time, 0.75, na.rm = TRUE)

# Step 6: Plot
ggplot(filtered_data, aes(x = Sample, y = Time, fill = Phase)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  
  # Add per-phase median lines (dotted)
  geom_segment(data = stats, aes(
    x = 0.5,
    xend = length(unique(filtered_data$Sample)) + 6,
    y = Median, yend = Median, color = Phase
  ), linetype = "dotted", linewidth = 1, show.legend = TRUE) +
  
  # 🔹 Add black dashed line for overall median
  geom_hline(yintercept = overall_median, linetype = "dashed", color = "black", linewidth = 1) +
  
  # 🔹 Annotate the overall median
  annotate("text",
           x = length(unique(filtered_data$Sample)) + 5,
           y = overall_median + 2,
           label = paste0("Overall Median: ", round(overall_median, 1)," days\n(IQR: ", round(overall_q1, 1), "-", round(overall_q3, 1), ")"),
           color = "black", size = 3.5, hjust = 1) +
  
  # Legend customization 
  scale_fill_manual(
    values = c("BI_Dx" = "#fc8d62", "seq" = "#8da0cb", "LP" = "#66c2a5"),
    labels = c(
      paste0("BI analysis and Report generation\n", stats$Label[stats$Phase == "BI_Dx"]),
      paste0("Sequencing\n", stats$Label[stats$Phase == "seq"]),
      paste0("cfDNA extraction and library preparation\n", stats$Label[stats$Phase == "LP"])
    )
  ) +
  scale_color_manual(
    values = c("BI_Dx" = "#fc8d62", "seq" = "#8da0cb", "LP" = "#66c2a5"),
    labels = NULL
  ) +
  guides(
    fill = guide_legend(override.aes = list(linetype = 0)),
    color = "none"
  ) +
  labs(
    title = "Liquid Biopsy: Processing time to Diagnosis (Filtered)",
    x = "Sample",
    y = "Time (Days)",
    fill = "Phase"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    legend.position = "right",
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )
