# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Reshape the dataset into long format
my_data_long <- my_data %>%
  pivot_longer(cols = c("T2coll", "SC2SR", "sample_processing", "1stnonGSDxrep", "2ndGSDxrep"),  
               names_to = "Phase", values_to = "Time")

# Calculate the correct medians and IQRs
stats <- my_data_long %>%
  group_by(Phase) %>%
  summarize(
    Median = median(Time, na.rm = TRUE),
    Q1 = quantile(Time, 0.25, na.rm = TRUE),
    Q3 = quantile(Time, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Label = paste0("Median: ", round(Median, 2), ", IQR: ", round(Q1, 2), "-", round(Q3, 2)))

# Reorder the phases for proper stacking
my_data_long$Phase <- factor(my_data_long$Phase, levels = c("2ndGSDxrep", "1stnonGSDxrep", "sample_processing", "SC2SR", "T2coll"))

# Match the colors to phases and append medians to the labels for better clarity
color_values <- c(
  "2ndGSDxrep" = "#377eb8", 
  "1stnonGSDxrep" = "#e41a1c", 
  "sample_processing" = "#4daf4a", 
  "SC2SR" = "#ff7f00", 
  "T2coll" = "#984ea3"
)

legend_labels <- c(
  paste0("Gold Standard Diagnostic Report\n", stats$Label[stats$Phase == "2ndGSDxrep"]),
  paste0("First Diagnostic Report\n", stats$Label[stats$Phase == "1stnonGSDxrep"]),
  paste0("Sample Processing\n", stats$Label[stats$Phase == "sample_processing"]),
  paste0("Sample Reception\n", stats$Label[stats$Phase == "SC2SR"]),
  paste0("Sample Collection\n", stats$Label[stats$Phase == "T2coll"])
)

# Create the stacked bar chart with integrated legend
ggplot(my_data_long, aes(x = Sample, y = Time, fill = Phase)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  # Add accurate median lines
  geom_segment(data = stats, aes(
    x = 0.5, xend = length(unique(my_data_long$Sample)) + 2.5,
    y = Median, yend = Median, color = Phase
  ), linetype = "dotted", linewidth = 1, show.legend = FALSE) +
  # Adjust legend and color scales
  scale_fill_manual(values = color_values, labels = legend_labels) +
  guides(
    fill = guide_legend(override.aes = list(linetype = 0))
  ) +
  labs(
    title = "Tissue Biopsy: Sample Collection to Diagnosis",
    x = "Sample",
    y = "Time (Days)",
    fill = "Phase"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    legend.position = "right",
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold")
  )


#Filtering out those bars >300 days

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Reshape the dataset into long format
my_data_long <- my_data %>%
  pivot_longer(cols = c("T2coll", "SC2SR", "sample_processing", "1stnonGSDxrep", "2ndGSDxrep"),  
               names_to = "Phase", values_to = "Time")

# Calculate medians and IQRs from the original dataset
stats <- my_data_long %>%
  group_by(Phase) %>%
  summarize(
    Median = median(Time, na.rm = TRUE),
    Q1 = quantile(Time, 0.25, na.rm = TRUE),
    Q3 = quantile(Time, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Label = paste0("Median: ", round(Median, 2), ", IQR: ", round(Q1, 2), "-", round(Q3, 2)))

# Calculate the total processing time for each sample
total_time <- my_data_long %>%
  group_by(Sample) %>%
  summarize(Total_Time = sum(Time, na.rm = TRUE))

# After total_time calculation
overall_stats <- total_time %>%
  summarize(
    Overall_Median = median(Total_Time, na.rm = TRUE),
    Q1 = quantile(Total_Time, 0.25, na.rm = TRUE),
    Q3 = quantile(Total_Time, 0.75, na.rm = TRUE)
  )

# Filter samples with total processing time <= 300 days
valid_samples <- total_time %>%
  filter(Total_Time <= 200) %>%
  pull(Sample)

# Filter data for visualization
my_data_filtered <- my_data_long %>%
  filter(Sample %in% valid_samples)

# Reorder the phases for proper stacking
my_data_filtered$Phase <- factor(my_data_filtered$Phase, levels = c("2ndGSDxrep", "1stnonGSDxrep", "sample_processing", "SC2SR", "T2coll"))

# Define colors for better visualization
color_values <- c(
  "2ndGSDxrep" = "#377eb8", 
  "1stnonGSDxrep" = "#e41a1c", 
  "sample_processing" = "#4daf4a", 
  "SC2SR" = "#ff7f00", 
  "T2coll" = "#984ea3"
)

legend_labels <- c(
  paste0("Gold Standard Diagnostic Report\n", stats$Label[stats$Phase == "2ndGSDxrep"]),
  paste0("First Diagnostic Report\n", stats$Label[stats$Phase == "1stnonGSDxrep"]),
  paste0("Sample Processing\n", stats$Label[stats$Phase == "sample_processing"]),
  paste0("Sample Receipt\n", stats$Label[stats$Phase == "SC2SR"]),
  paste0("Sample Collection\n", stats$Label[stats$Phase == "T2coll"])
)

# Create the stacked bar chart
ggplot(my_data_filtered, aes(x = Sample, y = Time, fill = Phase)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  # Add median lines (from original data)
  geom_segment(data = stats, aes(
    x = 0.5, xend = length(unique(my_data_filtered$Sample)) + 20,
    y = Median, yend = Median, color = Phase
  ), linetype = "dotted", linewidth = 1, show.legend = FALSE) +
  
  geom_hline(yintercept = overall_stats$Overall_Median, 
             linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", 
           x = length(unique(my_data_filtered$Sample)) + 1, 
           y = overall_stats$Overall_Median, 
           label = paste0("Overall Median: ", round(overall_stats$Overall_Median, 1), 
                          "\nIQR: ", round(overall_stats$Q1, 1), "-", round(overall_stats$Q3, 1)), 
           hjust = 0, vjust = -0.5, size = 3.5, color = "black") +
  # Adjust legend and color scales
  scale_fill_manual(values = color_values, labels = legend_labels) +
  guides(
    fill = guide_legend(override.aes = list(linetype = 0))
  ) +
  labs(
    title = "Tissue Biopsy: Sample Collection to Diagnosis (filtered)",
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

