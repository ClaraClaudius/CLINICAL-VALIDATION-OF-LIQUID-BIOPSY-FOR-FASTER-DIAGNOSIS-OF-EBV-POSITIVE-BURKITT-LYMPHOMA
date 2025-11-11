


#PLOT OF INDIVIDUAL VAF FOR SAMPLES
library(tidyverse)

# Ensure diagnosis is labeled properly
df <- merged %>%
  mutate(
    Diagnosis = factor(Diagnosis, levels = c(0, 1), labels = c("non-BL", "BL"))
  )

# Sample-level medians
sample_medians <- df %>%
  group_by(Sample, Diagnosis) %>%
  summarise(median_vaf = median(VAF, na.rm = TRUE), .groups = "drop")

# Group-level medians
group_medians <- df %>%
  group_by(Diagnosis) %>%
  summarise(median_vaf = median(VAF, na.rm = TRUE), .groups = "drop")

# Order samples inside each facet
df <- df %>%
  group_by(Diagnosis) %>%
  mutate(Sample = factor(Sample, levels = unique(Sample))) %>%
  ungroup()

sample_medians$Sample <- factor(sample_medians$Sample, levels = levels(df$Sample))

# Plot with facets for Diagnosis

ggplot(df, aes(x = Sample, y = VAF)) +
  geom_jitter(aes(color = Diagnosis), width = 0.2, alpha = 0.6, size = 2) +
  
  # Blue dot for sample-level median
  geom_point(data = sample_medians,
             aes(x = Sample, y = median_vaf),
             color = "blue", size = 3) +
  
  # Solid horizontal line for group-level median
  geom_hline(data = group_medians,
             aes(yintercept = median_vaf, color = Diagnosis),
             linetype = "solid", size = 1, inherit.aes = FALSE) +
  
  facet_wrap(~Diagnosis, scales = "free_x", nrow = 1) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_blank(),   # remove sample names
    axis.ticks.x = element_blank(),  # remove x ticks
    panel.grid.major.x = element_line(color = "grey85") # light vertical lines for samples
  ) +
  labs(x = "Samples (each bar = one sample)", 
       y = "Variant Allele Frequency (VAF)",
       title = "VAF distribution per sample by diagnosis",
       subtitle = "Dots = variants, blue dot = sample median, solid line = group median")








library(dplyr)
library(ggplot2)

# ---- 1. Compute medians per sample ----
sample_medians <- df %>%
  group_by(Sample, Diagnosis) %>%
  summarise(median_vaf = median(VAF, na.rm = TRUE),
            n_variants = n(), .groups = "drop")

# ---- 2. Compute medians per group ----
group_medians <- df %>%
  group_by(Diagnosis) %>%
  summarise(median_vaf = median(VAF, na.rm = TRUE)) %>%
  ungroup()

# ---- 3. Wilcoxon test ----
wilcox_res <- wilcox.test(VAF ~ Diagnosis, data = df)
p_val <- wilcox_res$p.value
p_label <- ifelse(p_val < 0.001, "p < 0.001",
                  paste0("p = ", signif(p_val, 3)))

# ---- 4. Make caption text ----
caption_text <- paste0(
  "Group medians: non-BL = ", signif(group_medians$median_vaf[group_medians$Diagnosis == "non-BL"], 4),
  ", BL = ", signif(group_medians$median_vaf[group_medians$Diagnosis == "BL"], 4),
  " | Wilcoxon test: ", p_label
)

# ---- 5. Order samples within each diagnosis ----
df <- df %>%
  mutate(Sample = factor(Sample,
                         levels = sample_medians$Sample))

# ---- 6. Plot ----
p <- ggplot(df, aes(x = Sample, y = VAF)) +
  geom_jitter(aes(color = Diagnosis), width = 0.2, alpha = 0.6, size = 2) +
  
  # Blue dot = sample median
  geom_point(data = sample_medians,
             aes(x = Sample, y = median_vaf), color = "blue", size = 1) +
  
  # Solid line = group median
  geom_hline(data = group_medians,
             aes(yintercept = median_vaf, color = Diagnosis),
             linetype = "solid", size = 1, inherit.aes = FALSE) +
  
  # Facet by diagnosis
  facet_wrap(~Diagnosis, scales = "free_x", nrow = 1) +
  
  # Replace x-axis text with number of variants
  scale_x_discrete(labels = setNames(sample_medians$n_variants, sample_medians$Sample)) +
  
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
    panel.grid.major.x = element_line(color = "grey90")
  ) +
  labs(
    x = "Samples (number of variants per sample on x-axis)", 
    y = "Variant Allele Frequency (VAF)",
    title = "VAF distribution per sample by diagnosis",
    subtitle = "Dots = variants, blue dot = sample median, solid line = group median",
    caption = caption_text
  )

print(p)


sample_counts <- merged %>%
  distinct(Sample, Diagnosis) %>%
  count(Diagnosis)

sample_counts
