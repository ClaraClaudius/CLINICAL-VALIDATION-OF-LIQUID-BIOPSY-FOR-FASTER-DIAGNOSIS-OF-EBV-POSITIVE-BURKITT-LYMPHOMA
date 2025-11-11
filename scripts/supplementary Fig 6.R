library(dplyr)
library(ggplot2)

# --- 1) Denominator: total unique samples per Dx group ---
denoms <- MYCmut %>%
  distinct(Sample, Diagnosis) %>%
  count(Diagnosis, name = "n_total")

# --- 2) Numerator: unique samples mutated at each position per Dx ---
pos_counts <- MYCmut %>%
  distinct(Diagnosis, Sample, Pos) %>%                 # avoid duplicate calls per sample/position
  group_by(Diagnosis, Pos) %>%
  summarise(n_mut = n_distinct(Sample), .groups = "drop") %>%
  left_join(denoms, by = "Diagnosis") %>%
  mutate(percent_mut = 100 * n_mut / n_total)

# (Optional) check a quick summary
# dplyr::arrange(pos_counts, desc(percent_mut)) %>% head()

# --- 3) Plot: percentage of samples mutated at each position, by Dx ---
ggplot(pos_counts, aes(x = Pos, y = percent_mut, color = Diagnosis)) +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = c("BL" = "red", "non BL" = "blue")) +
  labs(
    title = "Percentage of samples with mutations in MYC (Homopolymers removed)",
    x = "Mutation Position (hg19)",
    y = "Percentage of Samples Mutated",
    color = "Diagnosis"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
