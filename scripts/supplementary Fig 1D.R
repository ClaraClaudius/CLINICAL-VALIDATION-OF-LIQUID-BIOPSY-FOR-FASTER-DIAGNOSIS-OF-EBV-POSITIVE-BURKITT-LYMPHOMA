
library(dplyr)
library(ggplot2)

# --- 1. Count MYC mutations per sample, recode diagnosis immediately ---
myc_counts <- variants %>%
  filter(Gene == "MYC") %>%
  group_by(Sample, Diagnosis) %>%
  summarise(n_mutations = n(), .groups = "drop") %>%
  mutate(Diagnosis = ifelse(Diagnosis == 0, "non-BL", "BL"))

# --- 2. Group medians ---
group_medians <- myc_counts %>%
  group_by(Diagnosis) %>%
  summarise(median_mut = median(n_mutations), .groups = "drop")

# --- 3. Wilcoxon test ---
wilcox_res <- wilcox.test(n_mutations ~ Diagnosis, data = myc_counts)
p_value <- wilcox_res$p.value
p_text <- ifelse(p_value < 0.001, "p < 0.001", paste0("p = ", signif(p_value, 3)))

# --- 4. Build caption with semicolon ---
caption_text <- paste0(
  "Group medians: ",
  paste(group_medians$Diagnosis, group_medians$median_mut, sep = ": ", collapse = "; "),
  " | Wilcoxon test: ", p_text
)

# --- 5. Plot ---
p <- ggplot(myc_counts, aes(x = Sample, y = n_mutations, fill = Diagnosis)) +
  geom_col() +
  facet_wrap(~Diagnosis, scales = "free_x", nrow = 1) +
  geom_hline(data = group_medians,
             aes(yintercept = median_mut, color = Diagnosis),
             linetype = "dashed", size = 1, inherit.aes = FALSE) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_blank(),   # remove sample names
    axis.ticks.x = element_blank()
  ) +
  labs(
    title = "Number of MYC intron 1 mutations per sample",
    x = "Samples",
    y = "Number of MYC mutations",
    caption = caption_text
  )

print(p)

library(dplyr)
library(ggplot2)

# --- 1. Count MYC mutations per sample, recode diagnosis immediately ---
myc_counts <- variants %>%
  filter(Gene == "MYC") %>%
  group_by(Sample, Diagnosis) %>%
  summarise(n_mutations = n(), .groups = "drop") %>%
  mutate(Diagnosis = ifelse(Diagnosis == 0, "non-BL", "BL"))

# --- 2. Group medians ---
group_medians <- myc_counts %>%
  group_by(Diagnosis) %>%
  summarise(median_mut = median(n_mutations), .groups = "drop")

# --- 3. Wilcoxon test ---
wilcox_res <- wilcox.test(n_mutations ~ Diagnosis, data = myc_counts)
p_value <- wilcox_res$p.value
p_text <- ifelse(p_value < 0.001, "p < 0.001", paste0("p = ", signif(p_value, 3)))

# --- 4. Caption ---
caption_text <- paste0(
  "Group medians: ",
  paste(group_medians$Diagnosis, group_medians$median_mut, sep = ": ", collapse = "; "),
  " | Wilcoxon test: ", p_text
)

# --- 5. Plot with custom colours ---
p <- ggplot(myc_counts, aes(x = Sample, y = n_mutations, fill = Diagnosis)) +
  geom_col() +
  facet_wrap(~Diagnosis, scales = "free_x", nrow = 1) +
  geom_hline(
    data = group_medians,
    aes(yintercept = median_mut, color = Diagnosis),
    linetype = "dashed", size = 1, inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("BL" = "#0072B2", "non-BL" = "#F7A8A3")) +   # Blue for BL, Peach for non-BL
  scale_color_manual(values = c("BL" = "#0072B2", "non-BL" = "#F7A8A3")) +  # Match median line colors
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    title = "Number of MYC intron 1 mutations per sample",
    x = "Samples",
    y = "Number of MYC mutations",
    caption = caption_text
  )

print(p)
