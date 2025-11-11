#BOX PLOTS COMPARING BL vs BENIGN MEDIANOF EBV PARAMETERS

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

# Convert Diagnosis to a labeled factor
paper2dataset <- paper2dataset %>%
  mutate(Diagnosis = factor(Diagnosis,
                            levels = c(0, 1),
                            labels = c("Reactive", "BL")))

# Reshape dataset
data_long <- paper2dataset %>%
  select(Diagnosis, EBVmax, EBVP, EBVent, EBVSR) %>%
  pivot_longer(cols = c(EBVmax, EBVP, EBVent, EBVSR),
               names_to = "Variable",
               values_to = "Value")

# Plot boxplots with Wilcoxon test p-values
ggboxplot(data_long, x = "Diagnosis", y = "Value",
          fill = "Diagnosis", palette = "jco",
          facet.by = "Variable", scales = "free_y",
          add = "jitter") +
  stat_compare_means(method = "wilcox.test",
                     label = "p.format",
                     label.y.npc = "top") +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none") +
  labs(title = "Comparison of EBV Parameters between BL and Reactive Samples",
       y = "Value",
       x = "")






# Install if not already
if(!require(patchwork)) install.packages("patchwork")

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(patchwork)

# Label your groups
paper2dataset <- paper2dataset %>%
  mutate(Diagnosis = factor(Diagnosis,
                            levels = c(0, 1),
                            labels = c("Reactive", "BL")))

# Reshape to long format
data_long <- paper2dataset %>%
  select(Diagnosis, EBVmax, EBVP, EBVent, EBVSR) %>%
  pivot_longer(cols = c(EBVmax, EBVP, EBVent, EBVSR),
               names_to = "Variable", values_to = "Value")

# Safely compute p-values (skip variables lacking both groups)
p_values <- data_long %>%
  group_by(Variable) %>%
  summarise(
    has_two_groups = n_distinct(Diagnosis[!is.na(Value)]) == 2,
    p_value = ifelse(has_two_groups,
                     wilcox.test(Value ~ Diagnosis)$p.value,
                     NA_real_)
  ) %>%
  mutate(
    p_value = round(p_value, 5)
  ) %>%
  select(Variable, p_value)

# Create the boxplot
p1 <- ggboxplot(data_long, x = "Diagnosis", y = "Value",
                fill = "Diagnosis", palette = "jco",
                facet.by = "Variable", scales = "free_y",
                add = "jitter") +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  labs(title = "Comparison of EBV Parameters between BL and Reactive Samples",
       y = "Value",
       x = "")

# Create p-value table (no 'p_label' column)
p_table <- ggtexttable(
  p_values,
  rows = NULL,
  theme = ttheme("mOrange")
) %>%
  tab_add_title(text = "Wilcoxon test", face = "bold")

# Combine boxplot (left) and table (right)
final_plot <- p1 | p_table + plot_layout(widths = c(1, 1))

# Display final figure
final_plot






library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(patchwork)

# Label your groups
paper2dataset <- paper2dataset %>%
  mutate(Diagnosis = factor(Diagnosis,
                            levels = c(0, 1),
                            labels = c("Reactive", "BL")))

# Reshape to long format
data_long <- paper2dataset %>%
  select(Diagnosis, EBVmax, EBVP, EBVent, EBVSR) %>%
  pivot_longer(cols = c(EBVmax, EBVP, EBVent, EBVSR),
               names_to = "Variable", values_to = "Value")

# Compute p-values with scientific notation
p_values <- data_long %>%
  group_by(Variable) %>%
  summarise(
    has_two_groups = n_distinct(Diagnosis[!is.na(Value)]) == 2,
    p_value = ifelse(has_two_groups,
                     wilcox.test(Value ~ Diagnosis)$p.value,
                     NA_real_)
  ) %>%
  mutate(
    p_value = formatC(p_value, format = "e", digits = 2)
  ) %>%
  select(Variable, p_value)

# Add units to variable names
unit_labels <- c(
  EBVmax = "EBVmax (copies/cell)",
  EBVP   = "EBVP (proportion)",
  EBVent = "EBVent (bits)",
  EBVSR  = "EBVSR (size ratio)"
)

data_long$Variable <- factor(data_long$Variable,
                             levels = names(unit_labels),
                             labels = unit_labels)

# Create the boxplot
p1 <- ggboxplot(data_long, x = "Diagnosis", y = "Value",
                fill = "Diagnosis", palette = "jco",
                facet.by = "Variable", scales = "free_y",
                add = "jitter") +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  labs(title = "Comparison of EBV Parameters between BL and Reactive Samples",
       y = "Value",
       x = "")

# Create p-value table
p_table <- ggtexttable(
  p_values,
  rows = NULL,
  theme = ttheme("mOrange")
) %>%
  tab_add_title(text = "Wilcoxon test", face = "bold")

# Combine boxplot (left) and table (right)
final_plot <- p1 | p_table + plot_layout(widths = c(1, 1))

# Display final figure
final_plot


# Load libraries
library(pROC)
library(ggplot2)

# Assuming your dataset is called paper2dataset
# and Diagnosis is coded as factor or numeric (0 = Reactive/non-BL, 1 = BL)
# If it’s a factor, convert it for pROC:
paper2dataset$Diagnosis_bin <- ifelse(paper2dataset$Diagnosis == "BL", 1, 0)

# Compute ROC curves for each EBV parameter
roc_max  <- roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVmax)
roc_p    <- roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVP)
roc_ent  <- roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVent)
roc_sr   <- roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVSR)

# Plot all ROCs on one plot
plot(roc_max, col = "#E69F00", lwd = 2, main = "ROC Curves for EBV Parameters Predicting BL")
plot(roc_p,   col = "#56B4E9", lwd = 2, add = TRUE)
plot(roc_ent, col = "#009E73", lwd = 2, add = TRUE)
plot(roc_sr,  col = "#D55E00", lwd = 2, add = TRUE)

# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend with AUC values
legend("bottomright",
       legend = c(
         paste0("EBVmax (AUC = ", round(auc(roc_max), 3), ")"),
         paste0("EBVP (AUC = ", round(auc(roc_p), 3), ")"),
         paste0("EBVent (AUC = ", round(auc(roc_ent), 3), ")"),
         paste0("EBVSR (AUC = ", round(auc(roc_sr), 3), ")")
       ),
       col = c("#E69F00", "#56B4E9", "#009E73", "#D55E00"),
       lwd = 2, cex = 0.9, bty = "n")






# Load required libraries
library(pROC)
library(ggplot2)
library(dplyr)

# Ensure binary outcome
paper2dataset$Diagnosis_bin <- ifelse(paper2dataset$Diagnosis == "BL", 1, 0)

# Compute and smooth ROC curves
roc_list <- list(
  EBVmax = smooth(roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVmax, quiet = TRUE)),
  EBVP   = smooth(roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVP, quiet = TRUE)),
  EBVent = smooth(roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVent, quiet = TRUE)),
  EBVSR  = smooth(roc(paper2dataset$Diagnosis_bin, paper2dataset$EBVSR, quiet = TRUE))
)

# ✅ Extract metrics safely
roc_summary <- lapply(names(roc_list), function(var) {
  r <- roc_list[[var]]
  best <- coords(r, "best", best.method = "youden", ret = c("sensitivity", "specificity"))
  sens <- as.numeric(best["sensitivity"])
  spec <- as.numeric(best["specificity"])
  data.frame(
    Variable = var,
    AUC = round(as.numeric(auc(r)), 3),
    Sensitivity = round(sens, 2),
    Specificity = round(spec, 2)
  )
}) %>% bind_rows()

# Check values
print(roc_summary)

# Combine data for ggplot
roc_df <- bind_rows(lapply(names(roc_list), function(v) {
  data.frame(
    Variable = v,
    Specificity = 1 - roc_list[[v]]$specificities,
    Sensitivity = roc_list[[v]]$sensitivities
  )
}))

# Colors for each variable
colors <- c("EBVmax" = "#56B4E9", "EBVP" = "#009E73",
            "EBVent" = "#E69F00", "EBVSR" = "#D55E00")

# ✅ Create full legend labels
roc_labels <- paste0(
  roc_summary$Variable,
  " (AUC=", roc_summary$AUC,
  ", Se=", roc_summary$Sensitivity,
  ", Sp=", roc_summary$Specificity, ")"
)

# Plot
ggplot(roc_df, aes(x = Specificity, y = Sensitivity, color = Variable)) +
  geom_line(linewidth = 1.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_manual(values = colors, labels = roc_labels,
                     name = "EBV Parameter (AUC / Se / Sp)") +
  coord_equal() +
  theme_bw(base_size = 14) +
  labs(
    title = "ROC Curves for EBV Parameters Predicting BL",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme(
    legend.position.inside = c(0.7, 0.25),
    legend.background = element_rect(fill = "white", color = "gray90"),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

