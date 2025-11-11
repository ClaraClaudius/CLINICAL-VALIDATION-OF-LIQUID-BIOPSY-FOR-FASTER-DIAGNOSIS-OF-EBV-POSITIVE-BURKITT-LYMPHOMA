
# Load libraries
library(pROC)
library(ggplot2)
library(gridExtra)
library(ggplotify)

# Ensure Diagnosis is binary factor
paper2dataset$Diagnosis_bin <- factor(paper2dataset$Diagnosis,
                                      levels = c(0, 1),
                                      labels = c("non-BL", "BL"))

# EBV parameters to test
ebv_vars <- c("EBVmax", "EBVSR", "EBVP", "EBVent")

# Define consistent colors for each curve
curve_colors <- c("EBVmax" = "red",
                  "EBVSR" = "blue",
                  "EBVP" = "darkgreen",
                  "EBVent" = "orange")

# Store ROC results
roc_results <- list()
auc_table <- data.frame(Parameter = character(),
                        AUC = numeric(),
                        Sensitivity = numeric(),
                        Specificity = numeric())

# Compute ROC and AUC
for (var in ebv_vars) {
  roc_obj <- roc(paper2dataset$Diagnosis_bin, paper2dataset[[var]], ci = TRUE)
  roc_results[[var]] <- roc_obj
  
  # Optimal cutoff (Youden’s J)
  opt <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
  
  auc_table <- rbind(
    auc_table,
    data.frame(
      Parameter = var,
      AUC = round(auc(roc_obj), 3),
      Sensitivity = round(opt["sensitivity"], 3),
      Specificity = round(opt["specificity"], 3)
    )
  )
}

# Capture ROC plot with correct colors
roc_plot <- as.ggplot(~{
  plot(roc_results[[1]], col = curve_colors[ebv_vars[1]], lwd = 3,
       main = "ROC Curves for EBV Parameters (BL vs benign)")
  for (i in 2:length(ebv_vars)) {
    plot(roc_results[[i]], col = curve_colors[ebv_vars[i]], lwd = 3, add = TRUE)
  }
  abline(a = 0, b = 1, lty = 2, col = "gray")
  legend("bottomright",
         legend = ebv_vars,
         col = curve_colors[ebv_vars],
         lwd = 3,
         cex = 0.9,
         bty = "n")
})

# Convert AUC summary to a nice table grob
auc_table_grob <- tableGrob(auc_table, rows = NULL)

# Combine ROC plot and table side by side
grid.arrange(
  roc_plot, auc_table_grob,
  ncol = 2,
  widths = c(2, 1)
)

