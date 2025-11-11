# ========================
# CNA-aware analysis for MYC and TP53 using merged dataset
# ========================

library(tidyverse)
library(janitor)
library(viridis)
library(scales)
library(forcats)

# 1) Clean dataset
df <- CNA %>%
  janitor::clean_names() %>%
  mutate(
    sample   = as.character(sample),
    gene     = toupper(as.character(gene)),
    pos      = as.numeric(pos),
    vaf      = as.numeric(vaf),
    ct_dna   = as.numeric(dplyr::coalesce(ct_dna)),
    diagnosis = as.factor(diagnosis),
    ctdna_log = log10(ct_dna + 1)  # 🔑 log-transform here
  ) %>%
  filter(!is.na(sample), !is.na(gene), !is.na(pos), !is.na(vaf), !is.na(ct_dna))

df <- df %>%
  mutate(
    diagnosis = fct_recode(as.factor(diagnosis),
                           "non-BL" = "0",
                           "BL"     = "1")
  )

# 2) Fit expected VAF–ctDNA relationship from ID3 (copy-neutral baseline)
ref_df <- df %>% filter(gene == "ID3")
mod_ref <- lm(vaf ~ ctdna_log, data = ref_df)  # 🔑 regression on log10 ctDNA

# Residuals + tolerance
ref_resid <- ref_df$vaf - predict(mod_ref, newdata = ref_df)
mad_ref <- mad(ref_resid, na.rm = TRUE)
if (!is.finite(mad_ref) || mad_ref == 0) mad_ref <- sd(ref_resid, na.rm = TRUE)
if (!is.finite(mad_ref) || mad_ref == 0) mad_ref <- 0.02
band <- 2 * mad_ref

# 3) Apply model to MYC and TP53
genes_test <- c("MYC", "TP53")
test_df <- df %>%
  filter(gene %in% genes_test) %>%
  mutate(
    vaf_exp = predict(mod_ref, newdata = .),
    resid   = vaf - vaf_exp,
    flag = case_when(
      resid >  band ~ "Above expected (LOH-like)",
      resid < -band ~ "Below expected (Amplification-like)",
      TRUE          ~ "Within band"
    )
  )

# 4) Prediction grid on log-scale
xgrid <- tibble(ctdna_log = seq(min(test_df$ctdna_log, na.rm = TRUE),
                                max(test_df$ctdna_log, na.rm = TRUE), length.out = 200))
pred_grid <- xgrid %>%
  mutate(vaf_exp = predict(mod_ref, newdata = .),
         lower = vaf_exp - band,
         upper = vaf_exp + band,
         ct_dna = 10^ctdna_log - 1)  # back-transform for plotting axis

# 5) Plot function
plot_gene <- function(gene_name) {
  df_gene <- test_df %>% filter(gene == gene_name)
  
  ggplot(df_gene, aes(x = ct_dna, y = vaf)) +
    geom_ribbon(data = pred_grid,
                aes(x = ct_dna, ymin = lower, ymax = upper),
                inherit.aes = FALSE, fill = "grey70", alpha = 0.3) +
    geom_line(data = pred_grid, aes(x = ct_dna, y = vaf_exp),
              inherit.aes = FALSE, color = "black") +
    geom_point(aes(color = diagnosis), size = 3, alpha = 0.9) +
    geom_point(data = filter(df_gene, flag != "Within band"),
               shape = 21, stroke = 1, color = "black", fill = NA, size = 4) +
    scale_x_log10(labels = scales::comma) +   # 🔑 keep x-axis on log scale
    scale_color_brewer(palette = "Set1") +
    labs(x = "ctDNA (hGE/mL, log10 scale)", 
         y = "Variant allele frequency (VAF)",
         color = "Diagnosis",
         title = paste0(gene_name, " gene: ctDNA–VAF relationship of variants with Copy Number Alterations (CNA) highlighted\n",
                        "(Shaded band = expected range, black-rim = CNA)")) +
    theme_classic(base_size = 13)
}

# plots
p_myc  <- plot_gene("MYC")
p_tp53 <- plot_gene("TP53")

print(p_myc)
print(p_tp53)


# 6) Save figures
ggsave("MYC_ctDNA_vs_VAF.png", p_myc, width = 7, height = 5, dpi = 300)
ggsave("TP53_ctDNA_vs_VAF.png", p_tp53, width = 7, height = 5, dpi = 300)


#Summary tables for flagged variants
library(dplyr)
library(flextable)
library(officer)

# 1. Extract flagged variants only
flagged_variants <- test_df %>%
  filter(flag != "Within band") %>%
  arrange(gene, sample, desc(abs(resid))) %>%
  dplyr::select(sample, gene, pos, change, vaf, ct_dna, diagnosis, resid, flag)

# 2. Create summary tables
ft_flagged <- flextable(flagged_variants)

gene_summary <- flagged_variants %>%
  count(gene, flag, name = "n_flagged")
ft_gene <- flextable(gene_summary)

sample_summary <- flagged_variants %>%
  count(sample, gene, flag, name = "n_flagged")
ft_sample <- flextable(sample_summary)

# NEW: flagged counts by gene and diagnosis
gene_diag_summary <- flagged_variants %>%
  count(gene, diagnosis, flag, name = "n_flagged")
ft_gene_diag <- flextable(gene_diag_summary)

# 3. Save all tables into a Word document
doc <- read_docx() %>%
  body_add_par("Flagged variants (CNA-like outliers)", style = "heading 1") %>%
  body_add_flextable(ft_flagged) %>%
  body_add_par("Summary by gene", style = "heading 1") %>%
  body_add_flextable(ft_gene) %>%
  body_add_par("Summary by sample", style = "heading 1") %>%
  body_add_flextable(ft_sample) %>%
  body_add_par("Summary by gene and diagnosis", style = "heading 1") %>%
  body_add_flextable(ft_gene_diag)

print(doc, target = "flagged_variants1.docx")


#EXCLUDE FLAGGED CNA AFFECTED VARIANTS

library(dplyr)
library(openxlsx)

# Exclude flagged variants
variants_filtered <- variants %>%
  anti_join(affected, by = c("Sample", "Gene", "Pos", "Change"))

# Write to Excel
write.xlsx(variants_filtered, file = "variants_filtered_CNA_excluded.xlsx", rowNames = FALSE)
