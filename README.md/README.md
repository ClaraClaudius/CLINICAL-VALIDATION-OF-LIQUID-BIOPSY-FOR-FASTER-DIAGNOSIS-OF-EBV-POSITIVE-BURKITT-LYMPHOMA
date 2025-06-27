# Clinical Validation of Liquid Biopsy for Faster Diagnosis of EBV-Positive Burkitt Lymphoma

This repository contains all R scripts and synthetic data used to produce the results for the clinical validation study of a liquid biopsy test for the diagnosis of EBV-positive Burkitt Lymphoma.

## Contents
- `Datasets/` – The `datasets/` folder contains synthetic data files that replicate the structure and statistical characteristics of the original clinical dataset used in the study. These files do not contain any real patient data and were generated to support code reproducibility while preserving participant confidentiality. Each file corresponds to a figure or table in the manuscript and allows users to reproduce the analytical steps using the R scripts provided. Please note that results obtained using this synthetic data will not exactly match the findings reported in the paper.
- `Scripts/` – The `scripts/` folder contains all the R scripts used to generate the tables and figures presented in the manuscript. Each script is named according to the corresponding output (e.g., `table2.R`, `figure5.R`) and can be run using the simulated data provided in the `datasets/` folder. These scripts demonstrate the full analytical workflow, including data wrangling, statistical testing, and visualization, allowing users to reproduce the methodology described in the study. While the results from these scripts will differ slightly when run on synthetic data, the structure and logic of the analysis remain identical to those used on the original dataset.
  - Table 2 
  - Table 3
  - Figure 4
  - Figure 5
  - Figure 6
  - Figure 7A
  - Figure 7B
  - Figure 7C

## How to Reproduce
1. Clone this repository:
   ```bash
   git clone https://github.com/ClaraClaudius/CLINICAL-VALIDATION-OF-LIQUID-BIOPSY-FOR-FASTER-DIAGNOSIS-OF-EBV-POSITIVE-BURKITT-LYMPHOMA.git ```

2. Open RStudio or your preferred R environment.

3. Open and run scripts in the Scripts/ folder to reproduce results.
