# Clinical Validation of Liquid Biopsy for Faster Diagnosis of EBV-Positive Burkitt Lymphoma

This repository contains all R scripts and data used to produce the results for the clinical validation study of a liquid biopsy test for the diagnosis of EBV-positive Burkitt Lymphoma.

## Contents
- `Datasets/` – The datasets/ folder contains original, anonymized datasets generated from the study participants in Tanzania and Uganda. All personally identifiable information has been removed, and unique sample identifiers have been recoded to protect confidentiality in accordance with local and institutional data governance policies.
These anonymized datasets are shared solely for transparency and reproducibility of the analytical workflow. Each file corresponds to a specific figure or table in the manuscript and enables users to reproduce the analytical steps using the accompanying R scripts.
Please note that, while these datasets are derived from real study data, certain variables have been generalized or masked to prevent re-identification. As a result, numerical values may differ slightly from those reported in the manuscript, but analytical relationships remain valid for reproducibility purposes.
- `Scripts/` – The `scripts/` folder contains all the R scripts used to generate the tables and figures presented in the manuscript. Each script is named according to the corresponding output (e.g., `table2.R`, `figure5.R`) and can be run using the simulated data provided in the `datasets/` folder. These scripts demonstrate the full analytical workflow, including data wrangling, statistical testing, and visualization, allowing users to reproduce the methodology described in the study. While the results from these scripts will differ slightly when run on synthetic data, the structure and logic of the analysis remain identical to those used on the original dataset.
  - Table 2 
  - Table 3
  - Figure 4
  - Figure 5
  - Figure 6
  - Figure 7A
  - Figure 7B
  - Figure 7C
  - Supplementary Figure 1A and 1B
  - Supplementary Figure 1C
  - Supplementary Figure 1D
  - Supplementary Figure 1E
  - Supplementary Figure 6
  - Supplementary figure 8A
  - Supplementary Figure 8B
  - Supplementary Table 4

## How to Reproduce
1. Clone this repository:
   ```bash
   git clone https://github.com/ClaraClaudius/CLINICAL-VALIDATION-OF-LIQUID-BIOPSY-FOR-FASTER-DIAGNOSIS-OF-EBV-POSITIVE-BURKITT-LYMPHOMA.git ```

2. Open RStudio or your preferred R environment.

3. Open and run scripts in the Scripts/ folder to reproduce results.
