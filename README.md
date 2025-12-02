# PES-MADM II Decision Support Platform

This repository provides the full R Shiny implementation of the **Preference–Entropy Synergy Multi-Attribute Decision-Making II (PES-MADM II)** framework.  
The platform integrates PROMETHEE-type preference modelling with entropy-based probabilistic diagnostics, enabling a unified and robust evaluation of alternatives under multiple criteria. The implementation includes all computational stages required for the empirical case study on the **World Bank Logistics Performance Index (LPI) 2023**.

---

## Contents

- **PESMADMIIFinalAllPrefs.R**  
  Complete Shiny application implementing the PES-MADM II model, including preference functions, entropy measures, diagnostic indices (NMI, CES, CSF, ADI, NMGI), and full sensitivity-analysis modules.

- **LPI_2023\*.xlsx**  
  Cleaned decision matrices used for the LPI-2023 case study, including subjective weights, preference parameters, and benefit–cost specifications.

- **README.md**  
  Documentation and instructions for replicating the model.

---

## How to Run the Application

1. Open `PESMADMIIFinalAllPrefs.R` in **RStudio**.
2. Ensure the following packages are installed:

shiny, shinythemes, shinyjs, readxl, writexl,
ggplot2, dplyr, DT, gridExtra, reshape2, ggrepel


3. Set your working directory to the project folder.
4. Run:

```r
shiny::runApp()
or simply source the script inside RStudio.

The platform provides:

Baseline computations

Full entropy diagnostics

Preference-function comparison (Usual vs V-Shape vs others)

Sensitivity analysis (weights, data, criterion type, preference thresholds)

Automatic summary reporting
