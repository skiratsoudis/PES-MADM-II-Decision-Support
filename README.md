# PES-MADM II Decision Support Platform

This repository contains the R Shiny implementation of the
**Preference–Entropy Synergy Multi-Attribute Decision-Making II (PES-MADM II)** framework.

## Contents

- `PESMADMIIFinalAllPrefs.R` – full Shiny app implementing the PES-MADM II model.
- `*.xlsx` – input data (DataMatrix, SubjectiveWeights, PreferenceParams, BenefitCost) for the LPI-2023 case study.

## How to run

1. Open `PESMADMIIFinalAllPrefs.R` in RStudio.
2. Ensure the required packages are installed (shiny, shinythemes, shinyjs, readxl, writexl, ggplot2, dplyr, DT, gridExtra, reshape2, ggrepel).
3. Set the working directory to this folder.
4. Run `shiny::runApp()` or source the script.

## Citation

When reusing this code, please cite the PES-MADM II article (details in the associated publication).

