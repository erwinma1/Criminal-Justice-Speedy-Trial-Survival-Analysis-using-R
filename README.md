Queens Speedy Trial Survival Analysis (De-identified)

This repository demonstrates survival analysis on felony case data (2014–2018), with all identifiers and organizational references removed.
The project explores prosecutorial waiver practices in Queens and their impact on pretrial detention, conviction outcomes, and sentencing.

Highlights

Rigorous methodology: Survival (Kaplan–Meier) and Cox proportional hazards models

Data quality validation: Manual checks and cross-reference against external detention records

Policy relevance: Findings support concerns that waiver practices extended detention and increased conviction severity

Visualization: Kaplan–Meier survival curves and regression outputs

Reproducibility

Language: R (v4.x)

Key packages: survival, survminer, dplyr, tidyr, ggplot2, rcompanion

Run the analysis:

# Install dependencies
install.packages(c("survival","survminer","dplyr","tidyr","ggplot2","rcompanion","readr"))

# Load and run
source("queens_survival_analysis.R")

Data

Input: felony case records, arraignment through final disposition

Scope: 2014–2018, felony cases with bail set at arraignment

Note: All data here is de-identified. Replace data/raw/felony_2014_18.csv with your own or synthetic dataset to reproduce results.

Outputs

Figures: Kaplan–Meier curves for detention duration and case length

Models: Cox proportional hazards regression, probit regression on plea outcomes

Validation: T-tests showing algorithm outputs match hand-checks and external records

Notes

Original analysis completed in 2019; code has been lightly refactored for clarity.

This sample demonstrates transferable skills in statistical modeling, validation, and policy-oriented analysis.
