# Aspergillosis Epidemiology in the U.S., 2013–2023: Data and Code

This repository contains code and materials accompanying the preprint:
**"Epidemiology of Aspergillosis Diagnoses in the U.S. using a National EHR Database, 2013–2023"**  
DOI: [https://doi.org/10.1101/2025.06.19.25329882](https://doi.org/10.1101/2025.06.19.25329882)  
(Note: This work is currently a preprint and has not yet been peer-reviewed or published in a journal.)

## Repository contents

- `01_data_cleaning.R`: R script for initial data preparation and cleaning.
- `02_spatiotemporal_analysis.R`: R script for Bayesian spatiotemporal analysis.
- `03_model_building.R`: R script for building statistical models of aspergillosis prevalence.
- `04_data_visualizations.R`: R script for generating figures used in the manuscript.
- `Aspergillosis Epidemiology Manuscript.Rmd`: R Markdown document with exploratory data analysis and a walkthrough of key analytic steps; intended for readers who wish to follow along without replicating the entire workflow.

## How to use this repository

### Prerequisites
This project uses R (version >= 4.x recommended) and several R packages, including but not limited to:
- `tidyverse`
- `sf`
- `INLA`
- `ggplot2`
- `dplyr`

A full list of required packages is available in the `.Rmd` file and individual scripts.

### Running the analysis
These scripts are provided for transparency and reproducibility. **The underlying dataset (Oracle Health EHR Real World Data) is proprietary and not publicly available.** 

Researchers wishing to replicate the analysis will need access to a comparable dataset or their own licensed Oracle EHR data.

For an overview of the workflow and key analyses, we recommend starting with the R Markdown file:
