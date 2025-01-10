# The Impact of the General Data Protection Regulation (GDPR) on Online Tracking

**Authors:** Klaus M. Miller, Karlo Lukic, Bernd Skiera\
**Date:** January 10, 2025

## Description of Repository

This repository contains the reproducible code to generate tables and figures presented in the paper:

**"The Impact of the General Data Protection Regulation (GDPR) on Online Tracking"**

Note: The analysis was conducted using the R programming language.

------------------------------------------------------------------------

## Steps to Reproduce the Analysis

1.  **Choose the environment:** Decide whether you want to run the analysis using RStudio Desktop or Posit Cloud. Instructions for both environments are provided below.

2.  **Restore the environment:** Begin by running `renv::restore()` to restore all packages and their versions to ensure a reproducible data analysis. When prompted, confirm by typing `Y`. Restoring packages can take a few minutes. If you have already completed this step, you can skip it.

3.  **Locate the R script:** The key script for the analysis is located at `02_r_scripts/reproducible_analysis.R`. Open this self-contained script to run the data analysis.

4.  **Run the script:** For instance, in RStudio, run the script line by line `Run Selected Line(s)` to replicate the exact data analysis process conducted for the paper.

5.  **Locate the results**: The folder `03_results` will initially be empty. Upon complete execution of the script, the folder will populate with the results.

------------------------------------------------------------------------

## Data Sets

### Public Data:

The analysis relies on **WhoTracks.me** data, which is publicly available at: <https://github.com/whotracksme/whotracks.me>

### Proprietary Data:

Some data sets used in the paper (e.g., SimilarWeb) are proprietary and cannot be shared in this repository. Instead, we use a **filtered version** of the proprietary data that is necessary to produce the results reported in our paper.

------------------------------------------------------------------------

## How to Instantly Replicate Analysis Using Posit Cloud

Use the prepared project on **Posit Cloud** to instantly replicate the analysis. Simply click on the link below to get started after the Sign Up / Log In on Posit Cloud:

-   **"The Impact of the General Data Protection Regulation (GDPR) on Online Tracking" Reproducible Analysis** on [Posit Cloud](https://posit.cloud/content/9521737)

Once the project opens, locate the R script and run it (see the points 3.-5. above).

Note: Posit Cloud allows you to skip the steps 1.-2. above.

------------------------------------------------------------------------

## Instructions to Replicate Analysis on Local Machine Using RStudio Desktop

1.  **Download** this repository to your local machine, e.g.: `Code -> Download ZIP`

2.  Open the R project in **RStudio Desktop**.

3.  See the points 2.-5. above.

------------------------------------------------------------------------

## Reproducibility with `renv`

To ensure reproducibility, we use the **`renv`** package to snapshot the project environment and manage package versions. This package allows anyone to recreate the exact environment used for the analysis.

### How to Use `renv`

1.  After cloning the repository, open the project in **RStudio Desktop**.

2.  Run the following command to restore the package environment:

    ```         
    renv::restore()
    ```

3.  This command will install all necessary packages with their exact versions as used in the original analysis.

### Creating a New Snapshot

If you add or update packages, you can create a new snapshot of the environment by running:

```         
renv::snapshot()
```

This command will update the `renv.lock` file with the current package versions.

------------------------------------------------------------------------

## Acknowledgments

We thank the **WhoTracks.me** team for providing the data used in this research.\
This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation program (grant agreement No. 833714).

------------------------------------------------------------------------

## Licensing

This repository is licensed under the **MIT License**. You are free to use, modify, and distribute the code. If you use this script in your work, please cite the original paper:

**Miller, K.M., Lukic, K., & Skiera, B. (2025). The Impact of the General Data Protection Regulation (GDPR) on Online Tracking. IJRM.**
