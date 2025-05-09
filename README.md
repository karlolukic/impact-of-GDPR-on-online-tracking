# The Impact of the General Data Protection Regulation (GDPR) on Online Tracking

**Authors:** Klaus M. Miller, Karlo Lukic, Bernd Skiera\
**Date:** March 13, 2025

## Description of Repository

This repository contains the reproducible code to generate tables and figures for the paper:

### The Impact of the General Data Protection Regulation (GDPR) on Online Tracking

#### Abstract

This study explores the impact of the General Data Protection Regulation (GDPR) on online trackers—vital elements in the online advertising ecosystem. Using a difference-in-differences approach with a balanced panel of 294 publishers, it compares publishers subject to the GDPR with those unaffected (the control group). Drawing on data from WhoTracks.me, which spans 32 months from May 2017 to December 2019, it analyzes how the number of trackers used by publishers changed before and after the GDPR. The findings reveal that although online tracking increased for both groups, the rise was less significant for EU-based publishers subject to the GDPR. Specifically, the GDPR reduced about four trackers per publisher, equating to a 14.79% decrease compared to the control group. The GDPR was particularly effective in curbing privacy-invasive trackers that collect and share personal data, thereby strengthening user privacy. However, it had a limited impact on advertising trackers and only slightly reduced the presence of analytics trackers.

Keywords: Economics of Privacy; Online Privacy; Online Tracking; Privacy Law; Quasi-Experiment; Difference-in-Differences; Synthetic Control

*Note*: The analysis was conducted using the **R programming language.**

## Repository Structure

```         
├── 01_data                                           # Directory for data files
│   ├── lookup_tables                                 # Subdirectory for Excel lookup tables for reporting
│   └── (other .rds data files used for the analysis)
├── 02_r_scripts                                      # Directory for R scripts
│   ├── packages_and_functions-2025-03-07-KL.R        # R script with packages and functions
│   └── reproducible_analysis-2025-03-07-KL.R         # Main R script to reproduce results
├── 03_results                                        # Directory for output tables and figures
│   ├── figure_01.pdf 
│   └── web_appendix                                  # Subdirectory for web appendix outputs
│       └── (web appendix outputs)
├── 04_cascad_certification                           # Directory for CASCAD certification
│   ├── reproducibility_certificate-2025-03-13.pdf
│   ├── execution_report_cascad402cp-2025-03-12.pdf
├── images                                            # Directory for images
│   ├── erc-logo.png
│   ├── logo-hi-paris-retina.png
│   └── cascad-certification.png
└── renv                                              # Directory for renv cache files
    └── (renv cache files)
```

## Steps to Reproduce the Analysis

1.  **Choose the environment:** Decide whether you want to run the analysis using RStudio Desktop or Posit Cloud. Instructions for both environments are provided below.

2.  **Restore the environment:** Begin by running `renv::restore()` to restore all packages and their versions to ensure a reproducible data analysis. When prompted, confirm by typing `Y`. Restoring packages can take a few minutes. If you have already completed this step, you can skip it.

3.  **Locate the R script:** The key script for the analysis is located at `02_r_scripts/reproducible_analysis-2025-03-07-KL.R`. Open this self-contained script to run the data analysis.

4.  **Run the script:** For instance, run the script in RStudio line by line using **Run Selected Line(s)** to replicate the exact data analysis process conducted in the paper.

5.  **Locate the results:** The `03_results` folder will initially be empty, except for Figure 1, which we created without the R programming language. Upon complete execution of the script, the folder will populate with the results.

## Data Sets

### Public Data

The analysis relies on **WhoTracks.me** data, which is publicly available at:\
<https://github.com/whotracksme/whotracks.me>

### Proprietary Data

Some data that we use in the paper (e.g., SimilarWeb) is proprietary and we cannot share it in this repository. Instead, we share a **subset** of the proprietary data to produce the results reported in our paper.

## How to Instantly Replicate Analysis Using Posit Cloud

Use the prepared project on **Posit Cloud** to instantly replicate the analysis. Simply click on the link below—after signing up or logging in—to get started:

-   **Link to the Reproducible Analysis** on [Posit Cloud](https://posit.cloud/content/9968737).

Once the project opens, follow steps 3-5 above.

*Note*: Posit Cloud allows you to skip step 2.

## Instructions to Replicate Analysis on Local Machine Using RStudio Desktop

1.  **Download the repository** to your local machine, e.g., by selecting **Code** -\> **Download ZIP**.

2.  **Extract the ZIP file** and open the R project file `impact-of-GDPR-on-online-tracking-2025-03-07.Rproj` using RStudio Desktop.

3.  Follow steps 2-5 above.

## Reproducibility with `renv`

*Note:* To ensure reproducibility, we use the **`renv`** package to snapshot the project environment and manage package versions. This package allows anyone to recreate the exact environment used for the analysis.

### How to Use `renv`

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

## Acknowledgments

We thank the **WhoTracks.me** team for providing the data used in this research.

This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation program (grant agreement No. 833714). Miller gratefully acknowledges support from the Hi! PARIS Center on Data Analytics and Artificial Intelligence for Science, Business, and Society.

## Attribution

If you use this script in your work, please cite the original paper:

**K.M. Miller, K. Lukic, B. Skiera, The impact of the General Data Protection Regulation (GDPR) on online tracking, International Journal of Research in Marketing (2025), doi: [10.1016/j.ijresmar.2025.03.002](https://doi.org/10.1016/j.ijresmar.2025.03.002).**

## Reproducibility Certification

The computational reproducibility of the results in this paper has been verified by the [Certification Agency for Scientific Code and Data (CASCAD)](https://www.cascad.tech). The CASCAD reproducibility certificate and execution report are available in the [`04_cascad_certification`](./04_cascad_certification/) directory or via this [link](https://www.cascad.tech/certification/159-the-impact-of-the-general-data-protection-regulation-gdpr-on-online-tracking/).

<img src="images/erc-logo.png" alt="ERC Logo" width="460"/><img src="images/logo-hi-paris-retina.png" alt="HI Paris Logo" width="300"/>

<img src="images/cascad-certification.png" alt="CASCAD certification" width="760"/>
