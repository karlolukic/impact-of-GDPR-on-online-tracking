# The Impact of the General Data Protection Regulation (GDPR) on Online Tracking

**Authors:** Klaus M. Miller, Karlo Lukic, Bernd Skiera\
**Script Author:** Karlo Lukic\
**Date:** January 10, 2025

## Description of Repository

This repository contains the reproducible R script used to generate all tables and figures presented in the paper:

**"The Impact of the General Data Protection Regulation (GDPR) on Online Tracking"**

The paper analyzes the effect of the GDPR on online tracking using a Difference-in-Differences (DiD) approach and the Generalized Synthetic Control method. The code processes data from **WhoTracks.me** to reproduce the results.

------------------------------------------------------------------------

## Data Sources

### Public Data:

The analysis relies on **WhoTracks.me** data, which is publicly available at:\
<https://github.com/whotracksme/whotracks.me>

### Proprietary Data:

Some datasets used in the paper (e.g., SimilarWeb) are proprietary and cannot be shared in this repository. Instead, we use a **filtered version** of the proprietary data that is necessary to produce the results reported in our paper. This ensures that the analysis is reproducible without compromising the confidentiality of the raw proprietary data.

------------------------------------------------------------------------

## How to Instantly Replicate Analysis Using Posit Cloud

Use the prepared project on **Posit Cloud** to instantly replicate the analysis. Simply click on the link below to get started:

-   **GDPR Paper Analysis** on [Posit Cloud](https://posit.cloud/project/%3Cproject-id%3E)

Once the project opens, follow the instructions in the `README.md` file inside the Posit Cloud environment to reproduce the analysis.

------------------------------------------------------------------------

## Instructions to Replicate Analysis on Local Machine Using RStudio Desktop

1.  Clone this repository to your local machine:

    ``` bash
    git clone https://github.com/<your-github-username>/<repository-name>.git
    ```

2.  Open the R project in **RStudio Desktop**.

3.  Install the required packages if you haven't already:

    ``` r
    install.packages(c("dplyr", "ggplot2", "fixest", "flextable", "magick", "here", "readr", "data.table", "tidyr", "scales"))
    ```

4.  Run the script `reproducible_analysis.R` to reproduce all tables and figures as presented in the paper.

------------------------------------------------------------------------

## Acknowledgments

We thank the **WhoTracks.me** team for providing the data used in this research.\
This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation program (grant agreement No. 833714).

## Licensing

This repository is licensed under the **MIT License**. You are free to use, modify, and distribute the code. If you use this script in your work, please cite the original paper:

**Miller, K.M., Lukic, K., & Skiera, B. (2025). The Impact of the General Data Protection Regulation (GDPR) on Online Tracking. IJRM.**

------------------------------------------------------------------------

## 
