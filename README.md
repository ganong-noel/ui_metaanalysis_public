# Replication kit for Disemployment Effects of Unemployment Insurance: A Meta-Analysis
This repository contains the raw data, code, and output for all analyses 
in "Disemployment Effects of Unemployment Insurance: A Meta-Analysis". 

## How to run 
1. To run the analysis, download this replication kit into a folder called `repo`. This folder should be located in `[your_home_directory]/repo`. This way, the relative paths to the working directory (`~/repo/ui_pubbias`) will work and the analysis will run smoothly. 
2. Run `driver.R` to reproduce the analysis. This is located in `source/driver.R`.
3. If seeking to reproduce `random_pip.csv` and `random_pip_hist.png`, uncomment `source(paste0(dir, "/stats/generate_random_pip.R"))` in `driver.R`. Expect around 40 minutes of additional runtime. See section on ["Supplementary Analysis"](#supplementary-analysis-and-validation-files) for details. 

## Data and Code Availability Statement
The data used in this study can be freely obtained from publicly available sources and published articles referenced in Supplemental Appendix D. Replicating the steps necessary to obtain public data should only take a few minutes, while obtaining elasticity estimates from existing literature involves a lengthy systematic search and review. The authors acknowledge that they had legitimate access to these data for research purposes and that they have the rights to redistribute them in this replication package.

## Contents

### Data

All data files are located in `input/`. Raw data files are located in `input/raw/` and cleaned data files used in the analysis can be found in `input/clean/`. 

The file `raw_review.xlsx` is an export of this [google spreadsheet](https://docs.google.com/spreadsheets/d/1F-_PsaxvugRql5kcFueGN5jp3DI2M-GzAEEttqWirl4/edit?gid=0#gid=0). It contains multiple sheets, which are documented independently below.

| Type          | File                                           | Description                                                                                                                                                                                                                                                                                                          | Location     |
| ------------- | ---------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Raw Data      | PoPCites.csv                                   | Raw output from systematic literature search.                                                                                                                                                                                                                                                                        | input/raw/   |
| Raw Data      | raw_review.xlsx, "Main"                        | Studies included in the meta-analysis and their associated elasticities. This sheet sources values from the "Impact Factor" and "Tax wedge" sheets.                                                                                                                                                                    | input/raw/   |
| Raw Data      | raw_review.xlsx, "Auxiliary info"               | Additional information about the same studies. This sheet sources values from the "Tax wedge" page.                                                                                                                                                                                                                  | input/raw/   |
| Raw Data      | raw_review.xlsx, "Tax wedge"<sup>5,6</sup>                   | Tax wedge data for 18 nations. "Main" sources values from this page.                                                                                                                                                                                                                                                 | input/raw/   |
| Raw Data      | raw_review.xlsx, "World bank UE"<sup>1</sup> and "FRED UE"<sup>2,3,4,8</sup> | World bank UE: Annual unemployment rates (1991-2021) for 266 nations.<br>FRED UE: Austria's annual unemployment rates, 1969-2022.<br>FRED UE: Canada's annual unemployment rates, 1960-2022.<br>FRED UE: Germany's annual unemployment rates, 1962-2022.<br>FRED UE: USA's monthly unemployment rates, 1948-2023. | input/raw/   |
| Raw Data      | raw_review.xlsx, "Impact factor"<sup>7</sup>               | The influence of journals where included studies are published. "Main" sources values from this page.                                                                                                                                                                                                              | input/raw/   |
| Analysis Data | bma_input.csv                                  | Inputs for BMA; reformatted information from clean_review_estimates_long.csv.                                                                                                                                                                                                                                        | input/clean/ |
| Analysis Data | clean_review_estimates_long.csv                | Cleaned literature review from raw_review.xlsx.                                                                                                                                                                                                                                                                      | input/clean/ |
| Analysis Data | PoPCites_cleaned.csv                           | Cleaned information about published studies from literature search from PoPCites.csv.                                                                                                                                                                                                                                 | input/clean/ |

### Scripts


You can run the entire paper via `source/driver.R`. All the analysis scripts are in `source` and the locations are noted in the table below. Function scripts are located in `source/funcs/`. 


| Analysis Script       | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Location | Output File                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 01_clean_data.R      | This script first cleans the collected Google Scholar results to only the 407 that are journal publications. It then uses FRED and World Bank data to create a dataframe of unemployment rates for each country and year in our data. Then, it cleans the data from our data collection for use in our analysis and saves the results in clean_review_estimates_long.csv. It also generates Table C-2 and information used in Tables D-1 and D-2 of the paper. | source/  | clean_review_estimates_long.csv<br>PoPCites_cleaned.csv                                                                                                                                                                                                                                                                                  |
| 02_model_averaging.R | This script runs the Bayesian Model Averaging Analysis of Section 4. It aggregates the data as described in the appendix and saves this aggregated dataframe in input/clean/bma_input.csv. Then, it produces Table 3 and determines context-specific corrected elasticities.                                                                                                                                                                                                         | source/  | bma_input.csv<br>BMA_weighted_coefficients.tex                                                                                                                                                                                                                                                                                                                                                                                         |
| 03_andrews_kasy.R    | This script runs the GMM procedure from Andrews & Kasy (2019). These are the results reported in Table 2 of the paper.                                                                                                                                                                                                                                                                                                                                                               | source/  | n/a                                                                                                                                                                                                                                                                                                                                                                |
| 04_parametric_ak.R   | This script runs all parametric specifications from Andrews & Kasy (2019) that we include in Table 1 and Table 2 of the paper.                                                                                                                                                                                                                                                                                                                                                       | source/  | ak_robustness_table.tex                                                                                                                                                                                                                                                                                                                                           |
| 05_extra_figures.R   | This script produces Figure 1, Figure 2, Table 1, Figure B-1, Figure B-2, Figure B-3, Figure B-4, and Table C-1 of the paper.                                                                                                                                                                                                                                                                                                                                                        | source/  | quasi_experimental_by_year.png<br>funnel_by_margin_se_group_all.png<br>density_ridge.png<br>average_elasticity_per_se.png<br>bailychetty_ak.png<br>bailychetty_bma.png<br>bma_plot_1_of_5.png<br>bma_plot_2_of_5.png<br>bma_plot_3_of_5.png<br>bma_plot_4_of_5.png<br>bma_plot_5_of_5.png<br>tvaldensity_margin.png<br>bma_elasticities_by_baseline.png<br>descriptives_table.tex<br>research_design_table.tex<br>tvaldensity_margin.png |
| 06_lit_review.R      | Generates Figure B-5 of the paper.                                                                                                                                                                                                                                                                                                                                                                                                                                                   | source/  | lit_review.png                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| 07_make_df.R            | Creates a table with statistics that are referenced in the body of the text.                                                                                                                                                                                                                                                                                                                                                                                                         | source/   | stats.csv                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| driver.R | Calls scripts to replicate the analysis.| source/| n/a|

### Output / Exhibit Mapping

Outputs that map to exhibits can be found in `release/`. Output used to support factual statements in the text can be found in `stats/`. Outputs used to produce exhibits for the slides presentation are located in `release/slides/`. 


| Exhibit    | File                                                                                  |
| ---------- | --------------------------------------------------------------------------------------- |
| Figure 1   | average_elasticity_per_se.png<br>density_ridge.png<br>funnel_by_margin_se_group_all.png |
| Figure 2   | bailychetty_bma.png                                                                     |
| Table 1    | descriptives_table.tex*                                                                |
| Table 2    | ak_robustness_table.tex                                                                 |
| Table 3    | BMA_weighted_coefficients.tex                                                           |
| Figure B-1 | quasi_experimental_by_year.png                                                          |
| Figure B-2 | tvaldensity_margin.png                                                                  |
| Figure B-3 | bma_elasticities_by_baseline.png                                                        |
| Figure B-4 | bailychetty_ak.png                                                                      |
| Figure B-5 | lit_review.png                                                                          |
| Table C-1  | research_design_table.tex                                                               |
| Table C-2  | country_distribution.tex                                                                |

*You may observe slightly different floating point rounding behavior for `descriptives_table.tex` when running the GMM procedures from Andrews & Kasy (2019) on different machines. See section on ["Floating Point Errors"](#floating-point-errors) for more details. 

**Tables D-1 and D-2 of the paper are produced manually. 

### Other files

#### Function Scripts
- These are scripts that contain functions that are used in the analysis, visualization and working environment setup. They are located in the `source/funcs/` folder. 
  - `PublicationbiasPackage.R`: Contains functions for estimating publication bias in metastudies using Generalized Method of Moments (GMM). The functions used from this package are modified slightly to not produce unnecessary CSV files.
  - `bms.R`: Contains Bayesian Model Averaging (BMA) functions for regression analysis.
  - `metastudies.R`: Contains functions for estimating publication bias in meta-analyses, inspired by Andrews and Kasy (2019).
  - `prelim.R`: Contains custom functions for ggplot themes, winsorization, and tools saving animations and annotated plots.
  - `setup.R`: Sets up the working environment by importing necessary packages, creating filepaths, and sourcing other function scripts.

#### Visualizations for Slides
- These visualizations are not included in the text, but are used in the slide-deck.
    - `bma_plot_1_of_5.png`
    - `bma_plot_2_of_5.png`
    - `bma_plot_3_of_5.png`
    - `bma_plot_4_of_5.png`
    - `bma_plot_5_of_5.png`

#### Supplementary Analysis and Validation Files
- These files support the statement that "A covariate of random noise generally has a posterior inclusion probability of between 10 and 15%." They are located in the `/stats` folder. 
  - `generate_random_pip.R`: Calculates PIP values for a random covariate 50 times.
  - `random_pip.csv`: Contains the PIP values from each iteration.
  - `random_pip_hist.png`: Visualizes PIP values in a histogram.

### Scalar Values
The are hard-coded values in the replication kit. 

* RR and PBD for France.<sup>6</sup>
* US and Florida PBD.<sup>9</sup>
* US and Florida RR.<sup>10</sup> Calculate 2022 Jan - Dec, and use RR1 which excludes outliers >2.

## Citations 
1. **International Labour Organization.** 1991–2021. _ILO Modelled Estimates and Projections Database (ILOEST)._ Distributed by World Bank Group. Accessed June 18, 2024. https://ilostat.ilo.org/data.

2. **OECD, Organization for Economic Co-operation and Development.** 1960–2023. _Infra-Annual Labor Statistics: Unemployment Rate Total: 15 Years or Over for Canada [LRUNTTTTCAA156S]._ Distributed by FRED, Federal Reserve Bank of St. Louis. https://fred.stlouisfed.org/series/LRUNTTTTCAA156S (accessed December 13, 2024).

3. **OECD.** 1962–2022. _Infra-Annual Labor Statistics: Unemployment Rate Total: 15 Years or Over for Germany [LRUNTTTTDEA156S]._ Distributed by FRED, Federal Reserve Bank of St. Louis. https://fred.stlouisfed.org/series/LRUNTTTTDEA156S (accessed December 13, 2024).

4. **OECD.** 1969–2024. _Infra-Annual Labor Statistics: Unemployment Rate Total: 15 Years or Over for Austria [LRUNTTTTATA156S]._ Distributed by FRED, Federal Reserve Bank of St. Louis. https://fred.stlouisfed.org/series/LRUNTTTTATA156S (accessed December 13, 2024).

5. **OECD.** 2021. _Data from “Taxing Wages in Selected Partner Economies.”_ Distributed by Organization for Economic Co-operation and Development. https://www.oecd.org/content/dam/oecd/en/topics/policy-issues/tax-policy/taxing-wages-in-selected-partner-economies.pdf (accessed December 20, 2024).

6. **OECD.** 2000–2023. _Labour Taxation – OECD Comparative Indicators._ Distributed by OECD Data Explorer. https://www.oecd.org/en/data/indicators/taxwedge.html (accessed December 14, 2024).

7. **RePEc, Research Papers in Economics.** 2023. _Simple Impact Factors for Journals._ Distributed by IDEAS Bibliographic Database. https://ideas.repec.org/top/top.journals.simple.html (accessed April 10, 2023).

8. **U.S. Bureau of Labor Statistics.** 1948–2023. _Unemployment Rate [UNRATE]._ Distributed by FRED, Federal Reserve Bank of St. Louis. https://fred.stlouisfed.org/series/UNRATE (accessed December 13, 2024).

9. **U.S. Department of Labor, Employment and Training Administration.** 2022. _Comparison of State Unemployment Insurance Laws._ https://oui.doleta.gov/unemploy/statelaws.asp (accessed January 17, 2025).

10. **U.S. Department of Labor, Employment and Training Administration.** 2022. _Unemployment Insurance Data: UI Replacement Rates Report._ https://oui.doleta.gov/unemploy/ui_replacement_rates.asp (accessed January 17, 2025).




## Computational Requirements
The computational requirements for replication are accessible to most users with a standard laptop and common software. Below is the environment used in the most recent replication:

- **HARDWARE:**
  - CPU: Apple M1 Pro (x86_64-apple-darwin20). About 155 seconds CPU time used by R for computation. 
  - Operating System: macOS Sonoma 14.5. 
  - Memory: 16G. About 136.3 MB needed by R for computation.
  - Necessary Disk Space: ~12 MB. 
  - Runtime: Less than 3 minutes.

- **SOFTWARE**
  - R 4.4.1 (2024-06-14)
  - RStudio Version 2024.04.2+764 (2024.04.2+764)
  - Excel Version 16.90 (24101387)

- **MATRIX LIBRARIES** 
  - BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
  - LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

- **PACKAGE VERSIONS**
  - **Attached packages:**
    - rprojroot_2.0.4   
    - forcats_1.0.0     
    - purrr_1.0.2       
    - tibble_3.2.1      
    - tidyverse_2.0.0   
    - reldist_1.7-2     
    - rlang_1.1.4       
    - ggtext_0.1.2      
    - gridtext_0.1.5    
    - sandwich_3.1-0    
    - lmtest_0.9-40     
    - zoo_1.8-12        
    - ggridges_0.5.6    
    - scales_1.3.0      
    - gridExtra_2.3     
    - ggplot2_3.5.1     
    - latex2exp_0.9.6   
    - RColorBrewer_1.1-3
    - testthat_3.2.1.1  
    - xtable_1.8-4      
    - readr_2.1.5       
    - BMS_0.3.5         
    - magrittr_2.0.3    
    - pacman_0.5.1      
    - readxl_1.4.3      
    - kableExtra_1.4.0  
    - lubridate_1.9.3   
    - writexl_1.5.1     
    - stringr_1.5.1     
    - tidyr_1.3.1       
    - dplyr_1.1.4       
    - plyr_1.8.9
    - ragg_1.3.2
  - **Loaded via a namespace (and not attached):**
    - tidyselect_1.2.1    
    - viridisLite_0.4.2   
    - farver_2.1.2        
    - loo_2.8.0           
    - fastmap_1.2.0       
    - digest_0.6.36       
    - timechange_0.3.0    
    - lifecycle_1.0.4     
    - waldo_0.5.2         
    - StanHeaders_2.32.10 
    - densEstBayes_1.0-2.2
    - compiler_4.4.1      
    - tools_4.4.1         
    - utf8_1.2.4          
    - knitr_1.48          
    - labeling_0.4.3      
    - pkgbuild_1.4.4      
    - xml2_1.3.6          
    - pkgload_1.4.0       
    - withr_3.0.1         
    - desc_1.4.3          
    - grid_4.4.1          
    - stats4_4.4.1        
    - fansi_1.0.6         
    - colorspace_2.1-1    
    - inline_0.3.20       
    - cli_3.6.3           
    - crayon_1.5.3        
    - rmarkdown_2.27      
    - generics_0.1.3      
    - RcppParallel_5.1.9  
    - rstudioapi_0.16.0   
    - tzdb_0.4.0          
    - commonmark_1.9.2    
    - rstan_2.32.6        
    - splines_4.4.1       
    - parallel_4.4.1      
    - cellranger_1.1.0    
    - matrixStats_1.4.1   
    - vctrs_0.6.5         
    - Matrix_1.7-0        
    - hms_1.1.3           
    - systemfonts_1.1.0   
    - glue_1.7.0          
    - codetools_0.2-20    
    - stringi_1.8.4       
    - gtable_0.3.5        
    - QuickJSR_1.4.0      
    - munsell_0.5.1       
    - pillar_1.9.0        
    - htmltools_0.5.8.1   
    - brio_1.1.5          
    - R6_2.5.1            
    - textshaping_0.4.0   
    - evaluate_0.24.0     
    - lattice_0.22-6      
    - markdown_1.13       
    - rstantools_2.4.0    
    - Rcpp_1.0.14        
    - svglite_2.1.3       
    - nlme_3.1-164        
    - mgcv_1.9-1          
    - xfun_0.46           
    - pkgconfig_2.0.3      


### Floating Point Errors

`descriptives_table.tex` might exhibit different floating point rounding behavior on different machines, however results are consistent when reproduced on the same machine. 
- This is due to differences in CPU architecture and BLAS/LAPACK libraries, which may sum or reduce arrays in different orders. Floating‐point addition is not exactly associative, so different summation orders can produce different rounding errors.
- This table relies on the `PublicationbiasGMM()` function from `PublicationbiasPackage.R`, which optimises GMM using the BFGS method. Variance parameters are estimated from a non-linear function, which can magnify sensitivity to floating point differences and lead to numerical instability across machines.
- We replicated this analysis on three different machines and got the following range of results: 
    - _Replacement rate:_
      - SEs for v range from 1.43 to 1.47
      - SEs for theta range from 0.13 to 0.14
    - _Potential benefit Duration_
      - SEs for v range from 1.52 to 1.54.
- Git may also track changes in the following files, as they are generated from the same analysis as `descriptives_table.tex`.
    - bailychetty_ak.png
    - bma_plot_3_of_5.png 
    - stats.csv

