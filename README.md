# Baseline Correction of Spectra - baseline

## Installation

``` r
# Install release version from CRAN  
install.packages("baseline")  
# Install development version from GitHub  
devtools::install_github("khliland/baseline")
```

## Contents

- S4 interface baseline() with a range of baseline correction methods
    - 4S Peak Filling
    - Asymmetric Least Squares - ALS
    - IRLS
    - LowPass filtering
    - Median Window
    - Modified polynomial fitting
    - Peak Detection
    - Rolling Ball
    - RF baseline
    - Shirley
    - TAP
    - Common plotting function
- Customized baseline correction
    - Interval stretch/compress for more adaptive baseline
- Baseline optimisation for prediction and classification
    - Design of Experiments
    - Running of DoE
    - Analysis of results
- Tcl/Tk based Graphical User Interface for baseline correction
