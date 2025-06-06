<!-- badges: start -->
[![](https://zenodo.org/badge/DOI/10.5281/zenodo.15606592.svg)](https://doi.org/10.5281/zenodo.15606592)
<!-- badges: end -->

# **mobsim_app** - Simulation and visualisation of biodiversity patterns across spatial scales using the `mobsim` R package

## Authors

*Katharina Gerstner, Felix May, Dan McGlinn and Alban Sagouis*

## Description

This is an interactive tool that aims to simulate and visualize multiple biodiversity patterns across spatial scales using the `mobsim` R-package. A **Thomas model** is used to simulate point pattern distributions of individual species. Key parameters of the simulation are:

* **total number of individuals**,
* **total number of species**,
* the coefficient of variation in the species-abundance distribution that determines the **eveness of the abundances**,
* **spatial aggregation of species**.

## The tool

1. simulates locations of individuals of different species in a location (plot, area);

2. plots biodiversity patterns such as

    * species-abundance distributions (SAD) using Preston octave plot and rank-abundance curve,
    * spatial distribution of individuals within a unit area,
    * species accummulation curves (SAC), species-area relationships (SAR), and the distance-decay curve.

## Run the app on the web

The app is hosted at the following url: <https://github.com/AlbanSagouis/mobsim_app> forked from <https://github.com/MoBiodiv/mobsim_app>.

## Run the app locally

The app can be run locally. Several R packages are required for locally hosting the app, these can be installed
from the R terminal:

```r
install.packages(c("shiny", "shinyBS", "pals", "shinyjs", "devtools", "markdown",
  "mobsim"))
```

Once those dependencies are installed you can run the app from the R terminal using:

```r
shiny::runGitHub("albansagouis/mobsim_app")
```

## License

GNU GPL

## The `mobsim` package

The underlying `mobsim` package can be installed from CRAN with
`install.packages("CRAN")` or from GitHub with
`remotes::install_github("MoBiodiv/mobsim")`. `mobsim` version 0.3.2 or above is
needed.
