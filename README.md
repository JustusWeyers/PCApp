
# PCApp

Hydrological time series from monitoring networks are a valuable source
of information on the dynamics of the water cycle. Depending on the
issue at hand, it is hoped that the investigation of these will yield
insights into small-scale, possibly anthropogenic or large-scale,
possibly climatic influences on the landscape water balance. In Germany,
some programs for recording runoff volumes, groundwater or lake levels,
for example, have already been in existence for decades. This web
application is intended to offer the possibility of investigating these
time series using principal component analysis (PCA).

It should therefore be possible to install and run the PCApp locally as
an R package, see [Local execution](#local-execution). On the other
hand, it is intended to make the application available online, see [Web
Application](#web-application)

## Local execution

### Installation

You can install the development version of PCApp via GitHub:

``` r
# 1. Install remotes
utils::install.packages("remotes")
# 2. Install PCApp from GitHub
remotes::install_github("JustusWeyers/PCApp")
```

### Example

Launch shiny application like so:

``` r
library(PCApp)
PCApp::run_app()
```

## Web Application

in the making

<!-- # References -->
<!-- <div id="refs"></div> -->

------------------------------------------------------------------------

Justus Weyers, 12.03.2024

*– Translated by DeepL*
