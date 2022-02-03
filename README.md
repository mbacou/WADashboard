<img class="float-right img-thumbnail ml-3" src="wa.png"/>

# Visualization Dashboard for WA+ Water Accounts 

Development package for a [R/Shiny](https://shiny.rstudio.com/) data visualization tool
for [Water Accounting+](https://www.wateraccounting.org/) hydrological models.

The **water accounting** approach integrates hydrological processes with land use,
managed water flows and the services that result from water consumption in river
basins. Its objective is to achieve equitable and transparent water governance for all
users and a more sustainable water balance.

**Water Accouting+** is a multi-institutional effort from a consortium of
international research centers ([IWMI](https://www.iwmi.cgiar.org/),
[UNESCO-IHE](https://www.un-ihe.org/),
[FAO](https://www.fao.org/land-water/water/en/), and
[WWAP](https://en.unesco.org/wwap)). These institutions are neither politically or
geographically connected to any river basin.

## Installation

You can install this package from the development version on GitHub:

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("mbacou/WADashboard")
```

And then preview the dashboard in your browser with:

```r
if (!require("shiny")) install.packages("shiny")
shiny::shinyAppDir(system.file("app", package="WADashboard"))
```

If you choose to clone this repo instead, the application can be previewed with:

```r
shiny::runApp()
```

## Documentation

For complete R package documentation, technical guides, and deployment tips, see the [package vignettes](https://mbacou.github.io/WADashboard/articles/).


## License

This package is licensed under the terms of the [GNU General Public
License](https://www.gnu.org/licenses/gpl-3.0.html) version 3 or later.

Copyright 2021-2022 International Water Management Institute (IWMI).
