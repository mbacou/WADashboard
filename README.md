# Dashboard for WA+ Water Accounts

Development package for a [Shiny](https://shiny.rstudio.com/) data visualization tool
that collects results from [Water Accounting+](https://www.wateraccounting.org/) (WA+)
hydrological models.

Water accounting integrates hydrological processes with land use, managed water flows
and the services that result from water consumption in river basins. Its objective is
to achieve equitable and transparent water governance for all users and a
more sustainable water balance.

WA+ is a multi-institutional effort from a consortium of international research
centers ([IWMI](https://www.iwmi.cgiar.org/), [UNESCO-IHE](https://www.un-ihe.org/),
[FAO](https://www.fao.org/land-water/water/en/), and
[WWAP](https://en.unesco.org/wwap)). These institutions are neither politically or
geographically connected to any river basin.

## Installation

You can install this package from the development version on GitHub:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("mbacou/WADashboard")
```

The app can be previewed in the RStudio IDE with:

```r
source("./app.R")
```

## Documentation

In addition to the usual R package documentation, we also have extensive docs at:
[https://mbacou.github.io/WADashboard/](https://mbacou.github.io/WADashboard/).

To configure and extend this application:

1. Define environment variable `WA_DATA` pointing to the root location of your WA+
model output (collection of CSV and NetCDF files). Any local or cloud-based
storage type may be implemented (incl. Amazon S3).

2. Edit 2 configuration files:  
    - [List of river basins](https://github.com/mbacou/WADashboard/blob/main/data-raw/json/ISO3.json)
    - [Catalog of contextual spatial layers](https://github.com/mbacou/WADashboard/blob/main/data-raw/json/LAYERS.json)
    
3. Reload the package with `devtools::load_all(".")` or install system-wide with
`./build.sh` (edit target locations as needed).


## Application Deployment

Two Docker files are provided for deployment to AWS Fargate.


## License

This package is licensed under the terms of the [GNU General Public
License](https://www.gnu.org/licenses/gpl-3.0.html) version 3 or later.

Copyright 2021-2022 International Water Management Institute (IWMI).
