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

To configure, extend, and rebuild this application:

1. Edit environment variable `WA_DATA` in file `./.Renviron` pointing to the root location of your WA+ model output (collection of CSV and NetCDF files). Any local or cloud-based storage type may be implemented (incl. Amazon S3).

2. Edit 2 configuration files:  
    - [List of river basins](https://github.com/mbacou/WADashboard/blob/main/data-raw/json/ISO3.json)
    - [Catalog of contextual geospatial layers](https://github.com/mbacou/WADashboard/blob/main/data-raw/json/LAYERS.json)
    
3. Rebuild the R package and reinstall by executing `./build.sh`.


## Application Deployment

A Docker [image file](https://github.com/mbacou/WADashboard/Dockerfile) is provided for deployment to AWS ECS (Fargate). This image includes Shiny Server Community Edition and (for development only) RStudio Server Preview.

You can build and test this image locally with:

```sh
# replace {pwd} with a chosen password for the default rstudio user
docker build -t wadashboard .
docker run --name wadashboard PASSWORD={pwd} -p 80:3838 -p 8787:8787 wadashboard
```

Once you've established that this container runs locally, it can be deployed to AWS ECS (see [./deploy.sh](https://github.com/mbacou/WADashboard/deploy.sh) for an automated deployment script):

```sh
# Authenticate your local Docker client
aws ecr get-login-password --region {region} | docker login --username AWS --password-stdin {aws_account_id}.dkr.ecr.{region}.amazonaws.com
# Tag your image with your Amazon ECR registry
docker tag wadashboard {aws_account_id}.dkr.ecr.{region}.amazonaws.com/wadashboard
# Push the image to your AWS ECR registry
docker push {aws_account_id}.dkr.ecr.{region}.amazonaws.com/wadashboard
```

The dashboard application is served on the host at `http://localhost/WADashboard/`. RStudio IDE is reachable at `http://localhost:8787/` (replace `localhost` with your container's public IP or domain name).


## License

This package is licensed under the terms of the [GNU General Public
License](https://www.gnu.org/licenses/gpl-3.0.html) version 3 or later.

Copyright 2021-2022 International Water Management Institute (IWMI).
