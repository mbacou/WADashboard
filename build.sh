#!/bin/bash

# description: - rebuild and deploy 'WADashboard' R package and Shiny application

cd "$(dirname "$(realpath "$0")")";

# Update built-in package datasets
Rscript ./data-raw/data.R

# Build and install system-wide
R CMD INSTALL --no-multiarch --with-keep.source --library=$WA_LIB ./

# Update timestamp so shiny-server knows to flush and restart the app
touch restart.txt
cp -f ./app.R $WA_ROOT
cp -f ./restart.txt $WA_ROOT
