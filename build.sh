#!/bin/bash

# description: - rebuild and deploy 'wcapp.merlin' R package

cd "$(dirname "$(realpath "$0")")";

# Update built-in package datasets
Rscript ./data-raw/data.R

# Build and install system-wide
R CMD INSTALL --no-multiarch --with-keep.source --library='/usr/local/lib/R/site-library' ./

# Update timestamp so shiny-server knows to flush and restart the app
touch restart.txt
cp -f ./app.R /home/shiny/WAdashboard/
cp -f ./restart.txt /home/shiny/WAdashboard/
