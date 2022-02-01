#!/bin/bash

# description: - rebuild and deploy 'WADashboard' R package and Shiny application

cd "$(dirname "$(realpath "$0")")";

# Update built-in package datasets
Rscript ./data-raw/data.R

# Build and install
if [ "$WA_LIB" != "" ]
  then
    R CMD INSTALL --no-multiarch --with-keep.source --library=$WA_LIB ./
  else
    R CMD INSTALL --no-multiarch --with-keep.source ./
fi

# Publish to Shiny server root, if any
if [ "$WA_ROOT" != "" ]
  then
    # Update timestamp so shiny-server knows to flush and restart the app
    touch restart.txt
    cp -f ./app.R $WA_ROOT
    cp -f ./restart.txt $WA_ROOT
fi
