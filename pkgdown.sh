#!/bin/bash

# description: - publish package documentation

cd "$(dirname "$(realpath "$0")")";

# Build package documentation and publish HTML to shiny-server root
Rscript -e 'pkgdown::build_site("./", preview=FALSE, new_process=TRUE)'
