FROM rocker/shiny:4.1.2
ARG DEBIAN_FRONTEND=noninteractive
ARG APP=WADashboard

USER root

# Local or mounted data storage endpoint
ENV WA_DATA="/data"
RUN mkdir $WA_DATA

# Base
RUN \
  apt update && apt install -y \
  software-properties-common dirmngr apt-transport-https lsb-release ca-certificates \
  libgit2-dev make curl git wget sudo \
  libcurl4-openssl-dev libssl-dev libpng-dev \
  libicu-dev libglpk-dev libgmp3-dev libxml2-dev zlib1g-dev \
  libudunits2-dev libgdal-dev gdal-bin libgeos-dev libproj-dev \
  binutils libv8-dev unixodbc unixodbc-dev odbc-postgresql libsqliteodbc

# Clean up
RUN apt autoremove -y  

# Install R dependencies from Package Manager snapshots as of 2022.01.03
RUN \
  R -e "install.packages(c('devtools', \
  'sf', 'terra', 'data.table', 'bslib', 'r2d3', 'lubridate', 'scales', \
  'leaflet.extras', 'fresh', 'shinybusy', 'shinyWidgets', 'bs4Dash', \
  'stringr', 'highcharter', 'rmarkdown' \
  ), \
  repos='https://packagemanager.rstudio.com/all/2022-01-03+Y3JhbiwyOjQ1MjYyMTU7NTY4Qjk1ODA')"

# Install application R package from Github
RUN \
  R -e "devtools::install_github('mbacou/${APP}', dependencies=TRUE)"

# Copy the app directory into the image
RUN mkdir /srv/shiny-server/${APP}
COPY ./.Renviron /srv/shiny-server/${APP}/
COPY ./app.R /srv/shiny-server/${APP}/
COPY ./restart.txt /srv/shiny-server/${APP}/
RUN chown -R shiny:shiny /srv/shiny-server/

# Install RStudio (for development only)
ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=2021.09.1+372
ENV DEFAULT_USER=rstudio
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

EXPOSE 8787

CMD ["/init"]
