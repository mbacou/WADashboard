FROM rocker/shiny:4.1.2
ARG DEBIAN_FRONTEND=noninteractive
ARG RSTUDIO=2021.09.1-372
ARG APP=WADashboard

USER root
WORKDIR /root/

# Set locale
ENV PATH=/usr/lib/shiny-server/bin:/usr/lib/rstudio-server/bin:$PATH

# Local download location for raster files
ENV WA_DATA="/data"

# Save climatic rasters into this dir
RUN mkdir $WA_DATA

# Base
RUN \
  apt update && apt install -y \
  software-properties-common dirmngr apt-transport-https lsb-release ca-certificates \
  make curl git wget pandoc sudo
  libcurl4-openssl-dev libssl-dev libpng-dev
  libicu-dev libglpk-dev libgmp3-dev libxml2-dev zlib1g-dev
  libudunits2-dev libgdal-dev gdal-bin libgeos-dev libproj-dev \
  binutils libv8-dev unixodbc unixodbc-dev odbc-postgresql libsqliteodbc

# Install R packages from Package Manager
RUN \
  R -e "install.packages(c(
  'remotes', 'sf', 'terra', 'data.table', 'bslib', 'r2d3', 'lubridate', 'scales', \
  'leaflet.extras', 'fresh', 'shinybusy', 'shinyWidgets', \
  'stringr', 'highcharter', 'rmarkdown' \
  ),  \
  dependencies=TRUE, \
  repos='https://packagemanager.rstudio.com/cran/__linux__/focal/2022-01-01')"

# Install R package from Github
RUN \
  R -e "remotes::install_github('mbacou/${APP}', dependencies=TRUE)"

# Clean up
RUN apt autoremove -y

# Copy the app directory into the image
RUN mkdir /srv/shiny-server/${APP}
COPY ./.Renviron /srv/shiny-server/${APP}/
COPY ./app.R /srv/shiny-server/${APP}/
COPY ./restart.txt /srv/shiny-server/${APP}/

# Install RStudio
RUN \
  wget --quiet https://s3.amazonaws.com/rstudio-ide-build/server/bionic/amd64/rstudio-server-${RSTUDIO}-amd64.deb && \
  apt install ./rstudio-server-${RSTUDIO}-amd64.deb && \
  rm -f rstudio-server-${RSTUDIO}-amd64.deb && \
  echo "auth-required-user-group=staff" >> /etc/rstudio/rserver.conf \
  echo "server-app-armor-enabled=0" >> /etc/rstudio/rserver.conf

# Set password so that we can login
RUN \
  useradd -s /bin/bash -m rstudio
  echo "rstudio:rstudio" | chpasswd
  addgroup rstudio staff
  adduser rstudio shiny

# RStudio port
EXPOSE 8787

# Run
CMD ["/usr/lib/rstudio-server/bin/rserver"]
