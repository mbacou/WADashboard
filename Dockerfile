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
  curl git wget sudo
  libcurl4-openssl-dev libssl-dev
  libudunits2-dev libgdal-dev libgeos-dev libproj-dev r-base-dev \
  binutils libv8-dev unixodbc unixodbc-dev odbc-postgresql libsqliteodbc

# Install R package from Github
RUN \
  R -e "install.packages(c(
  'remotes', 'sf', 'terra' \
  ),  \
  dependencies=TRUE, \
  repos='https://packagemanager.rstudio.com/cran/__linux__/focal/2022-01-01')"

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
