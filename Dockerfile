FROM rocker/shiny:3.5.1

# Set locale
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

# R Package config
ENV GITHUB_TOKEN "9a192c1065671f61ef3e3809466de74bc4d83caa"
# Local download location for raster files
ENV DATA_DIR "/data"

# Save climatic rasters into this dir
RUN mkdir DATA_DIR
ADD . /wcutils

# Base
RUN \
  apt update && apt -y dist-upgrade && \
  apt -y install software-properties-common dirmngr apt-transport-https lsb-release ca-certificates curl wget

# Install spatial deps and R base
RUN \
  apt install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev r-base-dev && \
  apt install -y binutils libv8-dev unixodbc unixodbc-dev odbc-postgresql libsqliteodbc

# Install R package from Github
RUN \
  Rscript -e "install.packages('remotes')" && \
  Rscript -e "remotes::install_github('mbacou/WADashboard',  \
    dependencies = TRUE, auth_token = Sys.getenv('GITHUB_TOKEN'))" && \

# Clean up
RUN apt autoremove -y

# Rebuild package data cache (7GB data download SLOW)
RUN chmod u+x ./build.sh
# Uncomment next line in real deployment scenario
# RUN ./install.sh

# Apache ports
EXPOSE 80
EXPOSE 443
EXPOSE 8004

# Start non-daemonized webserver
CMD apachectl -DFOREGROUND
