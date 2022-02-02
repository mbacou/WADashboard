FROM rocker/shiny:4.1.2
ARG DEBIAN_FRONTEND=noninteractive
ARG APP=WADashboard

# Shared library
ENV WA_LIB="/usr/local/lib/R/site-library"
# Local or mounted data storage endpoint
ENV WA_DATA="/data"
RUN mkdir $WA_DATA

# Base
RUN \
  apt update && apt install -y --no-install-recommends \
  software-properties-common dirmngr apt-transport-https lsb-release ca-certificates \
  libgit2-dev make curl git wget sudo \
  libcurl4-openssl-dev libssl-dev libpng-dev \
  libicu-dev libglpk-dev libgmp3-dev libxml2-dev zlib1g-dev \
  libudunits2-dev libgdal-dev gdal-bin libgeos-dev libproj-dev \
  binutils libv8-dev unixodbc unixodbc-dev odbc-postgresql libsqliteodbc

# Clean up
RUN apt autoremove -y

# Install R dependencies from Package Manager snapshots as of 2022.02.01
RUN \
  R -e "install.packages(c('remotes', 'gtools', 'TTR', \
  'sf', 'terra', 'data.table', 'bslib', 'r2d3', 'lubridate', 'scales', \
  'leaflet.extras', 'fresh', 'shinybusy', 'shinyWidgets', 'bs4Dash', \
  'stringr', 'highcharter'), \
  repos='https://packagemanager.rstudio.com/all/2022-02-01+Y3JhbiwyOjQ1MjYyMTU7NDU1MjVERTc')"

# Install application R package from Github
RUN \
  R -e "remotes::install_github('mbacou/${APP}', \
  dependencies=TRUE, upgrade='default', build_manual=TRUE, build_vignettes=TRUE)"

# Remove boilerplate
RUN rm -rf /srv/shiny-server/

# Serve Shiny server on port 80
COPY ./shiny-server.conf /etc/shiny-server/

# Add app directory into Shiny server root
COPY ./.Renviron.deploy /home/shiny/.Renviron
RUN mkdir -p /srv/shiny-server/${APP}/docs
COPY ./app.R /srv/shiny-server/${APP}/
COPY ./restart.txt /srv/shiny-server/${APP}/
RUN chown -R shiny:shiny /srv/shiny-server
RUN chown -R shiny:shiny /home/shiny

# Install RStudio (optional, for development only)
ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=2021.09.1+372
ENV DEFAULT_USER=rstudio
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

# Custom RStudio IDE settings (optional)
COPY ./.Renviron.deploy /home/rstudio/.Renviron
RUN chown -R rstudio:rstudio /home/rstudio

# Ensure rstudio user can publish to Shiny server root
RUN usermod -aG shiny rstudio

EXPOSE 80
EXPOSE 8787

CMD ["/init"]
