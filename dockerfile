FROM rocker/shiny-verse
LABEL maintainer="candrews@usgs.com"

### COPY SHINY SERVER CONFIGURATION FILE - NEED TO ACCESS THESE OUTSIDE OF DOCKER
COPY longtermdroughtsimulator/shiny-server.conf /etc/shiny-server/shiny-server.conf

### INSTALL Install system dependencies for R packages #########################

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libssl-dev  \
  libxml2-dev \
  libgdal-dev \
  libproj-dev \
  gdal-bin

### INSTALL R PACKAGES ########################################################

RUN ["install2.r", "shinydashboard", "leaflet", "ggplot2", "data.table", "ncdf4", "lubridate", "foreach", "doParallel", "RColorBrewer", "hexbin", "circular", "zoo", "forcats"]
# hexbin is a dependecy of plotly
# circular is a dependecy of rSW2utils
#"Rcpp", "forcats", "raster", "RSQLite", "rgdal", "rgeos", "RCurl", "httr", "rmarkdown"]

### R Packages installed from source ------------------------------------------

COPY ./Packages /usr/local/app/LTDV/Packages

RUN R CMD INSTALL /usr/local/app/LTDV/Packages/htmlwidgets
RUN R CMD INSTALL --library= /usr/lib64/R/library /usr/local/app/LTDV/Packages/plotly
RUN R -e 'install.packages("/usr/local/app/LTDV/Packages/rSW2utils.tar.gz", repos = NULL, type = "source")'
RUN R CMD INSTALL /usr/local/app/LTDV/Packages/rSOILWAT2
# RUN R CMD INSTALL /usr/local/app/LTDV/Packages/rSFSW2

### CREATE VOLUME FOR LARGE DATASETS ##########################################
VOLUME /srv/shiny-server/ltdv/Data/

### COPY SHINY APP ############################################################
COPY ./shiny-app/ /srv/shiny-server/ltdv

### CHANGE PERMISSIONS TO SHINY FILES SO SHINY USER CAN ACCESS ################
#https://serverfault.com/questions/772227/chmod-not-working-correctly-in-docker
#https://support.rstudio.com/hc/en-us/articles/219044787-Root-requirements-for-Shiny-Server

RUN usermod -aG shiny shiny

RUN /bin/bash -c 'ls -la; chown -R root:shiny /srv/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rwx /srv/shiny-server/; ls -la'

#read / write
RUN /bin/bash -c 'ls -la; chown -R root:shiny /var/log/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rw /var/log/shiny-server/; ls -la'

RUN /bin/bash -c 'ls -la; chown -R root:shiny /opt/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rw /opt/shiny-server/; ls -la'

RUN /bin/bash -c 'ls -la; chown -R root:shiny /etc/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rw /etc/shiny-server/; ls -la'

### Usage instructions ########################################################

# Build the images with docker ------
# > cd /usr/local/app/LTDV
# > docker build --tag ltdv .

# Run with dockerfile ---------------
# >           docker run -itd \
# >              -p 3838:3838 \
# >              -v /srv/shinylog/:/var/log/shiny-server/ \
# >              -v /usr/local/app/Data/:/srv/shiny-server/ltdv/Data/ \
# >              ltdv
# then visit online at yourip:3838/ltdv

# Build with docker compose -------------


# to debug  ----------
# > docker run -it --rm --entrypoint /bin/bash ltdv

# log files stored @ /srv/shinylog
