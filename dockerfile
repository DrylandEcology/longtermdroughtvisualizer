FROM rocker/shiny-verse
LABEL maintainer="candrews@usgs.com"
COPY longtermdroughtsimulator/shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libssl-dev  \
  libxml2-dev \
  libgdal-dev \
  libproj-dev \
  gdal-bin

RUN ["install2.r", "fastmatch", "iotools", "iotools", "Hmisc", "data.table", "gridExtra", "RColorBrewer", "Rcpp", "forcats", "Rcpp", "raster", "leaflet", "circular", "RSQLite", "rgdal", "rgeos", "ncdf4", "RCurl", "sp", "maps", "maptools", "shinydashboard", "httr", "hexbin", "rmarkdown", "zoo"]

COPY ./Packages /usr/local/app/LTDV/Packages

RUN R CMD INSTALL /usr/local/app/LTDV/Packages/htmlwidgets
RUN R CMD INSTALL --library= /usr/lib64/R/library /usr/local/app/LTDV/Packages/plotly
RUN R -e 'install.packages("/usr/local/app/LTDV/Packages/rSW2utils.tar.gz", repos = NULL, type = "source")'
RUN R CMD INSTALL /usr/local/app/LTDV/Packages/rSOILWAT2
RUN R CMD INSTALL /usr/local/app/LTDV/Packages/rSFSW2

COPY ./shiny-app/ /srv/shiny-server/

VOLUME /usr/local/app/LTDV/Data:/srv/shiny-server/Data

RUN usermod -aG shiny shiny

#https://serverfault.com/questions/772227/chmod-not-working-correctly-in-docker
#https://support.rstudio.com/hc/en-us/articles/219044787-Root-requirements-for-Shiny-Server
RUN /bin/bash -c 'ls -la; chown -R root:shiny /srv/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rwx /srv/shiny-server/; ls -la'

#read / write
RUN /bin/bash -c 'ls -la; chown -R root:shiny /var/log/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rw /var/log/shiny-server/; ls -la'

RUN /bin/bash -c 'ls -la; chown -R root:shiny /opt/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rw /opt/shiny-server/; ls -la'

RUN /bin/bash -c 'ls -la; chown -R root:shiny /etc/shiny-server/; ls -la'
RUN /bin/bash -c 'ls -la; chmod -R g+rw /etc/shiny-server/; ls -la'
