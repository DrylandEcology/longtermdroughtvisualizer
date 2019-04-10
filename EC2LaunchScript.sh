#!/bin/bash

      exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console) 2>&1

      echo BEGIN
      date '+%Y-%m-%d %H:%M:%S'

      # setup development account
      useradd devel
      printf "%s\n" ltdsdev ltdsdev | passwd devel

      # set up sudo permissions
      echo "pr-candrews ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
      echo "pr-jhensleigh ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
      echo "pr-npayton-mccauslin ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
      echo "devel ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

      # install R
      yum install -y R

      # install git
      yum install -y git

      #install development packages so that some R packages will install
      # # png
      yum install -y libpng-devel

      # # rgdal
      yum -y install gdal gdal-devel
      yum -y install proj.x86_64 proj-devel.x86_64
      yum -y install proj-epsg.x86_64 proj-nad.x86_64

      # # rgeos
      yum -y install geos geos-devel

      # # ncdf4
      yum -y install netcdf-devel

      # # RCurl
      yum -y install libcurl-devel

      # # Devtools
      yum -y install openssl-devel

      # # AWS
      yum -y install libxml2-devel

      # install RStudio-Server
      wget https://download2.rstudio.org/rstudio-server-rhel-1.1.463-x86_64.rpm
      yum install -y --nogpgcheck rstudio-server-rhel-1.1.463-x86_64.rpm
      #rm rstudio-server-rhel-1.1.463-x86_64.rpm

      # install shiny and shiny-server
      wget https://download3.rstudio.org/centos6.3/x86_64/shiny-server-1.5.9.923-x86_64.rpm
      yum install -y --nogpgcheck shiny-server-1.5.9.923-x86_64.rpm
      # rm shiny-server-1.5.9.923-x86_64.rpm

      # install R packages/dependecies
      echo package download begin
      R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
      R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
      R -e "install.packages('fastmatch', repos='http://cran.rstudio.com/')"
      R -e "install.packages('iotools', repos='http://cran.rstudio.com/')"
      R -e "install.packages('Hmisc', repos='http://cran.rstudio.com/')"
      R -e "install.packages('zoo', repos='http://cran.rstudio.com/')"
      R -e "install.packages('data.table', repos='http://cran.rstudio.com/')"
      R -e "install.packages('gridExtra', repos='http://cran.rstudio.com/')"
      R -e "install.packages('RColorBrewer', repos='http://cran.rstudio.com/')"
      R -e "install.packages('forcats', repos='http://cran.rstudio.com/')"
      R -e "install.packages('Rcpp', repos='http://cran.rstudio.com/')"
      yes yes | R -e "install.packages('raster', repos='http://cran.rstudio.com/')"
      R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
      R -e "install.packages('circular', repos='http://cran.rstudio.com/')"
      R -e "install.packages('RSQLite', repos='http://cran.rstudio.com/')"
      R -e "install.packages('parallel', repos='http://cran.rstudio.com/')"
      R -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
      R -e "install.packages('rgeos', repos='http://cran.rstudio.com/')"
      R -e "install.packages('ncdf4', repos='http://cran.rstudio.com/')"
      R -e "install.packages('RCurl', repos='http://cran.rstudio.com/')"
      R -e "install.packages('gridGraphics', repos='http://cran.rstudio.com/')"
      R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
      R -e "install.packages('sp', repos='http://cran.rstudio.com/')"
      R -e "install.packages('maps', repos='http://cran.rstudio.com/')"
      R -e "install.packages('maptools', repos='http://cran.rstudio.com/')"
      R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"

      # TO DOGet dev version of plotly
      #devtools::install_github("ropensci/plotly")

      # clone and install rSOILWAT2
      git clone -b master --single-branch --recursive https://github.com/DrylandEcology/rSOILWAT2.git rSOILWAT2
      R CMD INSTALL rSOILWAT2

      # clone and install rSFSW2
      git clone -b feature_CDIfixes --single-branch https://github.com/DrylandEcology/rSFSW2
      R CMD INSTALL rSFSW2

      # clone and install Shiny App Code
      git clone -b master --single-branch https://code.chs.usgs.gov/candrews/longtermdroughtsimulator /srv/shiny-server/longtermdroughtsimulator

      # ensure that shiny server service file installed in the correct place
      cp /opt/shiny-server/config/systemd/shiny-server.service /etc/systemd/system
      systemctl restart shiny-server

      # give read and write permission to the app and to devel
      groupadd appallow
      usermod -a -G appallow devel
      usermod -a -G appallow shiny
      chown -R :appallow /srv/shiny-server/longtermdroughtsimulator
      chmod -R g+rwx /srv/shiny-server/longtermdroughtsimulator

      # download data from the S3 environment to the longtermdroughtsimulator folder
      cd /srv/shiny-server/longtermdroughtsimulator
      aws s3 sync s3://sbsc-upload-data .


      echo END
      date '+%Y-%m-%d %H:%M:%S'
