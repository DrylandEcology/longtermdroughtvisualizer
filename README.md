# Long-term Drought Visualizer Web Application

The Long-term Drought Visualizer (LTDV) is a web application built with RShiny and
hosted on Amazon Web Services (AWS) EC2 instance. This app simulates soil moisture and is an exploratory dashboard for historical and future climate and soil moisture.

## Overview

This application is a R based web application built with RShiny. The application 
serves as a graphical user interface for a mechanistic water-balance model, SOILWAT2, 
that has been proven in various publications. SOILWAT2 is a point
based (lat, long) model that requires inputs of climate, soil texture, and 
vegetation structure. The source code for this model can
be found [here](https://github.com/DrylandEcology/SOILWAT2). 

The LTDV app requires users to input site location, soils, and vegetation. If soils and vegetation are unknown soils are grabbed from [SSURGO](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/?cid=nrcs142p2_053627) via an API, and allometric equations estimation vegetation composition and stucture from climate.

 Based upon site coordinates gridded climate data is extracted from the [climatology lab](http://www.climatologylab.org/). Historical daily data (gridMET, 1980 - current year) is stored on the server. Future daily climate data (MACAv2, 2020 - 2099) is grabbed at the time of simulation for seven general circulation models (GCMs) and two representative concentration pathways (RCPs).

Once these inputs are generated / gathered, SOILWAT2 is executed for each climate scenario. Descriptive summary figures of climate and soil moisture are generated and available for download.

## Development

The application is a R application built with the RShiny web framework and hosted on an Elastic Cloud Computing (EC2) instance. The app relies on the suite of [Dryland Ecology's R packages](https://github.com/DrylandEcology) for running SOILWAT2. Historical weather data is provided by gridMET and stored on the EC2 instance. [Future weather data](https://climate.northwestknowledge.net/MACA/data_catalogs.php) is grabbed from an OPeNDAP server. Soils data is grabbed using the [soilDB R package](https://github.com/ncss-tech/soilDB).

## Deployment

This application is deployed via docker on an AWS EC2 instance with an amazon-linux OS.

The instance is deployed via the [EC2_AmazonLinux_Docker.yml](EC2_AmazonLinux_Docker.yml) cloud formation script. 

The app is deployed via [dockerfile](dockerfile).




