# Long Term Drought Simulator

The Long Term Drought Simulator (LTDS) is a web application built with R Shiny and hosted through amazon web services (AWS) that simulates soil moisture conditions for specific, user-defined locations.

## Overview

This application is a R based web application built with R Shiny. The application serves as a graphical user interface for a mechanistic water-balance model, SOILWAT2, that has been proven in various publications. The source code for this model can be found [here](https://github.com/DrylandEcology/SOILWAT2). SOILWAT2 is a point based (lat, long) model that requires users inputs of climate, soil texture, and vegetation structure in order to simulate water balance and soil moisture conditions for any time periods in which it is provided climate data.

The application's intent is to allow for user definition of site, specific attributes (location, soil, vegetation), and what is returned is long-term historical (1915 - 2015) and future (2015 - 2099) simulations of water balance for that site. Simulation occurs 'on the fly', as users are allowed to enter unique scenarios. The raw data from the simulation is available for download, and descriptive graphics of the location's moisture conditions are presented for exploration.


## Use

This project depends on R, RStudio, and R Shiny.

The application was built around two R packages: [rSOILWAT2](https://github.com/DrylandEcology/rSOILWAT2), a R plugin for the C based code of SOILWAT2, and [rSFSW2](https://github.com/DrylandEcology/rSFSW2), a wrapper for rSOILWAT2 that gather and formats inputs for model use.

For a full description of dependent programs and packages refer to the EC2LaunchScript.sh, which is used to deploy the app on Linux2 Amazon AMI instances.

Additionally, use of this application relies on pre-built weather databases stored in the Amazon S3 LTDS bucket.
