#!/usr/bin/env Rscript

#------------------------------------------------------------------------------#
# rSFSW2: FRAMEWORK FOR SOILWAT2 SIMULATIONS: CREATING SIMULATION RUNS,
#         EXECUTING SIMULATIONS, AND AGGREGATING OUTPUTS

#----- LICENSE
#    Copyright (C) 2017 by `r packageDescription("rSFSW2")[["Author"]]`
#    Contact information `r packageDescription("rSFSW2")[["Maintainer"]]`

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License.

#------ DISCLAIMER:
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#------ NOTES:
#  - You get an overview by: `r package?rSFSW2`
#  - An index of functionality is displayed by: `r help(package = "rSFSW2")`
#------------------------------------------------------------------------------#


################################################################################

library("rSFSW2")


################################################################################
#------ 1) CREATE A NEW / LOAD AN EXISTING SIMULATION PROJECT ------------------

# If code is run non-interactively or if this is a test project:
# then current working directory must be folder of projects,
# e.g., rSFSW2_tools/Test_projects/TestPrj4
mainPath <- getwd()
dir_prj <- file.path(mainPath, "data/rSFSW2_ProjectFiles")


SFSW2_prj_meta <- init_rSFSW2_project(
  fmetar = file.path(dir_prj, "SFSW2_project_descriptions.R"), update = TRUE,
  verbose = TRUE, print.debug = FALSE)

setwd(mainPath)
