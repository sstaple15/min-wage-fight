###########################################################################
# Goal:    Collect CPS ORG microdata from 2015-2019
# Author:  Stephen Stapleton
# Created: 2021-09-20
# Updated: 2021-09-22
###########################################################################

# rake_pums.R ->

# Steps of this script:
# (1) Read in CPS ORG data from file
# (2) Perform any necessary data imputation for CPS ORG
# (3) Limit data to only necessary attributes

# -> impute_wages.R

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'survey', 'srvyr', 'pewmethods', 'haven')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

if(length(new.packages)) install.packages(new.packages); rm(list.packages, new.packages)

require(tidyverse)
require(tidycensus)
require(magrittr)
require(here)
require(survey)
require(srvyr)
require(rake_survey)
require(haven)

#--------------------------------------------------------------------------
# Read in CPS ORG data from file

cps.file.list <- list.files( path = '/projects/sstapleton/meta/public_use_data/cepr_org/' )

cps.list <- list()

for ( f in cps.file.list ) {
  
  message( paste0('Reading in file: ', f) )
  
  cps.list[[f]] <- read_dta( paste0('/projects/sstapleton/meta/public_use_data/cepr_org/', f) )
  
}; rm( f, cps.file.list )

#--------------------------------------------------------------------------
# Perform any necessary data imputation for CPS ORG

# NOTE: if any have missingness, will need to impute via MICE
acs.raw %<>% impute_vars( seed = 11131992 )

#--------------------------------------------------------------------------
# Limit data to only necessary attributes