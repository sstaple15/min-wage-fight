###########################################################################
# Goal:    Limit ACS PUMS microdata
# Author:  Stephen Stapleton
# Created: 2021-08-15
# Updated: 2021-09-22
###########################################################################

# collect_pums.R ->

# Steps of this script:
# (1) Filter out observations that are not relevant

# -> rake_pums.R

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'survey', 'srvyr')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

if(length(new.packages)) install.packages(new.packages); rm(list.packages, new.packages)

require(tidyverse)
require(tidycensus)
require(magrittr)
require(here)
require(survey)
require(srvyr)

#--------------------------------------------------------------------------
# Filter out observations that are not relevant

# NOTE: THIS IS A GENERAL OUTLINE TO EDIT BASED ON EPI INFO:
# https://www.epi.org/publication/minimum-wage-simulation-model-technical-methodology/

pums.list %<>% lapply( function(x) {
  
  x %<>%
    filter(    WRK == 1      & !is.na(WRK)  ) %>% # remove any who did not work last week
    filter(    AGEP >= 16    & !is.na(AGEP) ) %>% # remove any minors
    filter(    WAGP > 0      & !is.na(WAGP) ) %>% # remove anyone who did not have a wage
    filter( !( MIL == 1   & !is.na(MIL) )   ) %>% # remove any active military
    select( -c(WRK, AGEP, WAGP, MIL) )
} )