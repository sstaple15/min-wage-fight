###########################################################################
# Goal:    Reweight ACS limited sample
# Author:  Stephen Stapleton
# Created: 2021-08-15
# Updated: 2021-08-27
###########################################################################

# Steps of this script:
# (1) Read in both limited ACS data and full ACS data
# (2) Perform any imputation steps necessary
# (3) Set target for raking based on full ACS and selected variables
# (4) Run iterative ranking and confirm closeness of final to target

#--------------------------------------------------------------------------
# Setup

list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'pewmethods', 'mice') # check for packages
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

if(length(new.packages)) install.packages(new.packages); rm(list.packages, new.packages)

require(tidyverse)
require(tidycensus)
require(magrittr)
require(here)
require(pewmethods)
require(mice)

#--------------------------------------------------------------------------
# Read in data inputs

source( INSERT NAME OF LIMITING SCRIPT )

acs.raw <- NON LIMITED ACS DATA
acs.lim <- LIMITED ACS DATA

# get identifier to limit raw data after imputation
acs.lim %<>% pull( IDENTIFIER ) %>% unique( )

#--------------------------------------------------------------------------
# set target for raking and impute

# set weighting variables of interest. As of 08/27/2021:
# - gender
# - age bins
# - marital status
# - white x ( 16-64 | 65+ )
# - black x ( 16-64 | 65+ )
# - nonwhite/nonblack x ( 16-64 | 65+ )
# - hispanic x ( 16-64 | 65+ )
# - education level for 16+
# - part-time status

# NOTE: to create interaction for raking, using VAR1:VAR2 nomenclature
rake.vars <- INSERT LIST OF RAKING VARS

# FIRST: exclude self-employed and any individuals working abroad?
acs.raw %<>%
  filter( AGEBIN >= 16 ) %>% # confirm acs.raw does not include < 16
  filter( EMPLOYED == T )    # remove all non-employed prior to target

# check data structure for each of our rake.vars
lapply( acs.raw %>% select( rake.vars ), levels )

# NOTE: if any have missingness, will need to impute via MICE
acs.raw %<>% impute_vars( seed = 11131992 )

# create rake target
rake.target <- create_raking_targets( 
  data = acs.raw,
  vars = rake.vars,
  wt   = WEIGHT )

#--------------------------------------------------------------------------
# rake to determine new weights

# get full imputed data for each client in limited sample
acs.lim <- acs.raw %>% filter( IDENTIFIER %in% acs.lim )

# rake to achieve weights
acs.lim %<>% mutate( rake_weight = rake_survey( pop_margins = target ) )

# check Kish approximatin of design effect
calculate_deff( acs.lim$rake_weight )

# NOTE: if trimming is needed, use the following:
acs.lim $<>$ mutate( trim_weight = trime_weights( lower_quantile = 0.05,
                                                  upper_quantile = 0.95 ) )

get_totals("recage", acs.lim, 
           wt = c(WEIGHT, "trim_weight"), 
           digits = 1)
