###########################################################################
# Goal:    Rake ACS weights to overall state population
# Author:  Stephen Stapleton
# Created: 2021-09-16
# Updated: 2021-09-22
###########################################################################

# collect_pums.R ->

# Steps of this script:
# (1) Collect population we will be raking to from ACS
# (2) Impute data in the case of missingness
# (3) Rake to new weights

# -> read_cpsorg.R

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'survey', 'srvyr', 'pewmethods')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

# install if not currently
if(length(new.packages)) { install.packages(new.packages);
  if(grepl('pewmethods', new.packages)) { # exception for git-only packages
    if(!( 'devtools' %in% installed.packages()[, 'Package'] )) { install.packages('devtools') }
    require(devtools); install_github("pewresearch/pewmethods") } }; rm(new.packages) 

lapply(list.packages, require, character.only = T); rm(list.packages) # load into environment

#--------------------------------------------------------------------------
# Collect population we will be raking to from ACS

# NOTE: THIS IS A GENERAL OUTLINE TO EDIT BASED ON EPI INFO:
# https://www.epi.org/publication/minimum-wage-simulation-model-technical-methodology/

# - gender
# - age bins
# - white x ( 16-64 | 65+ )
# - black x ( 16-64 | 65+ )
# - nonwhite/nonblack x ( 16-64 | 65+ )
# - hispanic x ( 16-64 | 65+ )
# - education level for 16+
# - part-time status
# - 

# take a look at the acs variables available
acs.dict <- load_variables( 2019, 'acs5' )

acs.2019 <- get_acs(
  variables = c('B01001_026',  # total female
                'C18121_002',  # worked full-time year-round
                'B06001_004',  # age 18-24
                'B06001_005',  # age 25-34
                'B06001_006',  # age 35-44
                'B06001_007',  # age 45-54
                'B06001_008',  # age 55-59
                'B01001A_007'  # age 18-19 male white
                 )
)

#standardize rake variable names and save
acs.2019 %<>% mutate()

rake.vars <- INSERT ListCallback

# create raking targets
rake.goal <- create_raking_targets(
  data = acs.2019,
  vars = rake.vars,
  wt = WEIGHT
)

#--------------------------------------------------------------------------
# Impute data in the case of missingness

pums.2019 %<>% impute_vars( seed = 11131992 )

#--------------------------------------------------------------------------
# Rake to new weights

# rake to the new weights
pums.2019 %<>% mutate( rake_weight = rake_survey( pop_margins = rake.goal) )

# check Kish approximation of design effect
calculate_deff( pums.2019$rake_weight )

# NOTE: if trimming is needed, use the following:
pums.2019 %<>% mutate( trim_weight = trim_weights( lower_quantile = 0.05,
                                                   upper_quantile = 0.95 ) )

get_totals("recage", pums.2019, 
           wt = c(WEIGHT, "trim_weight"), 
           digits = 1)
