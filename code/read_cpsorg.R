###########################################################################
# Goal:    Collect CPS ORG microdata from 2015-2019
# Author:  Stephen Stapleton
# Created: 2021-09-20
# Updated: 2021-09-22
###########################################################################

# Sources:
# https://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

# rake_pums.R ->

# Steps of this script:
# (1) Read in CPS ORG data from url
# (2) Perform any necessary data imputation for CPS ORG
# (3) Limit data to only necessary attributes

# -> impute_wages.R

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'survey', 'srvyr', 'mice', 'haven')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

# install if not currently
if(length(new.packages)) { install.packages(new.packages);
  if(grepl('pewmethods', new.packages)) { # exception for git-only packages
    if(!( 'devtools' %in% installed.packages()[, 'Package'] )) { install.packages('devtools') }
    require(devtools); install_github("pewresearch/pewmethods") } }; rm(new.packages) 

lapply(list.packages, require, character.only = T); rm(list.packages) # load into environment

#--------------------------------------------------------------------------
# Read in CPS ORG data

cps.file.list <- c('aces2015', 'aces2016', 'aces2017', 'aces2018', 'aces2019')

if ( !file.exists( 'projects/sstapleton/min_wage_fight/public_use_data/cps_aces/aces2019') )
  
download.file('https://www2.census.gov/programs-surveys/cps/datasets/2019/march/asecpub19csv.zip',
              '/projects/sstapleton/min_wage_fight/public_use_data/cps_aces/asec2019.zip' )

unzip( '/projects/sstapleton/min_wage_fight/public_use_data/cps_aces/asec2019.zip',
       exdir = '/projects/sstapleton/min_wage_fight/public_use_data/cps_aces/aces2019' )

unlink( '/projects/sstapleton/min_wage_fight/public_use_data/cps_aces/asec2019.zip' )

cps.path <- '/projects/sstapleton/min_wage_fight/public_use_data/cepr_march/'
cps.file.list <- list.files( cps.path )

cps.list <- list()

for ( f in cps.file.list ) {
  
  message( paste0('Reading in file: ', f) )
  
  cps.list[[f]] <- read_dta( paste0(cps.path, f) )
  
  # filter sample to only those we care about
  cps.list[[f]] %<>%
    filter( age >= 16 ) %>% # remove any minors
    filter( wrk != 3 ) %>% # remove anyone who did not have a wage
    # remove any active military
  
  # select relevant variables
  # - weeks worked last year
  # - gender
  # - education
  # - race/ethnicity
  # - education
  # - marital status
  # - major industry
  # - major occupation
  # - part-time status
  # - polynomial of age
  # - income (added)
  # - FEs for year and state
  cps.list[[f]] %<>%
    select( year, wgt, age, female, wbhao, married, marstat, fchild, wkslyr, hrslyr, clslyr, ftptlyr,
            wksrec, wrk, lfstat, empl, selfemp, state, educ, educ2, educ92, weeks, uhours, hours,
            incp_wag, rincp_wag )
  
}; rm( f, cps.path, cps.file.list )

#--------------------------------------------------------------------------
# Perform any necessary data imputation for CPS ORG

# NOTE: if any have missingness, will need to impute via MICE
cps.list %<>% impute_vars( seed = 11131992 )

#--------------------------------------------------------------------------
# Limit data to only necessary attributes
