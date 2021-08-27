###########################################################################
# Goal:    Impute hourly wages
# Author:  Stephen Stapleton
# Created: 2021-08-15
# Updated: 2021-08-26
###########################################################################

# Steps of this script:
# (1) Read in limited ACS data, full ACS data, and CPS-ORG data
# (2) Perform any imputation steps necessary for CPS-ORG
# (3) Build out weeks-worked bin-specific OLS prediction
# (4) Use this prediction to calculate hourly wage
# (5) Average this hourly wage with predicted hourly wage from CPS-ORG
# (6) Fit the wage distribution to most recent CPS-ORG hourly wage percentile

#--------------------------------------------------------------------------
# Setup

list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'mice') # check for packages
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

if(length(new.packages)) install.packages(new.packages); rm(list.packages, new.packages)

require(tidyverse)
require(tidycensus)
require(magrittr)
require(here)
require(mice)

#--------------------------------------------------------------------------
# Read in data inputs

source( INSERT NAME OF REWEIGHTING SCRIPT )

acs.raw <- NON LIMITED ACS DATA
acs.lim <- LIMITED ACS DATA # this should be reweighted and imputed

cps.raw <- LIST OF CPS ORG MARCH RESULTS EACH YEAR

#--------------------------------------------------------------------------
# Perform any necessary data imputation for CPS ORG

# NOTE: if any have missingness, will need to impute via MICE
acs.raw %<>% impute_vars( seed = 11131992 )

#--------------------------------------------------------------------------
# Hot-encode data where necessary

THIS CODE WILL BE SPECIFIC TO THE VARIABLES NEEDING HOT ENCODING
  
#--------------------------------------------------------------------------
# Build out weeks-worked bin-specific OLS models

# Rationale: ACS is binned, so impute within each bin using out-of-sample CPS ORG
#            data, which reports specific hours worked instead of bins

# set predictive variables of interest. As of 08/27/2021:
# - gender
# - age^3 + age^2 + age
# - marital status
# - white x ( 16-64 | 65+ )
# - black x ( 16-64 | 65+ )
# - nonwhite/nonblack x ( 16-64 | 65+ )
# - hispanic x ( 16-64 | 65+ )
# - education level for 16+
# - part-time status
# - major industry
# - major occupation
# - NOTE: include year and state fixed effects
# - NOTE: we're going to be running by age bins (see rake.vars)

pred.vars <- INSERT LIST OF PREDICTIVE VARS
age.bins  <- INSERT LIST OF AGE BINS

for ( bin in age.bins ) {
  
  age.low <- as.numeric( sub( "\\-.", age.bins ) )
  age.upp <- as.numeric( sub( ".-\\", age.bins ) )
  
  if ( is.na(age.upp) ) { age.up = 150 } # set as max age
  
  cps.now <- cps.raw %>% filter( between( AGE, age.low, age.upp ) )
  
  model.now <- lm( paste( "WEEKS WORKED ~ 0 +", paste( pred.vars, collapse = ' + ' ) ),
                   index = c(STATE, YEAR),
                   model = 'within',
                   data  = cps.now )
  
  if( !exists(weeks.models) ) { weeks.models <- list( ) }
    
  weeks.models %<>% append( bin = model.now )
  
}; rm( cps.now, model.now )
 
#--------------------------------------------------------------------------
# Predict out-of-sample and determine hourly wage

for ( bin %in% age.bins ) {
  
  age.low <- as.numeric( sub( "\\-.", age.bins ) )
  age.upp <- as.numeric( sub( ".-\\", age.bins ) )
  
  if ( is.na(age.upp) ) { age.up = 150 } # set as max age
  
  acs.now <- acs.lim %>% filter( between( AGE, age.low, age.upp ) )
  
  acs.now %<>%
    mutate( week_predict = predict( weeks.models[bin], newdata = acs.now ) ) %>%
    filter( IDENTIFIER, week_predict )
  
  if( !exists(weeks.predict) ) { weeks.predict <- head( acs.now, 0 ) }
  
  weeks.predict %<>% bind_rows( acs.now )
  
}; rm( acs.now )

# merge these predictions onto dataset
acs.pred <- acs.lim %<>% left_join( weeks.predict )

# now, use these to calculate hourly wage
acs.pred %<>%
  mutate( hourly_wage = EARNINGS PRIOR YEAR / ( USUAL HOURS * week_predict ) )

#--------------------------------------------------------------------------
# Average hourly wage with CPS-ORG linear regression

# Rationale: the hourly wages from CPS org are more accurate,
#            so weight equally to these measures

# build out the model
hwage.model <- lm( paste( "HOURLY WAGE ~ 0 +", paste( pred.vars, collapse = ' + ' ) ),
                  index = c(STATE, YEAR),
                  model = 'within',
                  data  = cps.raw )

# predict out the hourly wage
acs.pred %<>%
  mutate( hourly_wage_predict = predict( hwage.model, newdata = acs.pred ) )

# take the average between these two estimations
acs.pred %<>%
  mutate( hourly_wage_mean = ( hourly_wage + hourly_wage_predict ) / 2 ) )

#--------------------------------------------------------------------------
# Fit the wage distribution to most recent CPS-ORG hourly wage percentile

# get only most recent year of CPS-ORG data
cps.last <- cps.raw %>% filter( YEAR = max(YEAR) )

# create percentile rank
cps.list %<>% mutate( hourlywage_rank = ( rank(HOURLY WAGE, ties.method = 'max') / length( HOURLY WAGE ) )

# finally, due to mismeasurement in ACS wages causing long tails, final hourly wage is
# created based on CPS-ORG state-level wage distribution for 2017. Specifically,
# determine state and year-specific percentile location of ACS second-stage hourly wage,
# then assign CPS-ORD hourly wage percentile value of that state- and year-specific
# wage quantile
# - e.g. 2014 CA resident has second-stage hourly wage in 12th percentile in 2014 ACS CA sample:
# -   -> assign 12th-percentile hourly wage in 2017 CPS-ORG California sample
