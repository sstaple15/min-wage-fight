###########################################################################
# Goal:    Rake ACS weights to overall state population
# Author:  Stephen Stapleton
# Created: 2021-09-16
# Updated: 2021-11-11
###########################################################################

# impute_wages.R ->

# Steps of this script:
# (1) Retool PUMS data format
# (2) Rake to new weights

# -> simulate_wages.R

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
# Retool PUMS data format

# build variables that are relevant to raking
pums %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
                  age_65_up = if_else( agep >= 65, 1, 0 ),
                  age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
                  age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
                  age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
                  age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
                  age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
                  age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
                 
                  female_16_64 = ( female * age_16_64 ),
                  female_65_up = ( female * age_65_up ),
                  white_16_64  = ( racwht * age_16_64 ),
                  white_65_up  = ( racwht * age_65_up ),
                  black_16_64  = ( racblk * age_16_64 ),
                  black_65_up  = ( racblk * age_65_up ),
                  hispn_16_64  = ( rachisp * age_16_64 ),
                  hispn_65_up  = ( rachisp * age_65_up )
                  ) %>%
  mutate_at( vars(rake.vars), as.factor ) %>%
  select( -c(age_16_64) )

# split data on the basis of state
pums.list <- split(pums, f = pums$fip ); rm( pums )

#--------------------------------------------------------------------------
# Rake to new weights

# rake to new weights
for ( fip in names(pums.list) ) {
  
  # rake to new weights
  new_weights <- rake_survey( pums.list[[fip]] %>% select( pwgtp, rake.vars ),
                              base_weight = 'pwgtp',
                              pop_margins = pums.goal[[fip]] )
  
  # trim weights that are a little out there
  # trim_weights <- trim_weights( new_weights,
  #                               lower_quantile = 0.01,
  #                               upper_quantile = 0.99 )
  
  pums.list[[fip]] %<>% cbind( new_weights ) # %>% cbind( trim_weights )
  
  # now, rescale weights to the total found in the state
  pop.temp <- pums.totp[[fip]]
  
  pums.list[[fip]] %<>%
    
    mutate( pop_total = pop.temp,
            weights_total = sum(new_weights),
            prop_weight = new_weights / weights_total,
            final_weight = prop_weight * pop_total ) %>%
    
    select( -c(new_weights, pop_total, weights_total, prop_weight) )
  
}; rm( new_weights, fip, pop.temp )

rm( pums.goal, pums.totp ) # clean up bulky info


# FOR 11/12 meeting, make version with only < $15 / hour

# #--------------------------------------------------------------------------
# # Retool PUMS data format
# 
# # build variables that are relevant to raking
# pums.u15 %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
#                       age_65_up = if_else( agep >= 65, 1, 0 ),
#                       age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
#                       age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
#                       age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
#                       age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
#                       age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
#                       age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
#                       
#                       female_16_64 = ( female * age_16_64 ),
#                       female_65_up = ( female * age_65_up ),
#                       white_16_64  = ( racwht * age_16_64 ),
#                       white_65_up  = ( racwht * age_65_up ),
#                       black_16_64  = ( racblk * age_16_64 ),
#                       black_65_up  = ( racblk * age_65_up ),
#                       hispn_16_64  = ( rachisp * age_16_64 ),
#                       hispn_65_up  = ( rachisp * age_65_up )
#                       ) %>%
#   mutate_at( vars(rake.vars), as.factor ) %>%
#   select( -c(age_16_64) )
# 
# # split data on the basis of state
# pums.list.u15 <- split(pums.u15, f = pums.u15$fip ); rm( pums.u15 )
# 
# #--------------------------------------------------------------------------
# # Rake to new weights
# 
# # rake to new weights
# for ( fip in names(pums.list.u15) ) {
#   
#   # rake to new weights
#   new_weights <- rake_survey( pums.list.u15[[fip]] %>% select( pwgtp, rake.vars ),
#                               base_weight = 'pwgtp',
#                               pop_margins = pums.goal.u15[[fip]] )
#   
#   # trim weights that are a little out there
#   # trim_weights <- trim_weights( new_weights,
#   #                               lower_quantile = 0.01,
#   #                               upper_quantile = 0.99 )
#   
#   pums.list.u15[[fip]] %<>% cbind( new_weights ) # %>% cbind( trim_weights )
#   
#   # now, rescale weights to the total found in the state
#   pop.temp <- pums.totp.u15[[fip]]
#   
#   pums.list.u15[[fip]] %<>%
#     
#     mutate( pop_total = pop.temp,
#             weights_total = sum(new_weights),
#             prop_weight = new_weights / weights_total,
#             final_weight = prop_weight * pop_total ) %>%
#     
#     select( -c(new_weights, pop_total, weights_total, prop_weight) )
#   
# }; rm( new_weights, fip, pop.temp )
