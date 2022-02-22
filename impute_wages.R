###########################################################################
# Goal:    Impute hourly wages from CPS ORG data
# Author:  Stephen Stapleton
# Created: 2021-08-15
# Updated: 2021-12-16
###########################################################################

# Resource:
# https://walker-data.com/tidycensus/articles/pums-data.html

# read_cpsorg.R ->

# Steps of this script:
# (1) Predict weeks worked by week bin via CPS-ORG out-of-sample model
# (2) Predict hourly wage by week bin via CPS-ORG out-of-sample model
# (X) NEW: Remove data bin for lowest weeks worked due to high prediction variance
# (3) Calculate hourly wage as the average of hourly wage using (1) and (2)
# (4) Map this to distribution of PUMS 2019 wages within state

# -> rake_pums.R

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
# Predict weeks worked by week bin via CPS-ORG out-of-sample model

# fix any variable names inconsistent between PUMS and CEPR
pums.list %<>% lapply( function(x) { x %>% mutate(srvy_yr = as.character(srvy_yr) ) } )

# join data together between states
pums.bins <- bind_rows( pums.list, .id = 'fip' ); rm( pums.list )

# split data on basis of weekly bin
pums.bins %<>% split(f = pums.bins$wkw )

# predict weeks worked by bin using specific bin model
for ( i in c(1:6) ) {
    
  pums.bins[[i]] %<>% rownames_to_column( 'person_id' ) # need to keep a row record
  
  # create subset without 2019
  bin.temp <- pums.bins[[i]] %>% filter( srvy_yr != '2019' )
  
  # make predictions for all other years and join in on person_id
  bin.temp %<>%
    mutate( weeks_predict = predict( wkw.models[[i]], bin.temp ) ) %>%
    select( person_id, weeks_predict )
  
  # join in by person_id
  pums.bins[[i]] %<>% left_join( bin.temp, by = 'person_id' )
  
  # drop our person record id
  pums.bins[[i]] %<>% select( -person_id )

}; rm( bin.temp )

rm( wkw.models ) # don't need this bulky info anymore

#--------------------------------------------------------------------------
# Predict hourly wage by week bin via CPS-ORG out-of-sample model

# predict hourly wages by bin using specific bin model
for ( i in c(1:6) ) {
  
  pums.bins[[i]] %<>% rownames_to_column( 'person_id' ) # need to keep a row record
  
  # create subset without 2019
  bin.temp <- pums.bins[[i]] %>% filter( srvy_yr != '2019' )
  
  # make predictions for all other years and join in on person_id
  bin.temp %<>%
    mutate( hrwage_predict = predict( wage.models[[i]], bin.temp ) ) %>%
    select( person_id, hrwage_predict )
  
  # join in by person_id
  pums.bins[[i]] %<>% left_join( bin.temp, by = 'person_id' )
  
  # drop our person record id
  pums.bins[[i]] %<>% select( -person_id )
  
}; rm( bin.temp )

rm( wage.models ) # don't need this bulky info anymore

#--------------------------------------------------------------------------
# NEW: Remove data bin for lowest weeks worked due to high prediction variance

pums.bins <- pums.bins[1:5]

#--------------------------------------------------------------------------
# Calculate final hours wage based on predictions

pums <- bind_rows( pums.bins ); rm( pums.bins )

# re-incorporate PR 2019
pums %<>% bind_rows( pums.pr %>%
                       filter( srvy_yr == '2019') %>% # only year we have accurate data
                       filter( wkwn > 13 ) %>% # make consistent with earlier reduction
                       mutate( srvy_yr = as.character(srvy_yr),
                               fip = '72') ); rm( pums.pr )

# calculate within-year wage prediction pre-percentile correction
pums %<>%
  
  # correct for some negative value predictions
  mutate( hrwage_predict = if_else( hrwage_predict < 0, 0, hrwage_predict ) ) %>%
  
  mutate( firststage_wage = if_else( is.na(weeks_predict),
                                     wagp / wkhp / wkwn, # for 2019
                                     wagp / wkhp / weeks_predict ), # for not 2019
          secondstage_wage = if_else( is.na(hrwage_predict),
                                      firststage_wage,  # for 2019
                                      hrwage_predict ), # for not 2019 
          thirdstage_wage = if_else( secondstage_wage < 0,
                                     firststage_wage,
                                     ( firststage_wage + secondstage_wage ) / 2 ) # take average
          )

#--------------------------------------------------------------------------
# Map this to distribution of PUMS 2019 wages within state

# create the percentile measure within state, within year
pums %<>%
  group_by( st, srvy_yr ) %>%
  arrange( thirdstage_wage ) %>%
  mutate( thirdstage_wage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
  ungroup( )

# create person id for ease of left join
pums %<>% rownames_to_column( 'person_id' )

# now, compare to 2019 distribution and pull closest percentile value
pums.match.in <- pums %>%
  select( person_id, st, thirdstage_wage_pctl )

# get closest match by iterating on decimals
for ( i in c(6:1) ) {
  
  if ( nrow(pums.match.in) > 0 ) {
  
    pums.temp <- pums.match.in %>%
      mutate( pctl_short = round( thirdstage_wage_pctl, i ) ) %>%
      left_join( pums.2019 %>%
                 mutate( pctl_short = round( hrwage_pctl, i ) ) %>%
                 select( pctl_short, st, hrwage ) )
    
    pums.match.in  <- pums.temp %>%
      filter( is.na(hrwage) ) %>%
      select( -hrwage, -pctl_short )
    
    pums.temp %<>%
      filter( !is.na(hrwage) ) %>%
      select( person_id, hrwage ) %>%
      group_by( person_id ) %>%
      summarize( hrwage = mean(hrwage) )
    
    if ( !exists('pums.match.out') ) { pums.match.out <- head( pums.temp, 0 ) }
    
    pums.match.out %<>% bind_rows( pums.temp )
  }
  
}; rm( i, pums.match.in, pums.temp )

pums %<>% left_join( pums.match.out ); rm( pums.match.out )
  
rm( pums.2019 ) # don't need this bulky info anymore


# FOR 11/12 meeting, do this all for those making < $15 / hour

# #--------------------------------------------------------------------------
# # Predict weeks worked by week bin via CPS-ORG out-of-sample model
# 
# # fix any variable names inconsistent between PUMS and CEPR
# pums.list.u15 %<>% lapply( function(x) { x %>% mutate(srvy_yr = as.character(srvy_yr) ) } )
# 
# # join data together between states
# pums.bins.u15 <- bind_rows( pums.list.u15, .id = 'fip' ); rm( pums.list.u15 )
# 
# # split data on basis of weekly bin
# pums.bins.u15 %<>% split(f = pums.bins.u15$wkw )
# 
# # predict weeks worked by bin using specific bin model
# for ( i in c(1:6) ) {
#   
#   pums.bins.u15[[i]] %<>% rownames_to_column( 'person_id' ) # need to keep a row record
#   
#   # create subset without 2019
#   bin.temp <- pums.bins.u15[[i]] %>% filter( srvy_yr != '2019' )
#   
#   # make predictions for all other years and join in on person_id
#   bin.temp %<>%
#     mutate( weeks_predict = predict( wkw.models.u15[[i]], bin.temp ) ) %>%
#     select( person_id, weeks_predict )
#   
#   # join in by person_id
#   pums.bins.u15[[i]] %<>% left_join( bin.temp, by = 'person_id' )
#   
#   # drop our person record id
#   pums.bins.u15[[i]] %<>% select( -person_id )
#   
# }; rm( bin.temp )
# 
# rm( wkw.models.u15 ) # don't need this bulky info anymore
# 
# #--------------------------------------------------------------------------
# # Predict hourly wage by week bin via CPS-ORG out-of-sample model
# 
# # predict hourly wages by bin using specific bin model
# for ( i in c(1:6) ) {
#   
#   pums.bins.u15[[i]] %<>% rownames_to_column( 'person_id' ) # need to keep a row record
#   
#   # create subset without 2019
#   bin.temp <- pums.bins.u15[[i]] %>% filter( srvy_yr != '2019' )
#   
#   # make predictions for all other years and join in on person_id
#   bin.temp %<>%
#     mutate( hrwage_predict = predict( wage.models.u15[[i]], bin.temp ) ) %>%
#     select( person_id, hrwage_predict )
#   
#   # join in by person_id
#   pums.bins.u15[[i]] %<>% left_join( bin.temp, by = 'person_id' )
#   
#   # drop our person record id
#   pums.bins.u15[[i]] %<>% select( -person_id )
#   
# }; rm( bin.temp )
# 
# rm( wage.models.u15 ) # don't need this bulky info anymore
# 
# #--------------------------------------------------------------------------
# # Calculate final hours wage based on predictions
# 
# pums.u15 <- bind_rows( pums.bins.u15 ); rm( pums.bins.u15 )
# 
# # calculate within-year wage prediction pre-percentile correction
# pums.u15 %<>%
#   
#   # correct for some negative value predictions
#   mutate( hrwage_predict = if_else( hrwage_predict < 0, 0, hrwage_predict ) ) %>%
#   
#   mutate( firststage_wage = if_else( is.na(weeks_predict),
#                                      wagp / wkhp / wkwn, # for 2019
#                                      wagp / wkhp / weeks_predict ), # for not 2019
#           secondstage_wage = if_else( is.na(hrwage_predict),
#                                       firststage_wage,  # for 2019
#                                       hrwage_predict ), # for not 2019 
#           thirdstage_wage = ( firststage_wage + secondstage_wage ) / 2 ) # take average
# 
# #--------------------------------------------------------------------------
# # Map this to distribution of PUMS 2019 wages within state
# 
# # create the percentile measure within state, within year
# pums.u15 %<>%
#   group_by( st, srvy_yr ) %>%
#   arrange( thirdstage_wage ) %>%
#   mutate( thirdstage_wage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
#   ungroup( )
# 
# # create person id for ease of left join
# pums.u15 %<>% rownames_to_column( 'person_id' )
# 
# # now, compare to 2019 distribution and pull closest percentile value
# pums.match.in <- pums.u15 %>%
#   select( person_id, st, thirdstage_wage_pctl )
# 
# # get closest match by iterating on decimals
# for ( i in c(6:1) ) {
#   
#   if ( nrow(pums.match.in) > 0 ) {
#     
#     pums.temp <- pums.match.in %>%
#       mutate( pctl_short = round( thirdstage_wage_pctl, i ) ) %>%
#       left_join( pums.2019.u15 %>%
#                    mutate( pctl_short = round( hrwage_pctl, i ) ) %>%
#                    select( pctl_short, st, hrwage ) )
#     
#     pums.match.in <- pums.temp %>%
#       filter( is.na(hrwage) ) %>%
#       select( -hrwage, -pctl_short )
#     
#     pums.temp %<>%
#       filter( !is.na(hrwage) ) %>%
#       select( person_id, hrwage ) %>%
#       group_by( person_id ) %>%
#       summarize( hrwage = mean(hrwage) )
#     
#     if ( !exists('pums.match.out') ) { pums.match.out <- head( pums.temp, 0 ) }
#     
#     pums.match.out %<>% bind_rows( pums.temp )
#   }
#   
# }; rm( i, pums.match.in, pums.temp )
# 
# pums.u15 %<>% left_join( pums.match.out ); rm( pums.match.out )
# 
# rm( pums.2019.u15 ) # don't need this bulky info anymore

# # now, compare to 2019 distribution and pull closest percentile value
# i = 1
# 
# while ( i <= nrow(pums) ) {
#   
#   pums.2019 %>%
#     filter( st == pums$st[[i]] ) %>% # subset to same state
#     
#     # find nearest percentile
#     mutate( diff = abs(hrwage_pctl - pums$thirdstage_wage_pctl[i]) ) %>%
#     filter( diff == min(diff) ) %>%
#     
#     filter( row_number() == 1 ) %>% # in the case of ties, take first
#     # summarize( hrwage = mean(hrwage) ) %>% # in the case of ties, take average
#     
#     pull( hrwage ) -> temp # nab the results
#   
#   # save this as a dataframe
#   temp <- data.frame( person_id = i, final_wage = temp )
#   
#   # create dataframe to population
#  if ( !exists('laststage_wage') ) { laststage_wage <- head( temp, 0 ) }
#   
#   laststage_wage %<>% bind_rows( temp )
#   
#   i = i + 1
#   
# }; rm( i, temp )
# 
# # merge in these data
# pums %<>% left_join( laststage_wage, by = 'person_id' ); rm( laststage_wage )
  