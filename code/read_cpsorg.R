###########################################################################
# Goal:    Collect CPS ORG microdata from 2015-2019
# Author:  Stephen Stapleton
# Created: 2021-09-20
# Updated: 2021-11-11
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
# Read in CPS ORG data and limit to variables of interest

# Note ww don't need this for 2019, when they start collecting accurate info
# asec.file.list <- c('asec2015', 'asec2016', 'asec2017', 'asec2018')
# 
# # first, need to download the data locally
# for ( f in asec.file.list ) {
#   
#   if ( !file.exists( paste0('projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f ) ) ) {
#     
#     if ( parse_number(f) >= 2019 ) {
#       
#       download.file( url = paste0('https://www2.census.gov/programs-surveys/cps/datasets/', substr(f, 5, 8),
#                                   '/march/asecpub', substr(f, 7, 8), 'sas.zip'),
#                      destfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip') )
#       
#       unzip( zipfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip'),
#              exdir   = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f) )
#       
#       unlink( paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip') )
#       
#     } else if ( parse_number(f) == 2018 ) {
#       
#       download.file( url = 'https://www2.census.gov/programs-surveys/demo/datasets/income-poverty/time-series/data-extracts/2018/cps-asec-bridge-file/ppub18_bridge.sas7bdat',
#                      destfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.sas7bdat') )
#       
#     } else if ( parse_number(f) == 2017 ) {
# 
#       download.file( url = 'https://www2.census.gov/programs-surveys/demo/datasets/income-poverty/time-series/data-extracts/2017/cps-asec-research-file/pppub17.sas7bdat',
#                      destfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.sas7bdat') )
#       
#     } else if ( parse_number(f) == 2016 ) {
#       
#       download.file( url = 'https://www2.census.gov/programs-surveys/cps/datasets/2016/march/asec2016_pubuse_v3.zip',
#                      destfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip') )
#       
#       unzip( zipfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip'),
#              exdir   = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f) )
#       
#       unlink( paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip') )
#       
#     } else {
#       
#       download.file( url = 'https://www2.census.gov/programs-surveys/cps/datasets/2015/march/asec2015_pubuse.zip',
#                      destfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip') )
#       
#       unzip( zipfile = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip'),
#              exdir   = paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f) )
#       
#       unlink( paste0('/projects/sstapleton/min_wage_fight/public_use_data/cps_asec/', f, '.zip' ) )
#       
#     }
#   }
# }

# instead, we're going to get CEPR data up to March 2018

cepr.path <- '/projects/sstapleton/min_wage_fight/public_use_data/cepr_march/'
cepr.file.list <- list.files( cepr.path )
cepr.file.list <- cepr.file.list[2:5]

for ( f in cepr.file.list ) {

  message( paste0('Reading in file: ', f) )

  cepr.temp <- read_dta( paste0(cepr.path, f) )

  # filter sample to only those we care about
  cepr.temp %<>%
    filter( age >= 16 ) %>%        # remove any minors
    filter( wrk != 3 ) %>%         # remove anyone who did not have a wage
    filter( !(is.na(hrwage)) ) %>% # check secondary signal for wage
    filter( hrwage > 0 ) %>%
    filter( !(selfemp %in% c(1)) ) # remove self-employed

  # select relevant variables
  cepr.temp %<>%
    select( year, state, wgt, hrwage, age, female, wbhao, married, fchild, weeks,
            uhours, educ2, indly3d_14, occly3d_13, fulltimely, incp_wag )
  
  # create versions we'll be using to predict weeks worked within bin
  cepr.temp %<>%
    mutate( edu_nohsdegree = if_else( educ2 %in% c(1, 2), 1, 0),
            edu_hsdegree   = if_else( educ2 == 3, 1, 0 ),
            edu_somecolg   = if_else( educ2 == 4, 1, 0 ),
            edu_bachelor   = if_else( educ2 == 5, 1, 0 ),
            edu_advdegre   = if_else( educ2 == 6, 1, 0 ),
            
            racasn  = if_else( wbhao == 4, 1, 0 ),
            racblk  = if_else( wbhao == 2, 1, 0 ),
            racwht  = if_else( wbhao == 1, 1, 0 ),
            rachisp = if_else( wbhao == 3, 1, 0 ) ) %>%
    select( -educ2, -wbhao )
  
  # fix state number to character
  state.crosswalk <- attributes(cepr.temp$state)$labels %>%
    as.data.frame( ) %>%
    rownames_to_column( 'st' ) %>%
    rename( state = '.' )
  
  cepr.temp %<>%
    left_join( state.crosswalk ) %>%
    select( -state )
  
  # remove haven labels now
  cepr.temp %<>% zap_labels( )
  
  # remove any row with empty values
  cepr.temp %<>% na.omit( )
  
  # remove any rows with weight zero
  cepr.temp %<>% filter( wgt > 0 )
  
  # add this to the full dataframe
  if ( !exists('cepr') ) { cepr <- head( cepr.temp, 0 ) }
  
  cepr %<>% bind_rows( cepr.temp )

}; rm( f, cepr.path, cepr.file.list, cepr.temp, state.crosswalk )

# consolidate the industry and occupation codes somewhat
cepr %<>%
  mutate( major_industry = case_when( indly3d_14 < 170 ~ 'None',
                                      170  <= indly3d_14 & indly3d_14 <= 290  ~ 'AGR',
                                      370  <= indly3d_14 & indly3d_14 <= 490  ~ 'EXT',
                                      770  <= indly3d_14 & indly3d_14 <= 770  ~ 'CON',
                                      1070 <= indly3d_14 & indly3d_14 <= 3990 ~ 'MFG',
                                      4070 <= indly3d_14 & indly3d_14 <= 4590 ~ 'WHL',
                                      4670 <= indly3d_14 & indly3d_14 <= 5790 ~ 'RET',
                                      6070 <= indly3d_14 & indly3d_14 <= 6390 ~ 'TRN',
                                      570  <= indly3d_14 & indly3d_14 <= 690  ~ 'UTL',
                                      6470 <= indly3d_14 & indly3d_14 <= 6780 ~ 'INF',
                                      6870 <= indly3d_14 & indly3d_14 <= 7190 ~ 'FIN',
                                      7270 <= indly3d_14 & indly3d_14 <= 7790 ~ 'PRF',
                                      7860 <= indly3d_14 & indly3d_14 <= 7890 ~ 'EDU',
                                      7970 <= indly3d_14 & indly3d_14 <= 8290 ~ 'MED',
                                      8370 <= indly3d_14 & indly3d_14 <= 8470 ~ 'SCA',
                                      8561 <= indly3d_14 & indly3d_14 <= 8690 ~ 'ENT',
                                      8770 <= indly3d_14 & indly3d_14 <= 9290 ~ 'SRV',
                                      9370 <= indly3d_14 & indly3d_14 <= 9590 ~ 'ADM' ),
          
          major_occupation = case_when( 10   <= occly3d_13 & occly3d_13 <= 440  ~ 'MGR',
                                        500  <= occly3d_13 & occly3d_13 <= 750  ~ 'BUS',
                                        800  <= occly3d_13 & occly3d_13 <= 960  ~ 'FIN',
                                        1005 <= occly3d_13 & occly3d_13 <= 1304 ~ 'CMM',
                                        1305 <= occly3d_13 & occly3d_13 <= 1560 ~ 'ENG',
                                        1600 <= occly3d_13 & occly3d_13 <= 1980 ~ 'SCI',
                                        2001 <= occly3d_13 & occly3d_13 <= 2060 ~ 'CMS',
                                        2100 <= occly3d_13 & occly3d_13 <= 2180 ~ 'LGL',
                                        2205 <= occly3d_13 & occly3d_13 <= 2555 ~ 'EDU',
                                        2600 <= occly3d_13 & occly3d_13 <= 2970 ~ 'ENT',
                                        3000 <= occly3d_13 & occly3d_13 <= 3550 ~ 'MED',
                                        3601 <= occly3d_13 & occly3d_13 <= 3655 ~ 'HLS',
                                        3700 <= occly3d_13 & occly3d_13 <= 3960 ~ 'PRT',
                                        4000 <= occly3d_13 & occly3d_13 <= 4160 ~ 'EAT',
                                        4200 <= occly3d_13 & occly3d_13 <= 4255 ~ 'CLN',
                                        4330 <= occly3d_13 & occly3d_13 <= 4655 ~ 'PRS',
                                        4700 <= occly3d_13 & occly3d_13 <= 4965 ~ 'SAL',
                                        5000 <= occly3d_13 & occly3d_13 <= 5940 ~ 'OFF',
                                        6005 <= occly3d_13 & occly3d_13 <= 6130 ~ 'FFF',
                                        6200 <= occly3d_13 & occly3d_13 <= 6765 ~ 'CON',
                                        6800 <= occly3d_13 & occly3d_13 <= 6950 ~ 'EXT',
                                        7000 <= occly3d_13 & occly3d_13 <= 7460 ~ 'RPR',
                                        7700 <= occly3d_13 & occly3d_13 <= 8990 ~ 'PRD',
                                        9005 <= occly3d_13 & occly3d_13 <= 9760 ~ 'TRN' ) ) %>%
  select( -indly3d_14, -occly3d_13 ) %>%
  na.omit( )

# ensure our variable naming convention is consistent to pums
cepr %<>%
  rename( wagp = 'incp_wag', 
          agep = 'age',
          srvy_yr = 'year',
          wkhp = 'uhours',
          any_children = 'fchild',
          # mar = 'married',
          weeks_real = 'weeks',
          fulltime = 'fulltimely' )

# a couple touch-ups to variables for regression
cepr %<>%
  mutate( agep_square = agep^2 ) %>%
  mutate_at( vars(srvy_yr), as.character )

# create variable for week bin to predict within
cepr %<>%
  mutate( wkw = case_when(weeks_real < 14 ~ 6,
                          14 <= weeks_real & weeks_real <= 26 ~ 5,
                          27 <= weeks_real & weeks_real <= 39 ~ 4,
                          40 <= weeks_real & weeks_real <= 47 ~ 3,
                          48 <= weeks_real & weeks_real <= 49 ~ 2,
                          50 <= weeks_real ~ 1  ) )

#--------------------------------------------------------------------------
# Create subset to understand nominal wage growth

wage.growth <- cepr %>%
  
  # get only bottom 20% of wages by year and state
  group_by( srvy_yr, st ) %>%
  arrange( hrwage ) %>%
  mutate( weighted_pctile = lag( cumsum(wgt), default = 0 ) / ( sum(wgt) - 1 ) ) %>%
  filter( weighted_pctile <= 0.2 ) %>%
  
  # calculate average wage by year and state
  summarize( avg_hrwage = weighted.mean(hrwage, wgt) ) %>%
  
  # calculate wage growth between each year within state
  group_by( st ) %>%
  arrange( srvy_yr ) %>%
  mutate( pct_wage_growth = avg_hrwage / lag( avg_hrwage ) ) %>%
  
  # calculate average wage growth within state
  summarize( avg_wage_growth = mean(pct_wage_growth, na.rm = T) )

#--------------------------------------------------------------------------
# Create OLS prediction for weeks worked

# FOR 11/12 meeting, create version with those reporting <$15 / hour
# cepr.u15 <- cepr %>% filter( hrwage < 15 )

# uncount survey weights to allow for proportional representation
# cepr %<>%
#   mutate( wgt = floor(wgt/100) ) %>%
#   uncount( wgt )

# bin data based on weeks worked
cepr.list <- cepr %>% split( f = .$wkw ); rm( cepr )

# run regression models within these bins to predict true weeks worked
i = 1

# FOR 11/12 meeting added in married and parental status

while ( i <= length(cepr.list) ) {
  
  lm.temp <- lm( weeks_real ~ female + racblk + rachisp + racasn + racwht + married +
                              any_children + major_industry + major_occupation + agep +
                              agep_square + wagp + fulltime + st + srvy_yr,
                 data = cepr.list[[i]] )
  
  if ( !exists('wkw.models') ) { wkw.models <- list() }
    
  wkw.models[[i]] <- lm.temp
  
  i = i + 1
  
}; rm( lm.temp )

#--------------------------------------------------------------------------
# Create OLS prediction for hourly wage within 

# run regression within bins to predict hourly wage
i = 1

# FOR 11/12 meeting added in married and parental status

while ( i <= length(cepr.list) ) {
  
  lm.temp <- lm( hrwage ~ female + racblk + rachisp + racasn + racwht + married +
                          any_children + major_industry + major_occupation + agep +
                          agep_square + fulltime + st + srvy_yr,
                 data = cepr.list[[i]] )
  
  if ( !exists('wage.models') ) { wage.models <- list() }
  
  wage.models[[i]] <- lm.temp
  
  i = i + 1
  
}; rm( i, lm.temp )

rm( cepr.list )


# FOR 11/12, do this again with those under $15 / hour

# # bin data based on weeks worked
# cepr.list.u15 <- cepr.u15 %>% split( f = .$wkw ); rm( cepr.u15 )
# 
# # run regression models within these bins to predict true weeks worked
# i = 1
# 
# # FOR 11/12 meeting added in married and parental status
# 
# while ( i <= length(cepr.list.u15) ) {
#   
#   lm.temp <- lm( weeks_real ~ female + racblk + rachisp + racasn + racwht + married +
#                               any_children + major_industry + major_occupation + agep +
#                               agep_square + wagp + fulltime + st + srvy_yr,
#                  data = cepr.list.u15[[i]] )
#   
#   if ( !exists('wkw.models.u15') ) { wkw.models.u15 <- list() }
#   
#   wkw.models.u15[[i]] <- lm.temp
#   
#   i = i + 1
#   
# }; rm( lm.temp )
# 
# #--------------------------------------------------------------------------
# # Create OLS prediction for hourly wage within 
# 
# # run regression within bins to predict hourly wage
# i = 1
# 
# # FOR 11/12 meeting added in married and parental status
# 
# while ( i <= length(cepr.list.u15) ) {
#   
#   lm.temp <- lm( hrwage ~ female + racblk + rachisp + racasn + racwht + married +
#                           any_children + major_industry + major_occupation + agep +
#                           agep_square + fulltime + st + srvy_yr,
#                  data = cepr.list.u15[[i]] )
#   
#   if ( !exists('wage.models.u15') ) { wage.models.u15 <- list() }
#   
#   wage.models.u15[[i]] <- lm.temp
#   
#   i = i + 1
#   
# }; rm( i, lm.temp )
# 
# rm( cepr.list.u15 )

#--------------------------------------------------------------------------
# Create wage distribution of 2018 to match to

# cepr.2018 <- read_dta( '/projects/sstapleton/min_wage_fight/public_use_data/cepr_march/cepr_march_2018.dta' )
# 
# # get our data to those with an hourly wage
# cepr.2018 %<>%
#   filter( age >= 16 ) %>%
#   filter( wrk == 1 ) %>%
#   filter( !(is.na(hrwage)) ) %>%
#   filter( hrwage > 2.13 ) %>%
#   filter( hrwage < 1000 ) %>%
#   select( state, wgt, hrwage )
# 
# # fix state number to character
# state.crosswalk <- attributes(cepr.2018$state)$labels %>%
#   as.data.frame( ) %>%
#   rownames_to_column( 'st' ) %>%
#   rename( state = '.' )
# 
# cepr.2018 %<>%
#   left_join( state.crosswalk ) %>%
#   select( -state ); rm( state.crosswalk )
# 
# # remove haven labels now
# cepr.2018 %<>% zap_labels( )
# 
# # get weighted percentile of wage within a given state
# cepr.2018 %<>%
#   mutate( wgt = floor(wgt) ) %>%
#   group_by( st ) %>%
#   mutate( hrwage_pctl = lag( cumsum(wgt), default = 0 ) / ( sum(wgt) - 1 ) ) %>%
#   ungroup( ) %>%
#   distinct( )