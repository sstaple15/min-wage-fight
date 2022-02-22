###########################################################################
# Goal:    Conduct wage simulation
# Author:  Stephen Stapleton
# Created: 2021-11-11
# Updated: 2021-11-19
###########################################################################

# rake_pums.R ->

# Steps of this script:
# (1) Prep demographic shifts for simulation
# (2) Pull minimum wage changes
# (3) Create demographic shifted weights
# (4) Simulate wage increase based on function rule for wage change

# ->

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'magrittr', 'here', 'survey', 'srvyr', 'janitor', 'xlsx')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

# install if not currently
if(length(new.packages)) { install.packages(new.packages);
  if(grepl('pewmethods', new.packages)) { # exception for git-only packages
    if(!( 'devtools' %in% installed.packages()[, 'Package'] )) { install.packages('devtools') }
    require(devtools); install_github("pewresearch/pewmethods") } }; rm(new.packages) 

lapply(list.packages, require, character.only = T); rm(list.packages) # load into environment

#--------------------------------------------------------------------------
# Prep demographic shifts for simulation

dem.shifts <- data.frame(
  race = c('white', 'white', 'black', 'black', 'hispanic', 'hispanic', 'asian', 'asian'),
  female = c(0, 1, 0, 1, 0, 1, 0, 1),
  shift_scalar = c(1.001, 1.004, 1.008, 1.01, 1.02, 1.023, 1.022, 1.024)
)

# simplify pums race/ethnicity data for join
pums.list %<>% lapply( function(x)
  x %>% mutate( race = case_when( rachisp == 1 ~ 'hispanic',
                                  racblk  == 1 ~ 'black',
                                  racasn  == 1 ~ 'asian',
                                  T ~ 'white' ) ) )

pums.list %<>% lapply(
  function(x) left_join( x, dem.shifts, by = c('race', 'female') ) ); rm( dem.shifts)

# isolate simulation data and join in demographic scalar
sim.data <- bind_rows( pums.list ) %>%
  select( person_id, st, final_weight, shift_scalar, hrwage )

#--------------------------------------------------------------------------
# Pull minimum wage changes

wage.sched <- read_csv( '/projects/sstapleton/min_wage_fight/public_use_data/min_wage_schedule.csv' ) %>%
  rename( st = 'State' ) %>%
  mutate_at( vars(y2019:y2026), parse_number ) %>%
  rename_at( vars(y2019:y2026), function(x) paste0('minwage_', x) ) %>%
  select( st, minwage_y2019, minwage_y2020, minwage_y2021, minwage_y2022 )

# confirm state names are the same in these files
wage.sched %>%
  mutate( st_name_check = st %in% sim.data$st ) %>%
  tabyl( st_name_check )

# incorporate these data into simulation
sim.data %<>% left_join( wage.sched ); rm( wage.sched )

#--------------------------------------------------------------------------
# Allow rescale to BLS 2021 labor force estimate by state

labor.force <- read_csv( '/projects/sstapleton/min_wage_fight/public_use_data/laborforce_bls_2021.csv' )

# confirm state names are the same in these files
labor.force %>%
  mutate( st_name_check = st %in% sim.data$st ) %>%
  tabyl( st_name_check )

# incorporate these data into simulation
sim.data %<>% left_join( labor.force ); rm( labor.force )

#--------------------------------------------------------------------------
# Create demographic shifted weights

for ( yr in c(2020:2022) ) {
  
  var.name = paste0( 'weights_y', yr )
  n.compound = yr - 2019
  
  sim.data %<>% mutate( !!var.name := final_weight * ( shift_scalar ^ n.compound ) )
  
}; rm( yr, var.name, n.compound )

#--------------------------------------------------------------------------
# Add in natural wage growth expectation

# we don't have CEPR data for PR, assume average of bottom 10 (1%)
wage.growth %<>% rbind( data.frame( st = 'Puerto Rico', avg_wage_growth = 1.01) )

wage.growth %<>% mutate( avg_wage_growth = avg_wage_growth - 1,
                         avg_wage_growth = avg_wage_growth / 2,
                         avg_wage_growth = avg_wage_growth + 1)

sim.data %<>% left_join( wage.growth ) #; rm( wage.growth )

#--------------------------------------------------------------------------
# Simulate wage increase based on function rule for wage change

# Methodology:

# 11/19 MEETING: for tipped workers (by SOC), create a marker for later presentation

# Two groups of interest here:
# (1) Workers DIRECTLY affected by minimum wage == 0.7*min_wage < hrwage <= min_wage
# (2) Workers INDIRECTLY affected by minimum wage == min_wage < hrwage < 1.15*new_min_wage

relevantWorkers <- function(current_wage, min_wage, new_min_wage) {
  
  out <- ( min_wage != new_min_wage ) &
         ( ( 0.7 * min_wage < current_wage & current_wage <= min_wage ) |
         ( min_wage < current_wage & current_wage < 1.15 * new_min_wage ) )
  
  return( out )
}

# Create following rule for every worker with 0.7*min_wage < hrwage < 1.15*new_min_wage:
# Take the GREATER of:
# (1) 1/4 * ( 1.15*new_min_wage - current_wage )
# (2) min( new_min_wage - current_wage, new_min_wage - min_wage )

simulateWages <- function(relevant_worker, current_wage, min_wage, new_min_wage) {

  if ( relevant_worker ) {
    
    new_wage <- current_wage +
      pmax( 0.25 * ( 1.15 * new_min_wage - current_wage ),
            pmin( new_min_wage - current_wage,
                 new_min_wage - min_wage ) ) 
    
  } else { new_wage <- current_wage }

  return( new_wage )
}

# run these functions for each of our years
sim.data %<>% mutate( hrwage_y2019 = hrwage )

for ( yr in c(2020:2022) ) {
  
  var.relevant = paste0( 'affected_y', yr )
  var.hrwage   = paste0( 'hrwage_y', yr )
  
  current_wage = paste0( 'hrwage_y', yr - 1 )
  min_wage = paste0( 'minwage_y', yr - 1 )
  new_min_wage = paste0( 'minwage_y', yr )
  
  sim.data %<>%
    mutate( !!var.relevant := relevantWorkers( get(current_wage),
                                               get(min_wage),
                                               get(new_min_wage) ) )
  
  sim.data %<>%
    mutate( nat_current_wage = get(current_wage) * avg_wage_growth, # add in natural wage growth
            
            temp1 = nat_current_wage < get(min_wage),
            temp1 = 0.25 * ( 1.15 * get(new_min_wage) - nat_current_wage ),
            temp2 = get(new_min_wage) - nat_current_wage,
            temp3 = get(new_min_wage) - get(min_wage),
            temp4 = if_else( temp2 < temp3, temp2, temp3 ),
            temp5 = if_else( temp1 > temp4, temp1, temp4 ),
            temp6 = nat_current_wage + temp5,
            
            !!var.hrwage := if_else( get(var.relevant), temp6, nat_current_wage ) ) %>%
    
    select( -c(nat_current_wage:temp6) )
  
  # test %<>%
  #   mutate( !!var.hrwage := simulateWages( get(var.relevant),
  #                                          get(current_wage),
  #                                          get(min_wage),
  #                                          get(new_min_wage) ) )

}; rm( yr, var.relevant, var.hrwage, current_wage, min_wage, new_min_wage )

# add in final demographic attributes of interest
sim.data %<>% left_join(
  bind_rows( pums.list ) %>%
    select( person_id, agep, race, female, married, any_children),
  by = 'person_id' ) %>%
  
  mutate_at( vars(married, any_children), function(x) as.numeric(x) - 1 ) %>%

  mutate( male = !female,
          
          asian = ( race == 'asian' ),
          black = ( race == 'black' ),
          hispanic = ( race == 'hispanic' ),
          white = ( race == 'white' ),
          
          poc = race %in% c('asian', 'black', 'hispanic'),
          poc_men = !female & poc,
          poc_women = female & poc,
          
          married_parent  =  married & any_children,
          single_parent   = !married & any_children,
          working_mother  = any_children & female,
          working_father  = any_children & !female,
          
          age_16_19 = agep %in% c(16:19),
          age_20_up = agep >= 20,
          
          age_16_24 = agep %in% c(16:24),
          age_25_39 = agep %in% c(25:39),
          age_40_54 = agep %in% c(40:54),
          age_55_up = agep >= 55 ) %>%
  
  select( -agep, race ) %>%
  
  mutate_at( vars(male:age_55_up), as.numeric )

# rescale 2022 numbers to 2021 BLS overall labor force estimates
sim.data %<>%
  group_by( st ) %>%
  mutate( weights_y2022 = weights_y2022 * ( laborforce_2021 / sum(weights_y2022) ) ) %>%
  ungroup( )

# present these data
outcomes <- list()

# national estimate, overall
sim.data %>%
  summarize( #pct_minwage_affected_y2020 = sum(affected_y2020 * weights_y2020) / sum(weights_y2020),
             # pct_15orless_y2020 = sum( (hrwage_y2020 < 15) * weights_y2020 ) / sum(weights_y2020),
             # sum_weights_2020 = sum( weights_y2020 ),
             # sum_minwage_affected_y2020 = pct_minwage_affected_y2020 * sum_weights_2020,
             # sum_15orless_y2020 = pct_15orless_y2020 * sum_weights_2020,
             # 
             # pct_minwage_affected_y2021 = sum(affected_y2021 * weights_y2021) / sum(weights_y2021),
             # pct_15orless_y2021 = sum( (hrwage_y2021 < 15) * weights_y2021 ) / sum(weights_y2021),
             # sum_weights_2021 = sum( weights_y2021 ),
             # sum_minwage_affected_y2021 = pct_minwage_affected_y2021 * sum_weights_2021,
             # sum_15orless_y2021 = pct_15orless_y2021 * sum_weights_2021,
             # 
             pct_minwage_affected_y2022 = sum(affected_y2022 * weights_y2022) / sum(weights_y2022),
             pct_15orless_y2022 = sum( (hrwage_y2022 < 15) * weights_y2022 ) / sum(weights_y2022),
             sum_weights_2022 = sum( weights_y2022 ),
             sum_minwage_affected_y2022 = pct_minwage_affected_y2022 * sum_weights_2022,
             sum_15orless_y2022 = pct_15orless_y2022 * sum_weights_2022
             # 
             # pct_minwage_affected_y2023 = sum(affected_y2023 * weights_y2023) / sum(weights_y2023),
             # pct_15orless_y2023 = sum( (hrwage_y2023 < 15) * weights_y2023 ) / sum(weights_y2023),
             # sum_weights_2023 = sum( weights_y2023 ),
             # sum_minwage_affected_y2023 = pct_minwage_affected_y2023 * sum_weights_2023,
             # sum_15orless_y2023 = pct_15orless_y2023 * sum_weights_2023
  ) %>%
  ungroup( ) -> outcomes[['National Estimates, Overall']]

# national estimate, by demographic groups
for ( demo in c('asian', 'black', 'hispanic', 'white', 'poc',
                'female', 'male', 'poc_women', 'poc_men',
                'working_mother', 'working_father', 'single_parent', 'married_parent',
                'age_16_24', 'age_25_39', 'age_40_54', 'age_55_up',
                'age_16_19', 'age_20_up') ) {
  
  sim.data.temp <- sim.data
  
  sim.data.temp %<>%
      filter( !!sym(demo) == 1 ) %>%
      mutate( demo_group = demo  ) %>%
      group_by( demo_group )
  
  sim.data.temp %<>%
    summarize( # pct_minwage_affected_y2020 = sum(affected_y2020 * weights_y2020) / sum(weights_y2020),
               # pct_15orless_y2020 = sum( (hrwage_y2020 < 15) * weights_y2020 ) / sum(weights_y2020),
               # sum_weights_2020 = sum( weights_y2020 ),
               # sum_minwage_affected_y2020 = pct_minwage_affected_y2020 * sum_weights_2020,
               # sum_15orless_y2020 = pct_15orless_y2020 * sum_weights_2020,
               # 
               # pct_minwage_affected_y2021 = sum(affected_y2021 * weights_y2021) / sum(weights_y2021),
               # pct_15orless_y2021 = sum( (hrwage_y2021 < 15) * weights_y2021 ) / sum(weights_y2021),
               # sum_weights_2021 = sum( weights_y2021 ),
               # sum_minwage_affected_y2021 = pct_minwage_affected_y2021 * sum_weights_2021,
               # sum_15orless_y2021 = pct_15orless_y2021 * sum_weights_2021,
               # 
               pct_minwage_affected_y2022 = sum(affected_y2022 * weights_y2022) / sum(weights_y2022),
               pct_15orless_y2022 = sum( (hrwage_y2022 < 15) * weights_y2022 ) / sum(weights_y2022),
               sum_weights_2022 = sum( weights_y2022 ),
               sum_minwage_affected_y2022 = pct_minwage_affected_y2022 * sum_weights_2022,
               sum_15orless_y2022 = pct_15orless_y2022 * sum_weights_2022
               # 
               # pct_minwage_affected_y2023 = sum(affected_y2023 * weights_y2023) / sum(weights_y2023),
               # pct_15orless_y2023 = sum( (hrwage_y2023 < 15) * weights_y2023 ) / sum(weights_y2023),
               # sum_weights_2023 = sum( weights_y2023 ),
               # sum_minwage_affected_y2023 = pct_minwage_affected_y2023 * sum_weights_2023,
               # sum_15orless_y2023 = pct_15orless_y2023 * sum_weights_2023
    ) %>% ungroup( )
  
  if ( !exists('demo.out') ) { demo.out <- head( sim.data.temp, 0 ) }
    
  demo.out %<>% bind_rows( sim.data.temp )
  
}; rm( demo, sim.data.temp )

outcomes[['National Estimates, Demographic Breakdown']] <- demo.out; rm( demo.out )
  
# state estimates, overall
sim.data %>%
  group_by( st ) %>%
  summarize( # pct_minwage_affected_y2020 = sum(affected_y2020 * weights_y2020) / sum(weights_y2020),
             # pct_15orless_y2020 = sum( (hrwage_y2020 < 15) * weights_y2020 ) / sum(weights_y2020),
             # sum_weights_2020 = sum( weights_y2020 ),
             # sum_minwage_affected_y2020 = pct_minwage_affected_y2020 * sum_weights_2020,
             # sum_15orless_y2020 = pct_15orless_y2020 * sum_weights_2020,
             # 
             # pct_minwage_affected_y2021 = sum(affected_y2021 * weights_y2021) / sum(weights_y2021),
             # pct_15orless_y2021 = sum( (hrwage_y2021 < 15) * weights_y2021 ) / sum(weights_y2021),
             # sum_weights_2021 = sum( weights_y2021 ),
             # sum_minwage_affected_y2021 = pct_minwage_affected_y2021 * sum_weights_2021,
             # sum_15orless_y2021 = pct_15orless_y2021 * sum_weights_2021,
             # 
             pct_minwage_affected_y2022 = sum(affected_y2022 * weights_y2022) / sum(weights_y2022),
             pct_15orless_y2022 = sum( (hrwage_y2022 < 15) * weights_y2022 ) / sum(weights_y2022),
             sum_weights_2022 = sum( weights_y2022 ),
             sum_minwage_affected_y2022 = pct_minwage_affected_y2022 * sum_weights_2022,
             sum_15orless_y2022 = pct_15orless_y2022 * sum_weights_2022
             # 
             # pct_minwage_affected_y2023 = sum(affected_y2023 * weights_y2023) / sum(weights_y2023),
             # pct_15orless_y2023 = sum( (hrwage_y2023 < 15) * weights_y2023 ) / sum(weights_y2023),
             # sum_weights_2023 = sum( weights_y2023 ),
             # sum_minwage_affected_y2023 = pct_minwage_affected_y2023 * sum_weights_2023,
             # sum_15orless_y2023 = pct_15orless_y2023 * sum_weights_2023
  ) %>%
  ungroup( ) -> outcomes[['State Estimates, Overall']]

# state estimate, by demographic groups
for ( demo in c('asian', 'black', 'hispanic', 'white', 'poc',
                'female', 'male', 'poc_women', 'poc_men',
                'working_mother', 'working_father', 'single_parent', 'married_parent',
                'age_16_24', 'age_25_39', 'age_40_54', 'age_55_up',
                'age_16_19', 'age_20_up') ) {
  
  sim.data.temp <- sim.data
  
  sim.data.temp %<>%
    filter( !!sym(demo) == 1 ) %>%
    mutate( demo_group = demo  ) %>%
    group_by( st, demo_group )
  
  sim.data.temp %<>%
    summarize( # pct_minwage_affected_y2020 = sum(affected_y2020 * weights_y2020) / sum(weights_y2020),
               # pct_15orless_y2020 = sum( (hrwage_y2020 < 15) * weights_y2020 ) / sum(weights_y2020),
               # sum_weights_2020 = sum( weights_y2020 ),
               # sum_minwage_affected_y2020 = pct_minwage_affected_y2020 * sum_weights_2020,
               # sum_15orless_y2020 = pct_15orless_y2020 * sum_weights_2020,
               # 
               # pct_minwage_affected_y2021 = sum(affected_y2021 * weights_y2021) / sum(weights_y2021),
               # pct_15orless_y2021 = sum( (hrwage_y2021 < 15) * weights_y2021 ) / sum(weights_y2021),
               # sum_weights_2021 = sum( weights_y2021 ),
               # sum_minwage_affected_y2021 = pct_minwage_affected_y2021 * sum_weights_2021,
               # sum_15orless_y2021 = pct_15orless_y2021 * sum_weights_2021,
               # 
               pct_minwage_affected_y2022 = sum(affected_y2022 * weights_y2022) / sum(weights_y2022),
               pct_15orless_y2022 = sum( (hrwage_y2022 < 15) * weights_y2022 ) / sum(weights_y2022),
               sum_weights_2022 = sum( weights_y2022 ),
               sum_minwage_affected_y2022 = pct_minwage_affected_y2022 * sum_weights_2022,
               sum_15orless_y2022 = pct_15orless_y2022 * sum_weights_2022
               # 
               # pct_minwage_affected_y2023 = sum(affected_y2023 * weights_y2023) / sum(weights_y2023),
               # pct_15orless_y2023 = sum( (hrwage_y2023 < 15) * weights_y2023 ) / sum(weights_y2023),
               # sum_weights_2023 = sum( weights_y2023 ),
               # sum_minwage_affected_y2023 = pct_minwage_affected_y2023 * sum_weights_2023,
               # sum_15orless_y2023 = pct_15orless_y2023 * sum_weights_2023
    ) %>% ungroup( )
  
  if ( !exists('demo.out') ) { demo.out <- head( sim.data.temp, 0 ) }
  
  demo.out %<>% bind_rows( sim.data.temp )
  
}; rm( demo, sim.data.temp )

outcomes[['State Estimates, Demographic Breakdown']] <- demo.out; rm( demo.out )

file <- paste0('/projects/sstapleton/min_wage_fight/simulation7_output.xlsx')
wb <- createWorkbook()
sheetnames <- names(outcomes)

sheets <- lapply( sheetnames, createSheet, wb = wb )
void <- Map( addDataFrame, outcomes, sheets )

saveWorkbook(wb, file = file)

sim.data %>%
  filter( st == 'Massachusetts' ) %>%
  filter( hrwage_y2022 < 50 ) %>%
  ggplot( aes( x = hrwage_y2022, w = weights_y2022 ) ) +
  geom_density( fill = oxfam_colors[1] ) +
  theme_minimal( )















sim.data %>%
  group_by( st ) %>%
  summarize( minwage_affected_y2020 = sum(affected_y2020 * weights_y2020) / sum(weights_y2020),
             pct_15orless_y2020 = sum( (hrwage_y2020 < 15) * weights_y2020 ) / sum(weights_y2020),
             sum_weights_2020 = sum( weights_y2020 ),
             
             minwage_affected_y2021 = sum(affected_y2021 * weights_y2021) / sum(weights_y2021),
             pct_15orless_y2021 = sum( (hrwage_y2021 < 15) * weights_y2021 ) / sum(weights_y2021),
             sum_weights_2021 = sum( weights_y2021 ),
             
             minwage_affected_y2022 = sum(affected_y2022 * weights_y2022) / sum(weights_y2022),
             pct_15orless_y2022 = sum( (hrwage_y2022 < 15) * weights_y2022 ) / sum(weights_y2022),
             sum_weights_2022 = sum( weights_y2022 ),
             
             minwage_affected_y2023 = sum(affected_y2023 * weights_y2023) / sum(weights_y2023),
             pct_15orless_y2023 = sum( (hrwage_y2023 < 15) * weights_y2023 ) / sum(weights_y2023),
             sum_weights_2023 = sum( weights_y2023 ),
             
             minwage_affected_y2024 = sum(affected_y2024 * weights_y2024) / sum(weights_y2024),
             pct_15orless_y2024 = sum( (hrwage_y2024 < 15) * weights_y2024 ) / sum(weights_y2024),
             sum_weights_2024 = sum( weights_y2024 ),
             
             minwage_affected_y2025 = sum(affected_y2025 * weights_y2025) / sum(weights_y2025),
             pct_15orless_y2025 = sum( (hrwage_y2025 < 15) * weights_y2025 ) / sum(weights_y2025),
             sum_weights_2025 = sum( weights_y2025 ),
             
             minwage_affected_y2026 = sum(affected_y2026 * weights_y2026) / sum(weights_y2026),
             pct_15orless_y2026 = sum( (hrwage_y2026 < 15) * weights_y2026 ) / sum(weights_y2026),
             sum_weights_2026 = sum( weights_y2026 ),
             ) %>%
  ungroup( ) -> out

# print out aggregate data
out <- out[, c(1:10)]

write.csv(out, '/projects/sstapleton/min_wage_fight/simulation2_output.csv')

# visualize wage distributions in a sample of states
sim.data %>%
  select( st, final_weight, hrwage_y2019, hrwage_y2020, hrwage_y2021, hrwage_y2022 ) %>%
  filter( st %in% c('California', 'Connecticut',
                    'District of Columbia',
                    'Massachusetts', 'New Jersey',
                    'Washington') ) %>%
  pivot_longer( -c(st, final_weight) ) %>%
  mutate( name = parse_number(name) ) %>%
  filter( value < 50 ) %>%
  ggplot( aes( x = value, weights = final_weight ) ) +
  geom_density( adjust = 0.5, color = '#0C884A', fill = '#61A534' ) +
#  geom_histogram( binwidth = 2 ) +
  geom_vline( xintercept = 15,
              linetype = 'dashed',
              color = '#E70052' ) +
  facet_grid( st ~ name ) +
  labs( x = 'Hourly Wage ($USD)',
        y = '',
        title = 'Weighted Distribution of Hourly Wages After Minimum Wage Adjustments, 2019-2022',
        subtitle = 'The red vertical line indicates a $15 hourly wage. The states below have min wage changes impacting workers close to the $15 line.' ) +
  theme_minimal( ) +
  theme( axis.ticks.y = element_blank(),
         axis.text.y  = element_blank() )

# create demographic splits of the data
sim.data.dem <- sim.data %>% left_join(
  bind_rows( pums.list ) %>%
    select( person_id, race, female, married, any_children),
  by = 'person_id' )

sim.data.dem %>%
  group_by( st, race, female, married, any_children ) %>%
  summarize( minwage_affected_y2020 = sum(affected_y2020 * weights_y2020) / sum(weights_y2020),
             pct_15orless_y2020 = sum( (hrwage_y2020 < 15) * weights_y2020 ) / sum(weights_y2020),
             sum_weights_2020 = sum( weights_y2020 ),
             
             minwage_affected_y2021 = sum(affected_y2021 * weights_y2021) / sum(weights_y2021),
             pct_15orless_y2021 = sum( (hrwage_y2021 < 15) * weights_y2021 ) / sum(weights_y2021),
             sum_weights_2021 = sum( weights_y2021 ),
             
             minwage_affected_y2022 = sum(affected_y2022 * weights_y2022) / sum(weights_y2022),
             pct_15orless_y2022 = sum( (hrwage_y2022 < 15) * weights_y2022 ) / sum(weights_y2022),
             sum_weights_2022 = sum( weights_y2022 )
  ) %>%
  ungroup( ) -> out.dem

write.csv(out.dem, '/projects/sstapleton/min_wage_fight/simulation2_output_bydemographics.csv')

sim.data.dem %>%
  group_by( race, female, married, any_children ) %>%
  summarize( minwage_affected_y2020 = sum(affected_y2020 * weights_y2020) / sum(weights_y2020),
             pct_15orless_y2020 = sum( (hrwage_y2020 < 15) * weights_y2020 ) / sum(weights_y2020),
             sum_weights_2020 = sum( weights_y2020 ),
             
             minwage_affected_y2021 = sum(affected_y2021 * weights_y2021) / sum(weights_y2021),
             pct_15orless_y2021 = sum( (hrwage_y2021 < 15) * weights_y2021 ) / sum(weights_y2021),
             sum_weights_2021 = sum( weights_y2021 ),
             
             minwage_affected_y2022 = sum(affected_y2022 * weights_y2022) / sum(weights_y2022),
             pct_15orless_y2022 = sum( (hrwage_y2022 < 15) * weights_y2022 ) / sum(weights_y2022),
             sum_weights_2022 = sum( weights_y2022 )
  ) %>%
  ungroup( ) -> out.dem.tot

write.csv(out.dem.tot, '/projects/sstapleton/min_wage_fight/simulation2_output_bydemographics_national.csv')
