###########################################################################
# Goal:    Visualize results up to rake_pums.R
# Author:  Stephen Stapleton
# Created: 2021-11-06
# Updated: 2021-11-11
###########################################################################

# set oxfam color defaults
oxfam_colors <- c('#61A534', '#0C884A', '#E70052')

viz.data <-
  bind_rows( pums.list ) %>%
  mutate( sample = 'Full Population' ) %>%
  bind_rows(
    bind_rows( pums.list.u15 ) %>%
      mutate( sample = '$15/Hour or Less' ) )

# QUESTION: how many workers at below $15/hour does each model find?
viz.data %>%
  filter( hrwage < 15 ) %>%
  group_by( st, sample ) %>%
  summarize( n = sum(final_weight) ) %>%
  ggplot( aes( x = st, y = n/1000, fill = sample) ) +
  geom_bar( stat = 'identity', position = position_dodge( width = 0.8 ), width = 0.75 ) +
  theme_minimal( ) +
  scale_fill_manual( values = oxfam_colors[1:2] ) +
  theme( legend.position = 'bottom' ) +
  labs( x = '',
        y = 'Number of Workers (thousands)',
        fill = 'Sample',
        title = 'How many workers below $15 / hour does each modelling approach approximate?')

# QUESTION: what does the spread of wage look like in each model?
viz.data %>%
  filter( hrwage < 15 ) %>%
  ggplot( aes( x = hrwage, weight = final_weight, fill = sample ) ) +
  geom_density( alpha = 0.75 ) +
  theme_minimal( ) +
  scale_fill_manual( values = oxfam_colors[1:2] ) +
  theme( legend.position = 'bottom' ) +
  labs( x = '',
        y = 'Density of Wage Predictions',
        fill = 'Sample',
        title = 'How does sample selection affect the distribution of wage predictions?')

viz.data %>%
  filter( hrwage < 15 ) %>%
  ggplot( aes( x = hrwage, weight = final_weight, fill = sample ) ) +
  geom_density( alpha = 0.75 ) +
  theme_minimal( ) +
  scale_fill_manual( values = oxfam_colors[1:2] ) +
  theme( legend.position = 'bottom' ) +
  labs( x = '',
        y = 'Density of Wage Predictions',
        fill = 'Sample',
        title = 'Are there any outliers in model performance by state?') +
  facet_wrap( ~st, nrow = 2 )
  
  