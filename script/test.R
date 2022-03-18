library(baseballr)
library(tidyverse)
library(mgcv)

data <- baseballr::scrape_statcast_savant(start_date = "2021-03-01",
                                  end_date = "2021-11-30",
                                  player_type = "pitcher", playerid = 571760)

make_breakchart(data %>% filter(!is.na(pitch_name)), "pfx_x", "pfx_z", pitch_type = "pitch_name", split = 'stand', colour_palette = pitch_colour, lims = c(-2, 2, -1, 3))

make_okuyukichart(data, 
                  xlab = 'Velocity (mph)',
                  xlims = c(min(data$release_speed, na.rm = T),
                                  max(data$release_speed, na.rm = T)))
make_statsSummary(data, group_by = c('pitch_type'))
data %>%
  group_by('pitch_type', 'game_date') %>%
  summarise(n = n())

data[c('pitch_type', 'game_date')]

geom_zonebox(colour = 'red', scale = 'cm')


make_pitchLocation(data, scale = 'ft', box_colour = 'black') +
  labs(title = 'Heaney-Chan')

called <- data %>%
  filter(description %in% calledstr | description == 'ball') %>%
  mutate(
    strike = if_else(type == 'S', 1, 0),
  )

zone_grid <- called %>%
  make_gam_grid(., objective = 'strike', scale = 'ft')

geom_gamMap(zone_grid) +
  geom_zonebox(colour = 'red', scale = 'ft') +
  theme_bw()

ob_data <- called %>%
  split(pull(., stand)) %>%
  map(.f = ~ make_gam_grid(., objective = 'strike', scale = 'ft'))

g1 <- geom_gamMap(ob_data[[1]]) +
  geom_zonebox(colour = 'red', scale = 'ft') +
  theme_bw() +
  theme(legend.position = "none")

g2 <- geom_gamMap(ob_data[[2]]) +
  geom_zonebox(colour = 'red', scale = 'ft') +
  theme_bw()

library(patchwork)

g1 | g2
