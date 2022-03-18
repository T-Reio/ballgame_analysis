library(baseballr)
library(tidyverse)

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
