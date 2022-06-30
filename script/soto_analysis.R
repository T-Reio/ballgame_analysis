library(pacman)
p_load(tidyverse)
p_load(baseballr)
p_load(ggthemes)

source('functions/event_lists.R')
source('functions/colour_palette.R')
source("functions/func_cm.R")
source('functions/plot_battedball.R')
source('functions/geom_spraytable.R')

LN <- 'Soto'
FN <- 'Juan'

#baseballr::playerid_lookup(LN, FN) %>%
#  select(first_name, last_name, birth_year, mlbam_id)
id <- 665742
yr <- c(2019, 2020, 2021, 2022)


player <- map_df(
  .x = yr,
  .f = ~ scrape_statcast_savant(
    start_date = paste0(.x, '-', 3, '-', '1'),
    end_date = paste0(.x, '-', 11, '-', '30'),
    player_type = "batter", playerid = id
  ) %>%
    filter(type == 'X') %>%
    mutate(
      pfx_x_cm = cm(pfx_x),
      pfx_z_cm = cm(pfx_z),
      release_speed_km = km(release_speed),
      launch_speed_km = km(launch_speed),
    )
)

fb <- player %>%
  filter(type == 'X') %>%
  filter(bb_type %in% c("line_drive", "fly_ball")) %>%
  mutate(
    base_hits = factor(
      dplyr::case_when(
        events == 'single' ~ 'Single',
        events == 'double' ~ 'Double',
        events == 'triple' ~ 'Triple',
        events == 'home_run' ~ 'HR',
        events == 'field_error' ~ 'Error',
        TRUE ~ 'Out'
      ), 
      levels = c('HR', 'Triple', 'Double', 'Single', 'Error', 'Out')
    ),
    Barrel = if_else(launch_speed_angle == 6, 1, 0),
    launch_speed_km = launch_speed * 1.61,
  )

geom_spraytable(data = fb,
                aes(x = hc_x, y = -hc_y,
                    fill = base_hits)) +
  geom_point(alpha = .7, shape = "circle filled", colour = "black", size = 2) +
  ggplot2::scale_colour_manual(name = 'Result') +
  scale_fill_viridis_d(name = 'Result', option = "magma") +
  theme_few() +
  facet_wrap(~ game_year) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = 'Juan Soto: Flyball, Line Drive Spray-Chart') -> pl

ggsave(filename = "fig/soto_spray_chart.png", pl)
