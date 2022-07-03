library(pacman)
p_load(baseballr)
p_load(tidyverse)

LN <- 'Ohtani'
FN <- 'Shohei'

id <- 660271
yr <- 2022

source("functions/make_breakchart.R")
source("functions/make_okuyukichart.R")
source('cheatsheets/colour_palette.R')

player <- baseballr::scrape_statcast_savant(
  start_date = paste0(yr, '-', 3, '-', '1'),
  end_date = paste0(yr, '-', 11, '-', '30'),
  player_type = "pitcher", playerid = id
) %>%
  filter(!(pitch_type %in% c("PO", "IN", "null", ""))) %>%
  filter(game_type == "R") %>%
  mutate(
    pfx_x_cm = pfx_x * 30.48,
    pfx_z_cm = pfx_z * 30.48,
    release_speed_km = release_speed * 1.61,
  )

player %>%
  make_breakchart(., pfx_x = "pfx_x_cm", pfx_z = "pfx_z_cm", colour_palette = pitch_colour, plot_type = "colour") +
  ggtitle("Shohei Ohrani Break-Chart 2022")

player$pfx_z_cm %>% range()

player %>%
  make_breakchart(
    ., pfx_x = "pfx_x_cm", pfx_z = "pfx_z_cm", colour_palette = pitch_colour, plot_type = "colour", 
    split = "game_date",
    lims = c(-57, 57, -57, 57)
  ) +
  ggtitle("Shohei Ohtani Break-Chart 2022")

ggsave("fig/ohtani_break.png", width = 9.5, height = 9)

player$release_speed_km %>% range()

player %>%
  make_okuyukichart(
    ., release_speed = "release_speed_km", pfx_z = "pfx_z_cm", colour_palette = pitch_colour, plot_type = "colour", 
    split = "game_date",
    xlims = c(110, 164), ylims = c(-57, 57)
  ) +
  ggtitle("Shohei Ohtani Velo-VerticalBreak 2022")

ggsave("fig/ohtani_okuyuki.png", width = 9.5, height = 9)


player <- baseballr::scrape_statcast_savant(
  start_date = paste0(yr, '-', 3, '-', '1'),
  end_date = paste0(yr, '-', 11, '-', '30'),
  player_type = "batter", playerid = id
) %>%
  filter(!(pitch_type %in% c("PO", "IN", "null", ""))) %>%
  filter(game_type == "R") %>%
  mutate(
    pfx_x_cm = pfx_x * 30.48,
    pfx_z_cm = pfx_z * 30.48,
    release_speed_km = release_speed * 1.61,
  )


p_load(ggthemes)
p_load(lubridate)

source('functions/plot_battedball.R')
source('functions/geom_spraytable.R')

bb <- player %>%
  filter(type == 'X') %>%
  filter(game_date <= "2022-06-30") %>%
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
    game_month = month(game_date, label = T)
  )

geom_spraytable(data = bb,
                aes(x = hc_x, y = -hc_y,
                    fill = base_hits)) +
  geom_point(alpha = .9, shape = "circle filled", colour = "black", size = 2) +
  ggplot2::scale_colour_manual(name = 'Result') +
  scale_fill_viridis_d(name = 'Result', option = "C") +
  theme_few() +
  facet_wrap(~ game_month) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = 'Shohei Ohtani: Spray-Chart 2022')

ggsave("fig/ohtani_spray.png", width = 8, height = 4)
