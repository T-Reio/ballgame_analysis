library(pacman)
p_load(baseballr)
p_load(tidyverse)

LN <- 'Greene'
FN <- 'Hunter'

playerid_lookup(LN, FN)

id <- 668881
yr <- 2022

source("functions/make_breakchart.R")
source("functions/make_okuyukichart.R")
source('cheatsheets/colour_palette.R')

player <- map_df(
  .x = yr,
  .f = ~ scrape_statcast_savant(
    start_date = paste0(.x, '-', 3, '-', '1'),
    end_date = paste0(.x, '-', 11, '-', '30'),
    player_type = "pitcher", playerid = id
  ) %>%
    filter(!(pitch_type %in% c("PO", "IN", "null", ""))) %>%
    filter(game_type == "R") %>%
    mutate(
      pfx_x_cm = pfx_x * 30.48,
      pfx_z_cm = pfx_z * 30.48,
      release_speed_km = release_speed * 1.61,
    )
)

player %>%
  make_breakchart(., pfx_x = "pfx_x_cm", pfx_z = "pfx_z_cm", colour_palette = pitch_colour, plot_type = "colour") +
  ggtitle(paste0(FN, " ", LN, ": Break-Chart"))

player %>%
  make_okuyukichart(
    ., release_speed = "release_speed_km", pfx_z = "pfx_z_cm", colour_palette = pitch_colour, plot_type = "fill", 
    xlims = c(110, 164), ylims = c(-57, 57)
  ) +
  ggtitle(paste0(FN, " ", LN, ": Velo-VerticalBreak"))
          