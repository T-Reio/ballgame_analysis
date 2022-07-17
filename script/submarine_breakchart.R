library(pacman)
p_load(baseballr)
p_load(tidyverse)
p_load(lubridate)
p_load(patchwork)
source("functions/make_breakchart.R")
source("functions/make_okuyukichart.R")
source("functions/make_pitchLocation.R")
source("functions/geom_zonebox.R")
source("cheatsheets/colour_palette.R")
source("cheatsheets/event_files.R")

LN <- 'Rogers'
FN <- 'Tyler'

playerid_lookup(LN, FN) %>%
  select(first_name, last_name, birth_year, mlbam_id)

id <- c(628318, 643511, 446899, 643256)
yr <- c(2018, 2022, 2018, 2022)

player <- map2_df(
  .x = id, .y = yr,
  .f = ~ scrape_statcast_savant(
    start_date = paste0(.y, '-', 3, '-', '1'),
    end_date = paste0(.y, '-', 11, '-', '30'),
    player_type = "pitcher", playerid = .x
  ) %>%
    filter(!(pitch_type %in% c("PO", "IN", "null", ""))) %>%
    dplyr::filter(game_type != 'S') %>%
    mutate(
      pfx_x_cm = pfx_x * 30.48,
      pfx_z_cm = pfx_z * 30.48,
      plate_x_cm = plate_x * 30.48,
      plate_z_cm = plate_z * 30.48,
      release_speed_km = release_speed * 1.61,
    )
)

p1 <- make_breakchart(
  player, "pfx_x_cm", "pfx_z_cm", 
  colour_palette = pitch_colour,
  plot_type = "colour",
  split = "player_name"
)

ggsave(paste0("fig/Submarine_breakchart.png"), p1, width = 6, height = 5)

p2 <- make_okuyukichart(
  player, "release_speed_km", "pfx_z_cm",
  colour_palette = pitch_colour,
  plot_type = "colour",
  split = "player_name"
)

p2

ggsave(paste0("fig/Submarine__okuyukichart.png"), p2, width = 6, height = 5)

label <- c("Pitch Type", "#", "Usage", "Velocity (km)", "Spin Rate (rpm)", "HorzBreak (cm)", "InducedVertBreak (cm)", "Zone%", "CStr%", "Swing%", "Contact%", "SwStr%", "CSW%", "GB%", "LD%", "Hard%", "xwOBAcon", "wOBA", "PitchValue", "PV/C")
player %>%
  filter(player_name == "Cimber, Adam") %>%
  dplyr::group_by(pitch_name) %>%
  dplyr::summarise(number = n(), usage = round(100 * n() / nrow(player %>%
                                                                  filter(player_name == "Cimber, Adam")), 1),
                   speed = round(mean(release_speed_km), 1),
                   srate = round(mean(release_spin_rate, na.rm = T), 1),
                   hmov = round(mean(pfx_x_cm, na.rm = T), 1),
                   vmov = round(mean(pfx_z_cm, na.rm = T), 1),
                   Zone = round(sum(zone %in% 1:9)/n() * 100, 1),
                   CStr = round(sum(description %in% calledstr) / n() * 100, 1),
                   Swing = round(sum(description %in% swing) / n() * 100, 1),
                   Contact = round(sum(description %in% contact) / 
                                     sum(description %in% swing, na.rm = T) * 100, 1),
                   SwStr = round(sum(description %in% swst) / n() * 100, 1),
                   CSW = round((sum(description %in% calledstr) / n() + sum(description %in% swst) / n()) * 100, 1), 
                   GB = round(sum(bb_type == "ground_ball", na.rm = T) / 
                                sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
                   LD = round(sum(bb_type == "line_drive", na.rm = T) / 
                                sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
                   Hard = round(sum(launch_speed[type == 'X'] >= 95, na.rm = T) /
                                  sum(!is.na(launch_speed[type == 'X']), na.rm = T) * 100, digits = 1),
                   xwOBAcon = round(mean(estimated_woba_using_speedangle, na.rm = T), 3),
                   wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), 3),
                   PV = round(sum(-delta_run_exp, na.rm = T), 2),
                   PV_C = round(sum(-delta_run_exp, na.rm = T) / n(), 2),
  ) %>%
  dplyr::arrange(desc(number)) %>%
  set_names(label) -> summary


player %>%
  filter(player_name == "Makita, Kazuhisa") %>%
  dplyr::group_by(pitch_name) %>%
  dplyr::summarise(number = n(), usage = round(100 * n() / nrow(player %>%
                                                                  filter(player_name == "Makita, Kazuhisa")), 1),
                   speed = round(mean(release_speed_km), 1),
                   srate = round(mean(release_spin_rate, na.rm = T), 1),
                   hmov = round(mean(pfx_x_cm, na.rm = T), 1),
                   vmov = round(mean(pfx_z_cm, na.rm = T), 1),
                   Zone = round(sum(zone %in% 1:9)/n() * 100, 1),
                   CStr = round(sum(description %in% calledstr) / n() * 100, 1),
                   Swing = round(sum(description %in% swing) / n() * 100, 1),
                   Contact = round(sum(description %in% contact) / 
                                     sum(description %in% swing, na.rm = T) * 100, 1),
                   SwStr = round(sum(description %in% swst) / n() * 100, 1),
                   CSW = round((sum(description %in% calledstr) / n() + sum(description %in% swst) / n()) * 100, 1), 
                   GB = round(sum(bb_type == "ground_ball", na.rm = T) / 
                                sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
                   LD = round(sum(bb_type == "line_drive", na.rm = T) / 
                                sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
                   Hard = round(sum(launch_speed[type == 'X'] >= 95, na.rm = T) /
                                  sum(!is.na(launch_speed[type == 'X']), na.rm = T) * 100, digits = 1),
                   xwOBAcon = round(mean(estimated_woba_using_speedangle, na.rm = T), 3),
                   wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), 3),
                   PV = round(sum(-delta_run_exp, na.rm = T), 2),
                   PV_C = round(sum(-delta_run_exp, na.rm = T) / n(), 2),
  ) %>%
  dplyr::arrange(desc(number)) %>%
  set_names(label) -> summary
