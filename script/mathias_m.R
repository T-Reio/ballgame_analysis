library(pacman)
p_load(tidyverse)
p_load(baseballr)
p_load(ggrepel)
p_load(lubridate)
p_load(clipr)
p_load(patchwork)
p_load(rvest)
p_load(REdaS)
p_load(ggExtra)
p_load(mgcv)

setwd("C:/Users/r-tanji/analysis")
source("func/scrape_statcast_tj.R")
source("func/savant/savant_event_lists.R")
source("func/ggplot/geom_splaytable.R")

#get ID

playerName <- "mathias_m"

p_table <- chadwick_player_lu()
player_ids <- p_table %>%
  filter(name_last == "Mathias", name_first == "Mark") %>%
  select(key_mlbam, key_retro, key_mlbam, key_fangraphs, key_bbref_minors)
rm(p_table)

id_mlbam <- player_ids$key_mlbam
id_fg <- player_ids$key_fangraphs
id_bref_milb <- player_ids$key_bbref_minors

df <- read_rds(paste0("data/foreign_player/savant_", playerName, ".rds"))
mlb_gamelogs <- map_df( #ゲームログ
  2020:2022,
  .f = ~ baseballr::batter_game_logs_fg(playerid = id_fg, year = .x)
)

milb_gamelogs <- map_df( #FG MiLB gamelog
  2020:2022,
  .f = ~ baseballr::milb_batter_game_logs_fg(playerid = id_fg, year = .x)
)

# MLBAM ---------------------------------------

# df <- map_df( #5年分の鯖データ
#   2020:2022,
#   .f = ~ baseballr::scrape_statcast_savant(
#     start_date = paste0(.x, "-03-01"), end_date = paste0(.x, "-11-30"),
#     playerid = id_mlbam, player_type = "batter"
#   ) %>%
#     filter(game_type == "R")
# )

# df <- df %>% # 変数作成
#   mutate( # imperial to metric
#     release_speed_km = release_speed * 1.609,
#     pfx_x_cm = pfx_x * 30.48,
#     pfx_z_cm = pfx_z * 30.48,
#     plate_x_cm = plate_x * 30.48,
#     plate_z_cm = plate_z * 30.48,
#     xwOBA_value = if_else(is.na(estimated_woba_using_speedangle), woba_value, estimated_woba_using_speedangle),
#     launch_speed_km = launch_speed * 1.609,
#     Pitch = case_when(
#       pitch_name %in% c("2-Seam Fastball", "4-Seam Fastball", "Cutter", "Sinker") ~ "速球",
#       pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Sweeper", "Slurve") ~ "曲がり球",
#       pitch_name %in% c("Changeup", "Split-Finger") ~ "落ち球",
#       TRUE ~ "その他"
#     ),
#     pitch_name_2 = case_when(
#       pitch_name %in% c("2-Seam Fastball", "Sinker") ~ "2-Seam Fastball",
#       pitch_name %in% c("Slider", "Sweeper", "Slurve") ~ "Slider",
#       pitch_name %in% c("Curveball", "Knuckle Curve") ~ "Curveball",
#       TRUE ~ pitch_name
#     ),
#     launch_speed_angle_text = case_when(
#       launch_speed_angle == 1 ~ "Weak",
#       launch_speed_angle == 2 ~ "Topped",
#       launch_speed_angle == 3 ~ "Under",
#       launch_speed_angle == 4 ~ "Flare/Burner",
#       launch_speed_angle == 5 ~ "Solid Contact",
#       launch_speed_angle == 6 ~ "Barrel",
#       TRUE ~ NA_character_
#     ),
#     launch_speed_angle_text_2 = case_when(
#       launch_speed_angle_text == "Barrel" ~ "Barrel",
#       launch_speed_angle_text == "Solid Contact" ~ "Solid Contact",
#       launch_speed_angle_text == "Flare/Burner" ~ "Flare/Burner",
#       launch_speed_angle_text %in% c("Weak", "Topped", "Under") ~ "Other",
#       TRUE ~ NA_character_
#     ),
#     spray_angle = rad2deg(atan((hc_x - 125.42)/(198.27 - hc_y))),
#     mirror_spray = if_else(stand == 'R', -spray_angle, spray_angle),
#     bb_dim_type = case_when(
#       mirror_spray >= 15 ~ "Pull",
#       mirror_spray >= -15 & mirror_spray <= 15 ~ "Cent",
#       mirror_spray <= -15 ~ "Oppo",
#       TRUE ~ NA_character_
#     ),
#     result = factor(
#       case_when(
#         events == 'single' ~ 'Single',
#         events == 'double' ~ 'Double',
#         events == 'triple' ~ 'Triple',
#         events == 'home_run' ~ 'HR',
#         events == 'field_error' ~ 'Error',
#         TRUE ~ 'Out'
#       ),
#       levels = c('HR', 'Triple', 'Double', 'Single', 'Error', 'Out')
#     ),
#   ) %>%
#   mutate_if(.predicate = is.character, ~ if_else(. == "", NA_character_, .))
# 
# df %>%
#   saveRDS(paste0("data/foreign_player/savant_", playerName, ".rds"))

## 球種粗分類・基本スタッツ #############################
rv <- df %>%
  filter(game_year >= 2020) %>%
  #filter(events %in% PAresult) %>%
  group_by(p_throws, stand, Pitch) %>%
  summarise(
    RV_C = sum(delta_run_exp) / n() * 100,
  ) %>%
  ungroup()


df %>%
  filter(game_year >= 2020) %>%
  filter(!is.na(events), events != "") %>%
  #filter(events %in% PAresult) %>%
  group_by(p_throws, stand, Pitch) %>%
  summarise(
    PA = n(), #打席
    H = sum(events %in% c("single","double","triple","home_run")),
    `2B` = sum(events %in% c("double")),
    `3B` = sum(events %in% c("triple")),
    HR = sum(str_detect(events, "home_run")),
    BA = H / (PA - sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    OBP = sum(events %in% c("single","double","triple","home_run", "walk", "hit_by_pitch")) / 
      PA,
    SLG = (sum(events %in% c("single")) + `2B` * 2 + `3B` * 3 + HR * 4) / (PA - sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    OPS = SLG + OBP,
  ) %>%
  ungroup() %>%
  left_join(., rv) %>%
  mutate(p_throws = if_else(p_throws == "L", "左", "右")) %>%
  filter(Pitch != "その他") %>%
  select(-stand) %>%
  arrange(p_throws, -PA) %>%
  set_names(c("投手", "球種", "打席", "安打", "二塁打", "三塁打", "HR", "打率", "出塁率", "長打率", "OPS", "RV/100"))

clipr::write_clip(.Last.value)

# Plate-Discipline----------------------------------

df %>%
  filter(game_year >= 2020) %>%
  group_by(game_year, p_throws, stand) %>%
  summarise(
    N = n(),
    Zone_pct = sum(zone %in% 1:9, na.rm = T) / sum(!is.na(zone)) * 100,
    Swing = sum(description %in% swing)/ n() * 100,
    Chase = sum(description[zone >= 11] %in% swing)/ sum(zone >= 11) * 100,
    Contact = sum(description %in% contact) / sum(description %in% swing) * 100,
    Exit_Speed = mean(launch_speed_km[!is.na(bb_type)], na.rm = T),
    GB = sum(bb_type == "ground_ball", na.rm = T) / sum(!is.na(bb_type)) * 100,
    LD = sum(bb_type == "line_drive", na.rm = T) / sum(!is.na(bb_type)) * 100,
    FB = sum(bb_type == "fly_ball", na.rm = T) / sum(!is.na(bb_type)) * 100,
    PU = sum(bb_type == "popup", na.rm = T) / sum(!is.na(bb_type)) * 100,
    xwOBA = sum(xwOBA_value, na.rm = T) / sum(woba_denom, na.rm = T),
  ) %>%
  ungroup() %>%
  select(-stand, -LD, -PU) %>%
  mutate(p_throws = if_else(p_throws == "L", "左", "右")) %>%
  arrange(game_year, p_throws) %>%
  set_names(c("Season", "投手", "投球数", "Zone%", "Swing%", "Chase%", "Contact%", "打球速度", "GB%", "FB%", "xwOBA"))

clipr::write_clip(.Last.value)

### 球種別 ======

pitchtype_rv <- df %>%
  filter(game_year >= 2020) %>%
  group_by(p_throws, pitch_name_2) %>%
  summarise(
    RV = sum(delta_run_exp),
    RV_C = sum(delta_run_exp) * 100 / n(),
  ) %>%
  ungroup()

pitchtype <- df %>%
  filter(game_year >= 2020) %>%
  filter(!is.na(events), events != "") %>%
  group_by(p_throws, pitch_name_2) %>%
  summarise(
    PA = n(),
    OBP = sum(events %in% c("single","double","triple","home_run", "walk", "hit_by_pitch")) / 
      PA,
    SLG = (sum(events %in% c("single")) + sum(events %in% c("double")) * 2 + sum(events %in% c("triple")) * 3 + sum(str_detect(events, "home_run")) * 4) / (PA - sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    OPS = SLG + OBP,
    BA = sum(events %in% c("single","double","triple","home_run")) / (PA - sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    xBA = sum(estimated_ba_using_speedangle, na.rm = T) / sum(events %in% AtBat, na.rm = T),
    wOBA = sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T),
    xwOBA = sum(xwOBA_value, na.rm = T) / sum(woba_denom, na.rm = T),
    XBH = sum(events %in% c("double")) + sum(events %in% c("triple")) + sum(str_detect(events, "home_run")),
    HR = sum(str_detect(events, "home_run")),
  ) %>%
  ungroup() %>%
  select(-SLG, -OBP) %>%
  arrange(p_throws, -PA) %>%
  filter(pitch_name_2 != "Eephus")

pitchtype %>%
  left_join(., pitchtype_rv, by = join_by(p_throws, pitch_name_2)) %>%
  relocate(RV, .before = BA) %>%
  relocate(RV_C, .before = BA) %>%
  split(pull(., p_throws)) %>%
  map(
    ., 
    ~ select(.x, -p_throws) %>%
      set_names(c("球種", "打席", "OPS", "RV", "RV/100", "打率", "xBA", "wOBA", "xwOBA", "長打", "HR"))
  ) -> pitch_type_split

pitch_type_split[["R"]]

clipr::write_clip(.Last.value)

pitch_type_split[["L"]]

clipr::write_clip(.Last.value)

# 球種別Plate Discipline-----------------------------

total <- df %>%
  filter(game_year >= 2020) %>%
  group_by(p_throws, stand) %>%
  summarise(N_tot = n())

df %>%
  filter(game_year >= 2020) %>%
  group_by(p_throws, stand, pitch_name_2) %>%
  summarise(
    N = n(),
    Zone_pct = sum(zone %in% 1:9, na.rm = T) / sum(!is.na(zone)) * 100,
    Swing = sum(description %in% swing)/ n() * 100,
    O_Swing = sum(description[zone %in% 11:14] %in% swing) / sum(zone %in% 11:14) * 100,
    Contact = sum(description %in% contact) / sum(description %in% swing) * 100,
    SwStr = (Swing * .01) * (1 - Contact * .01) * 100,
  ) %>%
  ungroup() %>%
  left_join(total, by = join_by(p_throws, stand)) %>%
  mutate(ratio = N / N_tot * 100) %>%
  relocate(ratio, .after = pitch_name_2) %>%
  select(-stand, -N_tot) %>%
  mutate(p_throws = if_else(p_throws == "L", "左", "右")) %>%
  arrange(p_throws, -N) %>%
  filter(pitch_name_2 != "Eephus") %>%
  set_names(c("投手", "球種", "使用比率(%)", "投球数", "Zone%", "Swing%", "Chase%", "Contact%", "SwStr%"))

clipr::write_clip(.Last.value)

# 球速別 --------------------------------------

velo_rv <- df %>%
  filter(!is.na(release_speed)) %>%
  mutate(velo_class = if_else(release_speed_km >= 145, "145+", "145-")) %>%
  group_by(game_year, velo_class) %>%
  summarise(
    RV = sum(delta_run_exp),
    RV_C = sum(delta_run_exp) * 100 / n(),
    Swing = sum(description %in% swing)/ n() * 100,
    Chase = sum(description[zone >= 11] %in% swing)/ sum(zone >= 11) * 100,
    Contact = sum(description %in% contact) / sum(description %in% swing) * 100,
  ) %>%
  ungroup()

velo <- df %>%
  filter(!is.na(release_speed)) %>%
  filter(!is.na(events), events != "") %>%
  mutate(velo_class = if_else(release_speed_km >= 145, "145+", "145-")) %>%
  group_by(game_year, velo_class) %>%
  summarise(
    PA = n(),
    OBP = sum(events %in% c("single","double","triple","home_run", "walk", "hit_by_pitch")) / 
      PA,
    SLG = (sum(events %in% c("single")) + sum(events %in% c("double")) * 2 + sum(events %in% c("triple")) * 3 + sum(str_detect(events, "home_run")) * 4) / (PA - sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    OPS = SLG + OBP,
    BA = sum(events %in% c("single","double","triple","home_run")) / (PA - sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    xBA = sum(estimated_ba_using_speedangle, na.rm = T) / sum(events %in% AtBat, na.rm = T),
    wOBA = sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T),
    xwOBA = sum(xwOBA_value, na.rm = T) / sum(woba_denom, na.rm = T),
    XBH = sum(events %in% c("double")) + sum(events %in% c("triple")) + sum(str_detect(events, "home_run")),
    HR = sum(str_detect(events, "home_run")),
  ) %>%
  ungroup() %>%
  select(-SLG, -OBP) %>%
  arrange(game_year)

velo %>%
  left_join(., velo_rv, by = join_by(game_year, velo_class)) %>%
  relocate(RV, .before = BA) %>%
  relocate(RV_C, .before = BA) %>%
  split(pull(., velo_class)) %>%
  map(
    ., 
    ~ select(.x, -velo_class) %>%
      set_names(c("Season", "打席", "OPS", "RV", "RV/100", "打率", "xBA", "wOBA", "xwOBA", "長打", "HR", "Swing%", "Chase%", "Contact%"))
  ) -> velocity_split

velocity_split[["145+"]]

clipr::write_clip(.Last.value)

velocity_split[["145-"]]

clipr::write_clip(.Last.value)

# ゾーン別---------------------

df %>%
  filter(!is.na(zone)) %>%
  filter(game_year >= 2020) %>%
  group_by(zone, p_throws) %>%
  summarise(
    N = n(),
    RV = sum(delta_run_exp),
    PA = sum(!is.na(events)),
    H = sum(events %in% c("single","double","triple","home_run")),
    `2B` = sum(events %in% c("double")),
    `3B` = sum(events %in% c("triple")),
    HR = sum(str_detect(events, "home_run"), na.rm = T),
    BA = H/(PA-sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    SLG = (sum(events %in% c("single")) + `2B`*2 + `3B`*3 + HR*4)/(PA-sum(events %in% c("walk","sac_fly","hit_by_pitch"))),
    OBP = sum(events %in% c("single","double","triple","home_run", "walk", "hit_by_pitch"))/
      PA,
    RV_C = sum(delta_run_exp) * 100 / n(),
  ) %>%
  ungroup() %>%
  select(zone, p_throws, RV, SLG) %>%
  mutate(zone = as.character(zone)) %>%
  split(pull(., p_throws)) -> zone_chart

ids_4 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
ids_6 <- c("11", "12", "13", "14")

zone <- c(
  rep(ids_4, each = 4), rep(ids_6, each = 6)
)

homebase <- tribble(
  ~ width, ~ height,
  1.3, -.2,
  1, -1,
  2.5, -1.5,
  4, -1,
  3.7, -.2
)

SLG_plot <- list()
RV_plot <- list()

for (i in 1:length(zone_chart)) {
  
  value_table <- zone_chart[[i]]
  
  positions <- tribble(
    ~ width, ~ height,
    1, 4, 1, 3, 2, 3, 2, 4, #1
    
    2, 4, 2, 3, 3, 3, 3, 4, #2
    
    3, 4, 3, 3, 4, 3, 4, 4, #3
    
    1, 3, 1, 2, 2, 2, 2, 3, #4
    
    2, 3, 2, 2, 3, 2, 3, 3, #5
    
    3, 3, 3, 2, 4, 2, 4, 3, #6 
    
    1, 2, 1, 1, 2, 1, 2, 2, #7
    
    2, 2, 2, 1, 3, 1, 3, 2, #8
    
    3, 2, 3, 1, 4, 1, 4, 2, #9
    
    0, 5, 0, 2.5, 1, 2.5, 1, 4, 2.5, 4, 2.5, 5, #11
    
    2.5, 5, 2.5, 4, 4, 4, 4, 2.5, 5, 2.5, 5, 5, #12
    
    0, 2.5, 0, 0, 2.5, 0, 2.5, 1, 1, 1, 1, 2.5, #13
    
    2.5, 1, 2.5, 0, 5, 0, 5, 2.5, 4, 2.5, 4, 1 #14
  ) %>%
    bind_cols(zone = zone) %>%
    left_join(value_table, by = "zone")
  
  text_position <- tribble(
    ~ zone, ~ width, ~ height,
    1, 1.5, 3.5,
    2, 2.5, 3.5,
    3, 3.5, 3.5,
    4, 1.5, 2.5,
    5, 2.5, 2.5,
    6, 3.5, 2.5,
    7, 1.5, 1.5,
    8, 2.5, 1.5,
    9, 3.5, 1.5,
    11, .5, 4.5,
    12, 4.5, 4.5,
    13, .5, .5,
    14, 4.5, .5
  ) %>%
    mutate(zone = as.character(zone)) %>%
    left_join(value_table, by = "zone")
  
  SLG_plot[[i]] <- ggplot(positions) +
    aes(x = width, y = height, group = zone, fill = SLG) +
    geom_polygon(colour = "black") +
    scale_fill_gradient2(low = "royalblue", high = "hotpink", mid = "white", midpoint = .4) +
    geom_label(
      data = text_position, aes(x = width, y = height, label = SLG %>% round(3) %>% format(., digits = 3, nsmall = 3) %>% str_extract("([1-9]|)\\.[[:digit:]]{3}")),
      inherit.aes = F
    ) +
    geom_polygon(aes(x = width, y = height), data = homebase, inherit.aes = F, colour = "black", fill = "white") +
    theme_classic() +
    guides(x = "none", y = "none", fill = "none") +
    coord_fixed() +
    labs(x = "", y = "")
  
  RV_plot[[i]] <- ggplot(positions) +
    aes(x = width, y = height, group = zone, fill = RV) +
    geom_polygon(colour = "black") +
    scale_fill_gradient2(low = "royalblue", high = "hotpink", mid = "white", midpoint = 0) +
    geom_label(
      data = text_position, aes(x = width, y = height, label = RV %>% round(2) %>% format(., nsmall = 2)),
      inherit.aes = F
    ) +
    geom_polygon(aes(x = width, y = height), data = homebase, inherit.aes = F, colour = "black", fill = "white") +
    theme_classic() +
    guides(x = "none", y = "none", fill = "none") +
    coord_fixed() +
    labs(x = "", y = "")
  
}

SLG_plot[[1]]  | RV_plot[[1]] | SLG_plot[[2]] | RV_plot[[2]]

ggsave(paste0("output/foreign_player/batter/figure/", playerName, ".png"), width = 14, height = 6)

# 打球サマリー -----------------------------

df %>%
  filter(game_year >= 2020) %>%
  #mutate(period = if_else(game_year == 2018, "2018", "2019-")) %>%
  filter(!is.na(events), events != "") %>%
  group_by(game_year) %>%
  summarise(
    number = n(),
    Exit_Speed = mean(launch_speed_km, na.rm = T),
    Exit_Angle = mean(launch_angle, na.rm = T),
    GB = sum(bb_type == "ground_ball", na.rm = T) / sum(!is.na(bb_type)) * 100,
    LD = sum(bb_type == "line_drive", na.rm = T) / sum(!is.na(bb_type)) * 100,
    FB = sum(bb_type == "fly_ball", na.rm = T) / sum(!is.na(bb_type)) * 100,
    Hard = sum(launch_speed >= 95, na.rm = T) / sum(!is.na(launch_speed), na.rm = T) * 100,
    Pull = sum(bb_dim_type == "Pull", na.rm = T) / sum(!is.na(bb_type)) * 100,
    Cent = sum(bb_dim_type == "Cent", na.rm = T) / sum(!is.na(bb_type)) * 100,
    BABIP = sum(babip_value, na.rm = T) / sum(events %in% BABIP_den),
    xwOBAcon = mean(estimated_woba_using_speedangle, na.rm = T),
    xwOBA = sum(xwOBA_value, na.rm = T) / sum(woba_denom, na.rm = T),
    Barrel_BBE = sum(launch_speed_angle == 6, na.rm = T) / sum(type == "X", na.rm = T) * 100,
    Barrel_PA = sum(launch_speed_angle == 6, na.rm = T) / sum(events %in% PAresult, na.rm = T) * 100,
  ) %>%
  ungroup() %>%
  set_names(c("Season", "PA", "打球速度", "角度", "GB%", "LD%", "FB%", "HardHit%", "Pull%", "Cent%", "BABIP", "xwOBAcon", "xwOBA", "Barrel/BBE", "Barrel/PA"))

clipr::write_clip(.Last.value)

# スプレー -------------------------------------------------------

geom_spraytable(df %>% filter(type == "X") %>% filter(game_year >= 2020), aes(hc_x, -hc_y, fill = result)) +
  aes(hc_x, -hc_y, fill = result) +
  geom_point(shape = "circle filled", size = 2) +
  scale_fill_manual(values = viridis::viridis(5, option = "A", begin = .2)) +
  theme_classic() +
  guides(x = "none", y = "none") +
  #facet_wrap(~ game_year) +
  labs(fill = "結果")

ggsave(paste0("output/foreign_player/batter/figure/", playerName, "_spraychart.png"))  

# 打球角度・速度プロット----------------------------------------------

g <- ggplot(df %>% filter(!is.na(bb_type))) +
  aes(x = launch_speed_km, fill = result) +
  geom_histogram(binwidth = 3, position = "stack", colour = "gray") +
  geom_vline(xintercept = 95 * 1.601, colour = "dodgerblue", linewidth = 1) +
  scale_fill_manual(values = viridis::viridis(5, option = "A", begin = .2)) +
  guides(x = "none", y = "none", fill = "none") +
  theme_bw() +
  labs(x = "", y = "") +
  xlim(c(40, 200))



g1 <- ggplot(df %>% filter(!is.na(bb_type))) +
  aes(x = launch_speed_km, y = launch_angle, fill = result) +
  geom_vline(xintercept = 95 * 1.609, linewidth = 1, colour = "dodgerblue") +
  geom_point(shape = "circle filled", alpha = .8, size = 1) +
  scale_fill_manual(values = viridis::viridis(5, option = "A", begin = .2)) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  guides(fill = "none") +
  labs(x = "打球速度 (km/h)", y = "打球角度", caption = "青線はHardHit%の定義となる95mphに対応") +
  xlim(c(40, 200))

g + g1 + plot_layout(ncol = 1, heights = c(2, 3))

ggsave(paste0("output/foreign_player/batter/figure/", playerName, "_speedangle.png"))

# FG MLB-------------------------------------------

MLB_basic_stats <- mlb_gamelogs %>%
  mutate(game_year = lubridate::year(Date)) %>%
  group_by(Season = game_year, Age) %>%
  summarise(
    Level = "MLB",
    試合数 = sum(G),
    打席 = sum(PA),
    打数 = sum(AB),
    安打 = sum(H),
    二塁打 = sum(`2B`),
    三塁打 = sum(`3B`),
    本塁打 = sum(`HR`),
    四球 = sum(BB),
    盗塁 = sum(SB),
    盗塁死 = sum(CS),
    #盗塁成功率 = sum(SB) / (sum(SB) + sum(CS)),
    `BB%` = sum(BB) / sum(PA) * 100,
    `K%` = sum(SO) / sum(PA) * 100,
    打率 = sum(H) / sum(AB),
    出塁率 = sum(H + BB + HBP) / sum(AB + BB + HBP +SH),
    長打率 = sum(H + `2B` + `3B` * 2 + HR * 3) / sum(AB),
    wOBA = sum(wOBA * PA) / sum(PA),
    wBsR = sum(wBSR),
  ) %>%
  ungroup() %>%
  mutate(
    OPS = 出塁率 + 長打率,
  ) %>%
  relocate(OPS, .before = wOBA)

MLB_basic_stats %>% select(-wBsR)

clipr::write_clip(.Last.value)

# FG miLB-------------------------------------------


MiLB_basic_stats <- milb_gamelogs %>%
  mutate(
    game_year = lubridate::year(Date),
    birth_year = lubridate::year(BirthDate),
    Age = game_year - birth_year,
    Level = str_remove_all(Level, "[)(]"),
  ) %>%
  group_by(Season = game_year, Age, Level) %>%
  summarise(
    試合数 = sum(G),
    打席 = sum(PA),
    打数 = sum(AB),
    安打 = sum(H),
    二塁打 = sum(`2B`),
    三塁打 = sum(`3B`),
    本塁打 = sum(`HR`),
    四球 = sum(BB),
    盗塁 = sum(SB),
    盗塁死 = sum(CS),
    盗塁成功率 = sum(SB) / (sum(SB) + sum(CS)),
    `BB%` = sum(BB) / sum(PA) * 100,
    `K%` = sum(SO) / sum(PA) * 100,
    打率 = sum(H) / sum(AB),
    出塁率 = sum(H + BB + HBP) / sum(AB + BB + HBP +SH),
    長打率 = sum(H + `2B` + `3B` * 2 + HR * 3) / sum(AB),
    wBsR = sum(wBsR),
  ) %>%
  mutate(
    OPS = 出塁率 + 長打率,
  )

MiLB_basic_stats %>%
  select(-wBsR)

clipr::write_clip(.Last.value)

# 走塁 ------------------------------

#sprint <- statcast_leaderboards(leaderboard = "sprint_speed", year = .x)

sprint_speed <- map_df( #FG MiLB gamelog
  2020:2022,
  .f = ~ baseballr::statcast_leaderboards(leaderboard = "sprint_speed", year = .x) %>%
    mutate(Rk = row_number(), percentile = percent_rank(sprint_speed) * 100) %>%
    filter(player_id == id_mlbam)
)

sprint_speed %>%
  replace_na(replace = list(bolts = 0)) %>%
  select(Season = year, Age = age, 走行回数 = competitive_runs, 一塁到達 = hp_to_1b, スプリントスピード = sprint_speed, パーセンタイル = percentile)

clipr::write_clip(.Last.value)

MLB_basic_stats %>%
  select(Season, Age, Level, wBsR)

clipr::write_clip(.Last.value)

MiLB_basic_stats %>%
  select(Season, Age, Level, wBsR)

clipr::write_clip(.Last.value)

# 守備 ------------------------------------------

oaa_of <- map_df(
  .x = 2018:2022,
  .f = ~ statcast_leaderboards(leaderboard = "outs_above_average", year = .x, min_field = 5, oaa_position = "of") %>%
    mutate(year = .x, Pos = "OF") %>%
    filter(player_id == id_mlbam)
)

oaa_if <- map_df(
  .x = 2018:2022,
  .f = ~ statcast_leaderboards(leaderboard = "outs_above_average", year = .x, min_field = 5, oaa_position = "if") %>%
    mutate(year = .x, Pos = "IF") %>%
    filter(player_id == id_mlbam)
)

oaa_if %>%
  select(
    Season = year, 守備得点 = fielding_runs_prevented, OAA = outs_above_average,
    "前方" = outs_above_average_infront, "三塁側" = outs_above_average_lateral_toward3bline,
    "一塁側" = outs_above_average_lateral_toward1bline, "後方" = outs_above_average_behind
  )

clipr::write_clip(.Last.value)

oaa_of %>%
  select(
    Season = year, 守備得点 = fielding_runs_prevented, OAA = outs_above_average,
    "前方" = outs_above_average_infront, "三塁側" = outs_above_average_lateral_toward3bline,
    "一塁側" = outs_above_average_lateral_toward1bline, "後方" = outs_above_average_behind
  )

clipr::write_clip(.Last.value)

url <- paste0("https://www.baseball-reference.com/register/player.fcgi?id=", id_bref_milb)

base <- read_html(url) %>%
  paste0() %>%
  str_remove_all("<!--") %>%
  str_remove_all("-->") %>%
  read_html()

def_table <- base %>%
  html_nodes(xpath = '//*[@id="standard_fielding"]') %>%
  html_table() %>%
  {.[[1]]} %>%
  set_names(
    c('Year', 'Age', 'Tm', 'Lg', 'Lev', 'Aff', 'Pos', 'G', 'GS', 'CG', 'Inn', 'Ch', 'PO', 'A', 'E', 'DP', 'Fld%', 'RF/9', 'RF/G', 'PB', 'WP', 'SB', 'CS', 'CS%', 'lgCS%', 'PO')
  ) %>%
  select(-13) %>%
  filter(str_detect(Year, "^[:digit:]{4}$")) %>%
  mutate(Year = parse_number(Year)) %>%
  filter(Year >= 2020) %>%
  filter(!(Pos %in% c("DH", "OF"))) %>%
  filter(!str_detect(Tm, "[[:digit:]] Teams")) %>%
  select(1, 2, 5, Team = 6, 7, 8 , 11) %>%
  split(pull(., Lev))

def_table[[1]]

clipr::write_clip(.Last.value)

def_table[[2]]

clipr::write_clip(.Last.value)

def_table %>%
  filter(Year != "Year")

colnames(def_table) %>%
  paste(collapse = ", ")