library(pacman)
p_load(tidyverse)
p_load(baseballr)
p_load(viridis)

baseballr::playerid_lookup("Ohtani", "Shohei")

id <- 660271

df <- scrape_statcast_savant(
  start_date = "2022-03-01",
  end_date = "2022-11-30",
  playerid = id,
  player_type = "batter" # とりあえずバッターで、投手ならpitcherを指定
) %>% # パイプ演算子
  filter(game_type == "R") 

df %>% view()

df$zone %>% unique()

df %>%
  group_by(zone) %>%
  summarise(
    plate_x = mean(plate_x, na.rm = T),
    plate_z = mean(plate_z, na.rm = T),
  ) -> sum

ggplot(sum) +
  aes(x = plate_x, y = plate_z, fill = as.factor(zone), label = zone) +
  geom_point(size = 10, shape = "circle filled") +
  scale_fill_manual(values = viridis::viridis(13, option = "A")) +
  guides(fill = "none") +
  geom_label(sum, mapping = aes(x = plate_x, y = plate_z, label = zone), inherit.aes = F)
  
df %>%
  select(woba_denom, woba_value, delta_run_exp, zone) %>%
  head()

woba_table <- df %>%
  group_by(zone) %>%
  summarise(
    #PA = sum(woba_denom, na.rm = T),
    woba = sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T)
  )

df %>%
  group_by(zone) %>%
  summarise(
    N = n(),
    RV = sum(delta_run_exp),
    RV_100 = sum(delta_run_exp) / n() * 100
  )


ids_4 <- 1:9
ids_6 <- 11:14

zone_vector <- c(
  rep(ids_4, each = 4), rep(ids_6, each = 6)
)

positions <- tribble(
  ~ width, ~ height,
  1, 4,
  1, 3,
  2, 3,
  2, 4, #1
  
  2, 4,
  2, 3,
  3, 3,
  3, 4, #2
  
  3, 4,
  3, 3,
  4, 3,
  4, 4, #3
  
  1, 3,
  1, 2,
  2, 2,
  2, 3, #4
  
  2, 3,
  2, 2,
  3, 2,
  3, 3, #5
  
  3, 3,
  3, 2,
  4, 2,
  4, 3, #6
  
  1, 2,
  1, 1,
  2, 1,
  2, 2, #7
  
  2, 2,
  2, 1,
  3, 1,
  3, 2, #8
  
  3, 2,
  3, 1,
  4, 1,
  4, 2, #9
  
  0, 5,
  0, 2.5,
  1, 2.5,
  1, 4,
  2.5, 4,
  2.5, 5, #11
  
  2.5, 5,
  2.5, 4,
  4, 4,
  4, 2.5,
  5, 2.5,
  5, 5, #12
  
  0, 2.5,
  0, 0,
  2.5, 0,
  2.5, 1,
  1, 1,
  1, 2.5, #13
  
  2.5, 1,
  2.5, 0,
  5, 0,
  5, 2.5,
  4, 2.5,
  4, 1 #14
) %>%
  bind_cols(zone = zone_vector)


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
)

positions <- positions %>%
  dplyr::left_join(woba_table, by = "zone")
text_position <- text_position %>%
  left_join(woba_table, by = "zone")

homebase <- tribble(
  ~ width, ~ height,
  1.3, -.2,
  1, -1,
  2.5, -1.5,
  4, -1,
  3.7, -.2
)

value_min <- text_position$woba %>% min()
value_max <- text_position$woba %>% max()

ggplot(positions) +
  aes(x = width, y = height, group = zone, fill = woba) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(
    colours = c("navy", "white", "maroon"),
    values = scales::rescale(c(value_min, .320, value_max))
  ) +
  geom_label(
    data = text_position, aes(x = width, y = height, label = woba %>% format(., digits = 3, nsmall = 3) %>% str_extract("([1-9]|)\\.[[:digit:]]{3}")),
    inherit.aes = F
  ) +
  # geom_polygon(aes(x = width, y = height), data = homebase, inherit.aes = F, colour = "black", fill = "white") +
  theme_classic() +
  guides(x = "none", y = "none", fill = "none") +
  coord_fixed() +
  labs(x = "", y = "")


ggplot(positions) +
  aes(x = width, y = height, group = zone) +
  geom_polygon(colour = "dodgerblue", fill = "hotpink") +
  theme_dark() +
  coord_fixed()


ggplot(positions) +
  aes(x = width, y = height, group = zone, fill = woba) +
  geom_polygon(colour = "black") +
  scale_fill_viridis_c(
    values = scales::rescale(c(value_min, .320, value_max)),
    option = "C"
  ) +
  theme_classic() +
  guides(x = "none", y = "none") +
  coord_fixed() +
  labs(x = "", y = "")
  
  
ggplot(positions) +
  aes(x = width, y = height, group = zone, fill = woba) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(
    colours = c("navy", "white", "maroon"),
    values = scales::rescale(c(value_min, .320, value_max))
  ) +
  theme_classic() +
  guides(x = "none", y = "none") +
  coord_fixed() +
  labs(x = "", y = "")



# 投手成績

df <- scrape_statcast_savant(
  start_date = "2022-03-01",
  end_date = "2022-11-30",
  playerid = id,
  player_type = "pitcher"
) %>% # パイプ演算子
  filter(game_type == "R") 


rv_table <- df %>%
  group_by(zone) %>%
  summarise(
    N = n(),
    RV = sum(delta_run_exp),
    RV_100 = sum(delta_run_exp) / n() * 100
  )


positions <- positions %>%
  dplyr::left_join(rv_table, by = "zone")
text_position <- text_position %>%
  left_join(rv_table, by = "zone")

value_min <- text_position$RV %>% max()
value_max <- text_position$RV %>% min()

ggplot(positions) +
  aes(x = width, y = height, group = zone, fill = RV) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(
    colours = c("navy", "white", "maroon"),
    values = scales::rescale(c(value_min, 0, value_max))
  ) +
  geom_label(
    data = text_position, aes(x = width, y = height, label = RV %>% format(., digits = 2, nsmall = 2) %>% str_extract(".*([1-9]|)\\.[[:digit:]]{2}")),
    inherit.aes = F
  ) +
  geom_polygon(aes(x = width, y = height), data = homebase, inherit.aes = F, colour = "black", fill = "white") +
  theme_classic() +
  guides(x = "none", y = "none", fill = "none") +
  coord_fixed() +
  labs(x = "", y = "")



rv_table <- df %>%
  group_by(zone, pitch_name) %>%
  summarise(
    N = n(),
    RV = sum(delta_run_exp),
    RV_100 = sum(delta_run_exp) / n() * 100
  ) %>%
  filter(pitch_name != "other")

positions <- positions %>%
  full_join(rv_table, by = "zone")
text_position <- text_position %>%
  full_join(rv_table, by = "zone")


ggplot(positions) +
  aes(x = width, y = height, group = zone, fill = RV) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(
    colours = c("navy", "white", "maroon"),
    values = scales::rescale(c(value_min, 0, value_max))
  ) +
  geom_label(
    data = text_position, aes(x = width, y = height, label = RV %>% format(., digits = 2, nsmall = 2) %>% str_extract("-*([1-9]|)\\.[[:digit:]]{2}")),
    inherit.aes = F
  ) +
  theme_classic() +
  guides(x = "none", y = "none", fill = "none") +
  labs(x = "", y = "") +
  facet_wrap(~ pitch_name, ncol = 4)
