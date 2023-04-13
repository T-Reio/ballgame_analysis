library(pacman)
p_load(tidyverse)

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
  bind_cols(zone = zone_vector) #%>%
  left_join(value_table, by = "zone")

homebase <- tribble(
  ~ width, ~ height,
  1.3, -.2,
  1, -1,
  2.5, -1.5,
  4, -1,
  3.7, -.2
)

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
  left_join(value_table, by = "zone")

# ggplot(positions) +
#   aes(x = width, y = height, group = zone, fill = values) +
#   geom_polygon(colour = "black") +
#   scale_fill_gradient2(low = "royalblue", high = "hotpink", mid = "white", midpoint = 1) +
#   geom_label(
#     data = text_position, aes(x = width, y = height, label = values %>% format(., digits = 3, nsmall = 3) %>% str_extract("([1-9]|)\\.[[:digit:]]{3}")), 
#     inherit.aes = F
#   ) +
#   geom_polygon(aes(x = width, y = height), data = homebase, inherit.aes = F, colour = "black", fill = "white") +
#   theme_bw() +
#   guides(x = "none", y = "none", fill = "none") +
#   coord_fixed() +
#   labs(x = "", y = "")
