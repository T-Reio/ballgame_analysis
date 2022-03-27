make_gam_grid <- function(
  df, objective = "Strike", 
  plate_x = "plate_x", plate_z = "plate_z", 
  scale = 'cm'
) {
  
  objective <- df %>%
    pull(objective)
  plate_x <- df %>%
    pull(plate_x)
  plate_z <- df %>%
    pull(plate_z)
  
  fit <- df %>%
    mgcv::gam(
      formula = objective ~ s(plate_x, plate_z),
      family = 'binomial'
    )
  
  if (scale == 'ft') {
    plate_x_min <- -1.5 
    plate_x_max <- 1.5
    plate_z_min <- 1
    plate_z_max <- 4
  } else {
    plate_x_min <- -1.5 * 30.48 
    plate_x_max <- 1.5 * 30.48
    plate_z_min <- 1 * 30.48
    plate_z_max <- 4 * 30.48
  }
  zone_grid <- expand.grid(plate_x = seq(plate_x_min, plate_x_max, length = 50),
                           plate_z = seq(plate_z_min, plate_z_max, length = 50)) %>%
    mutate(
      lp = predict(fit, .) %>% c(),
      prob = exp(lp) / (1 + exp(lp)),
    )
  zone_grid
}


geom_gamMap <- function(...) {
  ggplot(...) +
    geom_tile(aes(x = plate_x, y = plate_z,
                fill = prob))
}

geom_gamcon <- function(..., level = .5) {
  ggplot(...) +
    geom_contour(aes(x = plate_x, y = plate_z,
                  fill = rounded), breaks = .5) +
    scale_colour_gradient(name = 'スイング', low = 'white', high = '#00008b')
}
