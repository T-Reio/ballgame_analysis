make_statsSummary <- function(df, group_by = NULL, stats = 'all') {
  all <- df %>%
    group_by(.data[[group_by]]) %>%
    summarise(
      n = n(),
      usage = round(100 * n() / nrow(df), 1),
      speed = round(mean(release_speed, na.rm = T), 1),
      srate = round(mean(release_spin_rate, na.rm = T), 1),
      hmov = round(mean(pfx_x, na.rm = T), 1),
      vmov = round(mean(pfx_z, na.rm = T), 1),
    )
  
  if (stats != 'all') {
    all %>%
      select(stats)
  } else {
    all
  }
}
