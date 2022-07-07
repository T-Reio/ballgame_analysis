make_okuyukichart <- function(
  df, release_speed = "release_speed", pfx_z = "pfx_z", 
  pitch_type = "pitch_name",
  xlab = "Velocity (km/h)", ylab = "Induced Vertical Break (cm)", 
  split = NULL, xlims = NULL, ylims = NULL, colour_palette = NULL,
  plot_type = c("colour", "fill")
) {
  if (plot_type == "colour") {
    base <- ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[[release_speed]], 
                   y = .data[[pfx_z]], 
                   colour = .data[[pitch_type]]) +
      ggplot2::geom_point(alpha = .7) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::geom_hline(yintercept = 0) + 
      ggplot2::labs(colour = "Pitch Type")
  } else {
    base <- ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[[release_speed]], 
                   y = .data[[pfx_z]], 
                   fill = .data[[pitch_type]]) +
      ggplot2::geom_point(shape = "circle filled", colour = "black", alpha = .7) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::geom_hline(yintercept = 0) + 
      ggplot2::labs(colour = "Pitch Type")
  }
  
  
  if (!is.null(colour_palette)) {
    pitch_list <- names(colour_palette) %>%
      intersect(unique(df$pitch_name))
    
    colour_palette <- pitch_colour[names(pitch_colour) %in% pitch_list]
    
    if (plot_type == "colour") {
      base <- base +
        ggplot2::scale_colour_manual(values = colour_palette, name = "Pitch Type")
    } else {
      base <- base +
        ggplot2::scale_fill_manual(values = colour_palette, name = "Pitch Type")
    }
    
  }
  
  if (!is.null(split)) {
    base <- base +
      ggplot2::facet_wrap(~ .data[[split]])
  }
  
  if (!is.null(xlims)) {
    base <- base + 
      ggplot2::xlim(xlims[1], xlims[2])
  } else {
    base <- base + 
      ggplot2::xlim(50, 180)
  }
  
  if (!is.null(ylims)) {
    base <- base + 
      ggplot2::ylim(ylims[1], ylims[2])
  }
  base
}
