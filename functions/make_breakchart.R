make_breakchart <- function(
  df, pfx_x = "pfx_x", pfx_z = "pfx_z", 
  pitch_type = "pitch_name",
  xlab = "Horizontal Break (cm)", ylab = "Induced Vertical Break (cm)", 
  split = NULL, lims = NULL, colour_palette = NULL,
  plot_type = c("colour", "fill")
) {
  if (plot_type == "colour") {
    base <- ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[[pfx_x]], 
                   y = .data[[pfx_z]], 
                   colour = .data[[pitch_type]]) +
      ggplot2::geom_point(alpha = .7) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0) +
      ggplot2::labs(colour = "Pitch Type")
  } else {
    base <- ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[[pfx_x]], 
                   y = .data[[pfx_z]], 
                   fill = .data[[pitch_type]]) +
      ggplot2::geom_point(shape = "circle filled", colour = "black", alpha = .7) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0) +
      ggplot2::labs(fill = "Pitch Type")
  }
  
  
  if (!is.null(colour_palette)) {
    pitch_list <- names(colour_palette) %>%
      intersect(unique(df$pitch_name))
    
    colour_palette <- pitch_colour[names(pitch_colour) %in% pitch_list]
    
    base <- base +
      ggplot2::scale_colour_manual(values = colour_palette)
  }
  
  if (!is.null(split)) {
    base <- base +
      ggplot2::facet_wrap(~ .data[[split]])
  }
  
  if (!is.null(lims)) {
    base <- base + 
      ggplot2::xlim(lims[1], lims[2]) + ggplot2::ylim(lims[3], lims[4])
  }
  
  base
}
