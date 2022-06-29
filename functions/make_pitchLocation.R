make_pitchLocation <- function(
  df, plate_x = "plate_x", plate_z = "plate_z", 
  colour = 'pitch_name',
  xlab = "Horizontal Location (cm)", ylab = "Vertical Location (cm)",
  split = NULL, lims = NULL, colour_palette = NULL,
  scale = 'cm',
  box_colour = 'red',
  plot_type = c("colour", "fill")
) {
  
  if (plot_type == "colour"){
    base <- ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[[plate_x]], 
                   y = .data[[plate_z]], 
                   colour = .data[[colour]]) +
      geom_zonebox(scale = scale, colour = box_colour) +
      ggplot2::geom_point(alpha = .7) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
      ggplot2::labs(colour = "Pitch Type")
  } else {
    base <- ggplot2::ggplot(df) +
      ggplot2::aes(x = .data[[plate_x]], 
                   y = .data[[plate_z]], 
                   fill = .data[[colour]]) +
      geom_zonebox(scale = scale, colour = box_colour) +
      ggplot2::geom_point(shape = "circle filled", colour = "black", alpha = .7) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
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
