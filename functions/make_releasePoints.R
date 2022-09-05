make_releasePoints <- function(
    df, type = "extension",
    release_pos_x = "release_pos_x", release_pos_z = "release_pos_z", 
    release_extension = "release_extention", 
    pitch_type = "pitch_name",
    colour = 'pitch_name',
    xlab = NULL, ylab = NULL,
    split = NULL, lims = NULL, colour_palette = NULL,
    scale = 'cm', # M, inch
    plot_alpha = .7,
    plot_type = c("colour", "fill")
) {
  if (is.null(xlab)) {
    if (type == "extension") {
      xlab <- "Release Side"
      ylab <- "Release Extension"
    } else if (type == "plane") {
      xlab <- "Release Side"
      ylab <- "Release Height"
    }
  }
  
  if (type == "extension") {
    #p_plate <- tribble(
    #  ~ Side, ~ Extension,
    #  -.31, 0,
    #  -.31, -.152,
    #  .31, -.152,
    #  .31, 0,
    #  -.31, 0,
    #)
    if (plot_type == "colour") {
      
      base <- ggplot(df) +
        aes(x = .data[[release_pos_x]], y = .data[[release_extension]], 
            colour = .data[[pitch_type]]
        ) +
        geom_point(
          #shape = "circle filled"
          alpha = plot_alpha
        ) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0, linetype = 2) +
        geom_rect(mapping = aes(xmin = -.31, ymin = -.152, xmax = .31, ymax = 0), colour = "black", fill = "white", size = .5, inherit.aes = F) +
        ggplot2::labs(colour = "Pitch Type")
        
      
    } else {
      
      base <- ggplot(df) +
        aes(x = .data[[release_pos_x]], y = .data[[release_extension]], 
            fill = .data[[pitch_type]]
        ) +
        geom_point(
          shape = "circle filled",
          alpha = plot_alpha
        ) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0, linetype = 2) +
        geom_rect(mapping = aes(xmin = -.31, ymin = -.152, xmax = .31, ymax = 0), colour = "black", fill = "white", size = .5, inherit.aes = F) +
        ggplot2::labs(fill = "Pitch Type")
      
    }
    base <- base +
      xlab(xlab) + ylab(ylab)
  } else if (type == "plane") {
    
    if (plot_type == "colour") {
      base <- ggplot2::ggplot(df) +
        aes(x = .data[[release_pos_x]], y = .data[[release_pos_z]], 
            colour = .data[[pitch_type]]) +
        geom_point(alpha = plot_alpha) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0, linetype = 2) +
        lims(x = c(-1, 1)) +
        geom_rect(mapping = aes(xmin = -1, ymin = -.25, xmax = 1, ymax = 0), fill = "#8b0000", inherit.aes = F) +
        geom_rect(mapping = aes(xmin = -.31, ymin = -.127, xmax = .31, ymax = 0), colour = "black", fill = "white", inherit.aes = F)
    } else {
      base <- ggplot2::ggplot(df) +
        aes(x = .data[[release_pos_x]], y = .data[[release_pos_z]], 
          fill = .data[[pitch_type]]) +
        geom_point(alpha = plot_alpha, shape = "circle filled") +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0, linetype = 2) +
        lims(x = c(-1, 1)) +
        geom_rect(mapping = aes(xmin = -1, ymin = -.25, xmax = 1, ymax = 0), fill = "#8b0000", inherit.aes = F) +
        geom_rect(mapping = aes(xmin = -.31, ymin = -.127, xmax = .31, ymax = 0), colour = "black", fill = "white", inherit.aes = F)
    }
    base <- base +
      xlab(xlab) + ylab(ylab)
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
  
  if (!is.null(lims)) {
    base <- base + 
      ggplot2::xlim(lims[1], lims[2]) + ggplot2::ylim(lims[3], lims[4])
  }
  
  base
}
