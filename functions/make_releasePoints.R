make_releasePoints <- function(
    df, type = "extension",
    release_x = "plate_x", release_z = "plate_z", release_extension = "release_extention", 
    colour = 'pitch_name',
    xlab = NULL, ylab = NULL,
    split = NULL, lims = NULL, colour_palette = NULL,
    scale = 'cm', # M, inch
    plot_type = c("colour", "fill")
) {
  if (is.null(xlab)) {
    if (type == "extension") {
      xlab <- "Release Extension (cm)"
      ylab <- "Release Height (cm)"
    } else if (type == "plane") {
      xlab <- "Release Side (cm)"
      ylab <- "Release Height (cm)"
    }
  }
  
  if (type == "extension") {
    
  } else if (type == "plane") {
    
  }
}