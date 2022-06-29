library(pacman)
p_load(viridisLite)
source("script/d_readData.R", encoding = "utf-8")
source("functions/make_breakchart.R")
source("functions/make_pitchLocation.R")
source("functions/geom_zonebox.R")

make_breakchart(df_sample425, "HorzBreak", "InducedVertBreak", pitch_type = "TaggedPitchType", split = "Pitcher", plot_type = "fill") +
  scale_fill_viridis_d()

colnames(df_sample425)
make_pitchLocation(
  df_sample425, 
  plate_x = "PlateLocSide_cm", plate_z = "PlateLocHeight_cm",
  colour = "TaggedPitchType",
  split = "Pitcher",
  box_colour = "black",
  plot_type = "fill"
)
