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

P <- sample(df_sample423$Pitcher, 1)

df <- df_sample423 %>%
  filter(Pitcher == P)

ggplot(df) +
  aes(x = Extension, y = RelHeight, fill = TaggedPitchType) +
  geom_point(shape = "circle filled") +
  scale_fill_viridis_d() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2)
  

ggplot(df) +
  aes(x = RelSide, y = RelHeight, fill = TaggedPitchType) +
  geom_point(shape = "circle filled") +
  scale_fill_viridis_d() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2)


p_plate <- tribble(
  ~ Side, ~ Extension,
  -.31, 0,
  -.31, -.152,
  .31, -.152,
  .31, 0,
)

ggplot(df) +
  aes(x = RelSide, y = Extension, fill = TaggedPitchType) +
  geom_point(shape = "circle filled") +
  scale_fill_viridis_d() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_path(p_plate, mapping = aes(x = Side, y = Extension), inherit.aes = F)
