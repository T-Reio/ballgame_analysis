library(pacman)
p_load(viridisLite)
p_load(ggforce)
source("script/d_readData.R", encoding = "utf-8")
source("functions/make_breakchart.R")
source("functions/make_pitchLocation.R")
source("functions/geom_zonebox.R")
source("cheatsheets/colour_palette.R")

# BreakChart-------------------------

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

df$PitcherThrows %>% unique()

p_plate <- tribble(
  ~ Side, ~ Extension,
  -.31, 0,
  -.31, -.152,
  .31, -.152,
  .31, 0,
  -.31, 0,
)

ggplot(df) +
  aes(x = RelSide, y = Extension, 
      #fill = TaggedPitchType 
      colour = TaggedPitchType
      ) +
  geom_point(
    #shape = "circle filled"
             ) +
  #scale_fill_viridis_d() +
  #scale_colour_viridis_d() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  #geom_path(p_plate, mapping = aes(x = Side, y = Extension), size = 2, inherit.aes = F) +
  geom_rect(mapping = aes(xmin = -.31, ymin = -.152, xmax = .31, ymax = 0), colour = "black", fill = "white", size = 2, inherit.aes = F)


ggplot(df) +
  aes(x = RelSide, y = RelHeight, fill = TaggedPitchType) +
  geom_point(shape = "circle filled") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  lims(x = c(-1.2, 1.2)) +
  geom_rect(mapping = aes(xmin = -1.2, ymin = -.25, xmax = 1.2, ymax = 0), fill = "#8b0000", inherit.aes = F) +
  geom_rect(mapping = aes(xmin = -.31, ymin = -.127, xmax = .31, ymax = 0), colour = "black", fill = "white", inherit.aes = F)

df %>%
  make_breakchart("HorzBreak", "InducedVertBreak", pitch_type = "TaggedPitchType", split = "Pitcher", plot_type = "fill")

#df %>%
#  filter(TaggedPitchType == "Fastball") %>%
#  filter(HorzBreak >= 0) %>%
#  select(RelSpeed, Tilt, SpinRate, HorzBreak, InducedVertBreak)


make_pitchLocation(
  df, 
  plate_x = "PlateLocSide_cm", plate_z = "PlateLocHeight_cm",
  colour = "TaggedPitchType",
  split = "TaggedPitchType",
  box_colour = "gray",
  plot_type = "fill",
  colour_palette = pitch_colour
)
pitch_colour

pitch <- tibble(
  Eff = c(seq(30, 100, 10)),
  SpinAxis = c(120, 120, 225, 180, 190, 200, 285, 195),
  PitchType = c("Slider", "Slider", "Splitter", "Cutter", "Cutter", "Fastball", "Changeup", "Fastball")
) %>%
  mutate(
    spin_axis_rad = ((-SpinAxis + 270) %% 360) / 360 * 2* pi,
    chartX = cos(spin_axis_rad) * Eff,
    chartY = sin(spin_axis_rad) * Eff
  )

ggplot(pitch) +
  aes(x = chartX, y = chartY, colour = PitchType) +
  geom_point() +
  theme_classic() +
  geom_circle(
    aes(x0 = 0, y0 = 0, r = 100, alpha = 0),
    show.legend = F,
    inherit.aes = F
  )
