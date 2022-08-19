library(pacman)
p_load(viridisLite)
source("script/d_readData.R", encoding = "utf-8")
source("functions/make_breakchart.R")
source("functions/make_okuyukichart.R")
source("functions/make_pitchLocation.R")
source("functions/geom_zonebox.R")
source("cheatsheets/colour_palette.R") #関数とサンプルデータの読み込み

df <- bind_rows(df_sample423, df_sample425)

player_name <- "Kosei, Shoji"

df_visualized <- df %>%
  filter(Pitcher == player_name)

# ブレイクチャート

make_breakchart(df_visualized, "HorzBreak", "InducedVertBreak", pitch_type = "TaggedPitchType", plot_type = "colour")

# 奥行きチャート

make_okuyukichart(df_visualized, "RelSpeed", "InducedVertBreak", pitch_type = "TaggedPitchType", plot_type = "colour")

# コースチャート

df_visualized %>%
  split(pull(., BatterSide)) -> df_visualized_split

df_visualized_split %>%
  map2(
    .x = ., .y = c("Left", "Right"), 
    .f = ~ make_pitchLocation(
      .x, 
      plate_x = "PlateLocSide_cm", plate_z = "PlateLocHeight_cm",
      colour = "TaggedPitchType",
      split = "TaggedPitchType",
      box_colour = "black",
      plot_type = "fill"
    ) +
      ggtitle(paste0("Stand: ", .y))
  )

# リリースポイント

make_releasePoints(df_visualized, type = "extension", "RelSide", 
                   release_extension = "Extension", pitch_type = "TaggedPitchType", plot_type = "colour")

make_releasePoints(df_visualized, type = "plane", release_pos_x = "RelSide",
                   release_pos_z = "RelHeight", pitch_type = "TaggedPitchType", plot_type = "colour")

# 回転 Rapsodoのみ

df_visualized %>%
  mutate(
    spin_axis_rad = ((-SpinAxis + 270) %% 360)
  ) %>% view()


