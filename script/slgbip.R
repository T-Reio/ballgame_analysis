library(pacman)
p_load(tidyverse)
p_load(rvest)
p_load(viridisLite)
p_load(patchwork)
p_load(ggrepel)
p_load(kableExtra)

source("cheatsheets/scrape_labels.R")

url_central <- "https://npb.jp/bis/2022/stats/bat_c.html"
url_pacific <- "https://npb.jp/bis/2022/stats/bat_p.html"

urls <- c(url_central, url_pacific)

htmls <- map(
  urls,
  .f = ~ read_html(.) %>%
    paste0() %>%
    str_remove_all("<!--") %>%
    str_remove_all("<!--") %>%
    read_html()
)

stats_league <- htmls %>%
  map2(.x = ., .y = c("Central", "Pacific"),
       .f = ~ html_nodes(.x, xpath = '//*[@id="stdivmaintbl"]/table') %>%
         html_table() %>%
         {.[[1]]} %>%
         slice(3:nrow(.)) %>%
         set_names(npb_official_league_batting) %>%
         mutate_at(.vars = vars(1, 4:25), .f = parse_number) %>%
         mutate(
           Name = str_remove_all(Name, "[:space:]"),
           TN = str_remove_all(TN, "[()]"),
           TN = case_when(
             TN == "ヤ" ~ "S",
             TN == "神" ~ "T",
             TN == "巨" ~ "G",
             TN == "広" ~ "C",
             TN == "デ" ~ "DB",
             TN == "中" ~ "D",
             TN == "オ" ~ "B",
             TN == "ロ" ~ "M",
             TN == "楽" ~ "E",
             TN == "ソ" ~ "H",
             TN == "日" ~ "F",
             TN == "西" ~ "L",
           ),
           lg = .y,
           BA = H/AB,
           OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
           SLG = TB / AB,
           OPS = OBP + SLG,
           ISO = SLG - BA,
           BABIP = (H - HR) / (AB - SO - HR + SF),
           SLGBIP = (TB - HR * 4) / (AB - SO - HR + SF),
           ISOBIP = SLGBIP - BABIP,
         ) %>%
         arrange(-ISOBIP) %>%
         mutate(
           Rk = row_number(),
           Name = factor(Name, levels = Name)
         ) %>%
         select(1, 2, 3, 31, 30, 9:12, 15, 4, 24:25, 27)
  )

stats_league[[1]] %>%
  kable(digits = 3) %>%
  kable_styling()

stats_league[[1]] %>%
  write_excel_csv("table/slgbip_CL.csv")

stats_league[[2]] %>%
  write_excel_csv("table/slgbip_PL.csv")

stats_league[[]] %>% view()

ggplot(stats_league[[1]]) +
  aes(x = HR, y = SLGBIP, label = Name) +
  geom_point(size = 1) +
  geom_label_repel(size = 2) +
  theme_bw()

ggplot(stats_league[[2]]) +
  aes(x = HR, y = SLGBIP, label = Name) +
  geom_point(size = 1) +
  geom_label_repel(size = 2) +
  theme_bw()


cor(stats_league[[2]]$SB, stats_league[[2]]$SLGBIP)
