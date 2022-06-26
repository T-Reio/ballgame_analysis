library(pacman)
p_load(tidyverse)
p_load(rvest)
p_load(viridisLite)
p_load(patchwork)

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

htmls[[1]] %>%
  html_nodes(xpath = '//*[@id="stdivmaintbl"]/table') %>%
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
    BA = H/AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = TB / AB,
    OPS = OBP + SLG,
    OPS_BB = (BB + HBP) / (AB + BB + HBP + SF),
    OPS_BH = SLG + H / (AB + BB + HBP + SF),
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
          OPS_BB = (BB + HBP) / (AB + BB + HBP + SF),
          OPS_BH = SLG + H / (AB + BB + HBP + SF),
        ) %>%
        arrange(-OPS) %>%
        mutate(
          Rk = row_number(),
          Name = factor(Name, levels = Name)
        )
  )

stats_league[[1]] %>% view()

long <- stats_league %>%
  map(
    .x = .,
    .f = ~ pivot_longer(., cols = c(OPS_BB, OPS_BH), names_to = "Type") %>%
      mutate(
        Type = if_else(Type == "OPS_BH", "安打", "四球"),
        Type = factor(Type, levels = c("安打", "四球"))
      )
  )

ggplot(long[[1]]) +
  aes(x = Name, y = value, fill = Type) +
  geom_bar(stat = "identity", colour = "black") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "", y = "OPS", fill = "Composition", title = "OPS分解: 安打 + 四死球") -> g1

g2 <- g1 %+% long[[2]] +
  labs(title = "")

gA <- g1 / g2

ggsave("fig/OPS_decomposition_A.png", gA)

ggplot(c) +
  aes(x = Name, y = value, fill = Type) +
  geom_bar(stat = "identity", colour = "black") +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "", y = "OPS", fill = "Composition")


c <- stats_league[[1]] %>%
  pivot_longer(cols = c(OBP, SLG), names_to = "Type") %>%
  mutate(Type = factor(Type, levels = c("SLG", "OBP")))

long <- stats_league %>%
  map(
    .x = .,
    .f = ~ pivot_longer(.x, cols = c(OBP, SLG), names_to = "Type") %>%
      mutate(Type = factor(Type, levels = c("SLG", "OBP")))
  )

g3 <- ggplot(long[[1]]) +
  aes(x = Name, y = value, fill = Type) +
  geom_bar(stat = "identity", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "inferno") +
  labs(x = "", y = "OPS", fill = "Composition", title = "OPS分解: SLG + OBP")

g4 <- g3 %+% long[[2]] +
  labs(title = "")

gB <- g3/g4

ggsave("fig/OPS_decomposition_B.png", gB)
