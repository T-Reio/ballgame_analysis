library(pacman)
p_load(rvest)
p_load(tidyverse)
p_load(googledrive)
p_load(googlesheets4)

url <- "https://www.espn.com/fantasy/baseball/story/_/id/33208450/fantasy-baseball-rankings-head-head-category-rotiserrie-leagues-2022"

base <- url %>%
  read_html() %>%
  paste0() %>%
  str_remove_all("<!--") %>%
  str_remove_all("<!--") %>%
  read_html()

tab <- base %>%
  html_nodes(xpath = '//*[@id="article-feed"]/article[1]/div/div[2]/aside[1]/table') %>%
  html_table() %>%
  {.[[1]]} %>%
  select(Rk = 1, 3, 4, Pos = 5, Pos_Rk = 6)

# Google -----------------------

drive_auth()
gs4_auth(token = drive_token())

#gs4_find(n_max = 5)

genso_id <- gs4_find(pattern = '幻想 ') %>% pull(id) %>% as.character()

genso_url <- gs4_get(genso_id)
genso_url

#range_write(
#  tab, # 追加するシート
#  ss = genso_url,
#  sheet = "2023_MLB",
#  range = 'A:E',
#  col_names = TRUE
#)



# ポジション別

tables <- map(
    .x = 2:10,
    .f =  ~ html_nodes(
      base, xpath = paste0(
        '//*[@id="article-feed"]/article[1]/div/div[2]/aside[', 
        .x,
        ']/table'
      )
    ) %>%
      html_table() %>%
      {.[[1]]} %>%
      select(Rk = 1, 4)
  )

# 1行でいいやつ--------------------------------

names(tables) <- c(
  "C", "1B", "2B", "3B", "SS", "OF", "DH", "SP", "RP"
)

DH <- tables[["DH"]] %>%
  pivot_wider(names_from = Rk, values_from = Player)

range_write(
  IF6, # 追加するデータ
  ss = genso_url,
  sheet = "キモリ",
  range = 'B13:13',
  col_names = FALSE
)

range_write(
  DH, # 追加するデータ
  ss = genso_url,
  sheet = "ダイナモ",
  range = 'B22:22',
  col_names = FALSE
)

# 複数行--------------------------------

C2 <- tables[[1]] %>%
  mutate(
    row = 2 - (Rk %% 2),
  ) %>%
  group_by(row) %>%
  mutate(
    col = row_number(),
  ) %>%
  ungroup() %>%
  select(-Rk) %>%
  pivot_wider(id_cols = row,names_from = col, values_from = Player) %>%
  select(-row)

range_write(
  C2, # 追加するデータ
  ss = genso_url,
  sheet = "キモリ",
  range = 'B8:9',
  col_names = FALSE
)

RP5 <- tables[["RP"]] %>%
  mutate(
    row = Rk %% 5,
    row = if_else(row == 0, 5, row),
  ) %>%
  group_by(row) %>%
  mutate(
    col = row_number(),
  ) %>%
  ungroup() %>%
  select(-Rk) %>%
  pivot_wider(id_cols = row,names_from = col, values_from = Player) %>%
  select(-row)

range_write(
  RP4, # 追加するデータ
  ss = genso_url,
  sheet = "キモリ",
  range = 'B4:7',
  col_names = FALSE
)

range_write(
  RP5, # 追加するデータ
  ss = genso_url,
  sheet = "ダイナモ",
  range = 'B7:11',
  col_names = FALSE
)
