library(pacman)
p_load(baseballr)

df <- playerid_lookup("Darvish")

statcast_search(start_date = "2022-09-01", end_date = "2022-09-01")

