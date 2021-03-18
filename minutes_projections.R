pbp <- read_rds("/Users/jake/ncaa_pbp_data/pbp202021.Rds")

schedule <- read_csv("/Users/jake/Dropbox/bigballR_data/data_files/schedules2021.csv")

recent <- schedule %>%
  filter(as.Date(Date, format = "%m/%d/%Y") > as.Date("2021-02-20"))

pbp_recent <- pbp %>%
  filter(ID %in% recent$Game_ID)

mins <- pbp_recent %>% 
  filter(!isGarbageTime) %>%
  dplyr::rename_with(~gsub("_","\\.",.x), matches("Home_[1-5]|Away_[1-5]")) %>%
  get_player_stats(T, T) %>%
  filter(!str_detect(Player, "TEAM_"))

write_csv(mins, "data/mins_proj.csv")

