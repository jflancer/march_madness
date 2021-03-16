library(bigballR)
library(tidyverse)
library(Matrix)
library(glmnet)
library(RMariaDB)

conn <- dbConnect(
  RMariaDB::MariaDB(),
  user = "root",
  password = "Ben25Joel21",
  dbname = "ncaa",
  host = "localhost"
)

# To-Do
# - Go through teams and get a list of injuries
# - Figure out minutes allocation in a better way to censor max
# - Join in CBB ref data and check the standardization process
# - Using 16.5 sd normal approx, create simple game predictions
# - Figure out way to code up the bracket, generate bracket probabilities


all_data <- dbGetQuery(conn, "SELECT p.cleanName, p.Team, p.Season, ps.GP, ps.MINS, ps.oPOSS,
ROUND(rp.ORAPM_prior,2) AS ORAPM, ROUND(rp.DRAPM_prior,2) AS DRAPM, ROUND(rp.RAPM_prior,2) AS RAPM
FROM players as p
LEFT JOIN rapm_prior AS rp ON p.player = rp.Name AND p.team = rp.Team AND p.season = rp.Season 
LEFT JOIN player_stats AS ps ON CONCAT(p.player, '_', p.team) = ps.player AND p.season = ps.season
WHERE ps.MINS > 0
ORDER BY rp.RAPM_prior DESC")

write_csv(all_data, "data/player_data.csv")

pbp <- read_rds("/Users/jake/ncaa_pbp_data/pbp202021.Rds")

team_poss <- pbp %>%
  group_by(ID, Home, Away) %>%
  summarise(
    Possessions = max(Poss_Num)
  )
teams <- unique(c(team_poss$Home, team_poss$Away))
teams_mat <- apply(team_poss, 1, function(x){
  teams %in% x[2:3]
}) %>% t() * 1

x = team_poss %>% 
  select(Tm = Home, Possessions) %>%
  bind_rows(team_poss %>% select(Tm = Away, Possessions)) %>%
  group_by(Tm) %>%
  summarise(
    Poss = mean(Possessions) / 2
  )

# Figure out expected number of possessions per game using two teams
# model <- cv.glmnet(teams_mat, team_poss$Possessions, alpha = 0, standardize = F, trace.it = 1, lambda = seq(.0001, .2, by = .0005))
# plot(model)

model <- glmnet(teams_mat, team_poss$Possessions, alpha = 0, standardize = F, lambda = .02)

# Format to generate predicted number of possessions in a game
new_df <- t(matrix((teams %in% c("Oklahoma St.", "Oklahoma St."))*1))
predict(
  model,
  new_df, 
  type = "response")


injuries <- c(
  # For now opting to leave in all questionable players, closer to gametime consider changing
  
  # "Josh Primo_Alabama", # Questionable
  # "RJ Cole_UConn", #Questionable
  # "J'Wan Roberts_Houston", #Questionable
  # "Jalen Wilson_Kansas", #Doubtful (COVID)
  # "Kyle Young_Ohio St.", #Questionable 
  # "Ethan Morton_Purdue", #Questionable
  # "Isaiah Poor Bear-Chandler_Wichita St.", #Questionable
  # "Jaden Seymour_Wichita St.", #Questionable
  # "Trevin Wade_Wichita St.", #Questionable
  # "Shereef Mitchell_Creighton", #Questionable
  
  
  "Jaylin Williams_Arkansas", #Out indefinitely
  "Khalen Robinson_Arkansas", #Out for season
  "Roman Penn_Drake", #Out for season
  "ShanQuan Hemphill_Drake", #Out indefinitely
  "Keyontae Johnson_Florida", #Out for season
  "Jalen Harris_Georgetown", #Out indefinitely
  "Caleb Mills_Houston", #Out indefinitely
  "Jack Nunge_Iowa", #Out for season
  "David McCormack_Kansas", #Out indefinitely
  "Tristan Enaruna_Kansas", #Out indefinitely
  "Jace Bass_Louisiana Tech", #Out indefinitely
  "Isaiah Livers_Michigan", #Out indefinitely
  "Puff Johnson_North Carolina", #Out indefinitely
  "Rubin Jones_North Texas", #Out indefinitely
  "Jimmy Sotos_Ohio St.", #Out for season
  "Chris Harris Jr._Oklahoma St.", #Out for season
  "Donovan Williams_Oklahoma St.", #Out for season
  "N'Faly Dante_Oregon", #Out for season
  "Anthony Roberts_St. Bonaventure", #Out indefinitely
  "Bourama Sidibe_Syracuse", #Out until late march
  "Frank Anselem_Syracuse", #Out indefinitely
  "John Fulkerson_Tennessee", #Out indefinitely
  "Jamarius Burton_Texas Tech", #Out indefinitely
  "Joel Ntambwe_Texas Tech", #Out for season
  "Chris Smith_UCLA", #Out for season
  "Jalen Hill_UCLA", #Out indefinitely
  "Keshawn Curry_VCU", #late march
  "Collin Gillespie_Villanova", #Out for season
  "Jalen Cone_Virginia Tech", #Out indefinitely
  "Cartier Diarra_Virginia Tech" #Out indefinitely
)

x = all_data %>%
  mutate(
    PctMins = pmin(MINS / (GP*40),1)
  ) %>%
  group_by(Team, Season) %>%
  arrange(desc(MINS)) %>%
  mutate(
    Rnk = rank(-MINS),
    RunningCount = cumsum(MINS),
    RunningCount = RunningCount / max(RunningCount)
  )

ggplot(x, aes(Rnk, RunningCount, group = paste(Team, Season))) + geom_line(alpha = .1) + theme_classic()

team_ratings <- all_data %>%
  filter(!(
    paste0(cleanName, "_", Team) %in% injuries &
    Season == "2020-21"
    )
  ) %>%
  mutate(
    PctMins = pmin(MINS / (GP*40),1)
  ) %>%
  group_by(Team, Season) %>%
  slice_max(n=7, order_by = PctMins) %>%
  mutate(
    POSS. = pmin(PctMins / sum(PctMins), .2),
    ExtraProb = 1 - sum(POSS.),
    Non20 = sum(POSS. * (POSS. != .2)),
    AddAmount = ifelse(POSS. == .2, 0, (POSS. / Non20) * ExtraProb),
    PCT_Full = POSS. + AddAmount
  ) %>%
  summarise(
    ORAPM = sum(ORAPM * POSS., na.rm = T)*5,
    DRAPM = sum(DRAPM * POSS., na.rm = T)*5,
    .groups = "drop"
  ) %>%
  mutate(
    RAPM = ORAPM + DRAPM
  )

march_madness_teams <- c(
  "Gonzaga",
  "Norfolk St.",
  "Appalachian St.",
  "Oklahoma",
  "Missouri",
  "Creighton",
  "UC Santa Barbara",
  "Virginia",
  "Ohio",
  "Southern California",
  "Wichita St.",
  "Drake",
  "Kansas",
  "Eastern Wash.",
  "Oregon",
  "VCU",
  "Iowa",
  "Grand Canyon",
  "Michigan",
  "Mt. St. Mary's",
  "Texas Southern",
  "LSU",
  "St. Bonaventure",
  "Colorado",
  "Georgetown",
  "Florida St.",
  "UNC Greensboro",
  "BYU",
  "Michigan St.",
  "UCLA",
  "Texas",
  "Abilene Christian",
  "UConn",
  "Maryland",
  "Alabama",
  "Iona",
  "Baylor",
  "Hartford",
  "North Carolina",
  "Wisconsin",
  "Villanova",
  "Winthrop",
  "Purdue",
  "North Texas",
  "Texas Tech",
  "Utah St.",
  "Arkansas",
  "Colgate",
  "Florida",
  "Virginia Tech",
  "Ohio St.",
  "Oral Roberts",
  "Illinois",
  "Drexel",
  "Loyola Chicago",
  "Georgia Tech",
  "Tennessee",
  "Oregon St.",
  "Oklahoma St.",
  "Liberty",
  "San Diego St.",
  "Syracuse",
  "West Virginia",
  "Morehead St.",
  "Clemson",
  "Rutgers",
  "Houston",
  "Cleveland St."
)

# Get team ratings from cbb ref
cbb_rate <- xml2::read_html("https://www.sports-reference.com/cbb/seasons/2021-ratings.html") %>%
  rvest::html_table() %>%
  .[[1]]

colnames(cbb_rate) <- cbb_rate[1,]
cbb_rate2 <- cbb_rate[,which(colnames(cbb_rate) != "NA")]
cbb_rate2 <- cbb_rate2 %>%
  mutate(across(c("Rk", "ORtg", "DRtg", "NRtg"), as.numeric)) %>%
  select(Rk:Conf, ORtg:NRtg) %>%
  filter(!is.na(Rk))

# Convert from bbref to NCAA. All others the same
team_map <- c(
  "UNC" = "North Carolina",
  "Manhattan College" = "Manhattan",
  "South Florida" = "South Fla.",
  "Samford University" = "Samford",
  "University of California, Irvine" = "UC Irvine",
  "Purdue-Fort Wayne" = "Purdue Fort Wayne",
  "Harvard University" = "Harvard",
  "Georgia State University" = "Georgia St.",
  "Rutgers University" = "Rutgers",
  "UMass" = "Massachusetts",
  "Western Kentucky" = "Western Ky.",
  "University of Northern Iowa" = "UNI",
  "Howard University" = "Howard",
  "Texas A&M-CC"="A&M-Corpus Christi",
  "Utah State" = "Utah St.",
  "Brigham Young" = "BYU",
  "Southern Methodist" = "SMU",
  "Northern Iowa" = "UNI",
  "Stephen F. Austin" = "SFA",
  
  "Texas A&M-Corpus Christi" = "A&M-Corpus Christi",
  "Texas-Arlington" = "UT Arlington",
  "Tennessee-Martin" = "UT Martin",
  "Alcorn St." = "Alcorn",
  "Arkansas-Pine Bluff" = "Ark.-Pine Bluff",
  "Army" = "Army West Point",
  "Boston University" = "Boston U.",
  "Bowling Green St." = "Bowling Green",
  "Central Arkansas" = "Central Ark.",
  "Central Michigan" = "Central Mich.",
  "Central Connecticut St." = "Central Conn. St.",
  "Charleston Southern" = "Charleston So.",
  "College of Charleston" = "Col. of Charleston",
  "Cal St. Bakersfield" = "CSU Bakersfield",
  "Eastern Illinois" = "Eastern Ill.",
  "Eastern Kentucky" = "Eastern Ky.",
  "Eastern Michigan" = "Eastern Mich.",
  "Eastern Washington" = "Eastern Wash.",
  "Florida Atlantic" = "Fla. Atlantic",
  "Western Michigan" = "Western Mich.",
  "Western Illinois" = "Western Ill.",
  "Western Carolina" = "Western Caro.",
  "UC-Irvine" = "UC Irvine",
  "Southern Illinois" = "Southern Ill.",
  "Southern Louisianna" = "Southern La.",
  "Southeast Missouri State" = "Southeast Mo. St.",
  "Mount St. Mary's" = "Mt. St. Mary's",
  "University of California" = "California",
  "Connecticut" = "UConn",
  "Georgia Southern" = "Ga. Southern",
  "Missouri-Kansas City" = "Kansas City",
  "Loyola \\(IL\\)" = "Loyola Chicago",
  "Lamar"=  "Lamar University",
  "Loyola \\(MD\\)" = "Loyola Maryland",
  "UC-Riverside" = "UC Riverside",
  "St. Francis \\(NY\\)" = "St. Francis Brooklyn",
  "Citadel" = "The Citadel",
  "Cal St. Northridge" = "CSUN",
  "East Tennessee St." = "ETSU",
  "Florida Gulf Coast" = "FGCU", 
  "Florida International" = "FIU",
  "Long Island University" = "LIU",
  "Cal St. Long Beach" = "Long Beach St.",
  "Louisiana St." = "LSU",
  "McNeese St." = "McNeese",
  "Middle Tennessee" = "Middle Tenn.",
  "Mississippi Valley St." = "Mississippi Val.",
  "North Carolina A&T" = "N.C. A&T",
  "North Carolina Central" = "N.C. Central",
  "North Carolina St." = "NC State",
  "North Alabama" = "North Ala.",
  "Northern Arizona" = "Northern Ariz.",
  "Northern Colorado" = "Northern Colo.",
  "Northern Illinois" = "Northern Ill.",
  "Northern Kentucky" = "Northern Ky.",
  "Seattle" = "Seattle U",
  "SIU Edwardsville" = "SIUE",
  "Southeast Missouri St."=  "Southeast Mo. St.",
  "Southeastern Louisiana" = "Southeastern La.",
  "Southeastern Mississippi" = "Southeastern Miss.",
  "Southern Utah"="Southern Utah",
  "Texas Christian" = "TCU",
  "Alabama-Birmingham" = "UAB",
  "UC-Davis" = "UC Davis",
  "UC-San Diego" = "UC San Diego",
  "UC-Santa Barbara" = "UC Santa Barbara",
  "Central Florida" = "UCF",
  "Southern Mississippi" = "Southern Miss.",
  "Georgia Southern" = "Ga. Southern",
  "Southern Illinois" = "Southern Ill.",
  "Illinois-Chicago" = "UIC",
  "Massachusetts-Lowell" = "UMass Lowell",
  "Maryland-Baltimore County" = "UMBC",
  "North Carolina-Asheville" = "UNC Asheville",
  "North Carolina-Greensboro" = "UNC Greensboro",
  "North Carolina-Wilmington" = "UNCW",
  "Nevada-Las Vegas" = "UNLV",
  "South Carolina Upstate" = "USC Upstate",
  "Texas-El Paso" = "UTEP",
  "Texas-Rio Grande Valley" = "UTRGV",
  "Texas-San Antonio" = "UTSA",
  "Virginia Commonwealth" = "VCU"
)

cbb_rate2$NCAA_Name <- stringr::str_replace_all(cbb_rate2$School, c("State"="St."))
cbb_rate2$NCAA_Name_2 <- stringr::str_replace_all(cbb_rate2$NCAA_Name, team_map)

cbb_rate2$NCAA_Name_2 <- case_when(
  cbb_rate2$NCAA_Name_2 == "Mississippi" ~ "Ole Miss",
  cbb_rate2$NCAA_Name_2 == "Southern" ~ "Southern U.",
  T ~ cbb_rate2$NCAA_Name_2
)

write_csv(cbb_rate2, "data/bballrefteam.csv")

march <- team_ratings %>%
  filter(Season == "2020-21") %>%
  left_join(cbb_rate2, by = c("Team"="NCAA_Name_2")) %>%
  mutate(
    zORAPM = (ORAPM - mean(ORAPM)) / sd(ORAPM),
    zDRAPM = (DRAPM - mean(DRAPM)) / sd(DRAPM),
    ORtg = ORtg - mean(ORtg),
    DRtg = DRtg - mean(DRtg),
    scaledORAPM = zORAPM * sd(ORtg) + mean(ORtg),
    scaledDRAPM = zDRAPM * sd(DRtg) + mean(DRtg)
  )# %>%
  # filter(Team %in% march_madness_teams)


ggplot(march, aes(ORAPM, color = "Regular")) + geom_density(alpha=0.5) + geom_density(aes(scaledORAPM, color = "Scaled")) + labs(title = "ORAPM")
ggplot(march, aes(DRAPM, color = "Regular")) + geom_density(alpha=0.5) + geom_density(aes(scaledDRAPM, color = "Scaled")) + labs(title = "DRAPM")


# Get game prediction
game_prediction <- function(tm1, tm2, useScaled = F, printData = F, long = F) {
  exp_poss <- predict(model,t(matrix((teams %in% c(tm1, tm2))*1)), type = "response") / 2
  t1_o <- if(useScaled) march$scaledORAPM[march$Team == tm1] else march$ORAPM[march$Team == tm1]
  t1_d <- if(useScaled) march$scaledDRAPM[march$Team == tm1] else march$DRAPM[march$Team == tm1]
  t2_o <- if(useScaled) march$scaledORAPM[march$Team == tm2] else march$ORAPM[march$Team == tm2]
  t2_d <- if(useScaled) march$scaledDRAPM[march$Team == tm2] else march$DRAPM[march$Team == tm2]
  
  # 97.12 = RAPM intercept
  tm1_score <- (t1_o - t2_d + 97.12) * (exp_poss / 100)
  tm2_score <- (t2_o - t1_d + 97.12) * (exp_poss / 100)
  net <- tm1_score - tm2_score
  prob <- pnorm(net, 0, 16.5)
  if(printData) {
    writeLines(paste(
      tm1, "vs", tm2, 
      "\nExpected Score:", round(tm1_score), "-", round(tm2_score), 
      "\nExpected Total:", round(tm1_score + tm2_score),
      "\nExpected Differential:", round(net,1), 
      "\n Expected Probability:", round(prob,2)))
  }
  if(long) {
    return(c(tm1_score, tm2_score, tm1_score+tm2_score, net, prob))
  } else {
    return(prob) 
  }
}

game_prediction("Winthrop", "Villanova", F, T, T)


