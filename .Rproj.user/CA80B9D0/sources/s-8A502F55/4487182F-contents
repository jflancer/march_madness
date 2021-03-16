# Load libraries ####
library(tidyverse)
library(glmnet)

# Load Data ####

all_data <- read_csv("data/player_data.csv")
load("data/poss_model.rda")
cbb_ref <- read_csv("data/bballrefteam.csv")
load("data/team_vec.rda")

# Manual Data ####

# Player injuries

injuries <- c(
  # For now opting to keep in all questionable players, closer to gametime consider changing (just uncomment)
  
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


# March Madness Teams
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

# Team Model ####
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

full_rating <- team_ratings %>%
  filter(Season == "2020-21") %>%
  left_join(cbb_ref, by = c("Team"="NCAA_Name_2")) %>%
  mutate(
    zORAPM = (ORAPM - mean(ORAPM)) / sd(ORAPM),
    zDRAPM = (DRAPM - mean(DRAPM)) / sd(DRAPM),
    ORtg = ORtg - mean(ORtg),
    DRtg = DRtg - mean(DRtg),
    scaledORAPM = zORAPM * sd(ORtg) + mean(ORtg),
    scaledDRAPM = zDRAPM * sd(DRtg) + mean(DRtg)
  ) %>%
filter(Team %in% march_madness_teams)


# Game Prediction ####
game_prediction <- function(df, model, teams_vector, tm1, tm2, useScaled = F, printData = F, long = F) {
  # df: dataframe of team ratings
  # model: model to predict possessions
  # teams_vector: needed to map team name to column for model
  # tm1, tm2: two teams using stats.ncaa formatted name
  # useScaled: whether to use base rating or scaled rating in dataframe (ORAPM vs. scaledORAPM, etc.)
  # printData: whether to print prediction output
  # long: whether to return all outputs of prediction vs. just probability
  
  exp_poss <- unname(predict(model,t(matrix((teams %in% c(tm1, tm2))*1)), type = "response")) / 2
  t1_o <- if(useScaled) df$scaledORAPM[df$Team == tm1] else df$ORAPM[df$Team == tm1]
  t1_d <- if(useScaled) df$scaledDRAPM[df$Team == tm1] else df$DRAPM[df$Team == tm1]
  t2_o <- if(useScaled) df$scaledORAPM[df$Team == tm2] else df$ORAPM[df$Team == tm2]
  t2_d <- if(useScaled) df$scaledDRAPM[df$Team == tm2] else df$DRAPM[df$Team == tm2]
  
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


game_prediction(full_rating, model, teams, "Winthrop", "Villanova", F, F, F)

# Tournament Simulation ####



