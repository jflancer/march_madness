# Load libraries ####
library(tidyverse)
library(glmnet)

# Load Data ####

all_data <- read_csv("data/player_data.csv")
load("data/poss_model.rda")
cbb_ref <- read_csv("data/bballrefteam.csv")
load("data/team_vec.rda")
pointspread_glm <- readRDS("data/glm_pointspread.rds")
mins <- read_csv("data/mins_proj.csv")

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
  "De'Vion Harmon_Oklahoma", #COVID tbd
  "Moses Wright_Georgia Tech", # Out first two rounds
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

seeds <- c(
  1,16,16,8,9,5,12,4,13,6,11,11,3,14,7,10,2,15,
  1,16,16,8,9,5,12,4,13,6,11,11,3,14,7,10,2,15,
  1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15,
  1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15
)

region <- c(
  rep("West", 18),
  rep("East", 18),
  rep("South", 16),
  rep("Midwest", 16)
)

# Team Model ####
# team_depth <- all_data %>%
#   filter(!(
#     paste0(cleanName, "_", Team) %in% injuries &
#       Season == "2020-21"
#   )) %>%
#   mutate(
#     PctMins = pmin(MINS / (GP*40),1)
#   ) %>%
#   group_by(Team, Season) %>%
#   slice_max(n=9, order_by = PctMins) %>%
#   mutate(
#     POSS. = pmin(PctMins / sum(PctMins), .2),
#     ExtraProb = 1 - sum(POSS.),
#     Non20 = sum(POSS. * (POSS. != .2)),
#     AddAmount = ifelse(POSS. == .2, 0, (POSS. / Non20) * ExtraProb),
#     PCT_Full = POSS. + AddAmount
#   )

team_depth <- mins %>%
  left_join(all_data %>% filter(Season == "2020-21") %>% mutate(Player = paste0(Player, "_", Team)), by = c("Player", "Team")) %>%
  filter(!(
    paste0(cleanName, "_", Team) %in% injuries &
      Season == "2020-21"
  )) %>%
  mutate(
    PctMins = pmin(MINS.x / (GP.x*40),1)
  ) %>%
  group_by(Team) %>%
  slice_max(n=8, order_by = PctMins) %>%
  mutate(
    POSS. = pmin(PctMins / sum(PctMins), .2),
    ExtraProb = 1 - sum(POSS.),
    Non20 = sum(POSS. * (POSS. != .2)),
    AddAmount = ifelse(POSS. == .2, 0, (POSS. / Non20) * ExtraProb),
    PCT_Full = POSS. + AddAmount
  ) %>%
  filter(Season == "2020-21") %>%
  select(Player, Team, GP = GP.y, MINS = MINS.y, PCT_Full, ORAPM:RAPM)

full_rating <- team_depth %>%
  group_by(Team) %>%
  summarise(
    ORAPM = sum(ORAPM * PCT_Full, na.rm = T)*5,
    DRAPM = sum(DRAPM * PCT_Full, na.rm = T)*5,
    .groups = "drop"
  ) %>%
  mutate(
    RAPM = ORAPM + DRAPM
  )

write_csv(full_rating, "data/team_ratings.csv")

# Game Predictions ####
game_prediction <- function(df, model, teams_vector, tm1, tm2, printData = F, long = F) {
  # df: dataframe of team ratings
  # model: model to predict possessions
  # teams_vector: needed to map team name to column for model
  # tm1, tm2: two teams using stats.ncaa formatted name
  # useScaled: whether to use base rating or scaled rating in dataframe (ORAPM vs. scaledORAPM, etc.)
  # printData: whether to print prediction output
  # long: whether to return all outputs of prediction vs. just probability
  exp_poss <- unname(predict(model,t(matrix((teams %in% c(tm1, tm2))*1)), type = "response")) / 2
  
  # 97.12 = RAPM intercept for 20-21
  tm1_score <- (df$ORAPM[df$Team == tm1] - df$DRAPM[df$Team  == tm2] + 97.12) * (exp_poss / 100)
  tm2_score <- (df$ORAPM[df$Team == tm2] - df$DRAPM[df$Team  == tm1] + 97.12) * (exp_poss / 100)

  # prob <- unname(predict(pointspread_glm, data.frame(pred_score_diff = tm1_score - tm2_score), type = "response"))
  prob <- pnorm(tm1_score - tm2_score, 0, 11.7)
  
  if(!printData & !long) {
    return(prob) 
  }
  else if(long & !printData) {
    return(c(tm1_score, tm2_score, tm1_score+tm2_score, tm1_score - tm2_score, prob))
  } else {
    writeLines(paste(
      tm1, "vs", tm2, 
      "\nExpected Score:", round(tm1_score), "-", round(tm2_score), 
      "\nExpected Total:", round(tm1_score + tm2_score),
      "\nExpected Differential:", round(tm1_score - tm2_score,1), 
      "\n Expected Probability:", round(prob,2)))
    return(c(tm1_score, tm2_score, tm1_score+tm2_score, tm1_score - tm2_score, prob))
  }
}

result = game_prediction(full_rating, model, teams, "Texas Tech", "Ohio St.", T, F)

team_games <- expand.grid(team = march_madness_teams, opponent = march_madness_teams)
team_games$team_score <- NA
team_games$opp_score <- NA
team_games$total <- NA
team_games$differential <- NA
team_games$probability <- NA

for(i in 1:nrow(team_games)) {
  vals <- game_prediction(full_rating, model, teams, 
                          team_games$team[i], team_games$opponent[i], F,T)
  team_games$team_score[i] <- vals[1]
  team_games$opp_score[i] <- vals[2]
  team_games$total[i] <- vals[3]
  team_games$differential[i] <- vals[4]
  team_games$probability[i] <- vals[5]
}

write_csv(team_games, "data/game_predictions.csv")

# Tournament Data Structure ####

build_bracket_df <- function() {
  start_ind <- data.frame(
    Team = march_madness_teams,
    Index = c(
      # West Region
      5,1,1,6,6,7,7,8,8,9,2,2,10,10,11,11,12,12,
      # East Region
      13,3,3,14,14,15,15,16,16,17,4,4,18,18,19,19,20,20,
      # South Region
      21,21,22,22,23,23,24,24,25,25,26,26,27,27,28,28,
      # Midwest Region
      29,29,30,30,31,31,32,32,33,33,34,34,35,35,36,36)
  )
  
  round_mapping <- data.frame(
    Round = c(
      rep("round0", 4),
      rep("round1", 32),
      rep("round2",16),
      rep("round3", 8),
      rep("round4", 4),
      rep("round5", 2),
      rep("round6", 1)
    ),
    Index = 1:67,
    NextRound = c(
      # Round 0
      5,9,13,17,
      # Round 1
      # Round 2
      rep(37:52, each = 2),
      # Sweet 16
      rep(53:60, each = 2),
      # Elite 8
      rep(61:64, each = 2),
      # Final 4
      rep(65:66, each = 2),
      # Championship
      67,67, NA
    )
  )
  
  full_path <- start_ind %>%
    left_join(round_mapping, by = c("Index")) %>%
    left_join(round_mapping %>% select(Index, NextRound), by = c("NextRound"="Index"), suffix = c("", "_2")) %>%
    left_join(round_mapping %>% select(Index, NextRound), by = c("NextRound_2"="Index"), suffix = c("", "_3")) %>%
    left_join(round_mapping %>% select(Index, NextRound), by = c("NextRound_3"="Index"), suffix = c("", "_4")) %>%
    left_join(round_mapping %>% select(Index, NextRound), by = c("NextRound_4"="Index"), suffix = c("", "_5")) %>%
    left_join(round_mapping %>% select(Index, NextRound), by = c("NextRound_5"="Index"), suffix = c("", "_6")) %>%
    arrange(Index)
  
  full_path[9:68, 5:9] <- full_path[9:68, 4:8] 
  full_path[9:68, 4] <- full_path[9:68, 2]
  full_path[9:68, 2] <- 5:64
  
  full_path <- full_path %>%
    select(Team, Rd0 = Index, Rd1 = NextRound, Rd2 = NextRound_2, Rd3 = NextRound_3, Rd4 = NextRound_4, Rd5 = NextRound_5, Rd6 = NextRound_6)
}

# Tournament Calculation ####

calculate_exact <- function(bracket_df, team_games) {
  # bracket_df: dataframe for tournament sim created by build_bracket_df()
  # team_games: dataframe of team predictions. Needs cols: team, opponent, probability
  
  # Set up dataframe - cols need to be stored with NA for do() to work
  exact_prob <- bracket_df
  exact_prob$Rd0_Exact <- NA_real_
  exact_prob$Rd1_Exact <- NA_real_
  exact_prob$Rd2_Exact <- NA_real_
  exact_prob$Rd3_Exact <- NA_real_
  exact_prob$Rd4_Exact <- NA_real_
  exact_prob$Rd5_Exact <- NA_real_
  exact_prob$Rd6_Exact <- NA_real_
  
  # Calculate first 4 round probabilities
  exact_prob <- exact_prob %>%
    group_by_at(2) %>%
    do({
      if(nrow(.) == 1) {
        .$Rd0_Exact = 1
      } else {
        p = team_games$probability[which(team_games$team == .$Team[1] & team_games$opponent == .$Team[2])]
        .$Rd0_Exact[1] = p
        .$Rd0_Exact[2] = 1-p
      }
      .
    }) 
  
  # Iterate through each round to calculate exact probability
  for(rnd in 1:6) {
    exact_prob <- exact_prob %>%
      # Group my bracket position
      group_by_at(2+rnd) %>%
      do({
        # If only one team, return 100%
        if(nrow(.) == 1) {
          .[[paste0("Rd",rnd,"_Exact")]] = 1
        } else {
          df = .
          teams <- df$Team
          # Calculate all possible matchup combinations
          grid <- expand.grid(Tm1 = teams, Tm2 = teams, stringsAsFactors = F) %>%
            arrange(Tm1) %>%
            # Remove duplicate team against itself
            filter(Tm1!=Tm2) %>%
            mutate(
              # Get bracket position of each team in the previous round
              PrevRnd = df[[paste0("Rd",rnd-1)]][match(Tm1, df$Team)],
              # Get probability team was "alive" in previous step
              Psurvive = df[[paste0("Rd",rnd-1,"_Exact")]][match(Tm1, df$Team)]
            ) %>%
            # Join back the exact dataframe to get other team probabilities
            left_join(df, by = c("Tm2"="Team")) %>%
            # Filter when teams were in same branch in previous round
            # ie. Gonzaga cannot play Appalachian St. in round 3 because they went through the same branch so only one could survive
            filter(PrevRnd != .[[paste0("Rd",rnd-1)]]) %>%
            # Pull the probabilities that each team wins against each opponent
            group_by(Tm1) %>%
            do({
              games <- filter(team_games, team == first(.$Tm1))
              p <- games$probability[match(.$Tm2, games$opponent)]
              # New probability is P(Team Alive in previous round) * P(Opponent Alive in previous round) * P(Team beats Opponent)
              exact <- sum(.[[paste0("Rd",rnd-1,"_Exact")]] * .$Psurvive * p)
              # Return as single numeric wrapped in df to fit grip
              data.frame(exact)
            })
          # Add round probabilities using the calculation above
          .[[paste0("Rd",rnd,"_Exact")]] <- grid$exact[match(.$Team, grid$Tm1)]
        }
        # Return the original dataframe with the new row modifications to RdX_Exact
        .
      })
    print(rnd)
  }
  
  return(exact_prob)
}

# Output ####

final_table <- calculate_exact(build_bracket_df(), team_games) %>%
  ungroup() %>%
  select(Team, Rd0_Exact:Rd6_Exact) %>%
  mutate(
    across(matches("[0-9]"), ~ round(.x*100, 3)),
    Seed = seeds[match(Team, march_madness_teams)],
    Region = region[match(Team, march_madness_teams)]
  ) %>%
  select(Region, Seed, Team, 
         First4 = Rd0_Exact, Round1 = Rd1_Exact, Round2 = Rd2_Exact, Sweet16 = Rd3_Exact, Elite8 = Rd4_Exact, Final4 = Rd5_Exact, Championship = Rd6_Exact)

write_csv(final_table, "data/table_predictions.csv")

# benz <- read_csv("/Users/jake/Downloads/ncaa_wp_matrix_2021.csv")
# 
# benz$team <- ifelse(benz$team == "Mount St. Mary's", "Mt. St. Mary's", benz$team)
# benz$opponent <- ifelse(benz$opponent == "Mount St. Mary's", "Mt. St. Mary's", benz$opponent)
# 
# benz2 <- left_join(benz, team_games, by = c("team", "opponent"))
# 
# ggplot(benz2, aes(pred_score_diff, differential)) + geom_point() + geom_abline(slope = 1, intercept = 0) + geom_smooth()
# ggplot(benz2, aes(win_prob, probability)) + geom_point() + geom_abline(slope = 1, intercept = 0) + geom_smooth()
# 
# x = filter(benz2, team == 'Gonzaga') %>% select(team, opponent, Luke_Differential = pred_score_diff, Jake_Differential=differential, Luke_Prob = win_prob, Jake_Prob = probability)
# 
# 
# 
