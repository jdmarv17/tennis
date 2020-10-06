library(tidyverse)


source("point_files/data_cleaning.R")
source("match_files/merging_matches.R")


# can change nmatches to add more players to data set
players_keep_atp <- 
  final_atp_df1 %>% 
  group_by(match_id) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_longer(c(player1, player2), names_to = "player", values_to = "name") %>%
  group_by(name) %>%
  summarise(nmatches = n()) %>%
  filter(nmatches >= 15 & is.na(name) == FALSE)

# add variables for server, returner, and side of court
nn_mod_df <- 
  new_atp_df2 %>% 
  ungroup() %>%
  filter(player1 %in% players_keep_atp$name & player2 %in% players_keep_atp$name) %>% 
  mutate(servingplayer = case_when(PointServer == 1 ~ player1, PointServer == 2 ~ player2)) %>%
  mutate(returningplayer = case_when(PointServer == 1 ~ player2, PointServer == 2 ~ player1)) %>%
  filter(is.na(importance2) == FALSE) %>%
  mutate(ServeSide = ifelse((serve_point + return_point) %% 2 == 0, "Deuce", "Ad"))

# get df with name and handedness
tmp <- 
  gs_decade %>%
  filter(winner_name %in% players_keep_atp$name & loser_name %in% players_keep_atp$name) %>%
  select(winner_name, winner_hand, loser_name, loser_hand) %>%
  gather(key = win_loss, value = name, winner_name, loser_name) %>%
  mutate(hand = case_when(
    win_loss == "winner_name" ~ winner_hand,
    win_loss == "loser_name" ~ loser_hand
  )) %>%
  within(., rm(win_loss, winner_hand, loser_hand))
tmp <- 
  tmp[!duplicated(tmp$name),]

# get df with name and height
tmp2 <- 
  gs_decade %>%
  filter(winner_name %in% players_keep_atp$name & loser_name %in% players_keep_atp$name) %>%
  select(winner_name, winner_ht, loser_name, loser_ht) %>%
  gather(key = win_loss, value = name, winner_name, loser_name) %>%
  mutate(height = case_when(
    win_loss == "winner_name" ~ winner_ht,
    win_loss == "loser_name" ~ loser_ht
  )) %>%
  within(., rm(win_loss, winner_ht, loser_ht))
tmp2 <- 
  tmp2[!duplicated(tmp2$name),]


# assign hand and height variable to servingplayer 
nn_mod_df <-
  left_join(nn_mod_df, tmp, by = c("servingplayer" = "name"))

# assign height variable to returningplayer 
nn_mod_df <-
  left_join(nn_mod_df, tmp2, by = c("returningplayer" = "name"))


# 0, 1 for categorical
nn_mod_df <-
  nn_mod_df %>%
  mutate(serve_side = case_when(
    ServeSide == "Deuce" ~ 1,
    ServeSide == "Ad" ~ 0), 
    serve_number = case_when(
      ServeIndicator == 1 ~ 0,
      ServeIndicator == 2 ~ 1),
    serve_depth = case_when(
      ServeDepth == "CTL" ~ 1,
      ServeDepth == "NCTL" ~ 0),
    Handedness = case_when(
      hand == "R" ~ 0,
      hand == "L" ~ 1),
    server_up = case_when(
      (serve_point - return_point) > 0 ~ 1,
      (serve_point - return_point) <= 0 ~ 0
    )) %>%
  filter(is.na(ServeWidth) == F & is.na(serve_number) == F & is.na(serve_depth) == F & is.na(serve_side) == F & is.na(Handedness) == F & is.na(server_up) == F)

nn_mod_df <-
  nn_mod_df %>%
  unite("ServePlacement", c(ServeWidth, ServeDepth), remove = F) %>%
  mutate(serve_width = case_when(
    ServeWidth == "BC" | ServeWidth == "C" ~ "C",
    ServeWidth == "BW" | ServeWidth == "W" ~ "W",
    ServeWidth == "B" ~ "B"
  ))

nn_mod_df <-
  nn_mod_df %>%
  mutate(Nishikori = ifelse(servingplayer == "Kei Nishikori", 1, 0),
         Murray = ifelse(servingplayer == "Andy Murray", 1, 0),
         Thiem = ifelse(servingplayer == "Dominic Thiem", 1, 0),
         Fed = ifelse(servingplayer == "Roger Federer", 1, 0),
         Stan = ifelse(servingplayer == "Stan Wawrinka", 1, 0),
         Nadal = ifelse(servingplayer == "Rafael Nadal", 1, 0),
         Novak = ifelse(servingplayer == "Novak Djokovic", 1, 0),
         Milos = ifelse(servingplayer == "Milos Raonic", 1, 0),
         Isner = ifelse(servingplayer == "John Isner", 1, 0),
         Tsonga = ifelse(servingplayer == "Jo Wilfried Tsonga", 1, 0),
         Zverev = ifelse(servingplayer == "Alexander Zverev", 1, 0),
         Tomic = ifelse(servingplayer == "Bernard Tomic", 1, 0),
         Ferrer = ifelse(servingplayer == "David Ferrer", 1, 0),
         Goffin = ifelse(servingplayer == "David Goffin", 1, 0),
         Monfils = ifelse(servingplayer == "Gael Monfils", 1, 0),
         Dimitrov = ifelse(servingplayer == "Grigor Dimitrov", 1, 0),
         Anderson = ifelse(servingplayer == "Kevin Anderson", 1, 0),
         Edmund = ifelse(servingplayer == "Kyle Edmund", 1, 0),
         Pouille = ifelse(servingplayer == "Lucas Pouille", 1, 0),
         Cilic = ifelse(servingplayer == "Marin Cilic", 1, 0),
         Kyrgios = ifelse(servingplayer == "Nick Kyrgios", 1, 0),
         Gasquet = ifelse(servingplayer == "Richard Gasquet", 1, 0),
         Agut = ifelse(servingplayer == "Roberto Bautista Agut", 1, 0),
         Querrey = ifelse(servingplayer == "Sam Querrey", 1, 0),
         Johnson = ifelse(servingplayer == "Steve Johnson", 1, 0),
         Berdych = ifelse(servingplayer == "Tomas Berdych", 1, 0),
  )

# separate to get slam and add binary variable
nn_mod_df <-
  nn_mod_df %>%
  separate(match_id, into = c("year", "tmp"), sep = "-", extra = "merge") 

nn_mod_df <-
  nn_mod_df %>%
  separate(tmp, into = c("slam", "tourn_id"), sep = "-", extra = "merge") %>%
  mutate(hard_court = case_when(
    slam == "usopen" | slam == "ausopen" ~ 1,
    slam == "wimbledon" | slam == "frenchopen" ~ 0
  ))


# get rid of players without height
nn_mod_df <-
  nn_mod_df %>%
  filter(is.na(height) == F)

rf_mod_df <-
  nn_mod_df %>%
  mutate(server_dist = case_when(
    PointServer == 1 ~ P1DistanceRun,
    PointServer == 2 ~ P2DistanceRun
  )) %>%
  mutate(returner_dist = case_when(
    PointServer == 1 ~ P2DistanceRun,
    PointServer == 2 ~ P1DistanceRun
  )) %>%
  mutate(returnerwin = ifelse(serverwin == 1, 0, 1))

# try to get match winner from point data so we have variable to merge winner_rank_points and loser_rank_points
#rf_mod_df2 <-
#  rf_mod_df %>%
#  group_by(match_num) %>%
#  mutate(match_winner = case_when(
#    (player1 == tmp4$winner_name & player2 == tmp4$loser_name & slam == tmp4$slam & year == tmp4$year) ~ player1 , 
#    (player1 == tmp4$loser_name & player2 == tmp4$winner_name & slam == tmp4$slam & year == tmp4$year) ~ player2
#  ))
    



# get df with rank points, player, result, tourney
tmp3 <-
  gs_decade %>%
  pivot_longer(c(winner_name, loser_name), values_to = "name", names_to = "win_loss") %>%
  mutate(rank_points = case_when(
    win_loss == "winner_name" ~ winner_rank_points,
    win_loss == "loser_name" ~ loser_rank_points
  )) %>%
  mutate(result = case_when(
    win_loss == "winner_name" ~ 1,
    win_loss == "loser_name" ~ 0
  )) %>%
  select(name, rank_points, tourney_name, tourney_date, tourney_id, result) %>%
  mutate(slam = case_when(
    tourney_name == "Australian Open" ~ "ausopen",
    tourney_name == "US Open" ~ "usopen",
    tourney_name == "Roland Garros" ~ "frenchopen",
    tourney_name == "Wimbledon" ~ "wimbledon"
  )) %>%
  separate(tourney_id, into = c("year", "tmp"), sep = "-", extra = "merge") %>%
  filter(year == "2016" | year == "2017")

#tmp4 <-
#  gs_decade %>%
#  select(winner_name, loser_name, winner_rank_points, loser_rank_points, tourney_name, tourney_date, tourney_id) %>%
#  mutate(slam = case_when(
#    tourney_name == "Australian Open" ~ "ausopen",
#    tourney_name == "US Open" ~ "usopen",
#    tourney_name == "Roland Garros" ~ "frenchopen",
#    tourney_name == "Wimbledon" ~ "wimbledon"
#  )) %>%
  
#  separate(tourney_id, into = c("year", "tmp"), sep = "-", extra = "merge") %>%
#  filter(year == "2016" | year == "2017")

# merge with point data to assign rank points and keep only important variables
#rf_df_with_rank <-
#  left_join(rf_mod_df, tmp3, by = c("slam", "year", "player1" = "name", "player2" = "name")) %>%
#  select(player1, player2, rank_points, slam, year, Speed_MPH, server_up, RallyCount, ServeSide, Handedness, height, 
#         serve_number, serverwin, returnerwin, importance2, game_score, point_score, serve_set, return_set, ElapsedTime,
#         PointNumber, PointWinner, PointServer, servingplayer, returningplayer, server_dist, returner_dist,
#         serve_score_name, return_score_name)

# pivot point data longer, join with tmp3 and pivot wide..........................................
rf_df_long <-
  rf_mod_df %>%
  pivot_longer(c(player1, player2), values_to = "name", names_to = "player_number") 

rf_df_long <-
  left_join(rf_df_long, tmp3, by = c("slam", "year", "name"))

rf_df_with_rank <-
  rf_df_long %>%
  pivot_wider(names_from = "player_number", values_from = "name")





nadal <-
  rf_df_with_rank %>%
  filter(player1 == "Rafael Nadal" | player2 == "Rafael Nadal")
nadal <-
  nadal[!duplicated(nadal$ElapsedTime),]
nadal <-
  nadal %>%
  mutate(p1_score = case_when(
    PointServer == 1 ~ serve_score_name,
    PointServer == 2 ~ return_score_name
  )) %>%
  mutate(p2_score = case_when(
    PointServer == 1 ~ return_score_name,
    PointServer == 2 ~ serve_score_name
  )) %>%
  select(player1, player2, p1_score, p2_score, PointNumber, PointWinner, year, slam)
# why are some point scores not consistent with PointWinner variable???






