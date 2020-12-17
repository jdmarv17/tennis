library(tidyverse)
atp2010 <- read_csv(here("SYE", "data", "atp_matches_2010.csv"))
atp2011 <- read_csv(here("SYE", "data", "atp_matches_2011.csv"))
atp2012 <- read_csv(here("SYE", "data", "atp_matches_2012.csv"))
atp2013 <- read_csv(here("SYE", "data", "atp_matches_2013.csv"))
atp2014 <- read_csv(here("SYE", "data", "atp_matches_2014.csv"))
atp2015 <- read_csv(here("SYE", "data", "atp_matches_2015.csv"))
atp2016 <- read_csv(here("SYE", "data", "atp_matches_2016.csv"))
atp2017 <- read_csv(here("SYE", "data", "atp_matches_2017.csv"))
atp2018 <- read_csv(here("SYE", "data", "atp_matches_2018.csv"))
atp2019 <- read_csv(here("SYE", "data", "atp_matches_2019.csv"))
atp2020 <- read_csv(here("SYE", "data", "atp_matches_2020.csv"))

wta2010 <- read_csv(here("SYE", "data", "wta_matches_2010.csv"))
wta2011 <- read_csv(here("SYE", "data", "wta_matches_2011.csv"))
wta2012 <- read_csv(here("SYE", "data", "wta_matches_2012.csv"))
wta2013 <- read_csv(here("SYE", "data", "wta_matches_2013.csv"))
wta2014 <- read_csv(here("SYE", "data", "wta_matches_2014.csv"))
wta2015 <- read_csv(here("SYE", "data", "wta_matches_2015.csv"))
wta2016 <- read_csv(here("SYE", "data", "wta_matches_2016.csv"))
wta2017 <- read_csv(here("SYE", "data", "wta_matches_2017.csv"))
wta2018 <- read_csv(here("SYE", "data", "wta_matches_2018.csv"))
wta2019 <- read_csv(here("SYE", "data", "wta_matches_2019.csv"))
wta2020 <- read_csv(here("SYE", "data", "wta_matches_2020.csv"))

# change variable type to merge
atp2019$winner_seed <- as.numeric(as.character(atp2019$winner_seed))
atp2020$winner_seed <- as.numeric(as.character(atp2020$winner_seed))

atp2019$loser_seed <- as.numeric(as.character(atp2019$loser_seed))
atp2020$loser_seed <- as.numeric(as.character(atp2020$loser_seed))

wta2016$winner_seed <- as.numeric(as.character(wta2016$winner_seed))
wta2019$winner_seed <- as.numeric(as.character(wta2019$winner_seed))
wta2020$winner_seed <- as.numeric(as.character(wta2020$winner_seed))

wta2016$loser_seed <- as.numeric(as.character(wta2016$loser_seed))
wta2019$loser_seed <- as.numeric(as.character(wta2019$loser_seed))
wta2020$loser_seed <- as.numeric(as.character(wta2020$loser_seed))

# bind all years
atp_decade <- bind_rows(atp2010, atp2011, atp2012, atp2013,atp2014, atp2015, atp2016, atp2017, 
                        atp2018, atp2019, atp2020)

wta_decade <- bind_rows(wta2010, wta2011, wta2012, wta2013, wta2014, wta2015, wta2016, wta2017, 
                        wta2018, wta2019, wta2020)

# make first and second serve variables
atp_decade <-
  atp_decade %>%
  mutate(`w_serveperc` = w_1stIn/w_svpt) %>%
  mutate(`l_serveperc` = l_1stIn/l_svpt) %>%
  mutate(`w_secondserve` = ((w_svpt - w_1stIn - w_df)/(w_svpt - w_1stIn ))) %>%
  mutate(`l_secondserve` = ((l_svpt - l_1stIn - l_df)/(l_svpt - l_1stIn )))
 
wta_decade <-
  wta_decade %>%
  mutate(`w_serveperc` = w_1stIn/w_svpt) %>%
  mutate(`l_serveperc` = l_1stIn/l_svpt) %>%
  mutate(`w_secondserve` = ((w_svpt - w_1stIn - w_df)/(w_svpt - w_1stIn ))) %>%
  mutate(`l_secondserve` = ((l_svpt - l_1stIn - l_df)/(l_svpt - l_1stIn )))

# filter for grand slam matches only
gs_decade <-
  atp_decade %>%
  filter(tourney_name == "Australian Open" | tourney_name == "Wimbledon" | 
           tourney_name == "Roland Garros" | tourney_name == "US Open")

wta_gs_decade <-
  wta_decade %>%
  filter(tourney_name == "Australian Open" | tourney_name == "Wimbledon" | 
           tourney_name == "Roland Garros" | tourney_name == "US Open")

# player column
gs_decade_long <-
  gs_decade %>%
  pivot_longer(c(winner_name, loser_name), names_to = "win_loss", values_to = "player")

wta_gs_decade_long <-
  wta_gs_decade %>%
  pivot_longer(c(winner_name, loser_name), names_to = "win_loss", values_to = "player")

# matches lost
losers <- 
  gs_decade_long %>%
  select(-starts_with("w_")) %>%
  filter(win_loss == "loser_name")

losers2 <- 
  wta_gs_decade_long %>%
  select(-starts_with("w_")) %>%
  filter(win_loss == "loser_name")

# matches won
winners <- 
  gs_decade_long %>%
  select(-starts_with("l_")) %>%
  filter(win_loss == "winner_name")

winners2 <- 
  wta_gs_decade_long %>%
  select(-starts_with("l_")) %>%
  filter(win_loss == "winner_name")


# bind winners and losers
gs_matches_final <- bind_rows(losers, winners)

wta_gs_final <- bind_rows(losers2, winners2)

# make just one variable for wether the player won
gs_matches_final <-
  gs_matches_final %>%
  mutate(win = case_when(
    win_loss == "loser_name" ~ 0,
    win_loss == "winner_name" ~ 1
    )) 

wta_gs_final <-
  wta_gs_final %>%
  mutate(win = case_when(
    win_loss == "loser_name" ~ 0,
    win_loss == "winner_name" ~ 1
    )) 

# one variable for first serve percentage
gs_matches_final <-
  gs_matches_final %>%
  mutate(first_serve = case_when(
    win == 0 ~ l_serveperc,
    win == 1 ~ w_serveperc
  ))

wta_gs_final <-
  wta_gs_final %>%
  mutate(first_serve = case_when(
    win == 0 ~ l_serveperc,
    win == 1 ~ w_serveperc
  ))

# one variable for second serve
gs_matches_final <-
  gs_matches_final %>%
  mutate(second_serve = case_when(
    win == 0 ~ l_secondserve,
    win == 1 ~ w_secondserve
  ))

wta_gs_final <-
  wta_gs_final %>%
  mutate(second_serve = case_when(
    win == 0 ~ l_secondserve,
    win == 1 ~ w_secondserve
  ))


# categories for rank and opponent rank
gs_matches_final <-
  gs_matches_final %>%
  mutate(top30 = case_when(
    win == 0 & loser_rank <= 30 ~ "top30",
    win == 0 & loser_rank > 30 ~ "outside_top30",
    win == 0 & is.na(loser_rank) == TRUE ~ "outside_top30",
    win == 1 & winner_rank <= 30 ~ "top30",
    win == 1 & winner_rank > 30 ~ "outside_top30",
    win == 1 & is.na(winner_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_top30 = case_when(
    win == 0 & winner_rank <= 30 ~ "top30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 30 ~ "top30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_ranking = case_when(
    win == 0 & winner_rank <= 10 ~ "top10",
    win == 0 & 10 < winner_rank & winner_rank <= 30 ~ "10_to_30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 10 ~ "top10",
    win == 1 & 10 < loser_rank & loser_rank <= 30 ~ "10_to_30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"))

wta_gs_final <-
  wta_gs_final %>%
  mutate(top30 = case_when(
    win == 0 & loser_rank <= 30 ~ "top30",
    win == 0 & loser_rank > 30 ~ "outside_top30",
    win == 0 & is.na(loser_rank) == TRUE ~ "outside_top30",
    win == 1 & winner_rank <= 30 ~ "top30",
    win == 1 & winner_rank > 30 ~ "outside_top30",
    win == 1 & is.na(winner_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_top30 = case_when(
    win == 0 & winner_rank <= 30 ~ "top30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 30 ~ "top30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"
  )) %>%
  mutate(opponent_ranking = case_when(
    win == 0 & winner_rank <= 10 ~ "top10",
    win == 0 & 10 < winner_rank & winner_rank <= 30 ~ "10_to_30",
    win == 0 & winner_rank > 30 ~ "outside_top30",
    win == 0 & is.na(winner_rank) == TRUE ~ "outside_top30",
    win == 1 & loser_rank <= 10 ~ "top10",
    win == 1 & 10 < loser_rank & loser_rank <= 30 ~ "10_to_30",
    win == 1 & loser_rank > 30 ~ "outside_top30",
    win == 1 & is.na(loser_rank) == TRUE ~ "outside_top30"))
# end 





