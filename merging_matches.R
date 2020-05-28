
atp2010 <- read_csv("data/atp_matches_2010.csv")
atp2011 <- read_csv("data/atp_matches_2011.csv")
atp2012 <- read_csv("data/atp_matches_2012.csv")
atp2013 <- read_csv("data/atp_matches_2013.csv")
atp2014 <- read_csv("data/atp_matches_2014.csv")
atp2015 <- read_csv("data/atp_matches_2015.csv")
atp2016 <- read_csv("data/atp_matches_2016.csv")
atp2017 <- read_csv("data/atp_matches_2017.csv")
atp2018 <- read_csv("data/atp_matches_2018.csv")
atp2019 <- read_csv("data/atp_matches_2019.csv")
atp2020 <- read_csv("data/atp_matches_2020.csv")

atp2019$winner_seed <- as.numeric(as.character(atp2019$winner_seed))
atp2020$winner_seed <- as.numeric(as.character(atp2020$winner_seed))
atp2019$loser_seed <- as.numeric(as.character(atp2019$loser_seed))
atp2020$loser_seed <- as.numeric(as.character(atp2020$loser_seed))

atp_decade <- bind_rows(atp2010, atp2011, atp2012, atp2013,atp2014, atp2015, atp2016, atp2017, 
                        atp2018, atp2019, atp2020)

atp_decade <-
  atp_decade %>%
  mutate(`w_serveperc` = w_1stIn/w_svpt) %>%
  mutate(`l_serveperc` = l_1stIn/l_svpt)

gs_decade <-
  atp_decade %>%
  filter(tourney_name == "Australian Open" | tourney_name == "Wimbledon" | 
           tourney_name == "Roland Garros" | tourney_name == "US Open")

# player column
gs_decade_long <-
  gs_decade %>%
  pivot_longer(c(winner_name, loser_name), names_to = "win_loss", values_to = "player")

# matches lost
losers <- gs_decade_long %>%
  select(-starts_with("w_")) %>%
  filter(win_loss == "loser_name")

# matches won
winners <- gs_decade_long %>%
  select(-starts_with("l_")) %>%
  filter(win_loss == "winner_name")

gs_matches_final <- bind_rows(losers, winners)

# make just one variable for wether the player won
gs_matches_final <-
  gs_matches_final %>%
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
# end of manipulating gs_matches_final


gs_matches_final %>%
  filter(player == "Roger Federer" | player == "Rafael Nadal" | player == "Novak Djokovic") %>%
  ggplot(., aes(x = first_serve, y = win, color = player)) +
  stat_smooth(method = "glm", method.args = c("binomial"), se = F) +
  theme_bw()

gs_matches_final %>%
  filter(player == "Rafael Nadal") %>%
  ggplot(., aes(x = first_serve, y = win, color = tourney_name)) +
  geom_jitter(height = 0.12, alpha = 0.25) +
  stat_smooth(method = "glm", method.args = c("binomial"), se = F) +
  theme_bw()
## this is pretty interesting
## only at the french does his chances of winning imporve with first_serve perc
## his probability is also almost 1 past 70% first serve
## the other lines dont make much sense to me

gs_matches_final %>%
  filter(player == "Rafael Nadal") %>%
  summarise(n())
## enough matches to make me think the pattern isnt just chance


gs_matches_final %>%
  filter(player == "Roger Federer") %>%
  ggplot(., aes(x = first_serve, y = win, color = tourney_name)) +
  geom_jitter(height = 0.12, alpha = 0.25) +
  stat_smooth(method = "glm", method.args = c("binomial"), se = F) +
  theme_bw()
# for some reason Fed's wimbledon line trends downward?

gs_matches_final %>%
  filter(player == "Novak Djokovic") %>%
  ggplot(., aes(x = first_serve, y = win, color = tourney_name)) +
  geom_jitter(height = 0.12, alpha = 0.25) +
  stat_smooth(method = "glm", method.args = c("binomial"), se = F) +
  theme_bw()
# very similar to Fed



