library(lme4)
source("data_cleaning.R")


player1df <- atp_df %>% group_by(match_id) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_longer(c(player1, player2), names_to = "player", values_to = "name") %>%
  group_by(name) %>%
  summarise(nmatches = n()) %>%
  filter(nmatches >= 4)

## only want matches where both players have played 4 or more matches.
only4 <- atp_df %>% filter(player1 %in% player1df$name &
    player2 %in% player1df$name)



test_df <- only4 %>% mutate(servingplayer = case_when(PointServer == 1 ~ player1,
  PointServer == 2 ~ player2)) %>%
  mutate(returningplayer = case_when(PointServer == 1 ~ player2,
    PointServer == 2 ~ player1))


mod <- lmer(serverwin ~ importance + (1 + importance | servingplayer) +
    (1 + importance | returningplayer), data = test_df, REML = FALSE)
summary(mod)

predict(mod)
mod@frame
fixef(mod)
sigma(mod)

ranef(mod)
coef(mod) ## coef are random effects plus fixed effects

## could then make a plot with a line for each player, importance (or whatever)
## on x-axis and whether the player won the point on the y-axis.

ggplot(test_df, aes(serverwin, importance, color = servingplayer)) +
  geom_point() +
  geom_line(aes(y = predict(mod)))


