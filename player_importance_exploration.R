## can consider doing something similar to this for the atp

source("data_cleaning.R")
library(tidyverse)
player1df <- wta_df2 %>% group_by(match_id) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_longer(c(player1, player2), names_to = "player", values_to = "name") %>%
  group_by(name) %>%
  summarise(nmatches = n()) %>%
  filter(nmatches >= 15) %>%
  filter(!is.na(name))

## only want matches where both players have played 4 or more matches.
match15_df <- wta_df2 %>% filter(player1 %in% player1df$name &
    player2 %in% player1df$name)


match15_df <- match15_df %>% mutate(servingplayer = case_when(PointServer == 1 ~ player1,
  PointServer == 2 ~ player2)) %>%
  mutate(returningplayer = case_when(PointServer == 1 ~ player2,
    PointServer == 2 ~ player1))

## explore level 1 effects first....consider Rally Count as response first.
## then, consider ServeDepth as response

match15_df %>% 
  ggplot(data = ., aes(x = importance, y = RallyCount)) +
  geom_point() + 
  facet_wrap( ~ servingplayer) +
  stat_smooth(method = "lm")

match15_df %>% 
  ggplot(data = ., aes(x = importance, y = serverwin)) +
  geom_point() + 
  facet_wrap( ~ servingplayer) +
  stat_smooth(method = "lm")

View(match15_df)
match15_df %>% filter(is.na(ServeDepth) == FALSE) %>% 
  mutate(serveind = if_else(ServeDepth == "CTL", true = 1, false = 0)) %>%
  ggplot(data = ., aes(x = importance, y = serveind)) +
  geom_point() +
  facet_wrap( ~ servingplayer) + 
  stat_smooth()

match15_df %>% filter(is.na(ServeWidth) == FALSE & 
    ServeNumber == 1) %>% 
  mutate(servewidthind = if_else(ServeWidth == "W" | 
      ServeWidth == "BW", true = 1, false = 0)) %>%
  ggplot(data = ., aes(x = importance, y = servewidthind)) +
  geom_point() +
  facet_wrap( ~ servingplayer) + 
  stat_smooth()

match15_df %>% filter(is.na(ServeWidth) == FALSE &
    ServeNumber == 1) %>% 
  mutate(servewidthind = if_else(ServeWidth == "C" | 
      ServeWidth == "BD", true = 1, false = 0)) %>%
  ggplot(data = ., aes(x = importance, y = servewidthind)) +
  geom_point() +
  facet_wrap( ~ servingplayer) + 
  stat_smooth()
# should this be ServeWidth == "BC"?

match15_df %>% filter(is.na(ace) == FALSE) %>%
  ggplot(data = ., aes(x = importance, y = ace)) +
  geom_point() +
  facet_wrap( ~ servingplayer) + 
  stat_smooth()
# serena seems like the only one with some upward trend

match15_df %>% filter(is.na(ace) == FALSE) %>%
  ggplot(data = ., aes(x = importance, y = doublefault)) +
  geom_point() +
  facet_wrap( ~ servingplayer) + 
  stat_smooth()

match15_df %>% filter(is.na(ServeNumber) == FALSE) %>% 
  mutate(firstserveind = if_else(ServeNumber == 1, true = 1, false = 0)) %>%
  ggplot(data = ., aes(x = importance, y = firstserveind)) +
  geom_point() +
  facet_wrap( ~ servingplayer) + 
  stat_smooth()

match15_df %>% filter(is.na(Speed_MPH) == FALSE & Speed_MPH != 0 &
    ServeNumber == 1) %>%
  ggplot(data = ., aes(x = importance, y = Speed_MPH)) +
  geom_point() +
  facet_wrap( ~ servingplayer) + 
  stat_smooth()

library(broom)
d <- match15_df %>% 
  nest(-servingplayer) %>% 
  mutate(fit = map(data, ~ lm(.$RallyCount ~ .$importance)),
    results = map(fit, tidy)) %>%
  unnest(results)

## level one model
d %>% filter(term == "(Intercept)") %>%
  ggplot(data = ., aes(x = estimate)) + geom_histogram()
d %>% filter(term == ".$importance") %>%
  ggplot(data = ., aes(x = estimate)) + geom_histogram()
d %>% filter(term == ".$importance") %>%
  ggplot(data = ., aes(x = p.value)) + geom_histogram()

## level 2: player specific characteristics
## no level 2 covariates in this case....so we would just use an intercept
## term to model the level 1 intercepts and slopes

interceptmod <- lm(estimate ~ 1, data = d %>% filter(term == "(Intercept)"))
summary(interceptmod) ## average rally length is different from 0
## obviously
slopemod <- lm(estimate ~ 1, data = d %>% filter(term == ".$importance"))
summary(slopemod) ## strong evidence that, on average, the slope
## is greater than 0

## combining these and comparing results for an actual mixed effects
## model

library(lme4)
mod <- lmer(RallyCount ~ importance + (1 + importance | servingplayer) +
    (1 | returningplayer),
  data = match15_df, REML = FALSE)
summary(mod)
coef(mod)








##### can ignroe from here down!!

player1df <- wta_df2 %>% group_by(player1) %>% summarise(ncount = n())
player2df <- wta_df2 %>% group_by(player2) %>% summarise(ncount2 = n())

inner_join(player1df, player2df, by = c("player1" = "player2")) %>%
  mutate(ncount_total = ncount + ncount2) %>%
  arrange(desc(ncount_total))

player <- "Angelique Kerber"
## two extreme cases: Angelique Kerber and Caroline Garcia
oneplayeronly <- wta_df2 %>% filter(player1 == player | 
    player2 == player) %>%
  mutate(vwin = case_when(player1 == player & PointWinner == 1 ~ 1,
    player1 == player & PointWinner == 2 ~ 0,
    player2 == player & PointWinner == 1 ~ 0,
    player2 == player & PointWinner == 2 ~ 1)) %>%
  mutate(vserve = case_when(player1 == player & PointServer == 1 ~ 1,
    player1 == player & PointServer == 2 ~ 0,
    player2 == player & PointServer == 1 ~ 0,
    player2 == player & PointServer == 2 ~ 1)) %>%
  mutate(import_ind = if_else(importance > 0.2, true = 1, false = 0))

small_wta <- wta_df2 %>% filter(player1 == "Venus Williams" | player1 == "Angelique Kerber" |
    player1 == "Caroline Garcia")

player <- "Caroline Garcia"

ggplot(data = small_wta, aes(x = importance, y = serverwin, colour = player1)) +
  geom_jitter(height = 0.12) +
  stat_smooth(method = "glm", method.args = c("binomial"), se = FALSE) +
  stat_smooth(data = filter(oneplayeronly, vserve == 1),
    aes(x = importance, y = serverwin), colour = "black",
    method = "gam") +
  stat_smooth(data = filter(oneplayeronly, vserve == 0), colour = "orange",
    method = "gam", formula = y ~ s(x, bs = "cs")) +
  stat_smooth(data = oneplayeronly, mapping = aes(x = importance, y = vwin),
    colour = "red", method = "glm")


## can look at how players actually play the more important points

## kerber looked like she played important points worse....
names(oneplayeronly)
ggplot(data = filter(oneplayeronly, vserve == 1) , aes(x = importance, y = RallyCount)) + 
  geom_point() +
  stat_smooth() ## points longer?

oneplayerserve <- oneplayeronly %>% filter(is.na(ServeDepth) == FALSE) %>% 
  mutate(serveind = if_else(ServeDepth == "CTL", true = 1, false = 0))
ggplot(data = filter(oneplayerserve, vserve == 1) , aes(x = importance, y = serveind)) + 
  geom_point() +
  stat_smooth(method = "glm", method.args = "binomial") ## does serve depth change on important points?




oneplayerreturn <- oneplayeronly %>% filter(is.na(ReturnDepth) == FALSE) %>% 
  mutate(returnind = if_else(ReturnDepth == "ND", true = 1, false = 0))
ggplot(data = filter(oneplayerreturn, vserve == 0),
  aes(x = importance, y = returnind)) +
  geom_point() + 
  stat_smooth(method = "glm", method.args = "binomial") ## does return depth change on important points?




