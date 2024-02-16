library(tidyverse)
#Data: https://github.com/nflverse/nflverse-data/releases/tag/pbp
pbp2023 <- read.csv("./play_by_play_2023.csv")
yards <- pbp2023 %>% filter(play_type %in% c("pass","run")) %>% pull(yards_gained)
punt <- pbp2023 %>% filter(play_type == "punt") %>% mutate(punt_net = kick_distance - return_yards) %>% pull(punt_net)

fg <- pbp2023 %>% filter(season_type == "REG" & play_type == "field_goal") %>% select(kick_distance, field_goal_result) %>% mutate(made = field_goal_result == "made")

fgmod <- glm(made ~ kick_distance, data = fg, family = "binomial")
summary(fgmod)

fgpct <- function(dist){
  xb <- fgmod$coefficients[1] +  fgmod$coefficients[2]*dist
  prob <- exp(xb)/(1+exp(xb))
  return(prob)
}


