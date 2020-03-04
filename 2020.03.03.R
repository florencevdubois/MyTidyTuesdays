library(ggthemes)
library(tidyverse)
library(gghighlight)
library(zoo)

# load data
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

# recode the variables: especially the cumulative sum of goals and shots
game_goals_viz <- game_goals %>% 
  dplyr::group_by(player) %>% 
  mutate(cum_goals = cumsum(goals),
         cum_shots = cumsum(shots)) %>%  # cumulative sum of goals and shots
  dplyr::ungroup() %>% 
  separate(age, c("years", "days"), "-") %>% 
  mutate(years = as.numeric(years)*365.25,
         days = as.numeric(days),
         age_in_days = years + days,
         age_in_years = age_in_days/365.25) %>% # recode age variable (for other viz purposes)
  distinct(player, age_in_years, season, cum_goals, date, cum_shots) %>% 
  dplyr::group_by(player) %>% 
  mutate(start_season = min(season)) %>% 
  dplyr::ungroup() %>% 
  mutate(start_season_cat = ifelse(start_season < 1990, "1980s",
                                   ifelse(start_season >= 1990 & start_season < 2000, "1990s",
                                          ifelse(start_season >= 2000 & start_season < 2010, "2000s", "2010s" )))) # add a category for when they started their careers (for other viz purposes)

# explore data  
length(unique(game_goals_viz$player))
table(game_goals_viz$player)
table(game_goals_viz$player, game_goals_viz$season)

# vizualize: trajectories of goals by player, highlighting those who scored more than 5000 times 
p <- ggplot(mapping = aes(x = as.yearmon(date), 
                          y = cum_goals, 
                          group = player), 
            data = game_goals_viz)
p1 <- p + geom_line() +
  theme_minimal() + 
  labs(title = "He Shoots, He Scores!",
       subtitle = "",
       x = "",
       y = "Cumulative number of goals",
       caption = "Only 5 NHL players shot at least 5,000 times. \n The black lines highlight the goals scored \n by Gretzky, Gartner, Shanahan, Jagr \n and Ovechkin after they hit the 5,000 mark.") +
  gghighlight(cum_shots > 5000 & cum_goals > 600, 
              use_direct_label = TRUE, 
              label_params = list(size = 2.5)) +
  theme(plot.caption = element_text(hjust = 0.5, 
                                    size = 12),
        plot.title = element_text(hjust = 0.5, 
                                  size = 15))
p1

# save plot using  ggsave(filename = "name.png", plot = last_plot(), width = 10, height = 8)

