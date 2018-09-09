
#Import libraries
library(ggplot2) # Data visualization
library(RSQLite)
library(dplyr)

con <- dbConnect(SQLite(),"C:/Users/am16222/Documents/European Soccer Database/data/database.sqlite")

#Functions
num_row <- function(d) {
  n <- nrow(d)
  if(!is.na(n)) {
    return(n)
  }
  d %>% 
    ungroup() %>%
    transmute(constant = 1.0) %>% 
    summarize(tot = sum(constant,na.rm=T)) %>%
    pull()
}


################################################################ 
# Read datasets from .sqlite database

country <- con %>% 
  tbl('Country') %>% 
  collect()

match <- con %>% 
  tbl('Match') %>% 
  collect()

league <- con %>% 
  tbl('League') %>% 
  collect()

team <- con %>% 
  tbl('Team') %>% 
  collect()

player <- con %>% 
  tbl('Player') %>% 
  collect()

################################################################ 

# Feature types and distributions
str(match)

unique(match$season)
# Data for 8 seasons

# League winners

league_ids <- league$id
league_names <- league$name

seasonwise_points <- list()

for(i in seq_along(league_ids)){
  lid <- league_ids[i]
  lname <- league_names[i]
  seasonwise_points[[lname]] <- 
    match %>% 
    filter(league_id == lid) %>%
    group_by(home_team_api_id, season) %>%
    summarize(home_wins = sum(home_team_goal > away_team_goal), 
              home_losses = sum(home_team_goal < away_team_goal),
              home_draws = sum(home_team_goal == away_team_goal),
              home_goals_for = sum(home_team_goal), 
              home_goals_against = sum(away_team_goal)) %>%
    rename(team_api_id = home_team_api_id) %>%
    left_join(
      match %>% 
        filter(league_id == lid) %>%
        group_by(away_team_api_id, season) %>%
        summarize(away_wins = sum(home_team_goal < away_team_goal),
                  away_losses = sum(home_team_goal > away_team_goal),
                  away_draws = sum(home_team_goal == away_team_goal),
                  away_goals_for = sum(away_team_goal), 
                  away_goals_against = sum(home_team_goal)) %>%
        rename(team_api_id = away_team_api_id), by = c('team_api_id'='team_api_id','season'='season')
    ) %>%
    mutate(points = 3*(home_wins + away_wins) + 1*(home_draws + away_draws),
           GF = home_goals_for + away_goals_for,
           GA = home_goals_against + away_goals_against) %>%
    mutate(GD = GF - GA) %>%
    mutate(league = league_names[i])
}

seasonwise_points = do.call(rbind, seasonwise_points)

# Join team names
seasonwise_points <- seasonwise_points %>% 
  left_join(team %>% select(team_api_id, team_long_name, team_short_name), 
            by = 'team_api_id') 

# Winners by season
seasonwise_points %>%
  group_by(season, league) %>%
  filter(points == max(points)) %>%
  filter(GD == max(GD)) %>%
  select(season, team_long_name, points, GD) %>%
  arrange(desc(season))


#Points distribution in different leagues
gg <- seasonwise_points %>%
  filter(league %in% c("England Premier League","France Ligue 1","Germany 1. Bundesliga", "Italy Serie A","Spain LIGA BBVA")) %>%
  select(team_api_id,season, league, points) %>%
  ggplot(aes(points, fill = league, colour = league)) +
    geom_density(aes(alpha = 0.4, frame = season, ids = team_api_id)) +
    xlim(15, 105)


p <- ggplotly(gg)

p <- p %>% 
  animation_opts(
    2000, easing = "linear-out", redraw = TRUE
  )

p

# English premier league team performances
epl_team_points <- seasonwise_points %>%
  filter(league == "England Premier League") %>%
  filter(team_short_name %in% c('ARS','MUN','CHE','LIV','MCI'))

temp <- reshape2::dcast(epl_team_points,formula = season ~ team_short_name, value.var = 'points')
temp$seasonid = seq(1,nrow(temp))

plot_ly(temp, x = ~seasonid) %>%
  add_trace(y = ~ARS, name = 'ARS',mode = 'scatter') %>%
  add_trace(y = ~CHE, name = 'CHE',mode = 'scatter') %>%
  add_trace(y = ~LIV, name = 'LIV', mode = 'scatter') %>%
  add_trace(y = ~MCI, name = 'MCI', mode = 'scatter') %>%
  add_trace(y = ~MUN, name = 'MUN', mode = 'scatter')



