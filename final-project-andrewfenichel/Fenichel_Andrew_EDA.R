### Andrew Fenichel Data Science 3 Final Project EDA

#Install packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#install.packages('Hmisc')

#Load Packages  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(janitor)

#Set the seed  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(3739)

#Load data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shot_logs_2015 <- read_csv("data/unprocessed/shot_logs.csv") %>%
  clean_names()
players_dat <- read_csv("data/unprocessed/players.csv") %>%
  clean_names()
defense_dat <- read_csv("data/unprocessed/NBA Season Data.csv") %>%
  clean_names()

#Data Wrangling  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shot_logs_2015_updated <- shot_logs_2015 %>%
  select(c(final_margin, shot_number, period, game_clock, shot_clock, 
           dribbles, touch_time, shot_dist, pts_type, shot_result, 
           closest_defender, close_def_dist, fgm, pts, player_name))

shot_logs_2015_updated <- shot_logs_2015_updated %>%
  mutate(closest_defender = sub("(\\w+),\\s(\\w+)","\\2 \\1", shot_logs_2015_updated$closest_defender))

players_dat <- players_dat %>%
  filter(active_to >= 2015) %>%
  select(c(height, name, position, weight, nba_3ptpct, 
           nba_efgpct, nba_fg_percent, nba_ppg)) %>%
  rename(c("player_name" = "name")) %>%
  mutate(player_name = tolower(player_name))

defense_dat <- defense_dat %>%
  filter(year == 2015) %>%
  select(c(player, per, stl_percent, blk_percent, dws, dws_48, dbpm, defense)) %>%
  rename(c("closest_defender" = "player"))

defense_dat <- defense_dat %>%
  group_by(closest_defender) %>% 
  transmute(
    per = mean(per),
    stl_percent = mean(stl_percent),
    blk_percent = mean(blk_percent),
    dws = sum(dws),
    dws_48 = mean(dws_48),
    dbpm = mean(dbpm),
    defense = mean(defense)
    ) %>%
  distinct(closest_defender,.keep_all = TRUE)

#Data Set Merging ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nba_2015_total_dat <- merge(shot_logs_2015_updated, players_dat, by = "player_name")
nba_2015_total_dat <- merge(nba_2015_total_dat, defense_dat, by = "closest_defender")

#Data Skimming  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
skimr::skim_without_charts(nba_2015_total_dat)
summary(nba_2015_total_dat)

###Graphing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Proving positive relationships between defensive variables
nba_2015_total_dat %>%
  ggplot(aes(dbpm, defense, color = dws_48)) +
  geom_point()

nba_2015_total_dat %>%
  ggplot(aes(dbpm, dws)) +
  geom_point()

nba_2015_total_dat %>%
  ggplot(aes(dws, dws_48)) +
  geom_point()

#Exploring response variable (fgm)
#Corrplot
cor_dat <- nba_2015_total_dat %>%
  select_if(is.numeric)
r <- cor(cor_dat, use = "complete.obs")
round(r, 2)
corrplot::corrplot(r)

#Other graphs
nba_2015_total_dat %>%
  ggplot(aes(nba_3ptpct, nba_fg_percent)) +
  geom_point()

nba_2015_total_dat %>%
  ggplot(aes(shot_dist)) +
  geom_bar()

nba_2015_total_dat %>%
  ggplot(aes(shot_dist)) +
  geom_area()

nba_2015_total_dat %>%
  ggplot(aes(weight, position)) +
  geom_boxplot()

nba_2015_total_dat %>%
  ggplot(aes(height, position)) +
  geom_boxplot()

nba_2015_total_dat %>%
  ggplot(aes(shot_number, nba_efgpct)) +
  geom_line()



