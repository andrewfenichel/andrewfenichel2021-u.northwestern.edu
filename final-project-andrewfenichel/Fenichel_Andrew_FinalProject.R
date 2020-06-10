###Data Science 301-3 Final Project

#Load Packages ---------------------------------------------------------------------------------
library(tidyverse)
library(skimr)
library(janitor)
library(rsample)
library(GGally)
library(glmnet)
library(modelr)
library(ranger)
library(vip)
library(pdp)
library(xgboost)
library(MASS)
library(tidyselect)

#Set the seed ----------------------------------------------------------------------------------
set.seed(3739)

#Load Data -------------------------------------------------------------------------------------
shot_logs_2015 <- read_csv("data/unprocessed/shot_logs.csv") %>%
  clean_names()
players_dat <- read_csv("data/unprocessed/players.csv") %>%
  clean_names()
defense_dat <- read_csv("data/unprocessed/NBA Season Data.csv") %>%
  clean_names()

#Data Wrangling -------------------------------------------------------------------------------
shot_logs_2015_updated <- shot_logs_2015 %>%
  dplyr::select(c(final_margin, shot_number, period, game_clock, shot_clock, 
           dribbles, touch_time, shot_dist, pts_type, shot_result, 
           closest_defender, close_def_dist, fgm, pts, player_name))

shot_logs_2015_updated <- shot_logs_2015_updated %>%
  mutate(closest_defender = sub("(\\w+),\\s(\\w+)","\\2 \\1", shot_logs_2015_updated$closest_defender))

players_dat <- players_dat %>%
  filter(active_to >= 2015) %>%
  dplyr::select(c(height, name, position, weight, nba_3ptpct, 
           nba_efgpct, nba_fg_percent, nba_ppg)) %>%
  rename(c("player_name" = "name")) %>%
  mutate(player_name = tolower(player_name))

defense_dat <- defense_dat %>%
  filter(year == 2015) %>%
  dplyr::select(c(player, per, stl_percent, blk_percent, dws, dws_48, dbpm, defense)) %>%
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

#Data Set Merging ------------------------------------------------------------------------------
nba_2015_total_dat <- merge(shot_logs_2015_updated, players_dat, by = "player_name")
nba_2015_total_dat <- merge(nba_2015_total_dat, defense_dat, by = "closest_defender")

nba_2015_total_dat <- nba_2015_total_dat %>%
  na.omit(players_dat, na.action = "omit")

# sum(is.na(nba_2015_total_dat))
# 
# write_csv(nba_2015_total_dat, path = "data/processed")

nba_model_dat <- nba_2015_total_dat %>%
  seplyr::deselect(c("player_name", "shot_result", "closest_defender", "pts", "position", "final_margin", "id"))

nba_model_dat$height <- as_factor(nba_model_dat$height)

nba_model_dat$game_clock <- as.numeric(nba_model_dat$game_clock)

nba_model_dat %>%
  skim_without_charts()

#Data Splitting --------------------------------------------------------------------------------
nba_model_dat$id <- 1:nrow(nba_model_dat)
train <- nba_model_dat %>% sample_frac(.75)
test  <- anti_join(nba_model_dat, train, by = 'id')

nba_dat_split <- tibble(
  train = train %>% list(),
  test = test %>% list()
)

#Modeling --------------------------------------------------------------------------------------

#Simple linear modeling
lm_fit_1 <- nba_model_dat %>% lm(formula = fgm ~ dribbles + close_def_dist)
lm_fit_1 %>%
  broom::glance()
modelr::mse(lm_fit_1, nba_model_dat)

#Simple logistic model
glm_fits <- nba_dat_split %>% 
  mutate(mod_01 = map(train, glm, 
                      formula = fgm ~ close_def_dist + shot_dist + touch_time + dribbles + shot_clock,
                      family = binomial))

glm_fits %>% 
  pluck("mod_01", 1) %>% 
  tidy()

glm_fits %>% 
  pluck("mod_01", 1) %>% 
  predict(type = "response") %>% 
  skim_without_charts()

demo_tib <- glm_fits %>%
  mutate(train_prob = map(mod_01, predict, type = "response"),
         train_direction = map(train_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(train_direction) %>% 
  mutate(prop = n / sum(n))

demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(fgm, train_direction) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(fgm))

demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  mutate(correct = if_else(train_direction == fgm, 1, 0)) %>% 
  summarise(train_accuracy = mean(correct),
            train_error = 1 - train_accuracy)

demo_tib <- demo_tib %>%
  mutate(test_prob = map2(mod_01, test, predict, type = "response"),
         test_direction = map(test_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib %>% 
  unnest(cols = c(test, test_direction)) %>% 
  mutate(correct = if_else(test_direction == fgm, 1, 0)) %>% 
  summarise(test_accuracy = mean(correct),
            test_error = 1 - test_accuracy)


glm_fits_2 <- nba_dat_split %>% 
  mutate(mod_01 = map(train, glm, 
                      formula = fgm ~ close_def_dist + shot_dist + touch_time + shot_clock + dbpm + nba_efgpct,
                      family = binomial))

glm_fits_2 %>% 
  pluck("mod_01", 1) %>% 
  tidy()

glm_fits_2 %>% 
  pluck("mod_01", 1) %>% 
  predict(type = "response") %>% 
  skim_without_charts()

demo_tib_2 <- glm_fits_2 %>%
  mutate(train_prob = map(mod_01, predict, type = "response"),
         train_direction = map(train_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib_2 %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(train_direction) %>% 
  mutate(prop = n / sum(n))

demo_tib_2 %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(fgm, train_direction) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(fgm))

demo_tib_2 %>% 
  unnest(cols = c(train, train_direction)) %>% 
  mutate(correct = if_else(train_direction == fgm, 1, 0)) %>% 
  summarise(train_accuracy = mean(correct),
            train_error = 1 - train_accuracy)

demo_tib_2 <- demo_tib_2 %>%
  mutate(test_prob = map2(mod_01, test, predict, type = "response"),
         test_direction = map(test_prob, ~ if_else(.x > 0.5, 1, 0)))

demo_tib_2 %>% 
  unnest(cols = c(test, test_direction)) %>% 
  mutate(correct = if_else(test_direction == fgm, 1, 0)) %>% 
  summarise(test_accuracy = mean(correct),
            test_error = 1 - test_accuracy)

#Random Forest
#Helper  functions ----------------------------------------------------------------------------
misclass_ranger <- function(model, test, outcome){
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  preds <- predict(model, test)$predictions
  misclass <- mean(test[[outcome]] != preds)
  return(misclass)
}

nba_rf_class <- nba_dat_split %>% 
  crossing(mtry = 1:(ncol(train) - 1)) %>%
  mutate(model = map2(.x = train, .y = mtry, 
                      .f = function(x, y) ranger(fgm ~ .-id,
                                                 mtry = y, 
                                                 data = x, 
                                                 splitrule = "gini",
                                                 importance = "impurity",
                                                 classification = TRUE)),
         train_misclass = map2(model, train, misclass_ranger, outcome = "fgm"),
         test_misclass = map2(model, test, misclass_ranger, outcome = "fgm"), 
         oob_misclass = map(.x = model, 
                            .f = function(x) x[["prediction.error"]])
  )

nba_rf_class %>%
  pluck("test_misclass")

ggplot(nba_rf_class) + 
  geom_line(aes(mtry, unlist(oob_misclass), color = "OOB Error")) +
  geom_line(aes(mtry, unlist(train_misclass), color = "Training Error")) +
  geom_line(aes(mtry, unlist(test_misclass), color = "Test Error")) +
  labs(x = "mtry", y = "Misclassification Rate") +
  scale_color_manual("", values = c("purple", "blue", "red")) +
  theme_bw()

nba_class_mtry5 = ranger(fgm ~ .-id,
                        data = nba_model_dat,
                        mtry = 5,
                        importance = "impurity",
                        splitrule = "gini", 
                        probability = TRUE)
vip(nba_class_mtry5)

pred_probs_rf <- predict(nba_class_mtry5, test, type = "response")
summary(pred_probs_rf)
pred_probs_rf$predictions[,2]

out_rf <- tibble(Id = test$id,
                 Category = as.character(as.integer(pred_probs_rf$predictions[,2] > .5)))
out_rf



###Boosted model
if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
  tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
  lab <- tmp[,1]
} else {
  lab <- dat[[outcome]]
}
xgb_matrix <- function(dat, outcome, exclude_vars){
  if(!is_tibble(dat)){
    dat <- as_tibble(dat)
  }
  dat_types <- dat %>% map_chr(class)
  outcome_type <- class(dat[[outcome]])
  if("character" %in% dat_types){
    print("You must encode characters as factors.")
    return(NULL)
  } else {
    if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
      tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
      lab <- tmp[,1]
    } else {
      lab <- dat[[outcome]]
    }
    mat <- dat %>% dplyr::select(-outcome, -all_of(exclude_vars)) %>%
      onehot::onehot() %>%
      predict(dat)
    return(xgb.DMatrix(data = mat, 
                       label = lab))
    
  }}

xg_error <- function(model, test_mat, metric = "mse"){
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  if(metric == "mse"){
    err <- mean((preds - vals)^2)
  } else if(metric == "misclass") {
    err <- mean(preds != vals)
  }
  return(err)
}

#Boosted model class 1
nba_xg_class <- nba_dat_split %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>%
  mutate(
    train_mat = map(train, xgb_matrix, outcome = all_of("fgm"), exclude_vars = "height"), 
    test_mat = map(test, xgb_matrix, outcome = all_of("fgm"), exclude_vars = "height"),
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y,
                                                                depth = 5,
                                                                objective = "multi:softmax",
                                                                num_class = 2),
                                                   data = x, 
                                                  nrounds = 100,
                                                  silent = TRUE)),
    xg_train_misclass = map2(xg_model, train_mat, xg_error, metric = "misclass"),
    xg_test_misclass = map2(xg_model, test_mat, xg_error, metric = "misclass") 
  )

nba_xg_class %>%
  pluck("xg_test_misclass")

ggplot(nba_xg_class) +
  geom_line(aes(learn_rate, unlist(xg_test_misclass)))

xg_class_mod <- nba_xg_class %>% 
  arrange(unlist(xg_test_misclass)) %>%
  pluck("xg_model", 1)

vip(xg_class_mod)


#Boosted model class update
nba_xg_class_6 <- nba_dat_split %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>%
  mutate(
    train_mat = map(train, xgb_matrix, outcome = all_of("fgm"), exclude_vars = "height"), 
    test_mat = map(test, xgb_matrix, outcome = all_of("fgm"), exclude_vars = "height"),
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y,
                                                                depth = 5,
                                                                objective = "multi:softmax",
                                                                num_class = 2),
                                                  data = x, 
                                                  nrounds = 50,
                                                  silent = TRUE)),
    xg_train_misclass = map2(xg_model, train_mat, xg_error, metric = "misclass"),
    xg_test_misclass = map2(xg_model, test_mat, xg_error, metric = "misclass") 
  )

nba_xg_class_6 %>%
  pluck("xg_test_misclass")

ggplot(nba_xg_class_6) +
  geom_line(aes(learn_rate, unlist(xg_test_misclass)))

xg_class_mod <- nba_xg_class_6 %>% 
  arrange(unlist(xg_test_misclass)) %>%
  pluck("xg_model", 1)

xg_class_mod

vip(xg_class_mod)
