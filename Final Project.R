library(curl)
library(tidytext)
library(rvest)
library(tidyverse)
library(tidymodels)
library(caret)
library(earth)
library(faraway)
library(doParallel)
library(vip)

setwd("C:/Users/Vince/Documents/Vincent Work/Rutgers/data wrangling/Final Project")

hockey_ref <- read.csv("hockey_reference_teams.csv") %>% 
  tibble() %>%
  drop_na() %>%
  mutate(Team = as.character(Team)) %>% 
  rename("rank" = ï..Rk,
         "PTS.percent" = PTS., 
         "Simp.Rating.Sys" = SRS,
         "Strength.Schedule" = SOS,
         "Power.Play.percent" = PP.,
         "Pen.Kill.percent" = PK.,
         "Short.Hand.G" = SH,
         "Short.Hand.GA" = SHA,
         "Shots" = S,
         "Shot.percent" = S.,
         "ShutOuts" = SO)

# made all variables class numeric
hockey_ref[, 3:33] <- lapply(hockey_ref[, 3:33], as.numeric)

# Some tests I ran to see if removing certain variables would change anything on first dataset (still commented out as I dont want it to affect the dataset going forward)

# new_df <- hockey_ref %>%
#   select(-c(W, L, OL, PTS.percent, Team))
# 
# tester <- hockey_ref %>% 
#   select(-Team)
# 
# ne3w <- lm(PTS ~ ., data = tester)
# summary(ne3w)

# second dataset
puckOn <- "http://www.puckon.net/" %>% 
  read_html() %>% 
  html_node("#dataTable") %>% 
  html_table() %>% 
  .[-33, ]


colnames(puckOn) <- c("Team.abv", "GP", "PTS", "C.SA", "C.SVA", "C.ESVA", "C.Close",
                      "F.SA", "F.SVA", "F.ESVA", "F.Close", "SOG.SA", "SOG.SVA", "SOG.ESVA", "SOG.Close")
puckOn <- puckOn[-1,]

puckOn[, 2:15] <- lapply(puckOn[, 2:15], as.numeric)


puckOn <- puckOn %>% 
  group_by(PTS, GP) %>% 
  arrange(-PTS, -GP)

# website had incorrect number of points on it, just hardcoded the correct number
puckOn[2,3] <- 94

puckOn$rank = 1:31

inter <- puckOn %>% 
  filter(rank %in% c(16:20)) %>% 
  mutate(rank = case_when(Team.abv == "NYR" ~ 16, 
                          Team.abv == "CGY" ~ 17, 
                          Team.abv == "VAN" ~ 18, 
                          Team.abv == "FLA" ~ 19, 
                          Team.abv == "NSH" ~ 20))

puckOn[16:20, ] <- inter

puckOn <- puckOn %>% 
  arrange(rank)

hockey_ref <- hockey_ref %>% 
  arrange(rank)

# merge to create full dataset
hackey <- left_join(hockey_ref, puckOn, by = c("rank", "GP", "PTS"))

hackey <- hackey %>%
  select(-c(Team.abv, AvAge)) %>% 
  .[, -1]

# *Here you can see me remove the Team variable (needs to be in model) and testing using an lm, as well as computing vif on both regular and standardized datsets*
# hackey <- hackey[, -1]
# test_lm <- lm(W ~., data = hackey)
# summary(test_lm)
# 
# lapply(hackey, class)
# 
# vif(hackey)


hockey_standard <- hackey

hockey_standard[, 2:43] <- scale(hackey[, 2:43], center = T, scale = T)

hockey_standard <- hockey_standard %>% 
  select(-c(GP, PTS, OL, PTS.percent, SOW, SOL, EVGF, EVGA, Power.Play.percent,
            PPA, PPOA, Pen.Kill.percent, Short.Hand.G, Short.Hand.GA, PIM.G,
            oPIM.G, ShutOuts))


# test and train

hockey_train.index <- hockey_standard %>%
  initial_split()

hockey.train <- training(hockey_train.index)
hockey.test <- testing(hockey_train.index)

# fullmodel <- lm(PTS ~ ., data = hockey.train)
# summary(fullmodel)
# 
# hybridselect <- step(fullmodel, scope = list(lower = ~ 1, upper = fullmodel),
#                      direction = "both", trace = FALSE, na.rm = T)
# summary(hybridselect)





# # build model
# (train.earth <- earth(PTS~., data = new_df))
# 
# # estimate variable importance
# (ev1 <- evimp (train.earth))
# 
# # Removing features not needed based on variable importance
# qb.train <- qb.train[ -c(1,3:5,7) ]
# qb.test <- qb.test[ -c(1,3:5,7) ]


hockey_recipe <- recipe(W ~., data = hockey.train) %>% 
  update_role(Team, new_role = "ID") %>% 
  step_zv(all_numeric(), -all_outcomes())


hockey_prep <- hockey_recipe %>% 
  prep(strings_as_factors = FALSE)

lasso_spec <- linear_reg(penalty = .10, mixture = 1) %>% # mixture = 1 means lasso 
  set_engine("glmnet")


wf <- workflow() %>% 
  add_recipe(hockey_recipe)


lasso_fit <- wf %>% 
  add_model(lasso_spec) %>% 
  fit(data = hockey.train)

lasso_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

## Tune LASSO Parameters

set.seed(3142)
hockey_boot <- bootstraps(hockey.train)

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

?penalty # check for regularization

lambda_grid <- grid_regular(penalty(),
             levels = 50)

doParallel::registerDoParallel()

set.seed(4231)
lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = hockey_boot,
  grid = lambda_grid
)

lasso_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(show.legend = F) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lowest_rmse <- lasso_grid %>% 
  select_best("rmse", maximize = F)

final_lasso <- finalize_workflow(wf %>% add_model(tune_spec),
                  lowest_rmse)

final_lasso %>% 
  fit(hockey.train) %>% 
  pull_workflow_fit() %>% 
  vip::vi(lambda = lowest_rmse$penalty) %>% 
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "Variable Importance of Lasso Regression", y = NULL)


last_fit(final_lasso,
        hockey_train.index) %>% 
  collect_metrics()
