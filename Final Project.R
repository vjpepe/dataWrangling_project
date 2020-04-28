library(curl)
library(tidytext)
library(rvest)
library(tidyverse)
library(tidymodels)
library(caret)
library(earth)
library(faraway)

setwd("C:/Users/Vince/Documents/Vincent Work/Rutgers/data wrangling/Final Project")

hockey_ref <- read.csv("hockey_reference_teams.csv") %>% 
  drop_na() %>% 
  rename("rank" = ï..Rk,
         "PTS.percent" = PTS., 
         "Simp.Rating.Sys" = SRS,
         "Strength.Schedule" = SOS,
         "Power.Play.percent" = PP.,
         "Pen.Kill.percent" = PK.,
         "Short.Hand.G" = SH,
         "Short.hand.GA" = SHA,
         "Shots" = S,
         "Shot.percent" = S.,
         "ShutOuts" = SO)

# new_df <- hockey_ref %>% 
#   select(-c(W, L, OL, PTS.percent, Team))
# 
# ne3w <- glm(PTS ~ ., data = new_df)  
# summary(ne3w)

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

# website had incorrect number of points on it, just hardcode edited the number
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

hockey_eh <- left_join(hockey_ref, puckOn, by = c("rank", "GP", "PTS"))

hockey_eh <- hockey_eh %>%
  select(-c(Team.abv, AvAge)) %>% 
  .[, -1]

# hockey_standard <- hockey_eh
#  
# hockey_standard[, 2:43] <- scale(hockey_eh[, 2:43], center = T, scale = T)
# 
# hockey_train.index <- hockey_eh %>% 
#   initial_split()
# 
# hockey.train <- training(hockey_train.index)
# hockey.test <- testing(hockey_train.index)


# build model
(train.earth <- earth(PTS~., data = new_df))

# estimate variable importance
(ev1 <- evimp (train.earth))

# Removing features not needed based on variable importance
# qb.train <- qb.train[ -c(1,3:5,7) ]
# qb.test <- qb.test[ -c(1,3:5,7) ]
# 
# 
# test <- hockey_standard[, -1]
# 
# whatever <- glm(PTS ~., data = test)
# summary(whatever)
# 
# cor(hockey_eh$PTS, hockey_eh[,2:43], method = c("pearson"))
# 
# vif(whatever)
