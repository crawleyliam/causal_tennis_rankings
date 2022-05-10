# data cleaning
library(tidyverse)
library(lubridate)
library(tsibble)
library(readxl)
library(glue)

# combine data
data_file_list <- list("data/atp_raw/2011.xls",
                       "data/atp_raw/2012.xls",
                       "data/atp_raw/2013.xlsx",
                       "data/atp_raw/2014.xlsx",
                       "data/atp_raw/2014.xlsx",
                       "data/atp_raw/2015.xlsx",
                       "data/atp_raw/2016.xlsx",
                       "data/atp_raw/2017.xlsx",
                       "data/atp_raw/2018(2).xlsx",
                       "data/atp_raw/2019.xlsx",
                       "data/atp_raw/2020.xlsx",
                       "data/atp_raw/2021.xlsx")

combined_odds_raw <- data_file_list %>% 
  map_dfr(read_excel, na = c("", "N/A"))

# clean data
combined_odds_clean <- combined_odds_raw %>% 
  # TODO: drop everything that's not a completed match
  filter(Comment == "Completed")
  
  
combined_odds <- combined_odds_clean %>% 
# TODO: convert odds to probabilities for each of 
  mutate(B365W_prob = B365W / (B365W + B365L),
         B365L_prob = B365L / (B365W + B365L)) %>% 
# TODO: create upset columns
  mutate(rank_upset = if_else(LRank < WRank, 1, 0), # "lower" rank is better
         pts_upset = if_else(LPts > WPts, 1, 0), 
         B365_upset = if_else(B365L_prob > B365W_prob, 1, 0)) %>% 

# TODO: winner minus loser point difference and probs (upsets will be negative)
  mutate(WL_pt_diff = WPts - LPts,
         WL_odds_diff = B365W_prob - B365L_prob) %>% 
# TODO: favored minus underdog point difference and probs 
  rowwise %>% 
  mutate(favored_pts = max(WPts, LPts),
         underdog_pts = min(WPts, LPts),
         fu_pt_difference = favored_pts - underdog_pts,
         fu_pt_ratio = favored_pts / underdog_pts, 
         favored_odds = max(B365W_prob, B365L_prob),
         underdog_odds = min(B365W_prob, B365L_prob),
         fu_odds_difference = favored_odds - underdog_odds,
         fu_odds_ratio = favored_odds / underdog_odds)
glimpse(combined_odds)

#write out data
write_csv(combined_odds, "data/combined_odds.csv")

