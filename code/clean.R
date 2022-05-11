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
  # drop everything that's not a completed match
  filter(Comment == "Completed") %>% 
  rename(best_of = `Best of`) %>% 
  mutate(Series = recode(Series,
                         "Grand Slam" = "GrandSlam",
                         "Masters 1000" = "Masters1000",
                         "Masters Cup" = "MastersCup"))
  
  
combined_odds <- combined_odds_clean %>% 
  # rename avg odds
  rename(avg_winner_odds = AvgW,
         avg_loser_odds = AvgL) %>% 
  
# create upset columns
  mutate(rank_upset = if_else(LRank < WRank, 1, 0), # "lower" rank is better
         pts_upset = if_else(LPts > WPts, 1, 0), 
         odds_upset = if_else(avg_loser_odds < avg_winner_odds, 1, 0)) %>% 
# favored minus underdog point difference and odds 
  rowwise %>% 
  mutate(favored_pts = max(WPts, LPts),
         underdog_pts = min(WPts, LPts),
         fu_pt_difference = favored_pts - underdog_pts,
         fu_pt_ratio = favored_pts / underdog_pts, 
         favored_odds = min(avg_winner_odds, avg_loser_odds),
         underdog_odds = max(avg_winner_odds, avg_loser_odds),
         fu_odds_difference = favored_odds - underdog_odds,
         fu_odds_ratio = favored_odds / underdog_odds)
glimpse(combined_odds)

# TODO: create dummies for series and surface

combined_odds_final <- combined_odds %>% 
  fastDummies::dummy_cols(select_columns = "Series") %>% 
  fastDummies::dummy_cols(select_columns = "Surface")

glimpse(combined_odds_final)
#write out data
write_csv(combined_odds_final, "data/combined_odds.csv")

