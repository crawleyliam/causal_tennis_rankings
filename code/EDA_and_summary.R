# EDA / summary stats
library(xtable)

# TODO: how often do upsets happen (maybe do by surface/level/etc)
# want to compare 
# rank (points) upsets
pts_upset_pct <- combined_odds_final %>% 
  count(rank_upset) %>% 
  filter(!is.na(rank_upset)) %>% 
  pivot_wider(names_from = "rank_upset", values_from = "n") %>% 
  rename(favored_win = `0`, underdog_win = `1`) %>% 
  mutate(upset_pct = underdog_win / (underdog_win + favored_win) * 100)
pts_upset_pct
xtable(pts_upset_pct)

pts_upset_pct_by_series <- combined_odds_final %>% 
  count(rank_upset, Series) %>% 
  filter(!is.na(rank_upset)) %>% 
  pivot_wider(names_from = "rank_upset", values_from = "n") %>% 
  rename(favored_win = `0`, underdog_win = `1`) %>% 
  mutate(upset_pct = underdog_win / (underdog_win + favored_win) * 100)
xtable(pts_upset_pct_by_series)

pts_upset_pct_by_surface <- combined_odds_final %>% 
  count(rank_upset, Surface) %>% 
  filter(!is.na(rank_upset)) %>% 
  pivot_wider(names_from = "rank_upset", values_from = "n") %>% 
  rename(favored_win = `0`, underdog_win = `1`) %>% 
  mutate(upset_pct = underdog_win / (underdog_win + favored_win) * 100)
xtable(pts_upset_pct_by_surface)


# odds upsets
odds_upset_pct <- combined_odds_final %>% 
  count(odds_upset) %>% 
  filter(!is.na(odds_upset)) %>% 
  pivot_wider(names_from = "odds_upset", values_from = "n") %>% 
  rename(favored_win = `0`, underdog_win = `1`) %>% 
  mutate(upset_pct = underdog_win / (underdog_win + favored_win) * 100)
odds_upset_pct
xtable(odds_upset_pct)

odds_upset_pct_by_series <- combined_odds_final %>% 
  count(odds_upset, Series) %>% 
  filter(!is.na(odds_upset)) %>% 
  pivot_wider(names_from = "odds_upset", values_from = "n") %>% 
  rename(favored_win = `0`, underdog_win = `1`) %>% 
  mutate(upset_pct = underdog_win / (underdog_win + favored_win) * 100)
odds_upset_pct_by_series
xtable(odds_upset_pct_by_series)

odds_upset_pct_by_surface <- combined_odds_final %>% 
  count(rank_upset, Surface) %>% 
  filter(!is.na(rank_upset)) %>% 
  pivot_wider(names_from = "rank_upset", values_from = "n") %>% 
  rename(favored_win = `0`, underdog_win = `1`) %>% 
  mutate(upset_pct = underdog_win / (underdog_win + favored_win) * 100)
xtable(odds_upset_pct_by_surface)

# 
# TODO: "summary stats of key variables"

# TODO: histo of points distribution
combined_odds_final %>%  
  ggplot(aes(WPts, LPts)) +
  geom_point(alpha = .1) +
  labs(title = "Player's ATP Points distribution",
       subtitle = "Typical match played is at around 1000 points each",
       x = "Winner's Points",
       y = "Loser's Points")
ggsave("figures/points_scatter.png", height = 6, width = 9, units = c("in"))
  # TODO: scatter of different betting sites odds
  