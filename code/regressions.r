
librarian::shelf(here, tidyverse, readxl, ivprobit, stargazer, modelsummary, ivmodel, ivreg, xtable)

combined_odds <- read_csv(here("data/combined_odds.csv"))

##Models##

##probit model, no instruments, upsets based on rank
probitMod_pts1 <- glm(pts_upset ~ fu_pt_ratio + Series + Round,
                 data=combined_odds, family=binomial(link="probit"))

summary(probitMod_pts1)
stargazer(probitMod_pts1)

probitMod_pts2 <- glm(pts_upset ~ fu_odds_ratio + fu_pt_ratio+Series + Round,
                     data=combined_odds, family=binomial(link="probit"))

summary(probitMod_pts2)
stargazer(probitMod_pts2)

##First stage iv probit, using favored (average) odds as an instrument for favorite/underdog points ratio
#dependent var = upset based on ranking point spread

ivProbitMod_1 <- ivprobit(pts_upset~
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            Round_1stRound+Round_2ndRound+Round_3rdRound+Round_4thRound+Round_Quarterfinals+Round_Semifinals+Round_Finals|
                            fu_pt_ratio|
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            Round_1stRound+Round_2ndRound+Round_3rdRound+Round_4thRound+Round_Quarterfinals+Round_Semifinals+Round_Finals+
                            favored_odds,
                          data=combined_odds)
ivProbit1_sum <- summary(ivProbitMod_1, diagnostics=TRUE)
xtable(ivProbit1_sum)



ivProbitMod_2 <- ivprobit(pts_upset~
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            Round_1stRound+Round_2ndRound+Round_3rdRound+Round_4thRound+Round_Quarterfinals+Round_Semifinals+Round_Finals|
                            fu_pt_ratio|
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            Round_1stRound+Round_2ndRound+Round_3rdRound+Round_4thRound+Round_Quarterfinals+Round_Semifinals+Round_Finals+
                            favored_odds+underdog_odds,
                          data=combined_odds)
ivProbit2_sum <- summary(ivProbitMod_2, diagnostics=TRUE)
xtable(ivProbit2_sum)




