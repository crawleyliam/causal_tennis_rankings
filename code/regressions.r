
librarian::shelf(here, tidyverse, readxl, ivprobit, stargazer, modelsummary, ivmodel, ivreg, xtable)

combined_odds <- read_csv(here("data/combined_odds.csv"))

##Models##

##probit model, no instruments, upsets based on rank
probitMod_pts <- glm(pts_upset ~ fu_pt_ratio + Series + Round,
                 data=combined_odds, family=binomial(link="probit"))

summary(probitMod_pts)
stargazer(probitMod_pts)

##probit model, no instruments, upsets based on betting odds
#probitMod_odds <- glm(odds_upset ~ fu_odds_difference + fu_pt_difference +Surface + Series + Round,
#                      data=combined_odds, family=binomial(link="probit"))

#summary(probitMod_odds)
#stargazer(probitMod_odds)


##first stage iv probit, using Bet365 odds as an instrument for favorite/underdog points ratio
#dependent var = upset based on ranking point spread

ivProbitMod_1 <- ivprobit(pts_upset~
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup|
                            fu_pt_ratio|
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            fu_odds_difference,
                          data=combined_odds)
ivProbit1_sum <- summary(ivProbitMod_1, diagnostics=TRUE)
xtable(ivProbit1_sum)
##Model verification



##first stage iv probit, using favorite/underdog points ratio as an instrument for favorite/underdog points ratio
ivProbitMod_2 <- ivprobit(pts_upset~
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup|
                            fu_odds_difference|
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            fu_pt_difference,
                          data=combined_odds)
ivProbit2_sum <- summary(ivProbitMod_2, diagnostics=TRUE)
xtable(ivProbit2_sum)

