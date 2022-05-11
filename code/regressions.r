
librarian::shelf(here, tidyverse, readxl, ivprobit, stargazer, modelsummary, ivmodel)

combined_odds <- read_csv(here("data/combined_odds.csv"))
##Models##

##probit model, no instruments, upsets based on rank
probitMod_pts <- glm(pts_upset ~ fu_pt_difference + Series + Round,
                 data=combined_odds, family=binomial(link="probit"))

summary(probitMod_pts)
stargazer(probitMod_pts)

##probit model, no instruments, upsets based on betting odds
probitMod_B365 <- glm(B365_upset ~ fu_odds_difference + Surface + Series,
                      data=combined_odds, family=binomial(link="probit"))

summary(probitMod_B365)


##first stage iv probit, using Bet365 odds as an instrument for favorite/underdog points ratio
#dependent var = upset based on ranking point spread

ivProbitMod_1 <- ivprobit(pts_upset~
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup|
                            fu_pt_ratio|
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            fu_odds_difference,
                          data=combined_odds)
summary(ivProbitMod_1)
stargazer(ivProbitMod_1)


##first stage iv probit, using favorite/underdog points ratio as an instrument for favorite/underdog points ratio
ivProbitMod_2 <- ivprobit(pts_upset~
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup|
                            fu_odds_difference|
                            Series_ATP500+Series_Masters1000+Series_GrandSlam+Series_MastersCup+
                            fu_pt_difference,
                          data=combined_odds)
summary(ivProbitMod_2)
stargazer(ivProbitMod_2)

