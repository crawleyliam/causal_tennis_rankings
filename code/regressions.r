
librarian::shelf(here, tidyverse, readxl, ivprobit, stargazer, modelsummary, ivmodel)

combined_odds <- read_csv(here("data/combined_odds.csv"))
##Models##

##probit model, no instruments, upsets based on rank
probitMod_pts <- glm(pts_upset ~ fu_pt_difference + Surface + Series,
                 data=combined_odds, family=binomial(link="probit"))

summary(probitMod_pts)

##probit model, no instruments, upsets based on betting odds
probitMod_B365 <- glm(B365_upset ~ fu_odds_difference + Surface + Series,
                      data=combined_odds, family=binomial(link="probit"))

summary(probitMod_B365)

##first stage iv probit, using Bet365 as instrument

ivprobitMod_1 <- ivprobit(pts_upset ~ Surface + Series | 
                            fu_pt_difference | Surface + Series + fu_odds_difference,
                          data=combined_odds )
summary(probitMod)