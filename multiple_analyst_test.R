source('helper_functions.R')

scen1 <- Scenario$new(prior = c(0.25, 0.25, 0.5))

evid1 <- Evidence$new(odds = list(c(0.9, 0.8, .85), c(0.85, 0.95, .75), c(0.825, 0.85, .925)),
                      timeStamp = now() + days(3))

evid2 <- Evidence$new(odds = list(c(0.9, 0.85, .3), c(0.875, 0.925, .1), c(0.9, 0.9, .95)),
                      timeStamp = now() + days(4))

evid3 <- Evidence$new(odds = list(c(0.6, 0.55, 0.8), c(0.15, 0.3, 0.15), c(0.05, 0.15, 0.15)),
                      timeStamp = now() + days(6))

evid4 <- Evidence$new(odds = list(c(0.9, 0.95, 0.9), c(0.1, 0.2, 0.15), c(0.05, 0.15, 0.15)),
                      timeStamp = now() + days(8))

scen1$addEvidence(evid1)
scen1$addEvidence(evid2)
scen1$addEvidence(evid3)
scen1$addEvidence(evid4)
