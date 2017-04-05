source('helper_functions.R')

scen1 <- Scenario$new(prior = c(0.25, 0.25, 0.5))

evid1 <- Evidence$new(odds = c(0.9, 0.9, 0.8),
                      timeStamp = now() + days(3))

evid2 <- Evidence$new(odds = c(0.9, 0.9, 0.9),
                      timeStamp = now() + days(4))

evid3 <- Evidence$new(odds = c(0.6, 0.2, 0.1),
                      timeStamp = now() + days(6))

scen1$addEvidence(evid1)
scen1$addEvidence(evid2)
scen1$addEvidence(evid3)