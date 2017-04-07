library(R6)
library(lubridate)
library(zoo)

Scenario <- R6Class("Scenario",
                    public = list(
                      description = NULL,
                      hypotheses = NULL,
                      startDate = NULL,
                      prior = NULL,
                      evidence = NULL,
                      initialize = function(description = NA,
                                            hypotheses = NA,
                                            startDate = now(),
                                            prior = NA,
                                            evidence = list()) {
                        self$description <- description
                        self$hypotheses <- hypotheses
                        self$startDate <- startDate
                        self$prior <- prior
                        self$evidence <- evidence
                      },
                      addEvidence = function(newEvidence) {
                        self$evidence <- c(self$evidence, newEvidence)
                      }
                    ))


Evidence <- R6Class("Evidence",
                    public = list(
                      description = NULL,
                      timeStamp = NULL,
                      odds = NULL,
                      initialize = function(description = NA,
                                            timeStamp = now(),
                                            odds = 1) {
                        self$description <- description
                        self$timeStamp <- timeStamp
                        self$odds <- odds
                      },
                      oddsMC = function(n = 10000) {
                        if(length(self$odds) == 1) return(self$odds)
                        return(runif(n,
                                     min = min(self$odds),
                                     max = max(self$odds)
                                     ))
                      }
                    ))

scenario_indicies <- function(S) {
  c(S$startDate,
      do.call("c", (lapply(S$evidence, function(x) x$timeStamp)))
    )
}

scenario_prob_ts <- function(S) {
  Index <- scenario_indicies(S)
  evidenceProb <- t(sapply(S$evidence, function(x) x$odds))
  Prob <- rbind(S$prior, evidenceProb)
  baseTS <- zoo(Prob, order.by = Index)
  names(baseTS) <- LETTERS[1:length(S$prior)]
  return(baseTS)
}

scenario_prob_evolution <- function(S) {
  Index <- scenario_indicies(S)
  probts <- scenario_prob_ts(S)
  probMat <- matrix(probts, ncol = length(S$prior))
  cumProbMat <- apply(probMat, 2, cumprod)
  sumVec <- rowSums(cumProbMat)
  zoo(cumProbMat / sumVec, Index)
}

sample_scenario <- function(S) {
  lapply(S$evidence, function(y) sapply(y$odds, sample, size = 1))
}

simulate_scenario <- function(S, n = 100) {
  Hypos <- S$hypotheses
  Priors <- S$prior
  simulateRealizations <- lapply(1:n, function(x) sample_scenario(S))
  allSims <- lapply(simulateRealizations, simulate_engine, P = Priors)
  matrix(unlist(allSims), nrow = length(allSims[[1]]))
}

simulate_engine <- function(R, P) {
  probMat <- rbind(P, matrix(unlist(R), ncol = 3) %>% t)
  cumProbMat <- apply(probMat, 2, cumprod)
  sumVec <- rowSums(cumProbMat)
  (cumProbMat / sumVec)[nrow(probMat),]
}