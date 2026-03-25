library(bayesrules)
library(tidyverse)
library(janitor)

rm(list = ls())

#' Notes:
#' pi denotes the unknown proportion of recent movies that pass the Bechdel test
#'
#' Y denotes the number of n movies that pass the Bechdel test
#' 
#' Informative (less variance) vs. Vague (more variance) prior 
#' 
#' Likehood functions are not pdf's
#' 
#' Regardless of differences between priors, folks will come to the same 
#' posterior conclusion in light of strong data
#' 
#' The posterior mean is a weighted average between the prior mean and the 
#' sample success rate

#' Example: Bechdel Test
data(bechdel, package = 'bayesrules')
set.seed(84735)
bechdel_20 = bechdel %>%
  sample_n(20)

bechdel_20 %>%
  tabyl(binary) %>%
  adorn_totals('row')

#' Morteza
bechdel %>%
  filter(year == 1991) %>%
  tabyl(binary) %>%
  adorn_totals('row')

#' Nadide
bechdel %>%
  filter(year == 2000) %>%
  tabyl(binary) %>%
  adorn_totals('row')

#' Ursula
bechdel %>%
  filter(year == 2013) %>%
  tabyl(binary) %>%
  adorn_totals('row')
