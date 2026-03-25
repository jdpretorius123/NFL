library(bayesrules)
library(tidyverse)
library(janitor)

rm(list = ls())

#' Example: Fraud Risk Calls Received per Day - Gamma-Poisson Bayesian Model
#' 
#' Notes:
#' lambda equals the rate of fraud risk calls received per day
#'
#' Y denotes the number of fraud risk calls received in a day
#' 
#' The data model is a Poisson model
#' 
#' The prior model is a Gamma model
#'   - When s > r, the central tendency is greater than 1
#'   - When s < r, the central tendency is less than 1
#'   - As s increases relative to r, the skew in lambda decreases and the
#'     variability increases

#' Gamma(10, 2) prior
plot_gamma(shape = 10, rate = 2)

#' Over 4 days, 6, 2, 2, and 1 fraud risk call(s) were received per day
#' Sum(y) = 11 and n = 4
#' Posterior Gamma(s + Sum(y), r + n)

#' Likelihood
plot_poisson_likelihood(y = c(6, 2, 2, 1), lambda_upper_bound = 10)

#' Gamma(21, 6) posterior
plot_gamma(shape = 21, rate = 6)

#' Plotting the prior, scaled likelihood, and the posterior
plot_gamma_poisson(shape = 10, rate = 2, sum_y = 11, n = 4)
summarize_gamma_poisson(shape = 10, rate = 2, sum_y = 11, n = 4)

#' Example: The Average Hippocampal Volume (cubic cm) Among all People with a 
#'          History of Concussions
#' 
#' Notes:
#' mu is the average hippocampal volume (cubic cm) among all people with a
#' history of concussions
#' 
#' Y denotes hippocampal volume (cubic cm)
#' 
#' The prior model is a normal model
#'     - Prior mu is 6.5
#'     - Prior sigma is 0.4
#'     
#' The data model is a normal model 

#' N(6.5, (0.4)^2) prior
plot_normal(mean = 6.5, sd = 0.4)

#' Loading the data
data(football)

concussion_subjects = football %>%
  filter(group == 'fb_concuss')

concussion_subjects %>%
  summarize(mean(volume))

ggplot(data = concussion_subjects, mapping = aes(x = volume)) +
  geom_density()

#' Plotting the Likelihood
plot_normal_likelihood(y = concussions_subjects$volume, sigma = 0.5)

#' Plotting the prior, scaled likelihood, and the posterior
plot_normal_normal(mean = 6.5, sd = 0.4, sigma = 0.5, y_bar = 5.735, n = 25)
summarize_normal_normal(mean = 6.5, sd = 0.4, sigma = 0.5, y_bar = 5.735, n = 25)