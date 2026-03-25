library(bayesrules)
library(tidyverse)
library(janitor)

#' Notes
#' Conjugate prior for likelihood function
#'  The prior and posterior distributions belong to the same probability family

#' Example: Minnesotan Pollsters
plot_beta(45, 55)
plot_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)
summarize_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)

#' Simulating a Beta-Binomial Posterior
#' 1: Simulating underlying support (pi) data - Beta model, a = 45, b = 55
#' 2: Simulating polling data for each value of pi

set.seed(84735)
n = 10000
michelle_sim = data.frame(pi = rbeta(n = n, shape1 = 45, shape2 = 55)) %>%
  mutate(y = rbinom(n = n, size = 50, prob = pi))

ggplot(data = michelle_sim, mapping = aes(x = pi, y = y)) +
  geom_point(mapping = aes(color = (y == 30)), size = 0.1)

#' Narrowing in on the shape of pi's distribution in light of the observed data
michelle_posterior = michelle_sim %>%
  filter(y == 30)

ggplot(data = michelle_posterior, mapping = aes(x = pi)) +
  geom_density()

#' Approximating the E(x) and SQRT(Var(x)) of the Beta-Binomial Posterior
michelle_posterior %>%
  summarize(mean(pi), sd(pi)) 

#' Milgram's Study
plot_beta(alpha = 1, beta = 10)
summarize_beta_binomial(alpha = 1, beta = 10, y = 26, n = 40)