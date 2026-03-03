library(bayesrules)
library(tidyverse)
library(janitor)

#' Example: Fake News

data(fake_news)

#' Forming a Prior
fake_news %>%
  tabyl(type) %>%
  adorn_totals('row')

#' Using Data in light of the Prior
#' to build the Posterior
fake_news %>%
  tabyl(title_has_excl, type) %>%
  adorn_totals('row')

#' Simulating the Posterior by:
#' 1: Define a Prior for the Article Data
#' 2: Simulate the Article Data
#' 3: Simulate the Exclamation Point Data
article = data.frame(type = c('real', 'fake'))

#' Defining a Prior for the Article Data
prior = c(0.6, 0.4)

#' Simulating the Article Data
set.seed(84735)
n = 10000
article_sim = sample_n(article, size = n, weight = prior, replace = TRUE)
ggplot(data = article_sim, mapping = aes(x = type)) +
  geom_bar()

article_sim %>%
  tabyl(type) %>%
  adorn_totals('row')

#' Simulating the Exclamation Point Data
article_sim = article_sim %>%
  mutate(data_model = case_when(
    type == 'fake' ~ 16/60,
    type == 'real' ~ 2/90
  )
)

data = c('no', 'yes')
set.seed(3)
article_sim = article_sim %>%
  group_by(1:n()) %>%
  mutate(
    usage = sample(data, size = 1, prob = c(1 - data_model, data_model))
  )

article_sim %>%
  tabyl(usage, type) %>%
  adorn_totals(c('col', 'row'))

ggplot(data = article_sim, mapping = aes(x = type, fill = usage)) +
  geom_bar(position = 'fill')
ggplot(data = article_sim, mapping = aes(x = type)) +
  geom_bar()

#' Assessing the Simulation
article_sim %>%
  filter(usage == 'yes') %>%
  tabyl(type) %>%
  adorn_totals('row')

ggplot(data = article_sim, mapping = aes(x = type)) +
  geom_bar() +
  facet_wrap(~ usage)

#' Example: Pop vs Soda vs Coke

data(pop_vs_soda)

#' Defining the Prior for the Regional Population Data
prior = c(0.21, 0.17, 0.38, 0.24)

pop_vs_soda %>%
  tabyl(pop, region) %>%
  adorn_percentages('col')

#' Updating out understanding 
posterior = c(0.48, 0.16, 0.11, 0.25)

#' Example: Kasparov vs Deep Blue

#' Probability that Y = 1
prior = c(0.1, 0.25, 0.65)
pi = c(0.2, 0.5, 0.8)
p = sum(prior * choose(6, 1)*pi*(1-pi)^(5))

#' Posterior Simulation
chess = data.frame(pi = c(0.2, 0.5, 0.8))

#' Simulating Kasporov's Win Probability Data
set.seed(84735)
n = 10000
chess_sim = sample_n(chess, size = n, weight = prior, replace = TRUE)

chess_sim = chess_sim %>%
  mutate(y = rbinom(n = n, size = 6, prob = pi))

chess_sim %>%
  head(3)
chess_sim %>%
  tabyl(pi) %>%
  adorn_totals('row')

ggplot(data = chess_sim, mapping = aes(x = y)) +
  stat_count(mapping = aes(y = after_stat(prop))) +
  facet_wrap(~ pi)

#' Updating our Beliefs
win_one = chess_sim %>%
  filter(y == 1)

win_one %>%
  tabyl(pi) %>%
  adorn_totals('row')

ggplot(data = win_one, mapping = aes(x = pi)) +
  geom_bar()
