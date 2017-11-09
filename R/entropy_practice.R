rm(list=ls())
library(entropy)
library(tidyverse)

## Information Theory Simuluations


## function to generate simulation of binomial process with different probability of success
get_entropy_binom <- function(prob_sucess) {
  data <- rbinom(n = 1000, size = 1, prob = prob_sucess)
  freqs <- table(data) / length(data)
  entropy(freqs, unit = "log2")
}

# Calculate entropy for different data generating processes 

probs <- seq(0, 1, by = 0.1)
entropy_vals <- sapply(probs, get_entropy_binom)

a <- qplot(x = probs, y = entropy_vals) +
  ylim (0, 1)

# Entropy of values is highest when the data generating process is random


# Calculate change in entropy of prior to posterior
# (1) make a prior distribution over parameters in the binomial model
# imagine you saw some data and now you have a posterior distribution over parameter p
# calculate the change in entropy from the prior to the posterior
# figure out what action would maxmize the change from prior to posterior entropy

y_unif <- runif(n = 1000, min = 0, max = 100) %>% discretize(10)
entropy(y_unif, unit = "log2")


### playing with the entropy calculations
y <- c(1,0,1,0)
y <- c(100, 100)

freqs.empirical(y)
entropy.empirical(y, unit = "log2")


## key: to get entropy you need the probability of a thing in your data. 
## so you need to go from counts to probability
## then you multiply the probability of the thing by the log of 1 over the probabitliy of that thing in your data
## the intuition is that distributions with higher entropy are more uncertain 


## roll my own entropy functions 

get_symbol_entropy <- function(x) {
  
  if(x == 0) {
    0
  } else {
    (x * log2(x)) * -1  
  }
  
}

compute_entropy <- function(vect) {
  # get counts of each symbol in the vector
  counts <- table(vect)
  # get probability of each symobol in the vector
  probs <- counts / length(vect)
  # compute entropy 
  sapply(probs, get_symbol_entropy) %>% sum()
}


### Test the function against the entropy function from the entropy package

get_entropy_binom_mine <- function(prob_sucess) {
  data <- rbinom(n = 1000, size = 1, prob = prob_sucess)
  compute_entropy(data)
}

my_ent <- sapply(probs, get_entropy_binom_mine)

b <- qplot(x = probs, y = my_ent) +
  ylim (0, 1)

cowplot::plot_grid(a,b)