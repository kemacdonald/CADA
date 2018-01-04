library(entropy)
library(tidyverse)

## roll my own information gain functions
get_symbol_ent <- function(prob) {
  if(prob == 0) {
    0
  } else {
    (prob * log2(prob)) * -1  
  }
}

# compute entropy for each symbol in the probability vector 
# and add them up
compute_entropy <- function(probs_vect) {
  sapply(probs_vect, get_symbol_ent) %>% 
    sum()
}

bayes_fun <- function(prior_a, prior_b, likelihood_b_given_a) {
  (prior_a * likelihood_b_given_a) / prior_b
}


info_gain_answer <- function(prior_ent, cond_probs) {
  posterior_ent <- compute_entropy(cond_probs)
  prior_ent - posterior_ent
}

info_gain_question <- function(prior_ent, info_gain_answer_vect, prob_answer_vect) {
  posterior_ent <- (info_gain_answer_vect * prob_answer_vect) %>% sum()
  round(posterior_ent, digits = 3) # should we taking the difference between prior and posterior entropy here?
}

################### Toy example

## setting up the problem, or what do we know about the world

# prior prob of glom
p_glom <- 0.7
p_notglom <- 1 - p_glom

# probs related to the hula question
p_hula <- .34
p_nothula <- 1 - p_hula
p_hula_glom <- 0.1
p_hula_fizo <- 0.9

##### compute prior entropy. this captures everything the learner knows and how well they know it
prior_ent_learner <- compute_entropy(probs_vect = c(p_glom, p_notglom))

#### compute information gain if the the answer was "yes" hula wor
cond_prob_glom_hula <- bayes_fun(p_glom, p_hula, p_hula_glom)
cond_prob_fizo_hula <- bayes_fun(p_notglom, p_hula, p_hula_fizo)


ig_hula_answer <- info_gain_answer(prior_ent = prior_ent_learner,
                                   cond_probs = c(cond_prob_glom_hula, cond_prob_fizo_hula))

## now do the same IG computation if the answer was "no" hula worn
cond_prob_glom_nothula <- bayes_fun(p_glom, p_nothula, 1 - p_hula_glom)
cond_prob_fizo_nothula <- bayes_fun(p_notglom, p_nothula, 1 - p_hula_fizo)


ig_nothula_answer <- info_gain_answer(prior_ent = prior_ent_learner,
                                      cond_probs = c(cond_prob_glom_nothula, 
                                                     cond_prob_fizo_nothula))

##### compute the expcted information gain for the hula question
## this is the step where we weight the utility (IG) of each answer by the probability of occurrence
info_gain_question(prior_ent = prior_ent_learner,
                   info_gain_answer_vect = c(ig_hula_answer, ig_nothula_answer),
                   prob_answer_vect = c(p_hula, p_nothula))


#### Now do the same process for the drinks tea question
p_tea <- .36
p_nottea <- 1 - p_tea
p_tea_glom <- 0.3
p_tea_fizo <- 0.5

#### compute information gain if the the answer was "yes" tea 
cond_prob_glom_tea <- bayes_fun(p_glom, p_tea, p_tea_glom)
cond_prob_fizo_tea <- bayes_fun(p_notglom, p_tea, p_tea_fizo)

ig_tea_answer <- info_gain_answer(prior_ent = prior_ent_learner,
                                   cond_probs = c(cond_prob_glom_tea, 
                                                  cond_prob_fizo_tea))


#### compute information gain if the the answer was "no" tea 
cond_prob_glom_nottea <- bayes_fun(p_glom, p_nottea, 1 - p_tea_glom)
cond_prob_fizo_nottea <- bayes_fun(p_notglom, p_nottea, 1 - p_tea_fizo)

ig_nottea_answer <- info_gain_answer(prior_ent = prior_ent_learner,
                                      cond_probs = c(cond_prob_glom_nottea, 
                                                     cond_prob_fizo_nottea))

##### compute the expcted information gain for the tea question
## this is the step where we weight the utility (IG) of each answer by the probability of occurrence
info_gain_question(prior_ent = prior_ent_learner,
                   info_gain_answer_vect = c(ig_tea_answer, ig_nottea_answer),
                   prob_answer_vect = c(p_tea, p_nottea))