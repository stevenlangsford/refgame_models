library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(assertthat)
library(here)
library(patchwork)

rm(list = ls()) #sorry JB
devtest <- FALSE
set_cmdstan_path("~/cmdstan")#may vary depending on your cmdstan installation
#set.seed(4)
theme_set(theme_light())

##WORLD SETUP
object_universe <- list(
    "color" = list("red", "green", "blue", "yellow"),
    "shape" = list("circle", "triangle", "square")
)#Can extend, but speakers will consider all combinations, which gets big fast.
#Later code assumes this order is preserved in utterances (possibly with '.' fillers)

stim_universe <- cross_df(object_universe)
utterance_universe <- cross_df(map(object_universe, function(l)c(l, ".")))
utterance_universe <- utterance_universe[-nrow(utterance_universe), ]##ban silence (controversial?)


##this-sim setup
options_per_trial <- 4
sim_p_under <- 0.1
sim_p_over <- 0.1
n_trials  <- 3


readable_trial_list <- list()
readable_trial_targs <- list()

##random refs
random_item <- function() {    
    list(map_chr(object_universe, function(x)sample(x, 1)%>%unlist))
}
random_optionset <- function() {
    ret <- list()
    while(length(ret) < options_per_trial){
        candidate  <-  random_item()
        if(candidate %in% ret) next;
        ret <- append(ret, candidate)
    }
    return(ret)
}

random_ref <- function() {
    sample(1:options_per_trial, 1)
}

ref_to_stanformat <- function(n) {
    ##yes this is silly, but it means you get to re-use listener code unmodified    
    ret <- rep(0,options_per_trial)
    ret[n] <- 1
    return(ret)
}

datalist  <- list(
    n_trials = n_trials,
    n_utterances = nrow(utterance_universe),
    n_options = options_per_trial,
    status_lists = list(),
    status_counts = list(),
    p_under = sim_p_under,
    p_over = sim_p_over,
    ref_probs = list()
)

source("get_status.R")

for(atrial in 1:n_trials){
    my_status_lists <- matrix(NA, nrow = options_per_trial,
                              ncol = nrow(utterance_universe))
    my_status_counts <- my_status_lists
    
        new_trial  <- random_optionset()
        new_targ  <- random_ref()
    readable_trial_list[[length(readable_trial_list) + 1]] <- new_trial
    readable_trial_targs[[length(readable_trial_targs) +1]] <- new_trial[new_targ]
    
    
    for (i in 1:options_per_trial){
       
        status_info <- get_status(new_trial[[new_targ]],#IF this was the target
                                  new_trial[-new_targ] #then these others are foils
                                  )
        my_status_lists[i, ] <- status_info %>% pull(status_code)
        my_status_counts[i, ] <- status_info %>% pull(n_thisstatus)
    }
    datalist$status_lists[[atrial]] <- my_status_lists
    datalist$status_counts[[atrial]] <- my_status_counts
    datalist$ref_probs[[atrial]] <- ref_to_stanformat(new_targ)
}

model <- cmdstan_model("speaker_get_utterance.stan")

start_time <- Sys.time()
fit <- model$sample(
                 data = datalist,
                 seed = 123,
                 chains = ifelse(devtest, 1, 3),
                 parallel_chains = ifelse(devtest, 1, 3),
                 iter_sampling = ifelse(devtest, 2, 1000),
                 max_treedepth = ifelse(devtest, 10, 10), #default 10 maybe ok
                 refresh = 500)
end_time  <-  Sys.time()

mysamples <- as_draws_df(fit$draws())

uttergen_df <-
    mysamples %>%
    select(starts_with("speaker_utterance")) %>%
    pivot_longer(everything()) %>%
    extract(name, ".*(\\d+)", into = "trial_id")%>%
    group_by(trial_id, value)%>%
    summarize(n = n())



tell_me_about_trial <- function(trialid) {
    print("options")
    print(readable_trial_list[[trialid]])
    print("targ")
    print(readable_trial_targs[[trialid]])
    uttergen_df %>%
        filter(trial_id == trialid)%>%
        mutate(value = sapply(value, function(x){
            paste(utterance_universe[x,], collapse = "_")
        }
            ))
    
}
