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
set.seed(4)
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

######TRIAL SETUP
test_utterance <- list(c("yellow", "square"),
                       c("blue",".")
                       )

test_options  <-
    list(
        list(c("yellow", "square"), c("yellow", "triangle"), c("blue", "circle")),
        list(c("yellow", "square"), c("yellow", "triangle"), c("blue", "circle"))
    )


#######sanity check and convert to stan format
n_trials  <- length(test_utterance)
assert_that(length(test_utterance) == length(test_options))

##following a dev strategy of 'get a single trial working, then wrap the code untouched in a function,
##smoosh the outputs of the function on each trial to get a multitrial version
##Big fan of simple->extended dev, but the end result looks kinda weird, maybe needs refactoring?

get_datalist <- function(test_utterance, test_options) {
##Check your setup is legal:
valid_obj <- function(feature_list) {
    if (length(feature_list) != length(object_universe)) {
        print("bad length")
        browser()
        return(FALSE)
    }
    for (i in 1:length(feature_list)){
        if (!(feature_list[i] %in% object_universe[[i]])) {
            print("feature not found")
            browser()
            return(FALSE)
        }
    }
    return(TRUE)
}

assert_that(all(map_lgl(test_options, valid_obj)))

##convert to convenient stan format:
utterance_id <- which(utterance_universe %>%
      unite("allcols", 1:ncol(.)) == paste(test_utterance, collapse = "_")
      )

assert_that(length(utterance_id) > 0,
            msg = "utterance id not found in utterance_universe")

source("get_status.R")

my_status_lists <- matrix(NA, nrow = length(test_options),
                          ncol = nrow(utterance_universe))
my_status_counts <- my_status_lists

for (i in 1:length(test_options)){
    status_info <- get_status(test_options[[i]],#IF this was the target
                              test_options[-i] #then these others are foils
                              )
   
    my_status_lists[i, ] <- status_info %>% pull(status_code)
    my_status_counts[i, ] <- status_info %>% pull(n_thisstatus)
}

    datalist  <- list(
        n_trials = 1,
        n_utterances = nrow(utterance_universe),
        n_options = length(test_options),
        status_lists = list(my_status_lists),
        status_counts = list(my_status_counts),
        obs = utterance_id)

    return(datalist)
}#end get datalist

combine_trial_datalists <- function(current, toadd) {
    return(
    list(
        n_trials = current$n_trials + 1,
        n_utterances = nrow(utterance_universe),#doesn't change
        n_options = current$n_options, #doesn't change (for now?)
        status_lists = append(current$status_lists, toadd$status_lists),
        status_counts = append(current$status_counts, toadd$status_counts),
        obs = c(current$obs, toadd$obs)
    )
    )
}

datalist <- get_datalist(test_utterance[[1]], test_options[[1]])
if (n_trials > 1) {
    for (i in 2:n_trials) {
        datalist <- combine_trial_datalists(datalist,
                                            get_datalist(test_utterance[[i]],
                                                         test_options[[i]]))
    }
}


model <- cmdstan_model("listener.stan")

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

ref_posteriors <- mysamples%>%
    select(starts_with("posterior_ref"))%>%
    pivot_longer(everything())%>%
    extract(name, regex = ".*(\\d+)", into = "trial_number")
(
ggplot(ref_posteriors, aes(x = value)) +
    geom_bar() +
facet_wrap(.~trial_number)
    ) /
(ggplot(mysamples) +
    geom_density(aes(x = p_under, color = "p_under")) +
    geom_density(aes(x = p_over, color = "p_over")) +
    geom_density(aes(x = prior_beta, color = "prior")))


##   ggplot(mysamples, aes(x = prior_beta)) +
##     geom_histogram() +
##     ggtitle(paste("95% of the distribution is under ",
##                   quantile(mysamples$prior_beta, .9)))
