##All the things you need take a bunch of options and turn them into a list of 'status' tags for every possible utterance (which then turns into utterance likelihood function)
##Intented to change/grow, if things go well.

##assumes utterance_universe and object_universe exist

##Try to do as much as possible in human-readable text vectors (for debugging)
##will need to convert to a stan format right at the end, which is a matrix with n_utterance rows and n_intended_referent cols and a status tag in each cell. You also need to know how many other utterances share that status.

#source("test.R")##setup utterance universe /test stim (includes a rm all)

##ATOMS
is_consistent <- function(utterance, target) {
    for(afeature in utterance) {
        if (afeature ==".") next; #special 'omit' char for silence
        if (!afeature %in% target) return(FALSE)
    }
    return(TRUE)
}

is_foilmatch <- function(utterance, foils){
    for (afoil in foils) if (is_consistent(utterance, afoil)) return(TRUE)
    return(FALSE)
}

get_cost <- function(utterance) {
    sum(utterance != ".")
}

## is_mincost <- function(df_of_utterances) {
##     ##ugly untidy implementation, come back and clean this up?
## ##    browser()
##     df_of_utterances$cost <- apply(df_of_utterances, 1, get_cost)
##     return(df_of_utterances %>%
##            mutate(ismin = cost == min(cost)) %>%
##            pull(ismin))
## }


##STATUS
get_status <- function(targ_ref, foils) {
    
     statusfn <- function(good, cheap, consistent) {
            if (good && cheap) return("justright")
            if (good) return("overinform") #consistent & nofoils
            if (consistent) return("underinform") #consistent but can match foils
            return("bad")
     }
    
    utterance_universe %>%
        mutate(#apply inside mutate? UGH go do some purrr tutes
            consistent = apply(utterance_universe, 1,
                               function(x)is_consistent(x, targ_ref)),
            nofoils = apply(utterance_universe, 1,
                              function(x)!is_foilmatch(x, foils)),
            cost = apply(utterance_universe, 1,
                              function(x)get_cost(x)),
            good = consistent & nofoils
        ) %>%
        group_by(good) %>%
        mutate(cheap = cost == min(cost)) %>%
        ungroup %>%
        rowwise %>%
        mutate(status = statusfn(good, cheap, consistent) %>%
                   factor(levels = c("bad",
                                     "underinform",
                                     "overinform",
                                     "justright"))
               ) %>%
        group_by(status) %>%
        mutate(n_thisstatus = n(),
               status_code = as.numeric(status)) %>%
        ungroup()%>%
        select(-consistent, -nofoils, -cost, -good, -cheap) %>%
        return
}


##TESTS: (can ignore)
##Currently assumes utterance_universe, test_targ and test_foils are set up
##These are eyeball tests, need to change setup each time yourself. Have fun
##TODO save a collection of interesting setup varieties + expected results?

test_is_consistent  <- function() {
    print(test_targ)
    print(
        utterance_universe%>%
    mutate(status = map_lgl(1:nrow(utterance_universe),
                            function(i) is_consistent(utterance_universe[i,], test_targ))))
    }

test_is_foilmatch  <- function() {
    print(test_foils)
    print(
utterance_universe%>%
    mutate(status = map_lgl(1:nrow(utterance_universe),
                            function(i) is_foilmatch(utterance_universe[i,], test_foils))))
}

test_get_cost <- function() {
    print(
utterance_universe%>%
    mutate(status = map_int(1:nrow(utterance_universe),
                            function(i) get_cost(utterance_universe[i,])))
)

    }
test_is_mincost <- function() {
    someselection <- 1:6
     print(
         utterance_universe%>%
         filter(row_number()%in%someselection)%>%
         mutate(ismin = is_mincost(utterance_universe[someselection,]))
     )

}
test_get_status <- function(){
    print(
        get_status(test_targ, test_foils)
    )
}
