library(tidyverse)
library(broom)

library(tidytext)
library(irlba)
library(stm)

library(assertthat)
library(tictoc)
library(furrr)

n_workers = 6
seed = 2021-05-25

# n_topics = c(5, seq(25, 150, by = 25))
n_topics = 5

data_dir = '../data/'

## Load data ----
H = read_rds(str_c(data_dir, '09_H.Rds'))
atm_sparse = cast_sparse(H$atm, row = auid, column = text, 
                         value = n)

n_authors = nrow(atm_sparse)

## Principal components ----
if (interactive()){
    # pc = atm %>% 
    #     spread(key = text, value = n, fill = 0) %>% 
    #     column_to_rownames(var = 'auid') %>% 
    #     prcomp()
    ## ~105 sec
    tic()
    pc = prcomp_irlba(atm_sparse, n = 300)
    toc()
    
    # plot(pc)
    
    #   A tibble: 1 x 4
    #   thresh_50 thresh_80 thresh_90 thresh_99
    #       <int>     <int>     <int>     <int>
    # 1        12        63       135       523
    tidy(pc, matrix = 'pcs') %>% 
        summarize(thresh_50 = sum(cumulative <= .5), 
                  thresh_80 = sum(cumulative <= .8), 
                  thresh_90 = sum(cumulative <= .9),
                  thresh_99 = sum(cumulative <= .99))
    
    tidy(pc, matrix = 'pcs') %>% 
        ggplot(aes(PC, cumulative)) +
        geom_line() +
        geom_hline(yintercept = c(.5, .8, .9, .99)) +
        xlim(0, 500)
}

## Topic modeling ----
holdout = make.heldout(atm_sparse, seed = seed)

## holdout auids are in the same order as dept_dummies$auid
assert_that(all(names(holdout$documents) == dept_dummies$auid))




## 872 sec for k = 100
# tic()
# test_model = stm(holdout$documents,
#                  holdout$vocab,
#                  K = 20,
#                  data = dept_dummies,
#                  prevalence = ~ . - auid,
#                  seed = seed*2,
#                  max.em.its = 150,
#                  verbose = TRUE)
# toc()
# 
# foo = estimateEffect(c(1) ~ . - auid, stmobj = test_model, metadata = dept_dummies)

print(str_c('Fitting ', length(n_topics), ' topic models'))
if (parallelly::supportsMulticore()) {
    message('Supports multicore')
}
plan(multisession, workers = n_workers)
tic()
models = tibble(k = n_topics) %>%
    arrange(desc(k)) %>% 
    mutate(model = future_map(k, 
                              ~ stm(holdout$documents, 
                                    holdout$vocab,
                                    K = ., 
                                    seed = seed,
                                    max.em.its = 150,
                                    verbose = FALSE), 
                              .options = furrr_options(
                                  seed = TRUE),
                              .progress = TRUE)) %>%
    arrange(k)
toc()


## Topic model quality statistics ----
plan(multisession, workers = n_workers)
## ~5 sec
tic()
k_result = models %>%
    mutate(semantic_coherence_topicwise = future_map(model, 
                                                     semanticCoherence,
                                                     holdout$documents),
           semantic_coherence = map_dbl(semantic_coherence_topicwise, 
                                        mean),
           ## Exclusivity is supposed to "complement" semantic coherence
           ## But interpretation isn't explained in docs
           exclusivity_topicwise = future_map(model, exclusivity), 
           exclusivity = map_dbl(exclusivity_topicwise, mean), 
           ## This is misused in Silge's code?
           ## Supposed to train model on holdout$documents, not whole corpus?
           ho_likelihood = {model %>%
                   map(eval.heldout,
                       holdout$missing) %>%
                   map_dbl('expected.heldout')},
           ## Dispersion of residuals should go to 1 as we get the correct number of topics
           residuals = {model %>%
                   map(checkResiduals,
                       holdout$documents) %>%
                   map_dbl("dispersion")},
           ## "approximate bound on the marginal likelihood at each step"
           bound =  map_dbl(model,
                            ~ max(.$convergence$bound)),
           ## (k+1)!
           lfact = map_dbl(model,
                           ~ lfactorial(.$settings$dim$K)),
           ## ??
           lbound = bound + lfact,
           iterations = map_dbl(model,
                                # ~ length(.$convergence$bound)))
                                ~ .$convergence$its)
    ) %>%
    select(-model)
toc()

if (interactive()) {
    k_result %>% 
        select(k, 
               semantic_coherence, ## want *high*
               exclusivity,        ## want *high*?
               ho_likelihood,      ## want *high*
               residuals,          ## want *low*
               lbound,             ## ??
               iterations          ## want low, but less important
        ) %>%
        gather(key = 'statistic', value = 'value', -k) %>% 
        ggplot(aes(k, value)) +
        geom_point() +
        geom_line() +
        facet_wrap(vars(statistic), scales = 'free')
}

## Output ----
if (interactive()) {
    write_rds(pc, str_c(data_dir, '10_atm_pc.Rds'))
}
write_rds(models, str_c(data_dir, '10_models.Rds'))
write_rds(k_result, str_c(data_dir, '10_model_stats.Rds'))
