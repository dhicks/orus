library(tidyverse)
library(broom)

library(tidytext)
library(stm)

library(assertthat)
library(tictoc)
library(furrr)

n_workers = 2
seed = 2019-05-10

n_topics = seq(5, 145, by = 20)

data_dir = '../data/'

## Load data ----
H = read_rds(str_c(data_dir, '09_H.Rds'))
atm = H$atm

n_authors = n_distinct(atm$auid)

## Principal components ----
pc = atm %>% 
    spread(key = lemma, value = n, fill = 0) %>% 
    column_to_rownames(var = 'auid') %>% 
    prcomp()

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


## Topic modeling ----
atm_sparse = cast_sparse(atm, row = auid, col = lemma, value = n)

holdout = make.heldout(atm_sparse, seed = seed)

## 500 sec for k = 100
# tic()
# test_model = stm(holdout$documents,
#                  holdout$vocab,
#                  K = 100,
#                  seed = seed*2,
#                  max.em.its = 150,
#                  verbose = TRUE)
# toc()

## 
print(str_c('Fitting ', length(n_topics), ' topic models'))
plan(multiprocess, workers = n_workers)
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
                              .progress = TRUE)) %>%
    arrange(k)
toc()


## Topic model quality statistics ----
plan(multiprocess, workers = n_workers)
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


## Output ----
write_rds(pc, str_c(data_dir, '10_atm_pc.Rds'))
write_rds(models, str_c(data_dir, '10_models.Rds'))
write_rds(k_result, str_c(data_dir, '10_model_stats.Rds'))
