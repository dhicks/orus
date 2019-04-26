## TODO:  
## - k = 200, 240 models
## - rewrite 07 to store the date of first affiliation w/ the ORU for DID sample
##      - then use before/after ORU affiliation in DID spec
## - both samples, in parallel
## - controls to incorporate:  
##      - publication count in each year
##      - number of coauthors? 
##      - RE for particular ORU? 
## - regressions for publication, coauthor, citation count

library(tidyverse)
library(tsibble)

library(assertthat)
library(tictoc)

library(lme4)

data_folder = '../data/'

## Load data ----
meta_df = read_rds(str_c(data_folder, '08_did.Rds'))

## topicDocs doesn't include the actual doc (author-year) identifiers; 
## so getting these from the dtm (aytm) will be the most reliable way to join
## **ASSUMES THE TOPIC MODELING PROCESS HASN'T REORDERED ANYTHING**
aytm = read_csv(str_c(data_folder, '10_aytm_did.csv'))
auid_year = aytm %>% 
    pull(auid_year) %>% 
    unique()

theta = read_csv(str_c(data_folder, '11_topicDocs_did.csv')) %>% 
    ## Add auid_year
    mutate(auid_year = auid_year) %>% 
    separate(auid_year, into = c('auid', 'year'), sep = '_') %>% 
    mutate(year = as.integer(year)) %>% 
    select(auid, year, starts_with('V'))
assert_that(length(auid_year) == nrow(theta))

n_topics = theta %>% 
    select(starts_with('V')) %>% 
    names() %>% 
    length()

theta_long = gather(theta, key = 'topic', value = 'theta', starts_with('V'))


## Department dummies ----
## Filtering departments loses 1 author but avoids 5 degeneracies
dept_dummies = meta_df$codepts %>% 
    ## Drop departments w/ only 1 member
    add_count(aff_name) %>% 
    filter(n > 1) %>%
    ## Spread to construct dummies
    select(auid, aff_name) %>% 
    mutate(t = 1) %>% 
    spread(key = aff_name, value = t, fill = 0)

# assert_that(nrow(dept_dummies) == nrow(meta_df$author_meta))

dept_dummies %>% 
    select(-auid) %>% 
    as.matrix() %>% 
    Matrix::rankMatrix() %>% 
    {. == ncol(dept_dummies) - 1} %>% 
    assert_that(msg = 'dept_dummies does not have full rank')


## Topic entropy ----
H = theta_long %>% 
    ## Calculate entropy
    group_by(auid, year) %>% 
    mutate(H_term = -theta * log2(theta)) %>% 
    summarize(H = sum(H_term)) %>% 
    ## Lag
    arrange(year) %>% 
    mutate(H_lag = lag(H)) %>% 
    filter(!is.na(H_lag)) %>% 
    ## Combine w/ metadata
    ungroup() %>% 
    right_join(meta_df$author_meta, by = 'auid') %>% 
    right_join(dept_dummies, by = 'auid') %>% 
    ## Time since first year
    mutate(career = year - first_year, 
           year.c = year - min(year))

assert_that(!any(is.na(H$H)))
assert_that(all(complete.cases(H)))

ggplot(H, aes(year, H, color = group)) +
    geom_smooth()


tic()
model = H %>% 
    select(H, group, year.c, career, H_lag, 
           matches('Department of')) %>% 
    lm(H ~ ., data = .)
# model = lmer(H ~ 1 + group + (1|year) + (1|aff_name), data = H)
toc()
summary(model)



## Department-level annual topic distributions ----
## ~8 sec
tic()
theta_dept = theta %>% 
    right_join(meta_df$codepts) %>% 
    filter(aff_name %in% names(dept_dummies)) %>% 
    group_by(aff_name, year) %>% 
    summarize_at(vars(starts_with('V')), mean) %>% 
    ungroup() %>% 
    gather(key = topic, value = theta, starts_with('V'))
toc()

## 10.9 sec
# tic()
# theta_long %>% 
#     right_join(meta_df$codepts) %>% 
#     group_by(aff_name, year, topic) %>% 
#     summarize(theta = mean(theta))
# toc()

all(!is.na(theta_dept$theta))


## Rolling summaries
## ~5 sec
tic()
theta_dept_mean = theta_dept %>% 
    as_tsibble(key = id(aff_name, topic), 
               index = year) %>% 
    group_by(aff_name, topic) %>%
    mutate(theta_mean = slide_dbl(theta, 
                                  mean, 
                                  .size = 5, 
                                  .fill = NA, 
                                  .partial = FALSE, 
                                  .align = 'r')) %>% 
    mutate(year_p1 = lead(year)) %>% 
    ungroup()
toc()    

## ~3 sec
tic()
kl_div = theta_long %>% 
    inner_join(meta_df$codepts) %>% 
    filter(aff_name %in% names(dept_dummies)) %>% 
    inner_join(theta_dept_mean, 
               by = c('aff_name', 'topic', 
                      'year' = 'year_p1'), 
               suffix = c('.author', '.dept')) %>% 
    ## D_KL (dep't || author) in each year
    ## <https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence>
    mutate(div_term = theta.dept * (- log2(theta.author) + log2(theta.dept))) %>% 
    group_by(auid, aff_name, year) %>% 
    summarize(div = sum(div_term)) %>% 
    ## Lagged values
    arrange(auid, aff_name, year) %>% 
    mutate(div_lag = lag(div)) %>% 
    ungroup() %>% 
    filter(!is.na(div_lag)) %>% 
    ## Other metadata
    left_join(meta_df$author_meta) %>% 
    mutate(career = year - first_year, 
           year.c = year - min(year))
toc()

assert_that(all(complete.cases(kl_div)))

ggplot(kl_div, aes(year, div, color = group)) +
    geom_smooth()

kl_div %>% 
    # filter(group == 'ORU') %>% 
    ggplot(aes(year, div, color = group)) +
    geom_line(aes(group = auid), alpha = .1) +
    geom_smooth(aes(group = group))


tic()
model = lmer(div ~ 1 + group + 
                 career + year.c +
                 div_lag +
                 (1|aff_name), data = kl_div)
toc()
summary(model)
