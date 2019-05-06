## TODO:  
## - k = 200, 240 models
## - rewrite 07 to store the date of first affiliation w/ the ORU for DID sample
##      - then use before/after ORU affiliation in DID spec
## - both samples, in parallel
## - controls to incorporate:  
##      - publication count in each year
##      - number of coauthors? 
##      - RE for particular ORU? 
## - regressions for publication, coauthor(?), citation count
## - note filtering of 1 author when using only dept's w/ >1 author

library(tidyverse)
# library(tsibble)

library(assertthat)
library(tictoc)

library(lme4)
library(broom)

data_folder = '../data/'

## Load data ----
meta_df = read_rds(str_c(data_folder, '08_comp.Rds'))

## topicDocs doesn't include the actual doc (author-year) identifiers; 
## so getting these from the dtm (aytm) will be the most reliable way to join
## **ASSUMES THE TOPIC MODELING PROCESS HASN'T REORDERED ANYTHING**
aytm = read_csv(str_c(data_folder, '10_aytm_comp.csv'))
auid_year = aytm %>% 
    pull(auid_year) %>% 
    unique()

theta = read_csv(str_c(data_folder, '11_topicDocs_comp_160.csv')) %>% 
    ## Add auid_year
    mutate(auid_year = auid_year) %>% 
    separate(auid_year, into = c('auid', 'year'), sep = '_') %>% 
    mutate(year = as.integer(year)) %>% 
    select(auid, starts_with('V')) %>% 
    group_by(auid) %>% 
    summarize_at(vars(starts_with('V')), mean)
# assert_that(nrow(meta_df$author_meta) == nrow(theta))

n_topics = theta %>% 
    select(starts_with('V')) %>% 
    names() %>% 
    length()

max_H = log2(n_topics)

theta_long = gather(theta, key = 'topic', value = 'theta', 
                    starts_with('V')) %>% 
    group_by(auid, topic) %>% 
    summarize(theta = mean(theta)) %>% 
    ungroup()


## Department dummies ----
## Filtering departments loses 1 author but avoids 5 degeneracies
dept_dummies = meta_df$author_meta %>% 
    unnest(data) %>% 
    ## Spread to construct dummies
    select(auid, aff_name) %>% 
    filter(!duplicated(.)) %>% 
    mutate(t = 1) %>% 
    spread(key = aff_name, value = t, fill = 0)

assert_that(nrow(dept_dummies) == nrow(meta_df$author_meta))

dept_dummies %>% 
    select(-auid) %>% 
    as.matrix() %>% 
    Matrix::rankMatrix() %>% 
    {. == ncol(dept_dummies) - 1} %>% 
    assert_that(msg = 'dept_dummies does not have full rank')


## ORU affiliations ----
oru_df = meta_df$author_meta %>% 
    unnest(data) %>% 
    select(auid, group, oru_name)#, first_year_oru)


## Topic entropy ----
## TODO:  modify this to unpack ORU affiliations but lose dep't affiliations
H = theta_long %>% 
    ## Calculate entropy
    group_by(auid) %>% 
    mutate(H_term = -theta * log2(theta)) %>% 
    summarize(H = sum(H_term)) %>% 
    ## Lags
    # arrange(year) %>% 
    # mutate(H_lag1 = lag(H), 
    #        H_lag2 = lag(H_lag1), 
    #        H_lag3 = lag(H_lag2), 
    #        H_lag4 = lag(H_lag3), 
    #        H_lag5 = lag(H_lag4)) %>% 
    ## Combine w/ metadata
    ungroup() %>% 
    right_join(meta_df$author_meta, by = 'auid')# %>%
    # unnest(data) %>% 
    # select(-data) %>% 
    # right_join(oru_df, by = c('auid', 'group')) %>% 
    # right_join(dept_dummies, by = 'auid') %>% 
    ## Time since first year
    # mutate(career = year - first_year,#_pubs, 
    #        year.c = year - min(year)#, 
    #        # oru_aff = year >= first_year_oru
    #        )# %>% 
    # filter(!is.na(H_lag4))

## After filtering for lag of 4, we lose 9 non-ORU authors
anti_join(meta_df$author_meta, 
          H, by = 'auid') %>% 
    count(oru)

# assert_that(!any(is.na(H$H_lag4)))
# assert_that(all(complete.cases(H)))


## Some EDA on H over time (indexed in various ways) and across groups
ggplot(H, aes(H, color = group)) +
    geom_density() +
    geom_vline(xintercept = max_H)

# ggplot(H, aes(year, H, color = group)) +
#     geom_smooth()

ggplot(H, aes(first_year, H, color = group)) +
    geom_point(alpha = .25) +
    geom_smooth() +
    geom_hline(yintercept = log2(n_topics)) +
    theme_bw()

# H %>% 
#     filter(group == 'ORU') %>% 
#     ggplot(aes(year - first_year_oru, H)) +
#     geom_line(aes(group = auid)) +
#     geom_smooth() +
#     geom_hline(yintercept = log2(n_topics)) +
#     theme_bw()

# H %>% 
#     # filter(group == 'ORU') %>% 
#     ggplot(aes(interaction(group, oru_aff), H)) +
#     geom_violin(draw_quantiles = .5) +
#     stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1)) +
#     geom_hline(yintercept = log2(n_topics)) +
#     theme_bw()

# ggplot(H, aes(first_year_oru)) +
#     geom_density()

## Jane's suggestion:  distribution of first_year_pubs across groups
ggplot(meta_df$author_meta, aes(group, first_year)) +
    geom_violin() +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1)) +
    theme_bw()

ggplot(meta_df$author_meta, aes(group, n_docs)) +
    geom_violin() +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1)) +
    theme_bw() +
    scale_y_log10()

# ggplot(H, aes(group, career)) +
#     geom_violin() +
#     stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1)) +
#     theme_bw()

# ggplot(H, aes(career, color = group)) +
#     geom_density()


## Autocorrelation seems to be small enough after lag 3
# H %>%
#     select(starts_with('H')) %>%
#     cor(use = 'pairwise.complete')
# 
# lm(H ~ 1 + H_lag1 + H_lag2 + H_lag3 + H_lag4 + H_lag5, 
#      data = H)

## Construct formula
# depts = H %>% 
#     select(matches('Department of')) %>% 
#     names() %>% 
#     str_c('\`', ., '\`') %>% 
#     str_c(collapse = ' + ')
# 
# reg_form = str_c('H ~ 1 + oru_aff', 
#       'career + (1|year.c) + H_lag', 
#       '(1|auid)',
#       depts, 
#       sep = ' + ') %>% 
#     as.formula()

tic()
# model = lmer(reg_form, data = H)
## This is raising convergence warnings
# model = lmer(H ~ 1 + (1+oru_aff|oru_name) + career + (1|year.c) + H_lag + (1|auid) + (1|aff_name), data = H)
model = H %>% 
    unnest() %>% 
    mutate(oru_name = fct_relevel(oru_name, 'none')) %>% 
    lmer(H ~ 1 + #(1|year.c) + career + 
                 log10(n_docs) +
                 (1|first_year) +
                 #H_lag1 + H_lag2 + H_lag3 + H_lag4 + #H_lag5 +
                 #(1|auid) + 
                 (1|aff_name) + oru_name, 
             data = .)
toc()
summary(model)

## <https://stats.stackexchange.com/questions/110004/how-scared-should-we-be-about-convergence-warnings-in-lme4>
# lmerControl()$checkConv$check.conv.grad$tol
# relgrad <- with(model@optinfo$derivs,solve(Hessian,gradient)); max(abs(relgrad))

broom.mixed::tidy(model, effects = 'fixed') %>% 
    # filter(group == 'oru_name') %>% 
    filter(str_detect(term, 'oru_name')) %>% 
    mutate(term = str_remove(term, 'oru_name')) %>% 
    mutate(ci.low = estimate + qnorm(.0275) * std.error, 
           ci.high = estimate + qnorm(.975) * std.error) %>% 
    ggplot(aes(term, estimate, ymin = ci.low, ymax = ci.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    # facet_wrap(vars(term), scales = 'free') +
    coord_flip() +
    theme_bw()

broom.mixed::augment(model) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    ## Ceiling imposed by maximum entropy
    stat_function(inherit.aes = FALSE, 
                  fun = function(x) {max_H - x}, 
                  linetype = 'dotted') +
    facet_wrap(vars(oru_name))

broom.mixed::augment(model) %>% 
    ggplot(aes(.resid, color = oru_name)) +
    geom_density(show.legend = FALSE) +
    geom_rug(show.legend = FALSE) +
    facet_wrap(vars(oru_name))


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
