library(tidyverse)
library(broom)
library(stm)
library(tidytext) ## for stm tidiers

library(gghighlight)
library(ggforce)
library(directlabels)
theme_set(theme_minimal())

library(tictoc)
library(assertthat)

data_dir = '../data/'
plots_dir = '../plots/'

## Values of k to use in gamma_sm
selected_k = c(25, 45, 85, 125)
# selected_k = c(85)


## Load data ----
models = read_rds(str_c(data_dir, '10_models.Rds'))

model_stats = read_rds(str_c(data_dir, '10_model_stats.Rds'))

H = read_rds(str_c(data_dir, '09_H.Rds'))
auids = H$atm %>% 
    spread(key = lemma, value = n) %>% 
    pull(auid)


author_meta = read_rds(str_c(data_dir, '05_author_meta.Rds')) %>% 
    mutate(first_year_1997 = first_year - 1997, 
           gender = fct_relevel(gender, 'male'))

dept_dummies = read_rds(str_c(data_dir, '05_dept_dummies.Rds')) %>% 
    filter(auid %in% author_meta$auid)

author_meta %>% 
    unnest(oru) %>% 
    ggplot(aes(oru, fill = oru)) +
    geom_bar(show.legend = FALSE) +
    stat_count(aes(y = ..count.., label = ..count..), 
               position = position_nudge(y = -.5),
               geom = 'text') +
    scale_y_sqrt() +
    ggtitle('Count of researchers by ORU', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_sample.png'))


## Extract topic models gamma ----
matrices = models %>% 
    mutate(beta = map(model, tidy, 
                      matrix = 'beta'),
           gamma = map(model, tidy, 
                       matrix = 'gamma', document_names = auids))

gamma = unnest(matrices, gamma) %>% 
    rename(auid = document) %>% 
    left_join(author_meta)

gamma_45 = filter(gamma, k == 45)
gamma_sm = filter(gamma, k %in% selected_k)




## Number of documents ----
n_docs_lm = author_meta %>% 
    select(n_docs, auid, oru_lgl, first_year_1997, gender) %>% 
    left_join(dept_dummies) %>% 
    lm(log10(n_docs) ~ . - auid, 
       data = .)

n_docs_lm %>% 
    augment() %>% 
    ggplot(aes(.resid)) +
    geom_density() +
    geom_rug(aes(color = oru_lgl))

n_docs_lm %>% 
    augment() %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point(aes(color = oru_lgl)) + 
    geom_smooth()

tidy(n_docs_lm, conf.int = TRUE) %>% 
    mutate(var_group = case_when(
        str_detect(term, 'Department') ~ 'department', 
        str_detect(term, 'Intercept') ~ 'intercept',
        TRUE ~ 'other terms'
    )) %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.) %>% 
    arrange(desc(estimate)) %>% 
    mutate(term = fct_inorder(term)) %>% 
    ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    ## gghighlight overrides facets
    # gghighlight(term == 'oru_lglTRUE',
    #             unhighlighted_colour = alpha('blue', .25)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    coord_flip() +
    facet_wrap(vars(var_group), scales = 'free',
               ncol = 2) +
    ylab('estimate (fold change)') +
    ggtitle('Est. effect of ORU affiliation on publication counts',
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_pub_regression.png'), 
       width = 7, height = 4, scale = 1.5)


## Citation counts ----
cites_lm = author_meta %>% 
    mutate(log_n_docs = log10(n_docs)) %>% 
    select(cited_by_count, auid, oru_lgl, first_year_1997,
           gender, log_n_docs) %>% 
    left_join(dept_dummies) %>% 
    # filter(cited_by_count > 0) %>% 
    lm(log10(cited_by_count+1) ~ . - auid, 
       data = .)

cites_lm %>% 
    augment() %>% 
    ggplot(aes(.resid)) +
    geom_density() +
    geom_rug(aes(color = oru_lgl))

## Should I be concerned about that left tail?  
cites_lm %>% 
    augment() %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point(aes(color = oru_lgl)) + 
    geom_smooth()

# cites_lm %>% 
#     augment() %>% 
#     filter(.resid < -.75) %>% 
#     mutate(.fitted = 10^.fitted-1) %>% 
#     select(auid, .fitted) %>% 
#     inner_join(author_meta)

cites_lm %>% 
    tidy(conf.int = TRUE) %>% 
    mutate(var_group = case_when(
        str_detect(term, 'Department') ~ 'department', 
        str_detect(term, 'Intercept') ~ 'intercept',
        str_detect(term, 'docs') ~ 'total publications',
        TRUE ~ 'other terms'
    )) %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.) %>% 
    arrange(desc(estimate)) %>% 
    mutate(term = fct_inorder(term)) %>% 
    ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    # gghighlight(term == 'oru_lglTRUE', 
    #             unhighlighted_colour = alpha('blue', .25)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    coord_flip() +
    facet_wrap(vars(var_group), scales = 'free', ncol = 2) +
    ylab('estimate (fold change)') +
    ggtitle('Est. effect of ORU affiliation on citation counts', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_cites_regression.png'), 
       width = 7, height = 4, scale = 1.5)

## Topic models ----

## Closer look at SC and EX ----
## In script 10, PCs suggested k = 45 would be reasonable
## Plotting just the means, it looks like there's a continuing substantial decrease past k = 45
## But plotting the values for each topic, the distributions don't look that different
model_stats %>% 
    unnest(semantic_coherence_topicwise) %>% 
    ggplot(aes(k, semantic_coherence_topicwise)) +
    # geom_point(aes(group = k)) +
    # geom_violin(aes(group = k)) +
    geom_sina(aes(group = k)) +
    geom_line(data = model_stats, 
              aes(y = semantic_coherence), 
              color = 'red')

## This is even more the case w/ exclusivity
model_stats %>% 
    unnest(exclusivity_topicwise) %>% 
    ggplot(aes(k, exclusivity_topicwise)) +
    # geom_point() +
    geom_sina(aes(group = k)) +
    geom_line(data = model_stats, 
              aes(y = exclusivity), 
              color = 'red')


## Topic-author entropy ----
## Author-level topic distributions, grouped by ORU, k = 45
at_plot = gamma_45 %>% 
    unnest(oru) %>% 
    ggplot(aes(auid, topic, fill = gamma)) +
    geom_raster() +
    scale_fill_viridis_c(option = 'A') +
    facet_wrap(vars(oru), scales = 'free') +
    coord_flip()
at_plot

## And same for 125
gamma_sm %>% 
    filter(k == 125) %>% 
    unnest(oru) %>% 
    {at_plot %+% .}

## ORU-level topic distributions
gamma_oru = gamma %>% 
    unnest(oru) %>% 
    # filter(oru != 'AQRC') %>% 
    group_by(k, oru, topic) %>% 
    summarize(gamma = mean(gamma)) %>% 
    ungroup()

ggplot(gamma_oru, aes(oru, topic, fill = gamma)) +
    geom_raster() +
    scale_fill_viridis_c(option = 'A', 
                         trans = 'sqrt') +
    facet_wrap(vars(k), scales = 'free_x') +
    coord_flip()

# ggplot(gamma, aes(document, topic, fill = gamma)) +
#     geom_raster() +
#     facet_wrap(vars(k), scales = 'free')

H_gamma = gamma %>% 
    group_by(k, auid) %>% 
    mutate(H_term = -gamma * log2(gamma)) %>% 
    summarize(H = sum(H_term)) %>% 
    ungroup() %>% 
    left_join(author_meta)

H_45 = filter(H_gamma, k == 45)

## Distributions of topic entropies
ggplot(H_gamma, aes(oru_lgl, H, color = oru_lgl)) +
    geom_violin(draw_quantiles = .5, size = 1) +
    geom_sina(alpha = .1) +
    # scale_x_discrete(breaks = NULL) +
    facet_wrap(vars(k), scales = 'free_y')

## ORU-level entropies
gamma_oru %>% 
    group_by(k, oru) %>% 
    mutate(H_term = -gamma * log2(gamma)) %>% 
    summarize(H = sum(H_term)) %>% 
    ungroup() %>% 
    ggplot(aes(k, H, color = oru)) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    geom_dl(aes(label = oru), method = 'last.points', 
            position = position_nudge(x = 5)) +
    xlim(NA, 165) +
    scale_color_viridis_d(option = 'B') +
    stat_function(fun = function(x) log2(x), 
                  inherit.aes = FALSE, color = 'black') +
    theme(panel.background = element_rect(fill = 'grey90'),
          legend.background = element_rect(fill = 'grey90'))
ggsave(str_c(plots_dir, '12_oru_entropy.png'), 
       width = 6, height = 4, scale = 1.5)

## Distributions within departments
dept_topics = author_meta %>% 
    unnest(department) %>% 
    # count(department) %>% arrange(desc(n)) %>% filter(n > 62)
    add_count(department) %>% 
    filter(n > 62) %>% 
    select(auid, department) %>% 
    inner_join(gamma_sm, by = 'auid') %>% 
    rename(department = department.x) %>% 
    ggplot(aes(topic, auid, fill = gamma)) +
    geom_raster() +
    scale_fill_viridis_c(option = 'A', 
                         trans = 'sqrt') +
    facet_wrap(vars(k, department), scales = 'free', 
               ncol = 4) +
    ggtitle('Author-topic distributions, grouped by department', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_dept_topics.png'), 
       dept_topics,
       width = 4*2+1, height = 8*2, 
       scale = 2)



## Regression model of entropy ----
# summary(lm(H ~ oru_lgl + n_docs, data = H_45, weights = match_occurrences))

H_lm = H_gamma %>% 
    select(k, auid, H, oru_lgl, first_year_1997, gender, 
           n_docs) %>% 
    left_join(dept_dummies) %>% 
    group_by(k) %>% 
    do(model = lm(H ~ . - auid, 
                  data = .
    )) %>% 
    ungroup() %>% 
    mutate(glance = map(model, glance), 
           coefs = map(model, tidy, conf.int = TRUE))

unnest(H_lm, glance)

H_lm %>% 
    unnest(coefs) %>% 
    filter(term == 'oru_lglTRUE') %>% 
    ggplot(aes(k, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    gghighlight(k == 45) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    xlab('number of topics (k)') +
    ylab('estimate (bits)') +
    ggtitle('Est. effect of ORU affiliation on topic entropy', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_entropy_regression.png'), 
       width = 6, height = 4, scale = 1.5)


## Silhouette analysis ----
## See the scratch file `Hellinger_low_memory.R` for an attempt to include non-ORU authors
## ~1.5 sec
tic()
crossed = gamma_sm %>%
    filter(oru_lgl) %>% 
    select(k, topic, auid, gamma) %>%
    group_by(k, topic) %>%
    group_split() %>% #str(max.level = 1)
    map2_dfr(., ., tidyr::crossing) %>%
    rename(auid.x = auid, auid.y = auid1,
           gamma.x = gamma, gamma.y = gamma1) %>%
    filter(auid.x != auid.y)
toc()

hellinger = function(dataf) {
    dataf %>%
        mutate(h_dist_term = sqrt(gamma.x * gamma.y)) %>%
        group_by(k, auid.x, auid.y) %>%
        summarize(h_dist = sqrt(1 - sum(h_dist_term))) %>%
        ungroup()
}



# ## Hellinger distances for all pairs
# ## ~1.4 sec for k = 5, 45, 85, 125
tic()
dist = hellinger(crossed)
toc()

## Mean distance w/in ORUs
## NB crossed should already eliminate self-pairs
interior_mean_dist = dist %>%
    left_join(author_meta, by = c('auid.x' = 'auid')) %>%
    left_join(author_meta, by = c('auid.y' = 'auid'),
              suffix = c('.x', '.y')) %>%
    unnest(oru.x, .drop = FALSE) %>%
    unnest(oru.y) %>%
    filter(oru.x == oru.y) %>%
    group_by(k, auid = auid.x, oru = oru.x) %>%
    summarize(int_mean_dist = mean(h_dist)) %>%
    ungroup()

# ## Minimum distance to non-ORU author
# comp_dist = dist %>% 
#     left_join(author_meta, by = c('auid.x' = 'auid')) %>% 
#     filter(oru_lgl, 
#            ## Anthony Wexler is the only person from AQRC
#            auid.x != '7005728145') %>% 
#     left_join(author_meta, by = c('auid.y' = 'auid'), 
#               suffix = c('.x', '.y')) %>% 
#     filter(!oru_lgl.y) %>% 
#     group_by(k, auid = auid.x) %>% 
#     summarize(comp_min_dist = min(h_dist)) %>% 
#     ungroup()

## Exterior minimum distance
exterior_min_dist = dist %>%
    left_join(author_meta, by = c('auid.x' = 'auid')) %>%
    left_join(author_meta, by = c('auid.y' = 'auid'),
              suffix = c('.x', '.y')) %>%
    unnest(oru.x, .drop = FALSE) %>%
    unnest(oru.y) %>%
    filter(oru.x != oru.y) %>%
    group_by(k, auid = auid.x, oru = oru.x) %>%
    summarize(ext_min_dist = min(h_dist)) %>%
    ungroup()


## Silhouette plot
# full_join(oru_dist, comp_dist, by = c('k', 'auid')) %>% 
full_join(interior_mean_dist, exterior_min_dist) %>% 
    ggplot(aes(int_mean_dist, ext_min_dist,
               color = oru)) +
    geom_point() +
    stat_function(fun = identity, linetype = 'dashed',
                  inherit.aes = FALSE) +
    facet_wrap(vars(k)) +
    coord_equal()

## MDS viz of Hellinger distances
mds_coords = dist %>%
    split(.$k) %>%
    map(select, -k) %>%
    map(spread, auid.y, h_dist) %>%
    map(column_to_rownames, var = 'auid.x') %>%
    map(as.dist) %>%
    map(cmdscale, k = 2) %>%
    map(as.data.frame) %>%
    map(rownames_to_column, var = 'auid') %>%
    map(as_tibble) %>%
    bind_rows(.id = 'k')

right_join(author_meta,
          mds_coords) %>%
    mutate(k = as.integer(k),
           name = paste(given_name, surname)) %>%
    ## For the moment, filter one author who isn't in the topic model
    filter(auid != '57203386115') %>%
    # filter(oru_lgl) %>%
    unnest(oru) %>%
    ggplot(aes(V1, V2, color = oru)) +
    geom_point(aes(label = name, fill = oru),
               color = 'black',
               show.legend = FALSE,
               shape = 21L) +
    geom_mark_ellipse(aes(filter = oru_lgl
                          # label = oru
    ),
    size = .8,
    show.legend = FALSE) +
    geom_dl(aes(label = oru), method = 'top.bumptwice') +
    # scale_color_brewer(palette = 'Set1') +
    # scale_fill_brewer(palette = 'Set1') +
    scale_color_viridis_d(option = 'A', direction = -1) +
    scale_fill_viridis_d(option = 'A', direction = -1) +
    coord_equal() +
    facet_wrap(vars(k), ncol = 2, scales = 'fixed') +
    theme_void() +
    theme(panel.border = element_rect(fill = 'transparent')) +
    # theme(panel.background = element_rect(fill = 'grey90'),
    #       legend.background = element_rect(fill = 'grey90'))
    ggtitle('MDS visualization of Hellinger distances between researchers',
            subtitle = Sys.time())

ggsave(str_c(plots_dir, '12_mds.png'), 
       height = 8, width = 8.5)

# Similarly, but faceting out by department
author_meta %>% 
    filter(auid %in% mds_coords$auid) %>% 
    unnest(department) %>% 
    # count(department) %>% arrange(desc(n))
    add_count(department) %>% 
    filter(n >= 5) %>%
    left_join(mds_coords) %>%
    mutate(k = as.integer(k)) %>%
    filter(k == 85) %>% 
    ggplot(aes(V1, V2, 
               color = oru_lgl)) +
    geom_point() +
    coord_equal() +
    facet_wrap(vars(k, department)) +
    theme_void() +
    theme(panel.border = element_rect(fill = 'transparent'))

# plotly::ggplotly()
