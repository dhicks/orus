#' ----
#' title: "Analysis"
#' author: "Dan Hicks"
#' 
#' toc: true
#' ----

library(tidyverse)
library(broom)
library(stm)
library(tidytext) ## for stm tidiers

library(igraph)
library(tidygraph)
library(ggraph)
# devtools::install_github("schochastics/smglr")
library(graphlayouts)

library(gghighlight)
library(ggforce)
library(ggridges)
library(directlabels)
library(patchwork)
theme_set(theme_minimal())

library(knitr)
library(kableExtra)

library(tictoc)
library(assertthat)

source('../R/hellinger.R')

data_dir = '../data/'
plots_dir = '../plots/'

## Values of k to use in gamma_sm
selected_k = c(25, 50, 100, 150)
# selected_k = c(85)

## Nice labels for regression estimate plots
term_labels = tribble(
    ~ term, ~ label, 
    '(Intercept)', 'intercept', 
    'first_year_1997', 'first pub. year',
    'genderwoman', 'gender: woman', 
    'gender(Missing)', 'gender: unknown', 
    'oru_lglTRUE', 'ORU affiliation', 
    'log_n_coauths', 'coauthors (log count)', 
    'log_n_docs', 'publications (log count)'
)



## Load data ----
models = read_rds(str_c(data_dir, '10_models.Rds'))

model_stats = read_rds(str_c(data_dir, '10_model_stats.Rds'))

H = read_rds(str_c(data_dir, '09_H.Rds'))
auids = H$atm %>% 
    spread(key = text, value = n) %>% 
    pull(auid)

coauths_df = read_rds(str_c(data_dir, '07_coauth_count.Rds'))

author_meta = read_rds(str_c(data_dir, '05_author_meta.Rds')) %>% 
    mutate(first_year_1997 = first_year - 1997, 
           # sub gender terms for sex terms
           gender = fct_recode(gender, 
                               'man' = 'male', 
                               'woman' = 'female'),
           # use men as ref level in regressions
           gender = fct_relevel(gender, 'man'),
           oru = map(oru, str_replace, '\\(Missing\\)', 
                     '(comparison)')) %>% 
    left_join(coauths_df) %>% 
    mutate(log_n_docs = log10(n_docs), 
           log_n_coauths = log10(n_coauthors))

assert_that(all(!is.na(author_meta$n_coauthors)))

dept_dummies = read_rds(file.path(data_dir, '11_dept_dummies.Rds'))

au_dept_xwalk = read_rds(file.path(data_dir, '11_au_dept_xwalk.Rds'))

test_train = read_rds(file.path(data_dir, '11_test_train.Rds'))


## Extract topic models gamma ----
matrices = models %>% 
    mutate(beta = map(model, tidy, 
                      matrix = 'beta'),
           gamma = map(model, tidy, 
                       matrix = 'gamma', document_names = auids))

gamma = matrices %>% 
    select(-model, -beta) %>% 
    unnest(gamma) %>% 
    rename(auid = document) %>% 
    left_join(author_meta)

gamma_50 = filter(gamma, k == 50)
gamma_sm = filter(gamma, k %in% selected_k)

dept_gamma = read_rds(file.path(data_dir, '11_dept_gamma.Rds'))


## Term lists ----
beta_50 = matrices %>% 
    filter(k == 50) %>% 
    pull(beta) %>% 
    flatten() %>% 
    as_tibble()

beta_50 %>% 
    group_by(topic) %>% 
    slice_max(beta, n = 5) %>% 
    arrange(topic, beta) %>% 
    kable(digits = 4, 
          format = 'latex', 
          longtable = TRUE, 
          booktabs = TRUE, 
          escape = TRUE,
          label = 'beta_table', 
          caption = 'Top 5 terms (noun phrases) in each topic, $k=50$ model') %>% 
    # collapse_rows(columns = 1) %>% 
    write_lines(file.path(plots_dir, '12_beta.tex'))
    

## Plot showing sample ----
author_meta %>% 
    select(oru) %>% 
    unnest(oru) %>% 
    ggplot(aes(oru, fill = oru)) +
    geom_bar(show.legend = FALSE) +
    stat_count(aes(y = ..count.., label = ..count..), 
               position = position_nudge(y = 1),
               geom = 'text') +
    xlab('ORU') +
    scale_y_sqrt() +
    ggtitle('Count of researchers by ORU', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_sample.png'), 
       height = 4, width = 7)


## Gender distribution ----
gender_coarse_plot = ggplot(author_meta, 
                            aes(oru_lgl, fill = gender)) +
    geom_bar(position = position_fill()) +
    scale_fill_brewer(palette = 'Set2') +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = 'ORU-affiliated', 
         y = 'share')
gender_coarse_plot

gender_fine_plot = author_meta %>% 
    select(gender, oru) %>% 
    unnest(oru) %>% 
    ggplot(aes(oru, fill = gender)) +
    geom_bar(position = position_fill()) +
    scale_fill_brewer(palette = 'Set2') +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = 'ORU', 
         y = 'share')
gender_fine_plot

gender_coarse_plot + labs(title = 'A') +
    gender_fine_plot + labs(title = 'B') +
    plot_layout(guides = 'collect')
ggsave(str_c(plots_dir, '12_gender.png'), 
       height = 4, width = 7, scale = 2)



## Network viz ----
## author-department network
dept_net = au_dept_xwalk %>% 
    graph_from_data_frame(directed = FALSE) %>% 
    as_tbl_graph() %>% 
    left_join(author_meta, by = c(name = 'auid')) %>%
    mutate(type = case_when(is.na(oru_lgl) ~ 'department',
                            oru_lgl ~ 'ORU faculty',
                            !oru_lgl ~ 'other authors', 
                            TRUE ~ 'error'))
## author-ORU network 
oru_net = author_meta %>% 
    filter(oru_lgl) %>% 
    select(-where(is.list), oru) %>% 
    unnest(oru) %>% 
    select(auid, oru) %>% 
    graph_from_data_frame(directed = FALSE) %>% 
    as_tbl_graph() %>% 
    left_join(author_meta, by = c(name = 'auid')) %>%
    mutate(type = case_when(is.na(oru_lgl) ~ 'ORU',
                            oru_lgl ~ 'ORU faculty',
                            !oru_lgl ~ 'other authors', 
                            TRUE ~ 'error'))

## Combined
net = graph_join(dept_net, oru_net, by = c('name', 'type')) %>%
    as.undirected() %>%
    as_tbl_graph() %>%
    mutate(degree = centrality_degree(),
           btwn = centrality_betweenness())

## Degree distributions for different node types
net %>%
    as_tibble() %>%
    group_by(type) %>%
    summarize(n = n(), 
              across(degree, lst(min, mean, median, max)))

## 110 sec
layout_file = str_c(data_dir, '12_layout.Rds')
if (!file.exists(layout_file)) {
    ## 110 sec
    stress = layout_with_stress(net)
    stress = stress %>%
        as_tibble() %>%
        rename(x = V1, y = V2)
    write_rds(stress, str_c(data_dir, layout_file))
} else {
    stress = read_rds(str_c(data_dir, layout_file))
}

# # ## 105 sec
# # tic()
# # backbone = layout_as_backbone(net)
# # toc()
# 
# graph_attr(thing, 'layout') = NULL
# 
net %>%
    mutate(x = stress$x,
           y = stress$y) %>%
    filter(!(degree == 1 & type == 'department')) %>%
    `graph_attr<-`('layout', data.frame(x = V(.)$x,
                                        y = V(.)$y)) %>%
    ggraph(layout = 'nicely') +
    geom_edge_link(alpha = .5) +
    geom_node_point(aes(color = type, size = btwn)) +
    geom_node_label(aes(label = name), 
                    alpha = .5, fill = 'yellow',
                    data = function(dataf) {
                        subset(dataf, type == 'ORU')
                    }) +
    geom_node_label(aes(label = name),
                    size = 1, alpha = .25,
                    data = function(dataf) {
                        subset(dataf, type == 'department')
                    }) +
    scale_color_manual(values = c('purple', 'yellow',
                                  'red', 'darkblue')) +
    # scale_color_viridis_d(option = 'E', direction = -1) +
    labs(size = 'betweenness centrality', 
         color = 'node type') +
    theme_graph()
ggsave(str_c(plots_dir, '12_network.png'),
       height = 10, width = 15, dpi = 300, scale = .75)

## ORU-dep't network
oru_dept_net = author_meta %>% 
    filter(oru_lgl) %>% 
    select(auid, oru) %>% 
    unnest(oru) %>% 
    left_join(au_dept_xwalk, by = 'auid') %>% 
    count(oru, dept) %>% 
    graph_from_data_frame(directed = FALSE) %>% 
    as_tbl_graph() %>% 
    mutate(type = case_when(name %in% au_dept_xwalk$dept ~ 'department',
                            TRUE ~ 'ORU'))

oru_dept_layout = layout_with_stress(oru_dept_net)

oru_dept_net %>%
    mutate(x = oru_dept_layout[,1],
           y = oru_dept_layout[,2]) %>%
    `graph_attr<-`('layout', data.frame(x = V(.)$x,
                                        y = V(.)$y)) %>%
    ggraph(layout = 'nicely') +
    geom_edge_link(aes(alpha = n, width = n)) +
    geom_node_point(aes(color = type)) +
    geom_node_label(aes(label = name), 
                    alpha = .5,
                    data = function(dataf) {
                        subset(dataf, type == 'ORU')
                    }) +
    geom_node_text(aes(label = name),
                   size = 2,
                   data = function(dataf) {
                       subset(dataf, type == 'department')
                   }) +
    scale_color_manual(values = c('purple', 'yellow')) + 
    scale_edge_width(range = c(.5, 3)) +
    theme_graph()
ggsave(str_c(plots_dir, '12_oru_dept_network.png'),
       height = 10, width = 15, dpi = 300, scale = .6)



## Coauthor count ----
n_coauths_lm = author_meta %>% 
    select(log_n_coauths, auid, oru_lgl, 
           first_year_1997, gender) %>% 
    left_join(dept_dummies) %>% 
    lm(log_n_coauths ~ . - auid, data = .)

n_coauths_lm %>% 
    augment() %>% 
    ggplot(aes(.resid)) +
    geom_density() +
    geom_rug(aes(color = oru_lgl))

n_coauths_lm %>% 
    augment() %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point(aes(color = oru_lgl)) + 
    geom_smooth()

tidy(n_coauths_lm, conf.int = TRUE) %>% 
    filter(term == 'oru_lglTRUE') %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.)

tidy(n_coauths_lm, conf.int = TRUE) %>% 
    mutate(term = str_remove_all(term, '`')) %>% 
    mutate(var_group = case_when(
        term %in% names(dept_dummies) ~ 'department',
        str_detect(term, 'Intercept') ~ 'intercept',
        TRUE ~ 'other terms'
    )) %>% 
    filter(var_group != 'department') %>% 
    left_join(term_labels) %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.) %>% 
    arrange(desc(estimate)) %>% 
    mutate(label = fct_inorder(label)) %>% 
    ggplot(aes(label, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    ## gghighlight overrides facets
    # gghighlight(term == 'oru_lglTRUE',
    #             unhighlighted_colour = alpha('blue', .25)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    coord_flip() +
    facet_wrap(vars(var_group), scales = 'free',
               ncol = 2) +
    xlab('covariate') +
    ylab('estimate (fold change in coauthor count)') +
    ggtitle('Est. effect of ORU affiliation on coauthor counts',
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_coauths_regression.png'), 
       width = 6, height = 3, scale = 1)


## Number of documents ----
n_docs_lm = author_meta %>% 
    select(log_n_docs, auid, oru_lgl, first_year_1997, 
           gender, log_n_coauths) %>% 
    left_join(dept_dummies) %>% 
    lm(log_n_docs ~ . - auid, 
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
    filter(term %in% c('oru_lglTRUE', 'log_n_coauths')) %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.)

tidy(n_docs_lm, conf.int = TRUE) %>% 
    mutate(term = str_remove_all(term, '`')) %>% 
    mutate(var_group = case_when(
        term %in% names(dept_dummies) ~ 'department',
        str_detect(term, 'Intercept') ~ 'intercept',
        TRUE ~ 'other terms'
    )) %>% 
    filter(var_group != 'department') %>% 
    left_join(term_labels) %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.) %>% 
    arrange(desc(estimate)) %>% 
    mutate(label = fct_inorder(label)) %>% 
    ggplot(aes(label, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    ## gghighlight overrides facets
    # gghighlight(term == 'oru_lglTRUE',
    #             unhighlighted_colour = alpha('blue', .25)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    coord_flip() +
    facet_wrap(vars(var_group), scales = 'free',
               ncol = 2) +
    xlab('covariate') +
    ylab('estimate (fold change in publication count)') +
    ggtitle('Est. effect of ORU affiliation on publication counts',
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_pub_regression.png'), 
       width = 6, height = 3, scale = 1)


## Citation counts ----
cites_lm = author_meta %>% 
    select(cited_by_count, auid, oru_lgl, first_year_1997,
           gender, log_n_docs, log_n_coauths) %>% 
    left_join(dept_dummies) %>% 
    # filter(cited_by_count > 0) %>% 
    lm(log10(cited_by_count+1) ~ . - auid, 
       data = .)

## As of 01-2021, bug means .resid isn't reported
## <https://github.com/tidymodels/broom/issues/937>
cites_lm %>% 
    augment() %>% 
    ggplot(aes(.std.resid)) +
    geom_density() +
    geom_rug(aes(color = oru_lgl))

cites_lm %>% 
    augment() %>% 
    ggplot(aes(.fitted, .std.resid)) +
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
    filter(term %in% c('oru_lglTRUE', 
                       'log_n_coauths', 'log_n_docs')) %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.)

cites_lm %>% 
    tidy(conf.int = TRUE) %>% 
    mutate(term = str_remove_all(term, '`')) %>% 
    mutate(var_group = case_when(
        term %in% names(dept_dummies) ~ 'department', 
        str_detect(term, 'Intercept') ~ 'intercept',
        str_detect(term, 'docs|coauths') ~ 'publications & coauthors',
        TRUE ~ 'other terms'
    )) %>% 
    filter(var_group != 'department') %>% 
    left_join(term_labels) %>% 
    mutate_at(vars(estimate, conf.low, conf.high), 
              ~ 10^.) %>% 
    arrange(desc(estimate)) %>% 
    mutate(label = fct_inorder(label)) %>% 
    ggplot(aes(label, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    # gghighlight(term == 'oru_lglTRUE', 
    #             unhighlighted_colour = alpha('blue', .25)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    coord_flip() +
    facet_wrap(vars(var_group), scales = 'free', ncol = 2) +
    xlab('covariate') +
    ylab('estimate (fold change in total citations received)') +
    ggtitle('Est. effect of ORU affiliation on citation counts', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_cites_regression.png'), 
       width = 6, height = 4, scale = 1)

## Topic models ----

model_stats %>% 
    select(k, 
           coherence = semantic_coherence, ## want *high*
           exclusivity,        ## want *high*?
           ho_likelihood,      ## want *high*
           residuals,          ## want *about 1*
           # lbound,             ## ??
           iterations          ## want low, but less important
    ) %>%
    gather(key = 'statistic', value = 'value', -k) %>% 
    ggplot(aes(k, value)) +
    geom_point() +
    geom_line() +
    facet_wrap(vars(statistic), scales = 'free')

## Closer look at SC and EX ----
## Scatterplot suggests models are spread along the SC-EX frontier
ggplot(model_stats, aes(semantic_coherence, exclusivity)) +
    geom_text(aes(label = k))

model_stats %>% 
    mutate(sc_ex = semantic_coherence * exclusivity) %>% 
    select(k, sc_ex, semantic_coherence, exclusivity) %>% 
    arrange(desc(sc_ex))

## In script 10, PCs suggested k = 45 would be reasonable
## Plotting just the means, it looks like there's a continuing substantial decrease past k = 45
## But plotting the values for each topic, the distributions don't look that different
model_stats %>% 
    select(k, semantic_coherence_topicwise) %>% 
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
    select(k, exclusivity_topicwise) %>% 
    unnest(exclusivity_topicwise) %>% 
    ggplot(aes(k, exclusivity_topicwise)) +
    # geom_point() +
    geom_sina(aes(group = k)) +
    geom_line(data = model_stats, 
              aes(y = exclusivity), 
              color = 'red')


## Topic-author entropy ----
## Author-level topic distributions, grouped by ORU, k = 50
at_plot = gamma_50 %>% 
    select(-where(is.list), oru) %>% 
    unnest(oru) %>% 
    ggplot(aes(auid, topic, fill = gamma)) +
    geom_raster() +
    scale_fill_viridis_c(option = 'A') +
    facet_wrap(vars(oru), scales = 'free') +
    coord_flip()
at_plot

## And same for 85
# gamma_sm %>% 
#     filter(k == 50) %>% 
#     select(-where(is.list), oru) %>% 
#     unnest(oru) %>% 
#     {at_plot %+% .}

## ORU-level topic distributions
gamma_oru = gamma %>% 
    select(-where(is.list), oru) %>% 
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

H_50 = filter(H_gamma, k == 50)

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
    left_join(au_dept_xwalk, by = 'auid') %>% 
    select(auid, dept) %>% 
    filter(!duplicated(.)) %>% 
    add_count(dept) %>% 
    filter(n > 60) %>% 
    inner_join(gamma_50, by = 'auid') %>% 
    ggplot(aes(topic, auid, fill = gamma)) +
    geom_raster() +
    scale_fill_viridis_c(option = 'A', 
                         trans = 'sqrt') +
    facet_wrap(vars(k, dept), scales = 'free', 
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
           log_n_docs, log_n_coauths) %>% 
    left_join(dept_dummies) %>% 
    group_by(k) %>% 
    do(model = lm(H ~ . - auid, 
                  data = .
    )) %>% 
    ungroup() %>% 
    mutate(glance = map(model, glance), 
           coefs = map(model, tidy, conf.int = TRUE))

H_lm %>% 
    select(-where(is.list), glance) %>% 
    unnest(glance)

H_lm %>% 
    select(-where(is.list), coefs) %>% 
    unnest(coefs) %>% 
    filter(term == 'oru_lglTRUE') %>% 
    ggplot(aes(k, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    gghighlight(k == 50) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    xlab('number of topics (k)') +
    ylab('estimate (bits)') +
    ggtitle('Est. effect of ORU affiliation on topic entropy', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_entropy_regression.png'), 
       width = 6, height = 4, scale = 1.5)


## Silhouette analysis ----
## See the scratch file `Hellinger_low_memory.R` for an attempt to include non-ORU authors
make_cross = function(dataf, join_cols = c('k', 'topic')) {
    full_join(dataf, dataf, by = join_cols)
}

set.seed(2021-05-27)
codept_sample = author_meta %>% 
    filter(!oru_lgl) %>% 
    sample_n(300) %>% 
    pull(auid)

## ~2 sec
tic()
crossed = gamma_sm %>%
    filter(oru_lgl|auid %in% codept_sample) %>%
    select(k, topic, auid, gamma) %>%
    group_by(k, topic) %>%
    group_split() %>% #str(max.level = 1)
    map_dfr(make_cross) %>% 
    filter(auid.x != auid.y)
toc()

## Hellinger distances for all pairs
## ~1.4 sec for k = 5, 45, 85, 125
tic()
dist = hellinger(crossed)
toc()

## Mean distance w/in ORUs
## NB crossed should already eliminate self-pairs
interior_mean_dist = dist %>%
    left_join(author_meta, by = c('auid.x' = 'auid')) %>%
    left_join(author_meta, by = c('auid.y' = 'auid'),
              suffix = c('.x', '.y')) %>%
    filter(oru_lgl.x, oru_lgl.y) %>% 
    select(-where(is.list), matches('oru')) %>% 
    unnest(c(oru.x, oru.y)) %>% 
    filter(oru.x == oru.y) %>%
    group_by(k, auid = auid.x, oru = oru.x) %>%
    summarize(int_mean_dist = mean(h_dist), 
              int_min_dist = min(h_dist)) %>%
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
    select(-where(is.list), matches('oru')) %>% 
    unnest(c(oru.x, oru.y)) %>% 
    filter(oru.x != oru.y) %>%
    group_by(k, auid = auid.x, oru = oru.x) %>%
    summarize(ext_min_dist = min(h_dist)) %>%
    ungroup()

## Silhouette plot
# full_join(oru_dist, comp_dist, by = c('k', 'auid')) %>% 
full_join(interior_mean_dist, exterior_min_dist) %>% 
    ggplot(aes(int_min_dist, ext_min_dist,
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
    # map(MASS::isoMDS, k = 2) %>% map('points') %>%
    # map(MASS::sammon, k = 2) %>% map('points') %>% 
    map(as.data.frame) %>%
    map(rownames_to_column, var = 'auid') %>%
    map(as_tibble) %>%
    bind_rows(.id = 'k')

## MDS check
## On average MDS distance corresponds to Hellinger distance
## But MDS distances can be 0 even for large Hellinger distance
full_join(mds_coords, mds_coords, by = 'k') %>% 
    ## Pairwise Euclidean distances
    filter(auid.x != auid.y) %>% 
    mutate(mds_dist = sqrt((V1.x-V1.y)^2 + (V2.x-V2.y)^2)) %>% 
    select(k, auid.x, auid.y, mds_dist) %>% 
    mutate(k = as.integer(k)) %>% 
    ## Join w/ Hellinger distances
    inner_join(dist, by = c('k', 'auid.x', 'auid.y')) %>% 
    ## Plot
    filter(auid.x > auid.y) %>% 
    ggplot(aes(h_dist, mds_dist)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(vars(k), scales = 'free')

mds_plot = right_join(author_meta,
                      mds_coords) %>%
    mutate(k = as.integer(k),
           name = paste(given_name, surname)) %>%
    select(-where(is.list), oru) %>% 
    unnest(oru) %>%
    ggplot(aes(V1, V2, color = oru)) +
    geom_point(aes(label = name, fill = oru, alpha = oru_lgl),
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
    scale_alpha_discrete(range = c(.5, 1)) +
    coord_equal() +
    facet_wrap(vars(k), ncol = 2, scales = 'fixed') +
    theme_void() +
    theme(panel.border = element_rect(fill = 'transparent')) +
    # theme(panel.background = element_rect(fill = 'grey90'),
    #       legend.background = element_rect(fill = 'grey90'))
    ggtitle('MDS visualization of Hellinger distances between researchers',
            subtitle = Sys.time())
mds_plot

ggsave(str_c(plots_dir, '12_mds.png'), 
       height = 8, width = 8.5)

ggsave(str_c(plots_dir, '12_mds_wide.png'), 
       plot = mds_plot + facet_wrap(vars(k), ncol = 4, scales = 'fixed'), 
       height = 4, width = 12)


# Similarly, but faceting out by department
author_meta %>% 
    filter(auid %in% mds_coords$auid) %>% 
    # select(-where(is.list), department) %>% 
    unnest(oru) %>% 
    left_join(au_dept_xwalk, by = 'auid') %>% 
    add_count(dept) %>% 
    filter(n >= 10) %>%
    left_join(mds_coords) %>%
    mutate(k = as.integer(k)) %>%
    filter(k == 100) %>%
    ggplot(aes(V1, V2, 
               fill = oru)) +
    geom_point(aes(alpha = oru_lgl), shape = 21L) +
    coord_equal() +
    facet_wrap(vars(k, dept)) +
    theme_void() +
    scale_fill_viridis_d(option = 'A', direction = -1) +
    scale_alpha_discrete(range = c(.5, 1)) +
    theme(panel.border = element_rect(fill = 'transparent'))

# plotly::ggplotly()



## Author-department distance ----
## Departments of interest:  >= 40 non-ORU authors
# depts_of_interest = author_meta %>% 
#     filter(!oru_lgl) %>% 
#     select(department) %>% 
#     unnest(department) %>% 
#     left_join(dept_xwalk) %>% 
#     count(dept_canonical) %>% 
#     filter(n >= 40) %>% 
#     pull(dept_canonical)
# 
# ## Split non-ORU authors from these departments
# set.seed(2019-06-05)
# train_authors = author_meta %>% 
#     filter(!oru_lgl) %>% 
#     select(department, auid) %>% 
#     unnest(department) %>% 
#     left_join(dept_xwalk) %>% 
#     filter(dept_canonical %in% depts_of_interest) %>% 
#     group_by(department) %>% 
#     sample_frac(size = .5) %>% 
#     ungroup() %>% 
#     pull(auid)
# 
# ## Departmental-mean topic distributions
# gamma_dept = gamma %>% 
#     filter(auid %in% train_authors) %>% 
#     select(-where(is.list), department) %>% 
#     unnest(department) %>% 
#     left_join(dept_xwalk) %>% 
#     select(-department) %>% 
#     rename(department = dept_canonical) %>% 
#     filter(department %in% depts_of_interest) %>% 
#     ## Department mean topic distributions
#     group_by(k, topic, department) %>% 
#     summarize(gamma_dept = mean(gamma)) %>% 
#     ungroup()
# 

tic()
dept_dist = gamma %>%
    select(-department) %>% 
    left_join(au_dept_xwalk, by = 'auid') %>%
    inner_join(dept_gamma, by = c('k', 'topic',
                                  'dept'), 
               suffix = c('', '_dept')) %>%
    filter(! auid %in% test_train$train) %>%
    hellinger(gamma, gamma_dept, auid, dept) %>%
    left_join(author_meta, by = 'auid',
              suffix = c('', '_meta'))
toc()

ggplot(dept_dist, aes(h_dist)) +
    geom_density() +
    facet_wrap(vars(k), scales = 'free')

ggplot(dept_dist, aes(dept, h_dist, color = oru_lgl)) +
    # geom_point() +
    stat_summary() +
    facet_wrap(vars(k), scales = 'free_x') +
    coord_flip()

# ggplot(divergence, aes(department, div, color = gender)) +
#     stat_summary() +
#     facet_wrap(vars(k))

dist_lm = dept_dist %>% 
    mutate(log_n_docs = log10(n_docs)) %>% 
    select(k, h_dist, auid, oru_lgl, first_year_1997, 
           gender, log_n_docs, log_n_coauths, dept) %>% 
    group_nest(k) %>% 
    mutate(model = map(data, ~lm(h_dist ~ . - auid, data = .)))

dist_lm %>% 
    select(-data) %>% 
    mutate(augment = map(model, augment)) %>% 
    unnest(augment) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point(aes(color = oru_lgl)) +
    geom_smooth() +
    facet_wrap(vars(k), scales = 'free')

dist_lm %>% 
    mutate(coefs = map(model, tidy, conf.int = TRUE)) %>% 
    select(-model) %>% 
    unnest(coefs) %>% 
    filter(!str_detect(term, 'department')) %>% 
    mutate(is_intercept = str_detect(term, 'Intercept')) %>% 
    ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    facet_wrap(vars(k, is_intercept), scales = 'free') +
    coord_flip()

dist_lm %>% 
    mutate(coefs = map(model, tidy, conf.int = TRUE)) %>% 
    select(-model) %>% 
    unnest(coefs) %>% 
    filter(term == 'oru_lglTRUE') %>% 
    ggplot(aes(k, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    gghighlight(k == 50) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    xlab('number of topics (k)') +
    ylab('estimate (Hellinger scale)') +
    ggtitle('Est. effect of ORU affiliation on departmental distance', 
            subtitle = Sys.time())

ggsave(str_c(plots_dir, '12_dept_dist_reg.png'), 
       width = 6, height = 4, scale = 1.5)

## Separate estimates for each ORU
dist_lm_fixed = dept_dist %>% 
    # filter(k == 45) %>% 
    select(-where(is.list), oru) %>% 
    unnest(oru) %>% 
    select(k, h_dist, auid, oru, first_year_1997, 
           gender, log_n_docs, log_n_coauths, dept) %>% 
    group_nest(k) %>% 
    mutate(model = map(data, ~lm(h_dist ~ . - auid, data = .)))

dist_lm_fixed %>% 
    mutate(coefs = map(model, tidy, conf.int = TRUE)) %>% 
    select(-where(is.list), coefs) %>% 
    unnest(coefs) %>% 
    filter(str_detect(term, 'oru')) %>% 
    mutate(term = str_remove(term, 'oru')) %>% 
    ggplot(aes(k, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    gghighlight(k == 50) +
    geom_hline(yintercept = 0, alpha = .25) +
    geom_hline(yintercept = c(-.05, .05), linetype = 'dashed') +
    facet_wrap(vars(term), scales = 'free_y') +
    xlab('ORU') +
    ylab('estimate (Hellinger scale)') +
    ggtitle('Est. effect of ORU affiliation on departmental distance', 
            subtitle = Sys.time())

ggsave(str_c(plots_dir, '12_dept_dist_fixed_reg.png'), 
       width = 6, height = 4, scale = 1.5)

## Silhouette plot, distance to codepartmentals vs. distance to co-ORUs
## Distances between ORU faculty and their non-ORU codepartmentals
codept_dist = gamma_sm %>% 
    # filter(auid %in% c('35394261000', '7005725041')) %>% 
    select(k, topic, oru_lgl, auid, gamma, auid) %>% 
    left_join(au_dept_xwalk, by = 'auid') %>% 
    filter(!duplicated(.)) %>% 
    group_split(oru_lgl) %>%   ## non-ORUs are first, ORUs are second
    reduce(full_join, 
           by = c('k', 'topic', 'dept')) %>% 
    filter(!is.na(auid.x), !is.na(auid.y)) %>% 
    hellinger(gamma1 = gamma.y, gamma2= gamma.x, 
              id1 = auid.y, id2 = auid.x, 
              dept) %>% 
    ## Codepartmental minimal distance
    group_by(k, auid = auid.y) %>% 
    summarize(min_codept_dist = min(h_dist), 
              mean_codept_dist = mean(h_dist)) %>% 
    ungroup()

inner_join(interior_mean_dist, 
           codept_dist, 
           by = c('k', 'auid')) %>% 
    ggplot(aes(int_min_dist, min_codept_dist, 
               fill = oru)) +
    # geom_mark_ellipse(aes(color = oru),
    #                   size = .8,
    #                   show.legend = FALSE) +
    # stat_density_2d(geom = 'polygon', 
    #                 aes(fill = oru, alpha = stat(nlevel)), 
    #                 show.legend = FALSE,
    #                 color = 'transparent') +
    # geom_dl(aes(color = oru, label = oru), method = 'chull.grid') +
    geom_point(shape = 21L) +
    stat_function(fun = identity, linetype = 'dashed', 
                  inherit.aes = FALSE) +
    scale_color_viridis_d(option = 'A', direction = -1, 
                          guide = FALSE) +
    scale_fill_viridis_d(option = 'A', direction = -1, 
                         guide = FALSE) +
    facet_wrap(vars(k, oru), ncol = 7) +
    coord_equal() +
    xlab('Minimal distance to co-ORU members') +
    ylab('Minimal distance to co-departmentals') +
    ggtitle('ORU vs. co-departmental distance', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_oru_dept_min_dist.png'), 
       width = 7*3*.75, height = 4*3, scale = .8)
last_plot() + aes(int_mean_dist, mean_codept_dist)
ggsave(str_c(plots_dir, '12_oru_dept_mean_dist.png'), 
       width = 7*3*.75, height = 4*3, scale = .8)


inner_join(interior_mean_dist, 
           codept_dist, 
           by = c('k', 'auid')) %>% 
    mutate(diff_min = min_codept_dist - int_min_dist, 
           diff_mean = mean_codept_dist - int_mean_dist) %>%
    ggplot(aes(diff_min, fct_rev(oru), color = oru, fill = oru)) +
    # stat_density(aes(y = stat(scaled)), position = 'identity', 
    #              fill = 'transparent') +
    geom_density_ridges(rel_min_height = 0.01, 
                        color = 'black', alpha = .7,
                        quantile_lines = TRUE, 
                        quantiles = 2, 
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.05,                                                          height = 0),
                        point_shape = '|', point_size = 2, 
                        point_alpha = 1) +
    # geom_rug() +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    scale_color_viridis_d(option = 'A', direction = -1, 
                          guide = FALSE) +
    scale_fill_viridis_d(option = 'A', direction = -1, 
                         guide = FALSE) +
    xlab('Min. departmental distance - min. ORU distance\n(Hellinger scale)') +
    ylab('ORU') +
    facet_wrap(vars(k), ncol = 2, scales = 'free') +
    ggtitle('ORU vs. co-departmental distance', 
            subtitle = Sys.time())
ggsave(str_c(plots_dir, '12_oru_dept_min_dist_ridges.png'), 
       width = 6, height = 4, scale = 1.5)
last_plot() + aes(diff_mean) +
    xlab('Mean departmental distance - mean ORU distance\n(Hellinger scale)')
ggsave(str_c(plots_dir, '12_oru_dept_mean_dist_ridges.png'), 
       width = 6, height = 4, scale = 1.5)
