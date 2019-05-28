## This script uses propensity scores to match ORU researchers to comparators
library(tidyverse)
library(broom)
library(cowplot)

theme_set(theme_minimal())

## To visualize matching structure
library(tidygraph)
library(ggraph)

library(assertthat)

data_dir = '../data/'
plots_dir = '../plots/'

dept_thresh = 3 ## Threshold # of researchers required to keep a department

year_thresh = 3 ## Max. difference in first_years for matching
caliper = .25 ## Width of caliper for matching, in sd of logit propensity scores

## Load data ----
dropouts_03 = read_rds(str_c(data_dir, '03_dropout.Rds'))
oru_df = read_rds(str_c(data_dir, '03_matched.Rds')) %>% 
    anti_join(dropouts_03, by = 'auid')
departments = read_rds(str_c(data_dir, '03_codepartmentals.Rds'))

author_meta = read_rds(str_c(data_dir, '04_author_meta.Rds')) %>% 
    mutate(gender = as.character(fct_explicit_na(gender))) %>% 
    left_join(oru_df, by = 'auid', suffix = c('', '.oru')) %>% 
    select(auid:probability, 
           oru = ORU) %>% 
    mutate(oru_lgl = !is.na(oru), 
           oru = fct_explicit_na(oru), 
           oru = as.character(oru)) %>% 
    group_by_at(vars(-oru)) %>% 
    summarize(oru = list(oru)) %>% 
    ungroup()

assert_that(nrow(author_meta) == n_distinct(author_meta$auid), 
            msg = 'author_meta does not have 1 row per author')


## Construct df for fitting propensity model ----
dept_dummies = departments %>%
    add_count(aff_name) %>% 
    filter(n >= dept_thresh) %>% 
    select(-n) %>% 
    mutate(value = 1L) %>% 
    spread(key = aff_name, value = value, fill = 0L)

## Confirm that this dummy construction is full rank
n_depts = ncol(dept_dummies) - 1L
dept_dummies %>% 
    column_to_rownames(var = 'auid') %>% 
    Matrix::rankMatrix() %>% 
    {. == n_depts} %>% 
    assert_that(msg = 'dept_dummies is not full rank')

## How many authors are lost w/ this department threshold? 
## Just 2 ORU authors
anti_join(author_meta, dept_dummies) %>% 
    count(oru_lgl)


## Propensity model ----
prop_model = author_meta %>% 
    select(oru_lgl, auid, gender, first_year) %>% 
    inner_join(dept_dummies) %>% 
    column_to_rownames(var = 'auid') %>% 
    glm(oru_lgl ~ ., 
        data = ., 
        family = binomial)

## Distribution of propensity scores
prop_model %>% 
    augment() %>% 
    mutate(.fitted.rs = scale(.fitted)) %>% 
    ggplot(aes(.fitted.rs, color = oru_lgl)) +
    geom_density() +
    geom_rug()

scores = prop_model %>% 
    augment() %>% 
    mutate(.fitted.rs = as.vector(scale(.fitted))) %>% 
    transmute(auid = .rownames, score = .fitted.rs) %>% 
    inner_join(author_meta) %>% 
    left_join(departments) %>% 
    group_by_at(vars(-aff_name, -oru)) %>% 
    summarize(departments = list(aff_name)) %>% 
    ungroup()    


## Matching ----
full_comp = tidyr::crossing(filter(scores, oru_lgl),
                            filter(scores, !oru_lgl)) %>% 
    mutate(score_diff = abs(score - score1), 
           first_year_diff = abs(first_year - first_year1)) %>% 
    arrange(score_diff)

matched = full_comp %>% 
    # filter(gender_namsor == gender_namsor1) %>% 
    filter(score_diff < caliper, first_year_diff <= year_thresh) %>% 
    group_by(auid) %>% 
    filter(score_diff == min(score_diff)) %>%
    ungroup()

matched %>%
    select(auid, auid1,
           surname, surname1,
           score_diff,
           first_year, first_year1,
           n_docs, n_docs1,
           gender, gender1,
           departments, departments1) %>%
    view()

## 9 ORU authors can't be matched this way
unmatched = author_meta %>% 
    filter(oru_lgl) %>% 
    filter(auid %in% dept_dummies$auid) %>% 
    anti_join(matched, by = 'auid')
nrow(unmatched)

## Checking minimal score differences for unmatched ORU authors
right_join(full_comp, unmatched, by = 'auid') %>% 
    mutate(inside_caliper = score_diff < caliper, 
           close_years = first_year_diff <= year_thresh) %>% 
    filter(close_years) %>%
    group_by(auid) %>% 
    summarize(any_inside_caliper = any(inside_caliper), 
              # any_close_years = any(close_years), 
              any_both = any(inside_caliper & close_years), 
              min_score = min(score_diff), 
              min_year = min(first_year_diff))


## Tidygraph to visualize matching structure ----
matched %>% 
    select(auid, auid1) %>% 
    as_tbl_graph() %>% 
    left_join(author_meta, by = c('name' = 'auid')) %>% 
    ggraph() +
    geom_edge_link() +
    geom_node_point(aes(color = oru_lgl)) +
    scale_color_brewer(palette = 'Set1') +
    coord_equal() +
    theme_graph()
ggsave(str_c(plots_dir, '05_matches.png'))


## Checking match quality ----
## Differences in scores vs. ORU researcher score
ggplot(matched, aes(x = score_diff, y = score, 
                    ymin = score, ymax = score1)) +
    geom_linerange(size = 3) +
    coord_flip() +
    theme_minimal()

## Gender
gender_before = ggplot(author_meta, aes(oru_lgl, 
                                        fill = gender)) +
    geom_bar(position = 'fill')

gender_after = ggplot(matched) +
    geom_bar(aes(x = oru_lgl, fill = gender), 
             position = 'fill') +
    geom_bar(aes(x = oru_lgl1, fill = gender), 
             position = 'fill')

plot_grid(gender_before, gender_after)

## First year of publication
first_year_before = ggplot(author_meta, aes(first_year, 
                                            color = oru_lgl)) +
    geom_density() +
    geom_rug()

first_year_after = ggplot(matched) +
    geom_density(aes(first_year, color = oru_lgl)) +
    geom_density(aes(first_year1, color = oru_lgl1))

plot_grid(first_year_before, first_year_after)

## Departments
## These shares aren't actually shares of author, but they're good enough for the moment
depts_before = departments %>% 
    add_count(aff_name) %>% 
    filter(n >= dept_thresh) %>% 
    inner_join(author_meta) %>% 
    ## Calculate "shares"
    count(aff_name, oru_lgl) %>% 
    group_by(oru_lgl) %>% 
    mutate(share = n / sum(n)) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ## Reshape for ggplot
    spread(key = oru_lgl, value = share, fill = 0) %>% 
    rename(oru = `TRUE`, 
           comparison = `FALSE`) %>% 
    arrange(oru, comparison) %>% 
    mutate(aff_name = fct_inorder(aff_name)) %>% 
    ## Plot
    ggplot(aes(aff_name)) +
    geom_linerange(aes(ymin = comparison, ymax = oru)) +
    geom_point(aes(y = oru, color = 'oru')) +
    geom_point(aes(y = comparison, color = 'comparison')) +
    coord_flip()

depts_after = matched %>% 
    ## Extract the control group columns
    select(auid1, oru_lgl1, departments1) %>% 
    rename(auid = auid1, oru_lgl = oru_lgl1, 
           departments = departments1) %>% 
    ## Combine with the ORU columns
    bind_rows(select(matched, auid, oru_lgl, departments)) %>% 
    unnest(departments) %>% 
    rename(aff_name = departments) %>% 
    ## Calculate "shares"
    count(aff_name, oru_lgl) %>% 
    group_by(oru_lgl) %>% 
    mutate(share = n / sum(n)) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ## Reshape for ggplot
    spread(key = oru_lgl, value = share, fill = 0) %>% 
    rename(oru = `TRUE`, 
           comparison = `FALSE`) %>% 
    arrange(oru, comparison) %>% 
    mutate(aff_name = fct_inorder(aff_name)) %>% 
    ## Plot
    ggplot(aes(aff_name)) +
    geom_linerange(aes(ymin = comparison, ymax = oru)) +
    geom_point(aes(y = oru, color = 'oru')) +
    geom_point(aes(y = comparison, color = 'comparison')) +
    coord_flip()

cowplot::plot_grid(depts_before + 
                       scale_x_discrete(breaks = NULL), 
                   depts_after +
                       scale_x_discrete(breaks = NULL))


## Dropouts ----
oru_df %>% 
    anti_join(matched, by = 'auid') %>% 
    write_rds(str_c(data_dir, '05_dropouts.Rds'))

write_rds(matched, str_c(data_dir, '05_matched.Rds'))

## Author metadata, with weights, for regression models
## NB doesn't include department dummies
reg_df = tibble(auid = c(matched$auid, matched$auid1)) %>% 
    count(auid, name = 'match_occurrences') %>% 
    left_join(author_meta, by = 'auid')

write_rds(reg_df, str_c(data_dir, '05_author_meta.Rds'))

## Department dummies matrix
dept_dummies %>% 
    filter(auid %in% reg_df$auid) %>% 
    write_rds(str_c(data_dir, '05_dept_dummies.Rds'))
