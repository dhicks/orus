## This script filters the authors to get a more tractable and reasonable set of comparators
## TODO:  move 04 dropouts to 04
library(tidyverse)
library(broom)
library(cowplot)

theme_set(theme_minimal())

## To visualize matching structure
# library(tidygraph)
# library(ggraph)

library(assertthat)

data_dir = '../data/'
plots_dir = '../plots/'


filter_conditions = quos(n_docs >= 15, first_year > 1970)


## Load data ----
dropouts_03 = read_rds(str_c(data_dir, '03_dropout.Rds'))
oru_df = read_rds(str_c(data_dir, '03_matched.Rds')) %>% 
    anti_join(dropouts_03, by = 'auid') %>% 
    filter(!duplicated(.))
departments = read_rds(str_c(data_dir, '03_codepartmentals.Rds')) %>% 
    rename(department = aff_name) %>% 
    group_by(auid) %>% 
    summarize(department = list(department)) %>% 
    ungroup()

author_meta_raw = read_rds(str_c(data_dir, '04_author_meta.Rds'))

dropouts_04 = anti_join(oru_df, author_meta_raw, by = 'auid')
write_rds(dropouts_04, str_c(data_dir, '04_dropouts.Rds'))
oru_df = anti_join(oru_df, dropouts_04, by = 'auid')

author_meta = author_meta_raw %>% 
    mutate(gender = as.character(fct_explicit_na(gender))) %>% 
    left_join(oru_df, by = 'auid', suffix = c('', '.oru')) %>% 
    select(auid:probability, 
           oru = ORU) %>% 
    mutate(oru_lgl = !is.na(oru), 
           oru = fct_explicit_na(oru), 
           oru = as.character(oru)) %>% 
    group_by_at(vars(-oru)) %>% 
    summarize(oru = list(oru)) %>% 
    ungroup() %>% 
    left_join(departments, by = 'auid')

assert_that(nrow(author_meta) == n_distinct(author_meta$auid), 
            msg = 'author_meta does not have 1 row per author')

assert_that(length(setdiff(oru_df$auid, author_meta$auid)) == 0L, 
            msg = 'Some auids in oru_df not found in author_meta')



## Plots to examine impact of filtering conditions ----
author_meta %>% 
    group_by(oru_lgl) %>% 
    summarize(ten = quantile(n_docs, probs = .6, na.rm = TRUE), 
              year = ecdf(n_docs)(15))

ggplot(author_meta, aes(n_docs, color = oru_lgl)) +
    # geom_density() +
    stat_ecdf() +
    geom_vline(xintercept = 15) +
    scale_x_sqrt()

author_meta %>% 
    group_by(oru_lgl) %>% 
    summarize(ninety = quantile(first_year, probs = c(.9)), 
              year = ecdf(first_year)(2009))

ggplot(author_meta, aes(first_year, color = oru_lgl)) +
    stat_ecdf() +
    geom_vline(xintercept = c(1970, 2009))

count(author_meta, oru_lgl)

author_meta %>% 
    filter(!!!filter_conditions) %>% 
    count(oru_lgl)

author_meta_fltd = filter(author_meta, !!!filter_conditions)




## Dropouts ----
oru_df %>% 
    anti_join(author_meta_fltd, by = 'auid') %>% 
    write_rds(str_c(data_dir, '05_dropouts.Rds'))

write_rds(author_meta_fltd, str_c(data_dir, '05_author_meta.Rds'))
