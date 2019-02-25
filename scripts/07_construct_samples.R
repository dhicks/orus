## This script first uses paper-level metadata for ORU authors to attempt to identify years of first affiliation for each author-ORU
## This is partially successful, matching ~2/3 ORU faculty overall but with large gaps (eg, almost no JMIE faculty).  
## This suggests splitting the analysis into two designs:  
## 1. A difference-in-differences (DID) design, comparing ORU faculty for whom we have ORU affiliation dates (referred to as "matched" in this script) to their codepartmentals, and 
## 2. A "comprehensive" design, including all faculty identified in steps 03+05 but only using papers published since 2000
## 
## This script then constructs parallel data files for each sample

library(tidyverse)
library(lubridate)

library(assertthat)

data_dir = '../data/'

## Load data ----
dropouts_03 = read_rds(str_c(data_dir, '03_dropout.Rds'))
dropouts_05 = read_rds(str_c(data_dir, '05_dropout.Rds'))
oru_df = read_rds(str_c(data_dir, '03_matched.Rds')) %>% 
    anti_join(dropouts_03, by = 'auid') %>% 
    anti_join(dropouts_05, by = 'auid')

codepts = read_rds(str_c(data_dir, '03_codepartmentals.Rds'))
## Author-level metadata, all authors
author_meta = read_rds(str_c(data_dir, '04_author_meta.Rds'))
## Partial paper metadata for all authors
histories = read_rds(str_c(data_dir, '05_author_histories.Rds'))

## More complete paper metadata, ORU authors only
pubs_df = read_rds(str_c(data_dir, '06_oru_histories.Rds')) %>% 
    mutate(date = ymd(date), 
           year = year(date)) %>% 
    filter(map_lgl(authors, ~length(.) > 0)) %>% 
    unnest(authors) %>% 
    filter(auid %in% oru_df$auid)

oru_matchnames = tribble(
    ~ ORU, ~ matchname, 
    'AQRC', 'Air Quality',
    'BML/CMSI', 'Bodega Marine Lab',
    'BML/CMSI', 'Coastal and Marine Sciences Institute', 
    'CNPRC', 'California National Primate', 
    'CCC', 'Comprehensive Cancer', 
    'CHPR', 'Healthcare Policy and Research', 
    'ITS', 'Transportation Studies', 
    'JMIE', 'John Muir', 
    'PICN', 'Community Nutrition'
)

## Affiliation IDs for UC Davis
ucd_ids = c('60014439', '60023317', '60003160', '60072478', '60000000', 
            '60022586', '60114314', '60114420', '60110341', '60086298', 
            '60028407')


## Year of first affiliation ----
## Affiliations in full author histories, ORU faculty
affiliations = pubs_df %>% 
    group_by(auid, aff_id, aff_name) %>% 
    summarize(first_year = min(year)) %>% 
    ungroup()

## First affiliation with UC Davis
affiliations %>% 
    filter(aff_id %in% ucd_ids) %>% 
    group_by(auid) %>% 
    summarize(first_year = min(first_year)) %>% 
    ggplot(aes(first_year)) +
    geom_bar()

## Anyone without a UCD affiliation? 
## Nope! 
affiliations %>% 
    filter(aff_id %in% ucd_ids) %>% 
    anti_join(oru_df, ., by = 'auid')


## Match to ORU names ----
matched = oru_df %>% 
    left_join(oru_matchnames) %>% 
    left_join(affiliations) %>% 
    filter(str_detect(aff_name, matchname)) %>% 
    group_by(ORU, auid) %>% 
    summarize(first_year = min(first_year)) %>% 
    ungroup()

## Most affiliations begin >= 2004; but some stretch back to the '80s and '90s
ggplot(matched, aes(first_year)) +
    geom_bar() +
    facet_wrap(~ ORU, scales = 'free')
    
## Who isn't matched? 
## 47 ORU faculty (39%)
## Almost all of JMIE; most of ITS; lots of CCC, CHPR
oru_df %>% 
    anti_join(matched) %>% 
    count(ORU) %>% 
    rename(unmatched = n) %>% 
    left_join(count(oru_df, ORU)) %>% 
    rename(total_faculty = n) %>% 
    mutate(unmatched_share = unmatched / total_faculty)
    
## Scrolling through this list, it doesn't seem like we can catch many more faculty by tweaking the ORU matching names
oru_df %>% 
    anti_join(matched) %>% 
    left_join(affiliations) #%>% View
    

## How do first affiliation dates compare to whole careers? ----
## Most researchers published quite a bit before their affiliation with the ORU
## NB first year here is first year **w/ ORU**
pubs_df %>% 
    count(auid, year) %>% 
    inner_join(matched) %>% 
    mutate(after_aff = year >= first_year) %>% 
    count(auid, first_year, after_aff) %>% 
    spread(after_aff, n) %>% 
    rename(before = `FALSE`, since = `TRUE`) %>% 
    ggplot(aes(before, since, color = first_year)) +
    geom_jitter() +
    stat_function(fun = identity)

## What if we restrict things to 2000? 
pubs_df %>% 
    count(auid, year) %>% 
    filter(year >= 2000) %>% 
    inner_join(matched) %>% 
    mutate(after_aff = year >= first_year) %>% 
    count(auid, first_year, after_aff) %>% 
    spread(after_aff, n) %>% 
    rename(before = `FALSE`, since = `TRUE`) %>% 
    ggplot(aes(before, since, color = first_year)) +
    geom_jitter() +
    stat_function(fun = identity)

## 2014? 
pubs_df %>% 
    count(auid, year) %>% 
    filter(year >= 2014) %>% 
    inner_join(matched) %>% 
    mutate(after_aff = year >= first_year) %>% 
    count(auid, first_year, after_aff) %>% 
    spread(after_aff, n) %>% 
    rename(before = `FALSE`, since = `TRUE`) %>% 
    ggplot(aes(before, since, color = first_year)) +
    geom_jitter() +
    stat_function(fun = identity)


## Define the DID sample ----
## This sample will contain (a) ORU faculty for whom we have first affiliation dates with their ORUs [in `matched`], and (b) their non-ORU codepartmentals
## Full histories will be used, without a date cutoff
## Codepartmentals for the DID sample
codepts_did = codepts %>% 
    ## Departments for (a)
    filter(auid %in% matched$auid) %>% 
    count(aff_name) %>% 
    select(-n) %>% 
    ## Bring in everyone else for (b)
    inner_join(codepts) %>% 
    ## Capture 05 filtering
    filter(auid %in% histories$auid) %>% 
    ## Construct grouping variables
    mutate(oru = auid %in% oru_df$auid, 
           in_matched = auid %in% matched$auid, 
           dropout_03 = auid %in% dropouts_03$auid,
           dropout_05 = auid %in% dropouts_05$auid,
           group = case_when(oru & in_matched ~ 'ORU', 
                             oru & !in_matched ~ 'ORU not in DID', 
                             !oru & in_matched ~ 'error', 
                             !oru & !in_matched ~ 'codepartmental')) %>% 
    ## Drop ORU faculty for whom we don't have first affiliation dates
    filter(group != 'ORU not in DID')

## Data validation:  no matching errors; no dropouts from scripts 03 and 05
assert_that(all(codepts_did$group != 'error'), msg = 'erroneous groups in codepts_did')
assert_that(all(!codepts_did$dropout_03))
assert_that(all(!codepts_did$dropout_05))

## 73 ORU faculty and 1.7k codepartmental comparators (counting by unique auids)
codepts_did %>% 
    count(auid, group) %>% 
    count(group)

author_meta_did = author_meta %>% 
    inner_join(codepts_did) %>% 
    select(-aff_name) %>% 
    filter(!duplicated(.))

## Data validation:  1 row in author_meta_comp per unique auid
assert_that(n_distinct(codepts_did$auid) == nrow(author_meta_did))

## Distributions of first publication years, ORU faculty vs. codepartmentals
## These are similar enough that I don't think we need matching or anything at this point
ggplot(author_meta_did, aes(group, first_year)) +
    geom_violin(draw_quantiles = .5, scale = 'count') +
    geom_dotplot(binaxis = 'y', binwidth = 1, stackdir = 'center', dotsize = .5)

## Author histories = papers to be used for DID analysis
histories_did = histories %>% 
    filter(auid %in% codepts_did$auid)
assert_that(n_distinct(histories_did$auid) == n_distinct(codepts_did$auid))

## 102k distinct papers
n_distinct(histories_did$eid)

## Write output
list(codepts = codepts_did, 
     author_meta = author_meta_did, 
     histories = histories_did) %>% 
    write_rds(str_c(data_dir, '07_did_sample.Rds'))


## Define the comprehensive sample ----
## This sample will contain all ORU faculty and their non-ORU codepartmentals, but only papers published since 2000
histories_comp = histories %>% 
    mutate(pub_date = ymd(pub_date), 
           year = year(pub_date)) %>% 
    filter(year >= 2000)

## 89k distinct papers
n_distinct(histories_comp$eid)

## 116 ORU faculty; 2.0k codepartmentals
codepts_comp = codepts %>%
    filter(auid %in% histories_comp$auid, 
           !duplicated(.)) %>% 
    mutate(oru = auid %in% oru_df$auid, 
           dropout_03 = auid %in% dropouts_03$auid,
           dropout_05 = auid %in% dropouts_05$auid,
           group = case_when(oru ~ 'ORU', 
                             !oru ~ 'codepartmental'))

## Data validation:  no erroneous groups; no dropouts from scripts 03 and 05
assert_that(all(!codepts_comp$dropout_03))
assert_that(all(!codepts_comp$dropout_05))

codepts_comp %>% 
    group_by(group) %>% 
    summarize(n = n_distinct(auid))

author_meta_comp = author_meta %>% 
    inner_join(codepts_comp) %>% 
    select(-aff_name) %>% 
    filter(!duplicated(.))

## Data validation:  1 row in author_meta_comp per unique auid
assert_that(n_distinct(codepts_comp$auid) == nrow(author_meta_comp))

ggplot(author_meta_comp, aes(group, first_year)) +
    geom_violin(draw_quantiles = .5, scale = 'count') +
    geom_dotplot(binaxis = 'y', binwidth = 1, stackdir = 'center', dotsize = .5, alpha = .5)


## Write output
list(codepts = codepts_comp, 
     author_meta = author_meta_comp, 
     histories = histories_comp) %>% 
    write_rds(str_c(data_dir, '07_comp_sample.Rds'))
