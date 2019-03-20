## TODO:  fixed length of 5k words; try 300 PCs

library(tidyverse)
library(lubridate)

library(tictoc)

data_dir = '../data/'

dt_ratio = 6  ## target ratio of unique documents:terms

## Load data ----
did = read_rds(str_c(data_dir, '07_did_sample.Rds'))
comp = read_rds(str_c(data_dir, '07_comp_sample.Rds'))

## Annotated abstract text
## ~140 sec
tic()
ann = read_rds(str_c(data_dir, '09_annotated.Rds'))
toc()

## Get nouns ----
get_nouns = function (sample, annotated = ann) {
    histories = sample$histories %>% 
        filter(!is.na(pub_date)) %>% 
        mutate(pub_date = ymd(pub_date), 
               year = year(pub_date), 
               auid_year = str_c(auid, '_', year))
    
    annotated %>% 
    {.$token} %>% 
        filter(upos == 'NOUN', 
               !str_detect(lemma, '[0-9]+')) %>% 
        inner_join(histories, by = c('id' = 'scopus_id'))
}

nouns_did = get_nouns(did)
nouns_comp = get_nouns(comp)

# rm(ann)


## Functions ----
calculate_H = function(nouns, 
                       term = lemma, 
                       doc = auid_year,
                       arrange = TRUE, 
                       n_terms = NULL) {
    ## Calculates the entropy of the document distribution for each term
    ## Also calculates total occurences (`n`), information gain (`delta_H`), 
    ## and log10(n)*delta_H (`ndH`)
    ## If `arrange`, then arranges in descending order
    ## If `arrange` and n_terms is passed, identifies the top n_terms terms as "selected"
    term = enquo(term)
    doc = enquo(doc)
    
    H = nouns %>% 
        count(!!term, !!doc) %>% 
        group_by(!!term) %>% 
        mutate(p = n / sum(n), 
               H_term = -p*log2(p)) %>% 
        summarize(n = sum(n), 
                  H = sum(H_term)) %>% 
        ungroup() %>% 
        mutate(max_H = max(H), 
               delta_H = max_H - H, 
               ndH = log10(n)*delta_H)
    
    if (arrange) {
        H = arrange(H, desc(ndH))
        if (!is.null(n_terms)) {
            H = mutate(H, selected = row_number() <= n_terms)
        }
    }
    
    return(H)
}


## DID sample ----
## 70k distinct terms
n_distinct(nouns_did$lemma)
## 32k distinct documents (author-year combinations)
n_docs_did = n_distinct(nouns_did$auid_year)

## How many terms to get desired doc:term ratio? 
## 5k; top 7%
## But this includes a lot of words with nDH = 0
n_terms_did = ceiling(n_docs_did / dt_ratio)
quantile_did = n_terms_did / n_distinct(nouns_did$lemma)

H_did = calculate_H(nouns_did, 
                    term = lemma, 
                    doc = auid_year,
                    n_terms = n_terms_did)

ndH_thresh = H_did %>% 
    filter(selected) %>% 
    pull(ndH) %>% 
    min()

ggplot(H_did, aes(ndH)) +
    stat_ecdf() +
    geom_hline(yintercept = 1-quantile_did) +
    geom_rug(aes(color = selected))

ggplot(H_did, aes(log10(n), delta_H, label = lemma)) +
    geom_point(aes(alpha = selected, color = selected)) +
    stat_function(fun = function(x) {ndH_thresh / x}, 
                  color = 'black') +
    ylim(0, ndH_thresh)

## Plot to illustrate how ndH works
focal_terms = c('study', 'poleward', 'ligation', 'pavement')

nouns_did %>% 
    filter(lemma %in% focal_terms) %>% 
    count(lemma, auid_year) %>% 
    complete(lemma, auid_year, fill = list(n = 0)) %>% 
    ggplot(aes(auid_year, n, color = lemma, group = lemma)) +
    geom_line(show.legend = FALSE) +
    geom_label(inherit.aes = FALSE, 
               data = filter(H_did, lemma %in% focal_terms),
               aes(x = '6701865692_2017', y = 100, 
                   label = str_c('delta H: ', round(delta_H, digits = 2)))) +
    geom_label(inherit.aes = FALSE, 
               data = filter(H_did, lemma %in% focal_terms),
               aes(x = '6701865692_2017', y = 75, 
                   label = str_c('n: ', round(n, digits = 2)))) +
    geom_label(inherit.aes = FALSE, 
               data = filter(H_did, lemma %in% focal_terms),
               aes(x = '6701865692_2017', y = 50, 
                   label = str_c('ndH: ', round(ndH, digits = 2)))) +
    scale_x_discrete(breaks = NULL) +
    scale_y_sqrt() +
    facet_wrap(~ lemma, ncol = 2)


## Actually pull out the vocabulary
vocab_did = H_did %>% 
    filter(selected) %>%
    # filter(ndH > 0) %>% 
    pull(lemma)

## What fraction of each authors' nouns appears in the vocabulary list?  
## 75% have >=15% of their nouns in their vocabulary
nouns_did %>% 
    mutate(in_vocab = lemma %in% vocab_did) %>% 
    count(auid, in_vocab) %>% 
    group_by(auid) %>% 
    mutate(share = n / sum(n)) %>% 
    filter(in_vocab) %>% 
    ggplot(aes(share)) +
    stat_ecdf()

## What fraction of each authors' papers have at least 1 term in the vocabulary list?  
## Basically all of them
nouns_did %>% 
    mutate(in_vocab = lemma %in% vocab_did) %>% 
    group_by(auid, eid) %>% 
    summarize(in_vocab = any(in_vocab)) %>% 
    summarize(tot_papers = n(), 
              in_vocab = sum(in_vocab)) %>% 
    mutate(share_in_vocab = in_vocab / tot_papers) %>% 
    ggplot(aes(share_in_vocab)) +
    stat_ecdf()

## Author-year-term matrix (long, sparse)
aytm_did = nouns_did %>% 
    filter(lemma %in% vocab_did) %>% 
    count(auid_year, lemma)


## Comprehensive sample ----
## 63k distinct terms
n_distinct(nouns_comp$lemma)
## 28k distinct documents (author-year combinations)
n_docs_comp = n_distinct(nouns_comp$auid_year)

## How many terms to get desired doc:term ratio? 
## 4.7k; top 7%
## But this includes a lot of words with nDH = 0
n_terms_comp = ceiling(n_docs_comp / dt_ratio)
quantile_comp = n_terms_comp / n_distinct(nouns_comp$lemma)

H_comp = calculate_H(nouns_comp, 
                    term = lemma, 
                    doc = auid_year,
                    n_terms = n_terms_comp)

ndH_thresh = H_comp %>% 
    filter(selected) %>% 
    pull(ndH) %>% 
    min()

ggplot(H_comp, aes(ndH)) +
    stat_ecdf() +
    geom_hline(yintercept = 1-quantile_comp) +
    geom_rug(aes(color = selected))

ggplot(H_comp, aes(log10(n), delta_H, label = lemma)) +
    geom_point(aes(alpha = selected, color = selected)) +
    stat_function(fun = function(x) {ndH_thresh / x}, 
                  color = 'black') +
    ylim(0, ndH_thresh)

## Actually pull out the vocabulary
vocab_comp = H_comp %>% 
    filter(selected) %>%
    # filter(ndH > 0) %>% 
    pull(lemma)

## What fraction of each authors' nouns appears in the vocabulary list?  
## 75% have >=13% of their nouns in their vocabulary
nouns_comp %>% 
    mutate(in_vocab = lemma %in% vocab_comp) %>% 
    count(auid, in_vocab) %>% 
    group_by(auid) %>% 
    mutate(share = n / sum(n)) %>% 
    filter(in_vocab) %>% 
    ggplot(aes(share)) +
    stat_ecdf()

## What fraction of each authors' papers have at least 1 term in the vocabulary list?  
## Basically all of them
nouns_comp %>% 
    mutate(in_vocab = lemma %in% vocab_comp) %>% 
    group_by(auid, eid) %>% 
    summarize(in_vocab = any(in_vocab)) %>% 
    summarize(tot_papers = n(), 
              in_vocab = sum(in_vocab)) %>% 
    mutate(share_in_vocab = in_vocab / tot_papers) %>% 
    ggplot(aes(share_in_vocab)) +
    stat_ecdf()

## Author-year-term matrix (long, sparse)
aytm_comp = nouns_comp %>% 
    filter(lemma %in% vocab_comp) %>% 
    count(auid_year, lemma)


## Output ----
## Substantial overlap in vocabularies
length(intersect(vocab_did, vocab_comp))
setdiff(vocab_comp, vocab_did)
setdiff(vocab_did, vocab_comp)

list(H = H_comp, 
     vocab = vocab_comp, 
     aytm = aytm_comp) %>% 
    write_rds(str_c(data_dir, '10_H_comp.Rds'))
write_csv(aytm_comp, str_c(data_dir, '10_aytm_comp.csv'))

list(H = H_did, 
     vocab = vocab_did, 
     aytm = aytm_did) %>% 
    write_rds(str_c(data_dir, '10_H_did.Rds'))
write_csv(aytm_did, str_c(data_dir, '10_aytm_did.csv'))

