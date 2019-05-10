## TODO:  fixed length of 5k words; try 300 PCs

library(tidyverse)
library(lubridate)

library(tictoc)

data_dir = '../data/'

dt_ratio = 1/10  ## target ratio of unique documents:terms

## Load data ----
author_meta = read_rds(str_c(data_dir, '06_author_histories.Rds'))

## Annotated abstract text
## ~20 sec
tic()
ann = read_rds(str_c(data_dir, '08_annotated.Rds'))
toc()

## Get nouns ----
get_nouns = function (meta_df, annotated = ann) {
    annotated %>% 
    {.$token} %>% 
        filter(upos == 'NOUN', 
               !str_detect(lemma, '[0-9]+')) %>% 
        inner_join(meta_df, by = c('id' = 'scopus_id'))
}

nouns = get_nouns(author_meta)

# rm(ann)


## Functions ----
calculate_H = function(nouns, 
                       term = lemma, 
                       doc = auid,
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


## Vocabulary exploration ----
## 27k distinct terms
n_distinct(nouns$lemma)
## 32k distinct documents (author-year combinations)
n_docs = n_distinct(nouns$auid)

## How many terms to get desired doc:term ratio? 
## 5k; top 7%
## But this includes a lot of words with nDH = 0
n_terms = ceiling(n_docs / dt_ratio)
quantile = n_terms / n_distinct(nouns$lemma)

tic()
H = calculate_H(nouns, 
                    term = lemma, 
                    doc = auid,
                    n_terms = n_terms)
toc()

ndH_thresh = H %>% 
    filter(selected) %>% 
    pull(ndH) %>% 
    min()

ggplot(H, aes(ndH)) +
    stat_ecdf() +
    geom_hline(yintercept = 1-quantile) +
    geom_rug(aes(color = selected))

ggplot(H, aes(log10(n), delta_H, label = lemma)) +
    geom_point(aes(alpha = selected, color = selected)) +
    stat_function(fun = function(x) {ndH_thresh / x}, 
                  color = 'black') +
    ylim(0, ndH_thresh)

## Plot to illustrate how ndH works
focal_terms = c('study', 'poleward', 'ligation', 'pavement')

nouns %>%
    filter(lemma %in% focal_terms) %>%
    count(lemma, auid) %>%
    complete(lemma, auid, fill = list(n = 0)) %>%
    ggplot(aes(auid, n, color = lemma, group = lemma)) +
    geom_line(show.legend = FALSE) +
    geom_label(inherit.aes = FALSE,
               data = filter(H, lemma %in% focal_terms),
               aes(x = '6701865692_2017', y = 100,
                   label = str_c('delta H: ', round(delta_H, digits = 2)))) +
    geom_label(inherit.aes = FALSE,
               data = filter(H, lemma %in% focal_terms),
               aes(x = '6701865692_2017', y = 75,
                   label = str_c('n: ', round(n, digits = 2)))) +
    geom_label(inherit.aes = FALSE,
               data = filter(H, lemma %in% focal_terms),
               aes(x = '6701865692_2017', y = 50,
                   label = str_c('ndH: ', round(ndH, digits = 2)))) +
    scale_x_discrete(breaks = NULL) +
    scale_y_sqrt() +
    facet_wrap(~ lemma, ncol = 2)


## Actually pull out the vocabulary
vocab = H %>% 
    filter(selected) %>%
    # filter(ndH > 0) %>% 
    pull(lemma)

## What fraction of each authors' nouns appears in the vocabulary list?  
## >80% have >15% of their nouns in their vocabulary
nouns %>% 
    mutate(in_vocab = lemma %in% vocab) %>% 
    count(auid, in_vocab) %>% 
    group_by(auid) %>% 
    mutate(share = n / sum(n)) %>% 
    filter(in_vocab) %>% 
    ggplot(aes(share)) +
    stat_ecdf()

## What fraction of each authors' papers have at least 1 term in the vocabulary list?  
## Basically all of them
nouns %>%
    mutate(in_vocab = lemma %in% vocab) %>%
    group_by(auid, eid) %>%
    summarize(in_vocab = any(in_vocab)) %>%
    summarize(tot_papers = n(),
              in_vocab = sum(in_vocab)) %>%
    mutate(share_in_vocab = in_vocab / tot_papers) %>%
    ggplot(aes(share_in_vocab)) +
    stat_ecdf()

## Author-year-term matrix (long, sparse)
aytm = nouns %>% 
    filter(lemma %in% vocab) %>% 
    count(auid, lemma)


## Output ----
list(H = H, 
     vocab = vocab, 
     aytm = aytm) %>% 
    write_rds(str_c(data_dir, '09_H.Rds'))

write_csv(aytm, str_c(data_dir, '10_aytm.csv'))
