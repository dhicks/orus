## - Construct canonical department names
## - Construct departmental dummy matrix
## - Construct topic distributions for departments

library(tidyverse)
library(tidytext)
library(stm)

library(assertthat)
library(tictoc)

data_dir = file.path('..', 'data')

#' Estimate topic distribution for new docs
#' @param tm A fitted `stm` topic model
#' @param dtm 
#' @return A dataframe with columns `topic`, `doc_id`, and `gamma`
fit_new_docs = function(tm, dtm, verbose = FALSE) {
    corpus = dtm %>% 
        asSTMCorpus() %>% 
        alignCorpus(tm$vocab, verbose)
    
    new_fit = fitNewDocuments(model = tm, 
                              documents = corpus$documents)
    theta = new_fit$theta %>% 
        as_tibble(.name_repair = 'unique') %>% 
        # set_names(., str_remove(names(.), '...')) %>% 
        mutate(doc_id = names(corpus$documents)) %>%
        select(doc_id, everything()) %>%
        pivot_longer(-doc_id, 
                     names_to = 'topic',
                     names_prefix = '...',
                     values_to = 'gamma') %>%
        mutate(topic = as.integer(topic))
    return(theta)
}

# foo = fit_new_docs(models$model[[5]], orutm, verbose = TRUE)
# filter(foo, is.na(topic))



## Load data ----
author_meta = read_rds(file.path(data_dir, '05_author_meta.Rds'))

models = read_rds(file.path(data_dir, '10_models.Rds'))

papers_df = read_rds(file.path(data_dir, 
                               '06_author_histories.Rds'))

## Annotated abstract text
## ~15 sec
tic()
ann = read_rds(file.path(data_dir, '08_phrases.Rds'))
toc()

vocab = read_rds(file.path(data_dir, '09_H.Rds'))$vocab

## Canonical department names ----
depts_out = file.path(data_dir, '11_departments.csv')
if (!file.exists(depts_out)) {
    warning('Writing department list for manual cleaning')
    author_meta %>% 
        select(department) %>% 
        unnest(department) %>% 
        count(department) %>% 
        write_csv(file.path(data_dir, '11_departments.csv'))
}

## Crosswalk between uncleaned affiliation strings 
## and canonical department names
dept_xwalk = read_csv(
    file.path(data_dir, 
              '11_departments_canonical.csv')) %>% 
    select(-n)

dept_dummies = author_meta %>% 
    select(auid, department) %>% 
    unnest(department) %>% 
    left_join(dept_xwalk) %>% 
    select(auid, dept = dept_canonical) %>% 
    filter(!duplicated(.)) %>% 
    mutate(t = 1L) %>% 
    pivot_wider(names_from = dept, values_from = t, 
                values_fill = 0L)

## Crosswalk between auid and canonical departments
au_dept_xwalk = author_meta %>% 
    select(auid, department) %>% 
    unnest(department) %>% 
    left_join(dept_xwalk) %>% 
    select(auid, dept = dept_canonical) %>% 
    filter(!duplicated(.))


## Split non-ORU authors into training and testing sets ----
## One row = 1 auid
assert_that(are_equal(nrow(author_meta), 
                      n_distinct(author_meta$auid)))

set.seed(2019-06-05)
train_authors = author_meta %>% 
    filter(!oru_lgl) %>% 
    left_join(au_dept_xwalk) %>% 
    group_by(dept) %>% 
    sample_frac(size = .5) %>% 
    ungroup() %>% 
    pull(auid)

test_authors = author_meta %>% 
    filter(!oru_lgl, ! auid %in% train_authors) %>% 
    pull(auid)

assert_that(
    are_equal(
        length(intersect(train_authors, test_authors)), 
        0L), 
    msg = 'Auid in both training and testing set')

oru_authors = author_meta %>% 
    filter(oru_lgl) %>% 
    pull(auid)


## Papers for the training set ----
## Everything written by the training set authors, 
## *except* also written by ORU authors

oru_papers = papers_df %>% 
    filter(auid %in% oru_authors) %>% 
    select(scopus_id) %>% 
    filter(!duplicated(.)) %>% 
    pull(scopus_id)

train_papers = papers_df %>% 
    filter(auid %in% train_authors, 
           ! scopus_id %in% oru_papers) %>% 
    select(scopus_id) %>% 
    filter(!duplicated(.)) %>% 
    pull(scopus_id)

assert_that(
    are_equal(length(intersect(oru_papers, train_papers)), 
              0L), 
    msg = 'Papers in both oru_papers and train_papers')


## Fit departments ----
## Department-term matrix
tic()
dtm = ann %>% 
    filter(doc_id %in% train_papers, 
           clean_text %in% vocab) %>% 
    select(doc_id, text = clean_text) %>% 
    left_join(papers_df, by = c('doc_id' = 'scopus_id')) %>% 
    select(scopus_id = doc_id, auid, text) %>% 
    filter(auid %in% train_authors) %>% 
    left_join(au_dept_xwalk, by = 'auid') %>% 
    count(dept, text) %>% 
    cast_sparse(row = dept, col = text, value = n)
toc()

tic()
dept_gamma = models %>% 
    mutate(dept = map(model, ~fit_new_docs(., dtm))) %>% 
    select(k, dept) %>% 
    unnest(dept) %>% 
    rename(dept = doc_id)
toc()

# dept_gamma %>% 
#     filter(k == 25) %>% 
#     ggplot(aes(dept, topic, fill = gamma)) +
#     geom_tile() +
#     coord_flip()


## Fit ORUs ----
auid_oru = author_meta %>% 
    filter(oru_lgl) %>% 
    unnest(oru) %>% 
    select(auid, oru) %>% 
    filter(!duplicated(.))

## ORU-term matrix
orutm = inner_join(auid_oru, papers_df, by = 'auid') %>% 
    select(oru, scopus_id) %>% 
    filter(!duplicated(.)) %>% 
    inner_join(ann, by = c('scopus_id' = 'doc_id')) %>% 
    filter(clean_text %in% vocab) %>% 
    count(oru, text = clean_text) %>% 
    cast_sparse(row = oru, col = text, value = n)

oru_gamma = models %>% 
    mutate(oru = map(model, ~fit_new_docs(., orutm))) %>% 
    select(k, oru) %>% 
    unnest(oru) %>% 
    rename(oru = doc_id)

## Output ----
write_rds(dept_dummies, file.path(data_dir, '11_dept_dummies.Rds'))
write_rds(au_dept_xwalk, file.path(data_dir, '11_au_dept_xwalk.Rds'))

list(test = test_authors, 
     train = train_authors, 
     oru = oru_authors, 
     train_papers = train_papers) %>% 
    write_rds(file.path(data_dir, '11_test_train.Rds'))

write_rds(dtm, file.path(data_dir, '11_dept_term_matrix.Rds'))

write_rds(dept_gamma, file.path(data_dir, '11_dept_gamma.Rds'))

write_rds(orutm, file.path(data_dir, '11_oru_term_matrix.Rds'))

write_rds(oru_gamma, file.path(data_dir, '11_oru_gamma.Rds'))
