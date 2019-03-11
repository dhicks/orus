library(tidyverse)
library(cleanNLP)

library(tictoc)
library(assertthat)

## Load data
data_dir = '../data/'

pubs_df = read_rds(str_c(data_dir, '08_full_histories.Rds')) %>% 
    filter(!is.na(abstract), 
           abstract != '')

assert_that(n_distinct(pubs_df$scopus_id) == nrow(pubs_df))

## spaCy init
reticulate::use_condaenv('spacy')
cnlp_init_spacy()


## Annotate ----
## 8.4 sec / 100 docs -> 2.3 hours
tic()
pubs_ann = pubs_df %>% 
    # head(100) %>% 
    select(scopus_id, abstract) %>% 
    cnlp_annotate()
toc()

write_rds(str_c(data_dir, '09_annotated.Rds'))
