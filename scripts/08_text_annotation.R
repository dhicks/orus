library(tidyverse)
library(cleanNLP)

library(tictoc)
library(assertthat)

## Load data
data_dir = '../data/'

pubs_df = read_rds(str_c(data_dir, '07_parsed_histories.Rds')) %>% 
    filter(!is.na(abstract), 
           abstract != '')

assert_that(n_distinct(pubs_df$scopus_id) == nrow(pubs_df))

## spaCy init
reticulate::use_condaenv('spacy')
cnlp_init_spacy(entity_flag = FALSE)


## Annotate ----
## ~10 sec / 100 docs -> ~30-40 minutes
tic()
pubs_ann = pubs_df %>% 
    # head(100) %>%
    # select(scopus_id, abstract) %>% 
    cnlp_annotate(as_strings = TRUE, 
                  doc_var = 'scopus_id', 
                  text_var = 'abstract')
toc()

write_rds(pubs_ann, str_c(data_dir, '08_annotated.Rds'))
