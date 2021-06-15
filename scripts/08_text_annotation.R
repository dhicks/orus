library(tidyverse)
library(spacyr)
# spacy_install(version = "latest_v1")

library(tictoc)
library(assertthat)


## Load data
data_dir = '../data/'

pubs_df = read_rds(str_c(data_dir, 
                         '07_parsed_histories.Rds')) %>% 
    filter(!is.na(abstract), 
           abstract != '')

assert_that(n_distinct(pubs_df$scopus_id) == nrow(pubs_df))

## spaCy init
spacy_initialize()

clean_phrases = function(phrase) {
    phrase %>% 
        tolower() %>% 
        str_replace_all('[^[:word:]]', ' ') %>% 
        str_replace_all('^(the|a|this) ', '') %>% 
        str_squish() %>% 
        str_replace_all(' ', '_')
}

# foo = head(pubs_df, 2000)
## 22.524 sec/2000 -> 1500 sec
tic()
phrases_df = pubs_df %>% 
    # head(5) %>%
    select(doc_id = scopus_id, 
           text = abstract) %>% 
    spacy_extract_nounphrases(multithread = TRUE) %>% 
    mutate(clean_text = clean_phrases(text)) %>% 
    as_tibble()
toc()

spacy_finalize()

keep_phrases = phrases_df %>% 
    count(clean_text) %>% 
    filter(n != 1L) %>% 
    pull(clean_text)

phrases_df %>% 
    filter(clean_text %in% keep_phrases) %>% 
    write_rds(str_c(data_dir, '08_phrases.Rds'))
