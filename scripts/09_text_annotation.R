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

# library(SpeedReader)
## <https://github.com/matthewjdenny/SpeedReader/blob/master/vignettes/getting_started_with_SpeedReader.Rmd>
# # Load in example data and form a dtm:
# data("congress_bills")
# # Only keep tokens that are all letters and of length 4 or greater:
# quanteda_dtm <- quanteda::dfm(congress_bills,
#                               select = "[a-zA-Z]{4,}",
#                               valuetype = "regex")
# dtm <- convert_quanteda_to_slam(quanteda_dtm)
# 
# # Now create some fake covariate data
# doc_covariates <- data.frame(Chamber = c(rep("House",40), 
#                                          rep("Senate",39)),
#                              Party = c(rep("Democrat",20),
#                                        rep("Republican",20),
#                                        rep("Democrat",20),
#                                        rep("Republican",19)),
#                              Date = c(rep("Jan10",30),
#                                       rep("Jan11",29),
#                                       rep("Jan14",20)),
#                              stringsAsFactors = FALSE)
# 
# cont_table <- contingency_table(metadata = doc_covariates,
#                                 document_term_matrix = dtm,
#                                 variables_to_use = "Chamber")
# 
# # Feature selection with informed Dirichlet model (Monroe et al., 2008):
# fw_results <- feature_selection(contingency_table = cont_table,
#                                 method = "informed Dirichlet",
#                                 rank_by_log_odds = TRUE)
# 
# # Create a funnel plot of feature selection results:
# fightin_words_plot(fw_results,
#                    right_margin = 9,
#                    display_top_words = 10)

# library(tidytext)
# pubs_comp %>% 
#     head(100) %>% 
#     select(scopus_id, authors, abstract) %>% 
#     filter(!map_lgl(authors, is.null)) %>% 
#     ## Unnest tokens
#     unnest_tokens(output = 'token', input = abstract) %>%
#     ## Unnest authors
#     unnest(authors) %>% 
#     filter(auid %in% comp$author_meta$auid) %>% 
#     ## Author-term matrix
#     count(auid, token) %>% 
#     ## Entropy
#     group_by(token) %>% 
#     mutate(p = n / sum(n), 
#            H_term = -p*log2(p)) %>% 
#     summarize(H = sum(H_term), 
#               n = sum(n)) %>% 
#     ungroup() %>% 
#     mutate(delta_H = max(H) - H, 
#            ndH = delta_H*log10(n)) %>% 
#     arrange(desc(ndH))
    

