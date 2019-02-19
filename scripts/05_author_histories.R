## This script retrieves EIDs for all of every author's prior papers
## 
## The next step here would be to use the abstract retrieval API to retrieve the full metadata for each paper
## However, with current filtering there are 90k-106k papers to retrieve
## Some possibilities for further filtering:  
## - increase the n_docs threshold, eg, to 20 or 25
##      - this starts to remove more ORU faculty
## - cut off author histories, eg, at 2000
##      - this doesn't cut things down that much, eg, 2000 cutoff -> 90k
## - something based on ORU affiliation dates
##      - this assumes that we can get data on ORU affiliations outside of Scopus
##      - emailed Christine Park for help
## - use Crossref instead
##      - Crossref doesn't have stable author IDs, has patchy abstract coverage

library(tidyverse)
library(xml2)

library(furrr)
library(RCurl)
source('api_key.R')

library(assertthat)
library(tictoc)

data_dir = '../data/'
author_folder = str_c(data_dir, 'author_histories/')

plan(multiprocess, workers = 1)

## Load data ----
oru_df = read_rds(str_c(data_dir, '03_matched.Rds'))

author_meta_unfltd = read_rds(str_c(data_dir, '04_author_meta.Rds'))
## Poisson distribution? 
## Substantially overdispersed
mean(author_meta_unfltd$n_docs, na.rm = TRUE)
sd(author_meta_unfltd$n_docs, na.rm = TRUE)

## ***Filter conditions here***
author_meta = author_meta_unfltd %>% 
    filter(n_docs >= 15,
           first_year <= 2014,
           first_year > 1970) %>%
    mutate(oru = auid %in% oru_df$auid)

## Basically no overdispersion now
mean(author_meta$n_docs, na.rm = TRUE)
sd(author_meta$n_docs, na.rm = TRUE)

ggplot(author_meta, aes(n_docs)) +
    geom_density(data = author_meta_unfltd, aes(color = 'unfiltered')) +
    geom_density(aes(color = 'filtered')) +
    ## dpois() doesn't play nice with ggplot's interpolation of x values
    # stat_function(fun = function (x) dpois(x, 71),
    #               data = tibble(n_docs = seq(1, 1000, by = 3))) +
    scale_x_log10()


## Functions for scraping from API ----
scrape_ = function (this_auid, page_idx) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/search/scopus?'
    query_url = str_c(base_url, 
                      'query=au-id(', this_auid, ')',
                      '&count=200',
                      '&start=', 200*(page_idx-1),
                      '&httpAccept=application/xml',
                      '&apiKey=', api_key)
    print(query_url)
    raw = getURL(query_url)
    raw
}

scrape = function (this_auid, n_docs, target_folder) {
    ## In this data, max(n_docs) = 650, so max(total_pages) = 4
    total_pages = n_docs %/% 200 + 1
    target_files = str_c(this_auid, '_', 1:total_pages, '.xml')
    for (page_idx in 1:total_pages) {
        ## Either scrape from the API + save the result OR pass
        target_file = str_c(target_folder, target_files[page_idx])
        if (!file.exists(target_file)) {
            raw = scrape_(this_auid, page_idx)
            write_file(raw, target_file)
        }
    }
    return(target_files)
}

# ## Case w/ 2 pages
# scrape('35203386900', 202, author_folder)
# ## Case w/ incorrect count, actually only has <200 
# ## This gives a valid xml with no entries
# scrape('35498339000', 205, author_folder)

## Do the scraping ----
## 12 sec/10 -> 4085 sec = ~70 min
tic()
author_history_files = author_meta %>% 
    # head(10) %>% 
    # rowwise() %>% 
    future_map2(.x = .$auid, .y = .$n_docs, 
                .f = ~scrape(.x, .y, author_folder), 
                .progress = TRUE)
toc()

## Need this for the set_names() call in the parsing workflow  
assert_that(length(author_history_files) == nrow(author_meta))

## Functions for parsing ----
parse_ = function(raw) {
    xml = read_xml(raw)
    xml = xml_ns_strip(xml)
    
    entries = xml_find_all(xml, '//entry')
    
    scopus_id = entries %>% 
        xml_find_first('dc:identifier') %>% 
        xml_text() %>% 
        str_extract('[0-9]+')
    eid = entries %>% 
        xml_find_first('eid') %>% 
        xml_text()
    doi = entries %>% 
        xml_find_first('prism:doi') %>% 
        xml_text()
    
    title = entries %>% 
        xml_find_first('dc:title') %>% 
        xml_text()
    journal = entries %>% 
        xml_find_first('prism:publicationName') %>% 
        xml_text()
    
    pub_date = entries %>% 
        xml_find_first('prism:coverDate') %>% 
        xml_text()
    
    oa = entries %>% 
        xml_find_first('openaccess') %>% 
        xml_text() %>% 
        {. == 1}
    
    cited_by = entries %>% 
        xml_find_first('citedby-count') %>% 
        xml_text() %>% 
        as.integer()
    
    tibble(scopus_id, eid, doi, 
           title, journal, 
           pub_date, 
           oa, 
           cited_by)
}
# parse_(raw)
# raw = read_file(str_c(author_folder, author_history_files[1][[1]]))

parse = function(files, author_folder) {
    files = files %>% 
        str_c(author_folder, .)
    parsed_df = files %>% 
        map(read_file) %>% 
        map_dfr(parse_)
    return(parsed_df)
}
# parse(author_history_files[[9]], author_folder)

## Do the parsing ----
## 7.4 sec / 100 -> ~250 sec = 4+ minutes
tic()
histories_df = author_history_files %>% 
    set_names(author_meta$auid) %>% 
    # head(100) %>%
    future_map_dfr(parse, author_folder, .id = 'auid', .progress = TRUE)
toc()

## Correct number of docs per author? 
## Numbers are off for 75 authors
## Usually the metadata results have 1 more than the search results; but not always
## 1 case w/ difference of 6:  56323044500
## Metadata retrieval now gives 39, so this may just be due to the time between API queries
histories_df %>% 
    count(auid) %>% 
    right_join(author_meta, by = 'auid') %>% 
    mutate(right_count = n == n_docs) %>% 
    filter(!right_count) %>% 
    # count(n - n_docs)
    filter(n - n_docs == 6)
    # ggplot(aes(log10(n), log10(n_docs))) +
    # geom_point() +
    # stat_function(fun = identity)

## How many unique papers? 
## 125k
histories_df %>% 
    pull(eid) %>% 
    unique() %>% 
    length()
## 106k using DOIs
histories_df %>% 
    filter(!is.na(doi)) %>% 
    pull(doi) %>% 
    unique() %>% 
    length()

# library(lubridate)
# ## pub_date is either ymd or NA
# # histories_df %>% 
# #     mutate(pubdate = ymd(pub_date)) %>% 
# #     filter(is.na(pubdate))
# ## Negative-cumulative count of papers by year
# ## Trimming things around 2000 -> ~90k papers
# histories_df %>% 
#     mutate(pub_date = ymd(pub_date), 
#            pub_year = year(pub_date)) %>% 
#     count(scopus_id, pub_year) %>% 
#     count(pub_year) %>% 
#     rename(n = nn) %>% 
#     mutate(cum_n = cumsum(n), 
#            cum_n_rev = max(cum_n) - cum_n) %>% 
#     ggplot(aes(pub_year, cum_n_rev)) +
#     geom_line()

## Write output ----
write_rds(histories_df, str_c(data_dir, '05_author_histories.Rds'))
