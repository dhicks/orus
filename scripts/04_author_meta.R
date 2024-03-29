## This script retrieves metadata for all authors identified in the previous script, both ORU affiliates and codepartmentals
library(tidyverse)
library(xml2)

library(furrr)
library(RCurl)
source('api_key.R')

library(tictoc)
library(assertthat)

data_dir = '../data/'
author_folder = str_c(data_dir, 'authors_meta/')

plan(multiprocess, workers = 4)

force_scrape = FALSE

## Load data ----
codepts = read_rds(str_c(data_dir, '03_codepartmentals.Rds'))

## Functions for scraping from API ----
scrape_ = function (this_auid) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/author/author_id/'
    query_url = str_c(base_url, 
                      this_auid, '?',
                      'apiKey=', api_key)
    print(query_url)
    raw = getURL(query_url)
    raw
}

scrape = function (this_auid, target_folder, force_scrape = FALSE) {
    ## Either scrape from the API + save the result OR pass
    target_file = str_c(target_folder, this_auid, '.xml')
    # target_file = str_c(target_folder, '/', this_auid, '.xml.zip')
    if (!file.exists(target_file)|force_scrape) {
        raw = scrape_(this_auid)
        write_file(raw, target_file)
        # zip(target_file, target_file_xml, flags = '-9Xq')
        # unlink(target_file_xml)
        return(target_file)
    } else {
        return(target_file)
    }
}

# scrape(auids[1], author_folder)

## Do the scraping ----
## 42 sec / 100 records -> ~2.5 sec = ~42 min
tic()
author_meta_files = codepts %>% 
    pull(auid) %>% 
    unique() %>% 
    # head(1e2) %>%
    future_map_chr(scrape, target_folder = author_folder, 
                   force_scrape = force_scrape,
                   .progress = TRUE)
toc()


## Parse ----
#- total document counts
#- year of first publication

parse_ = function(raw) {
    xml = read_xml(raw)
    xml = xml_ns_strip(xml)
    
    surname = xml %>% 
        xml_find_first('//preferred-name/surname') %>% 
        xml_text()
    
    given_name = xml %>% 
        xml_find_first('//preferred-name/given-name') %>% 
        xml_text()
    
    auid = xml %>%
        xml_find_first('//dc:identifier') %>% 
        xml_text() %>% 
        str_extract('[0-9]+')
    
    n_docs = xml %>% 
        xml_find_first('//document-count') %>% 
        xml_text() %>% 
        as.integer()
    
    cited_by_count = xml %>% 
        xml_find_first('//cited-by-count') %>% 
        xml_text() %>% 
        as.integer()
    
    citation_count = xml %>% 
        xml_find_first('//citation-count') %>% 
        xml_text() %>% 
        as.integer()
    
    first_year = xml %>% 
        xml_find_first('//publication-range') %>% 
        xml_attr('start') %>% 
        as.integer()
    last_year = xml %>% 
        xml_find_first('//publication-range') %>% 
        xml_attr('end') %>% 
        as.integer()
    
    tibble(auid, surname, given_name,
           n_docs, 
           cited_by_count, citation_count, 
           first_year, last_year)
}

parse = function(target_file) {
    print(target_file)
    raw = read_file(target_file)
    if (raw == '') {
        ## Handle empty responses
        scopus_id = str_extract(target_file, '[0-9]{8,}')
        return(tibble(scopus_id = scopus_id))
    } else {
        return(parse_(raw))
    }
}

# raw = read_file(author_meta_files[1])
# parse_(raw)
# parse(author_meta_files[1])

## 1.4 sec / 100 -> ~84 sec
tic()
author_meta_df_unfltd = author_meta_files %>% 
    # head(100) %>% 
    future_map_dfr(parse, .progress = TRUE)
toc()

author_meta_df = author_meta_df_unfltd %>% 
    filter(!is.na(given_name))

assert_that(all(!is.na(author_meta_df$auid)), 
            msg = 'empty rows in author_meta_df')



## Gender attribution ----
genderize_list = function(given, api_key = NULL) {
    names = str_c('name[]=', given, collapse = '&')
    query_url = str_c('https://api.genderize.io/?', names)
    if (!is.null(api_key)) {
        query_url = str_c(query_url, '&apikey=', api_key)
    }
    response = RCurl::getURL(URLencode(query_url))
    json = jsonlite::fromJSON(response, simplifyDataFrame = TRUE)
    json = jsonlite:::null_to_na(json)
    return(json)
}

# genderize_list(head(author_meta_df$given_name), genderize.io_key)
# author_meta_df %>% 
#     separate(given_name, into = c('first', 'rest'), 
#              extra = 'merge') %>% 
#     slice(1:300) %>% 
#     pull(first) %>% 
#     genderize_list(genderize.io_key)

genderize_in_chunks = function(given_df, 
                               given_col = given_name,
                               api_key = NULL, 
                               chunk_length = 10) {
    given = enquo(given_col)
    
    chunks = given_df %>% 
        mutate(row_num = row_number(), 
               chunk = as.integer(row_num %/% chunk_length)) %>% 
        split(.$chunk) %>% 
        map(pull, !!given)
    
    # return(chunks)
    future_map_dfr(chunks, genderize_list, api_key, 
                   .progress = TRUE)
}

genderize_file = str_c(data_dir, '04_genderize')
if (!(file.exists(genderize_file))) {
    ## 65 sec
    tic()
    genderize_df = author_meta_df %>% 
        separate(given_name, into = c('given', 'the_rest'), 
                 extra = 'merge', fill = 'right') %>% 
        # slice(1:200) %>%
        genderize_in_chunks(given, genderize.io_key) %>% 
        as_tibble()
    toc()
    
    write_rds(genderize_df, genderize_file)
} else {
    genderize_df = read_rds(genderize_file)
}

count(genderize_df, gender)


## Write output ----
author_meta_df %>% 
    bind_cols(genderize_df) %>% 
    select(-name, -count) %>% 
    write_rds(str_c(data_dir, '04_author_meta.Rds'))


stop("Don't automatically run EDA")
## A little EDA ----
# author_meta_df = read_rds(str_c(data_dir, '04_author_meta.Rds'))
## Load ORU-auid matching df
oru_df = read_rds(str_c(data_dir, '03_matched.Rds'))
author_meta_df = mutate(author_meta_df, 
                        oru = auid %in% oru_df$auid)
## UCD publications, 2016-18
# pubs = read_rds(str_c(data_dir, '02_pubs.Rds'))

## 1 author missing all info; 1 author missing total doc count
author_meta_df %>% 
    filter(is.na(n_docs))

## 3.4k authors w/ 5+ pubs, active 2014 or earlier
author_meta_df %>% 
    filter(n_docs >= 5, first_year <= 2019-5)

## Might want to trim authors active prior to, say, 1970? 
count(author_meta_df, first_year)
count(author_meta_df, last_year)

ggplot(author_meta_df, aes(first_year, last_year, 
                           color = oru, 
                           alpha = oru)) +
    geom_jitter() +
    geom_vline(xintercept = 2019-5) +
    geom_vline(xintercept = 1970)

ggplot(author_meta_df, aes(first_year, n_docs, 
                           color = oru, 
                           alpha = oru)) +
    geom_jitter() +
    geom_vline(xintercept = 2019-5) +
    geom_vline(xintercept = 1970) +
    geom_hline(yintercept = 15) +
    scale_y_log10() +
    theme_bw()

## n_docs >= 5, (1970, 2014]: 120 ORU faculty; 3236 comparators
## n_docs >= 15, (1970, 2014]: 116 ORU faculty; 2144 comparators
author_meta_df %>% 
    filter(n_docs >= 15, 
           first_year <= 2014,
           first_year > 1970) %>% 
    count(oru)

