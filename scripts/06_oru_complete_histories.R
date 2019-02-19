## This script retrieves complete metadata for the author histories, ORU faculty only
## It is mostly copied from script 02 (another reason why 02 should be a package)
library(tidyverse)
library(xml2)

library(furrr)
library(RCurl)
source('api_key.R')

library(tictoc)

data_dir = '../data/'
pub_folder = str_c(data_dir, 'docs/')  ## Same folder as 02

plan(multiprocess, workers = 6)

## Load author histories DF ----
oru_df = read_rds(str_c(data_dir, '03_matched.Rds'))

histories_df = read_rds(str_c(data_dir, '05_author_histories.Rds')) %>% 
    mutate(oru = auid %in% oru_df$auid) %>% 
    filter(oru)

eids = unique(histories_df$eid)

## Functions for scraping from API ----
scrape_ = function (this_eid) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/abstract/eid/'
    query_url = str_c(base_url, 
                      this_eid, '?',
                      'apiKey=', api_key)
    print(query_url)
    raw = getURL(query_url)
    raw
}

scrape = function (this_eid, target_folder) {
    ## Either scrape from the API + save the result OR pass
    target_file = str_c(target_folder, this_eid, '.xml')
    # target_file = str_c(target_folder, '/', this_eid, '.xml.zip')
    if (!file.exists(target_file)) {
        raw = scrape_(this_eid)
        write_file(raw, target_file)
        # zip(target_file, target_file_xml, flags = '-9Xq')
        # unlink(target_file_xml)
        return(target_file)
    } else {
        return(target_file)
    }
}

# scrape(eids[1], pub_folder)

## Do the scraping ----
tic()
pub_files = eids %>% 
    # head(2e4) %>% 
    future_map_chr(scrape, target_folder = pub_folder, 
                   .progress = TRUE)
toc()

stop()
## Parse XML files ----
parse_ = function (raw) {
    xml = read_xml(raw)
    xml = xml_ns_strip(xml)
    
    scopus_id = xml %>%
        xml_find_first('//dc:identifier') %>%
        xml_text() %>%
        str_extract('[0-9]+')
    
    doi = xml %>%
        xml_find_first('//prism:doi') %>%
        xml_text()
    title = xml %>%
        xml_find_first('//dc:title') %>%
        xml_text()
    journal = xml %>%
        xml_find_first('//prism:publicationName') %>%
        xml_text()
    issn = xml %>%
        xml_find_first('//prism:issn') %>%
        xml_text()
    date = xml %>%
        xml_find_first('//prism:coverDate') %>%
        xml_text()
    abstract = xml %>%
        xml_find_first('//abstract//ce:para') %>%
        xml_text()
    keywords = xml %>%
        xml_find_all('//authkeywords//author-keyword') %>%
        xml_text() %>%
        list()
    
    subject_nodes = xml %>%
        xml_find_all('//subject-areas/subject-area')
    subject_code = subject_nodes %>%
        xml_attr('code')
    subject_name = subject_nodes %>%
        xml_text()
    subject_area = subject_nodes %>%
        xml_attr('abbrev')
    subjects = tibble(subject_code, 
                      subject_name, 
                      subject_area)
    
    ## This code is simpler, but only gets the high-level affiliation
    ## ie, only UCD
    # author_nodes = xml %>%
    #     xml_find_all('//authors/author')
    # auids = author_nodes %>%
    #     xml_attr('auid')
    # surnames = author_nodes %>%
    #     xml_find_first('ce:surname') %>%
    #     xml_text()
    # given_names = author_nodes %>%
    #     xml_find_first('ce:given-name') %>%
    #     xml_text()
    # 
    # if (length(author_nodes) > 0) {
    #     affiliation_map = author_nodes %>%
    #         `names<-`(auids) %>%
    #         map(xml_find_all, './/affiliation') %>%
    #         map(xml_attr, 'id') %>%
    #         map(~ tibble(aff_id = .)) %>%
    #         bind_rows(.id = 'auid')
    #     
    #     affiliations = xml %>%
    #         xml_find_all('./affiliation')
    #     aff_ids = affiliations %>%
    #         xml_attr('id')
    #     aff_names = affiliations %>%
    #         xml_find_first('affilname') %>%
    #         xml_text()
    #     affiliation_key = tibble(aff_id = aff_ids, 
    #                              aff_name = aff_names)
    #     
    #     aff_tbl = full_join(affiliation_map, affiliation_key)
    # } else {
    #     aff_tbl = tibble(auid = character())
    # }
    # 
    # authors = tibble(auid = auids, 
    #                  surname = surnames, 
    #                  given_name = given_names) %>% 
    #     left_join(aff_tbl)
    
    ## This approach gives us the affiliation as given in the paper
    author_groups = xml %>% 
        xml_find_all('//author-group') %>% 
        `names<-`(., 1:length(.))
    ## Extract affiliations and build author group-affiliation map
    aff_names = author_groups %>% 
        map(xml_find_all, './/affiliation') %>% 
        map(xml_find_first, './organization') %>% 
        map(xml_text)
    aff_ids = author_groups %>% 
        map(xml_find_all, './/affiliation') %>% 
        map(xml_attr, 'afid')
    aff_map = list(aff_name = aff_names, aff_id = aff_ids) %>% 
        transpose() %>% 
        map_dfr(as_tibble, .id = 'author_group')
    ## Extract authors
    author_nodes = author_groups %>% 
        map(xml_find_all, './/author')
    auids = author_nodes %>%
        map(xml_attr, 'auid')
    surnames = author_nodes %>%
        map(xml_find_first, 'ce:surname') %>%
        map(xml_text)
    given_names = author_nodes %>%
        map(xml_find_first, 'ce:given-name') %>%
        map(xml_text)
    ## Combine w/ affiliations
    authors = list(auid = auids, surname = surnames, 
                   given_name = given_names) %>% 
        transpose() %>% 
        map_dfr(as_tibble, .id = 'author_group') %>% 
        left_join(aff_map)
    
    references = xml %>%
        xml_find_all('//bibliography/reference') %>%
        xml_attr('id')
    
    tibble(scopus_id, doi, title, journal, issn,
           date, abstract, keywords, 
           authors = list(authors), 
           subjects = list(subjects),
           references = list(references))
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

# parse('../data/docs/2-s2.0-85007135357.xml') %>% 
#     unnest(authors) %>% 
#     select(surname, aff_id, aff_name)

## 46 sec/100 files -> ~3 hours
## 7785.614 sec = 2+ hours
tic()
pubs = pub_files %>% 
    future_map_dfr(parse, .progress = TRUE)
toc()

