## This script retrieves complete metadata for the author histories, for the matched samples defined in 05
library(tidyverse)
library(xml2)

library(furrr)
library(RCurl)
source('api_key.R')

library(assertthat)
library(tictoc)

data_dir = '../data/'
pub_folder = str_c(data_dir, 'docs/')  ## Same folder as 02
parsed_blocks_folder = str_c(data_dir, 'parsed_blocks/')

known_na = 3L

parsed_file = str_c(data_dir, '07_parsed_histories.Rds')  ## Overall output file

scrape_workers = 4
parse_workers = 6

valid_eid = function(eid) {
    ! is.na(eid) & str_detect(eid, '2-s2.0-')
}


## Load data ----
paper_meta = read_rds(str_c(data_dir, '06_author_histories.Rds')) %>% 
    filter(valid_eid(eid))

eids = paper_meta %>% 
    pull(eid) %>% 
    unique()

## How many more papers need to be downloaded?  
to_be_downloaded = pub_folder %>%
    dir() %>%
    setdiff(str_c(eids, '.xml'), .) %>%
    length()
to_be_downloaded

## Functions for scraping from API ----
scrape_ = function (this_eid, print_url = FALSE, download = TRUE) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/abstract/eid/'
    query_url = str_c(base_url, 
                      this_eid, '?',
                      'apiKey=', api_key)
    if (print_url) {
        message(query_url)
    }
    if (download) {
        raw = getURL(query_url)
        raw
    } else {
        return(query_url)
    }
}

# scrape_('2-s2.0-85075958355', download = FALSE)

scrape = function (this_eid, target_folder, ...) {
    ## Either scrape from the API + save the result OR pass
    target_file = str_c(target_folder, this_eid, '.xml')
    # target_file = str_c(target_folder, '/', this_eid, '.xml.zip')
    if (!file.exists(target_file)) {
        raw = scrape_(this_eid, ...)
        write_file(raw, target_file)
        # zip(target_file, target_file_xml, flags = '-9Xq')
        # unlink(target_file_xml)
        return(target_file)
    } else {
        return(target_file)
    }
}

# scrape(eids[1], pub_folder, download = FALSE)

## Do the scraping ----
if (to_be_downloaded > 0) {
    plan(multisession, workers = scrape_workers)
    tic()
    pub_files = eids %>% 
        # head(2e3) %>%
        future_map_chr(scrape, target_folder = pub_folder, 
                       .progress = TRUE)
    toc()
} else {
    pub_files = str_c(pub_folder, eids, '.xml')
}

assert_that(length(pub_files) == length(eids))

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
        left_join(aff_map, by = 'author_group')
    
    references = xml %>%
        xml_find_all('//bibliography/reference') %>%
        xml_attr('id')
    
    tibble(scopus_id, doi, title, journal, issn,
           date, abstract, keywords, 
           authors = list(authors), 
           subjects = list(subjects),
           references = list(references))
}

parse = function(target_file, stop_on_error = FALSE) {
    # print(target_file)
    raw = target_file %>% 
        read_file() %>% 
        str_remove_all('<[0-9][:word:]+>')
    if (raw == '' | str_length(raw) == 155) {
        ## Handle empty responses
        scopus_id = str_extract(target_file, '[0-9]{8,}')
        return(tibble(scopus_id = scopus_id))
    } else {
        parsed = safely(parse_)(raw)
        if (is.null(parsed$error)) {
            return(parsed$result)
        } else {
            if (stop_on_error) {
                stop(paste('Error parsing file ', target_file))
            }
            scopus_id = str_extract(target_file, '[0-9]{8,}')
            return(tibble(scopus_id = scopus_id, error = as.character(parsed$error)))
        }
    }
}

# parse('../data/docs/2-s2.0-85007135357.xml') %>% 
#     unnest(authors) %>% 
#     select(surname, aff_id, aff_name)
# debugonce(parse)
# parse('../data/docs/2-s2.0-85063329503.xml')
# debugonce(parse_)
# read_file('../data/docs/2-s2.0-85037746694.xml') %>% 
#     parse_()

get_eid = function (path) {
    str_extract(path, '[^/]+(?=\\.xml)')
}

parse_block = function(target_files, 
                       prefix = '08', sep = '_',
                       target_folder = data_dir, 
                       write = TRUE,
                       ...) {
    start = get_eid(first(target_files))
    end = get_eid(last(target_files))
    
    block_file = str_c(target_folder, 
                       str_c(prefix, start, end, sep = sep), 
                       '.Rds')
    if (!write | !file.exists(block_file)) {
        parsed_df = map_dfr(target_files, parse, ...)
    } else {
        parsed_df = read_rds(block_file)
    }
    if (write & !file.exists(block_file)) {
        write_rds(parsed_df, block_file)
    }
    
    return(parsed_df)
}

# parse_block('../data/docs/2-s2.0-85063329503.xml', write = FALSE)

if (!file.exists(parsed_file)) {
    block_size = 300
    n_blocks = ceiling(length(pub_files) / block_size)
    blocks_idx = sort(rep_len(1:n_blocks, length(pub_files)))
    blocks = split(pub_files, blocks_idx)
    
    
    options(error = recover)
    
    ## ~20 sec for block of 300 -> 24 minutes
    ## 20 * n_blocks / parse_workers / 60
    # tic()
    # parse_block(blocks[[1]])
    # toc()

    plan(multisession, workers = parse_workers)
    tic()
    pubs = blocks %>% 
        # head(2) %>% 
        future_map_dfr(parse_block, 
                       target_folder = parsed_blocks_folder,
                       .progress = TRUE, 
                       .id = 'block') %>% 
        mutate(block = as.integer(block))
    toc()
    
    ## Validation:  no NA scopus IDs
    assert_that(sum(is.na(pubs$scopus_id)) == 0L)
    ## Validation:  no parsing errors
    assert_that(all(is.na(pubs$error)))
    ## Validation:  exactly 1 row per input document ID
    assert_that(length(eids) == nrow(pubs))
    ## Validation:  complete coverage of paper_meta
    assert_that(length(setdiff(paper_meta$scopus_id, pubs$scopus_id)) == 0L)
    ## Validation:  exactly `known_na` empty rows
    assert_that(sum(is.na(pubs$date)) == known_na)
    ## Validation:  no empty rows
    # assert_that(all(!is.na(pubs$date)))
    
    ## If there are problems, the following can be used to identify and delete blockfiles with problems
    # filter(pubs, !is.na(error)) %>% view()
    # filter(pubs, is.na(date)) %>% view()
    # problem_blocks = pubs %>% 
    #     filter(!is.na(error)) %>% 
    #     count(block) %>% 
    #     pull(block)
    # problem_block_files = blocks[problem_blocks] %>% 
    #     modify(get_eid) %>%
    #     map(~ str_c(first(.), last(.), sep = '_')) %>%
    #     str_c('08_', ., '.Rds')
    # file.remove(str_c(parsed_blocks_folder, problem_block_files))
    
    write_rds(pubs, parsed_file)
} else {
    pubs = read_rds(parsed_file)
}

## Coauthor count ----
## ~90 sec
auids = unique(paper_meta$auid)
tic()
coauths_df = pubs %>% 
    # slice(1:500) %>%
    select(scopus_id, authors) %>% 
    unnest(authors) %>% 
    ## Data has 1 line per author-affiliation-paper combination
    count(scopus_id, auid) %>% 
    select(-n) %>% 
    ## Coauthor pairs w/in articles
    full_join(., ., by = 'scopus_id') %>% 
    filter(auid.x != auid.y) %>% 
    ## Only auids in paper_meta
    filter(auid.x %in% auids) %>% 
    ## Count number of unique coauthors for each author
    count(auid.x, auid.y) %>% 
    select(-n) %>% 
    count(auid = auid.x, name = 'n_coauthors')
toc()

assert_that(are_equal(nrow(coauths_df), length(auids)))

write_rds(coauths_df, str_c(data_dir, '07_coauth_count.Rds'))

