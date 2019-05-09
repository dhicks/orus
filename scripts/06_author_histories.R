## This script retrieves EIDs for all of every author's prior papers

## TODO:  put the department-ORU network viz back in somewhere

library(tidyverse)
library(xml2)

# library(igraph)
# library(tidygraph)
# library(ggraph)
# devtools::install_github("schochastics/smglr")
# library(smglr)

library(furrr)
library(RCurl)
source('api_key.R')

library(assertthat)
library(tictoc)

data_dir = '../data/'
author_folder = str_c(data_dir, 'author_histories/')

plan(multiprocess, workers = 2)


## Load and filter data ----
dropouts_03 = read_rds(str_c(data_dir, '03_dropout.Rds'))
dropouts_05 = read_rds(str_c(data_dir, '05_dropouts.Rds'))
oru_df = read_rds(str_c(data_dir, '03_matched.Rds')) %>% 
    anti_join(dropouts_03, by = 'auid') %>% 
    anti_join(dropouts_05, by = 'auid')
departments = read_rds(str_c(data_dir, '03_codepartmentals.Rds'))

author_meta = read_rds(str_c(data_dir, '05_author_meta.Rds'))

anti_join(oru_df, author_meta, by = 'auid') %>% 
    nrow() %>% 
    are_equal(0L) %>% 
    assert_that(msg = 'oru_df authors missing from author_meta')


## Network viz ----
# dept_net = departments_filtered %>% 
#     filter(auid %in% author_meta$auid) %>% 
#     graph_from_data_frame(directed = FALSE) %>% 
#     as_tbl_graph() %>% 
#     left_join(author_meta, by = c(name = 'auid')) %>% 
#     mutate(type = case_when(is.na(oru) ~ 'department', 
#                             oru ~ 'ORU faculty', 
#                             !oru ~ 'other faculty'))
# 
# oru_net = oru_df %>% 
#     select(oru = ORU, auid) %>% 
#     graph_from_data_frame(directed = FALSE) %>% 
#     simplify() %>% 
#     as_tbl_graph() %>% 
#     mutate(type = case_when(str_detect(name, '[0-9]+') ~ 'ORU faculty',
#                             TRUE ~ 'ORU'))
# 
# net = graph_join(dept_net, oru_net) %>% 
#     as.undirected() %>% 
#     as_tbl_graph() %>% 
#     mutate(degree = centrality_degree(), 
#            btwn = centrality_betweenness())

## Degree distributions for different node types
# net %>% 
#     as_tibble() %>% 
#     group_by(type) %>% 
#     summarize_at(vars(degree), 
#                  funs(n = n(), min, mean, median, max))

## 110 sec
# layout_file = '05_layout.Rds'
# if (!file.exists(layout_file)) {
#     ## 110 sec
#     stress = layout_with_stress(net)
#     stress = stress %>% 
#         as_tibble() %>% 
#         rename(x = V1, y = V2)
#     write_rds(stress, str_c(data_dir, layout_file))
# } else {
#     stress = read_rds(str_c(data_dir, layout_file))
# }
# 
# # ## 105 sec
# # tic()
# # backbone = layout_as_backbone(net)
# # toc()
# 
# graph_attr(thing, 'layout') = NULL
# 
# net %>% 
#     mutate(x = stress$x, 
#            y = stress$y) %>% 
#     filter(degree > 1) %>% 
#     `graph_attr<-`('layout', data.frame(x = V(.)$x, 
#                                         y = V(.)$y)) %>% 
#     ggraph(layout = 'nicely') +
#     geom_edge_link(alpha = .5) +
#     geom_node_point(aes(color = type, size = btwn)) +
#     geom_node_text(aes(label = name), 
#                    data = function(dataf) {
#                        subset(dataf, type == 'ORU')
#                    }) +
#     geom_node_text(aes(label = name), 
#                    size = 1,
#                    data = function(dataf) {
#                        subset(dataf, type == 'department')
#                    }) +
#     theme_graph()
# ggsave(str_c('../plots/', '05_network.png'), 
#        height = 10, width = 15, dpi = 300, scale = 1/2)

## ORU-dep't network
## NB could use dep't-dep't and ORU-ORU connections
# inner_join(oru_df, departments_filtered) %>% 
#     select(oru = ORU, aff_name) %>% 
#     graph_from_data_frame(directed = FALSE) %>% 
#     as_tbl_graph() %>% 
#     mutate(type = ifelse(name %in% oru_df$ORU, 
#                          'ORU', 
#                          'department')) %>% 
#     ggraph(layout = 'nicely') +
#     geom_edge_fan(alpha = .5) +
#     geom_node_point(aes(color = type)) +
#     geom_node_text(aes(label = name), 
#                    data = function(dataf) {
#                        subset(dataf, type == 'ORU')
#                    }) +
#     geom_node_text(aes(label = name), 
#                    size = 1,
#                    data = function(dataf) {
#                        subset(dataf, type == 'department')
#                    }) +
#     theme_graph()



## Functions for scraping from API ----
scrape_ = function (this_auid, page_idx, print_url = FALSE) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/search/scopus?'
    query_url = str_c(base_url, 
                      'query=au-id(', this_auid, ')',
                      '&count=200',
                      '&start=', 200*(page_idx-1),
                      '&httpAccept=application/xml',
                      '&apiKey=', api_key)
    if (print_url) {
        print(query_url)
    }
    raw = getURL(query_url)
    raw
}

scrape = function (this_auid, n_docs, target_folder, 
                   force_scrape = FALSE) {
    ## In this data, max(n_docs) = 370, so max(total_pages) = 2
    total_pages = n_docs %/% 200 + 1
    target_files = str_c(this_auid, '_', 1:total_pages, '.xml')
    for (page_idx in 1:total_pages) {
        ## Either scrape from the API + save the result OR pass
        target_file = str_c(target_folder, target_files[page_idx])
        if (!file.exists(target_file) | force_scrape) {
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
## ~460 sec when force_scrape = TRUE
tic()
author_history_files = author_meta %>% 
    # head(10) %>%
    # rowwise() %>% 
    future_map2(.x = .$auid, .y = .$n_docs, 
                .f = ~scrape(.x, .y, author_folder, 
                             force_scrape = FALSE), 
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
    files = str_c(author_folder, files)
    parsed_df = files %>% 
        map(read_file) %>% 
        map_dfr(parse_)
    return(parsed_df)
}
# parse(author_history_files[[9]], author_folder)

## Do the parsing ----
## 14 sec / 100 -> ~1 minute
tic()
histories_df = author_history_files %>% 
    set_names(author_meta$auid) %>% 
    # head(100) %>%
    future_map_dfr(parse, author_folder, .id = 'auid', .progress = TRUE)
toc()

## Correct number of docs per author? 
## Except for 1 case, we have a few papers in the search that aren't in the metadata
histories_df %>% 
    count(auid) %>% 
    right_join(author_meta, by = 'auid') %>% 
    mutate(right_count = n == n_docs) %>% 
    filter(!right_count) %>% 
    # count(n - n_docs)
    filter(abs(n - n_docs) >= 6) %>% view()
    # ggplot(aes(log10(n), log10(n_docs))) +
    # geom_point() +
    # stat_function(fun = identity)

## How many unique papers? 
## 22.6k
histories_df %>% 
    pull(eid) %>% 
    unique() %>% 
    length()
## 18.3k using DOIs
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
write_rds(histories_df, str_c(data_dir, '06_author_histories.Rds'))
