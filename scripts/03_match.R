library(tidyverse)

## Load data ----
data_dir = '../data/'

## ORU Senate Faculty
faculty = read_rds(str_c(data_dir, '01_faculty.Rds'))
## UCD publications, 2016-18
pubs = read_rds(str_c(data_dir, '02_pubs.Rds'))

## Affiliation IDs for UC Davis
ucd_ids = c('60014439', '60023317', '60003160', '60072478', '60000000', 
            '60022586', '60114314', '60114420', '60110341', '60086298', 
            '60028407')

## AUID table ----
## This covers all UCD-affiliated authors
auid_df = pubs %>% 
    filter(!is.na(doi)) %>% 
    unnest(authors) %>% 
    filter(aff_id %in% ucd_ids) %>% 
    count(auid, surname, given_name) %>% 
    ## Canonical names
    mutate(given_name = ifelse(is.na(given_name), 
                               '', 
                               given_name)) %>% 
    # arrange(auid) %>% 
    mutate(given_len = str_length(given_name)) %>% 
    group_by(auid) %>% 
    summarize(surname = surname[which.max(given_len)], 
              given_name = given_name[which.max(given_len)], 
              n = sum(n)) %>% 
    ungroup() %>% 
    arrange(surname, given_name, desc(n)) %>% 
    ## Clean diacritics
    mutate(given_name = iconv(given_name, to = 'ASCII//TRANSLIT'), 
           given_name = str_remove_all(given_name, "'"))


## Join ----
## faculty$name looks like 'Daniel Hicks'
## pubs has separate surname and given_name

matched = faculty %>%
    ## Split faculty$name
    rowwise() %>%
    mutate(split = list(str_match(name, '(.+) ([^ ]+)'))) %>%
    ungroup() %>%
    mutate(given_name = map_chr(split, 2),
           surname = map_chr(split, 3)) %>%
    select(-split) %>%
    ## Join w/ auids on surname only
    inner_join(auid_df, by = 'surname') %>% 
    ## ORU roster name (given_name.x) should be a substring of Scopus name (given_name.y)
    mutate(substring = str_detect(given_name.y, given_name.x)) %>% 
    filter(substring) %>%
    select(-substring, -given_name.x) %>% 
    rename(given_name = given_name.y)

## Who isn't matched automatically?  
anti_join(faculty, matched) %>% 
    count(ORU) %>% 
    left_join(count(faculty, ORU), by = 'ORU') %>% 
    mutate(frac_unmatched = n.x / n.y) %>% 
    arrange(desc(frac_unmatched))

# View(anti_join(faculty, matched), 'unmatched')

## Manual matches
manual_match = read_csv(str_c(data_dir, '00_manual_matches.csv'), 
         col_types = 'cc') %>% 
    inner_join(faculty) %>% 
    inner_join(auid_df)

## Combine automatic and manual matches
matched = bind_rows(matched, manual_match)

## Remaining unmatched
anti_join(faculty, matched)
1 - nrow(count(matched, name)) / nrow(count(faculty, name))

## Are departmental affiliations unique?  
## ORU faculty first
## No, not at all
## Even matching 'Department of' to the beginning of the string wouldn't pull things down to 1
pubs %>% 
    filter(!is.na(doi)) %>% 
    unnest(authors) %>% 
    inner_join(matched, by = 'auid') %>% 
    filter(aff_id %in% ucd_ids, 
           str_detect(aff_name, 'Department of')) %>% 
    count(surname.x, given_name.y, aff_name) #%>% 
    # count(surname.x, given_name.y) %>% 
    # arrange(desc((n)))


## Departments ----
departments = pubs %>% 
    filter(!is.na(doi)) %>% 
    unnest(authors) %>% 
    inner_join(matched, by = 'auid') %>% 
    count(auid, aff_name, aff_id) %>% 
    filter(aff_id %in% ucd_ids, !is.na(aff_name)) %>% 
    count(aff_name) %>% 
    arrange(desc(n)) %>% 
    filter(str_detect(aff_name, 'Department of')) %>% 
    pull(aff_name)

## Subjects for our analysis ----
## Authors in the same departments
codept = pubs %>% 
    filter(!is.na(doi)) %>% 
    unnest(authors) %>% 
    filter(aff_id %in% ucd_ids, 
           aff_name %in% departments) %>% 
    count(auid) %>% 
    pull(auid)

## Publication counts (2016-18)
pub_counts = pubs %>% 
    filter(!is.na(doi)) %>% 
    unnest(authors) %>% 
    filter(auid %in% codept) %>% 
    count(auid, doi) %>% 
    select(-n) %>% 
    count(auid)

## (counts below ignore false duplicate auids)
## 6.0k authors have 1+ paper
## 2.8k authors have 3+ papers
pub_counts %>% 
    count(n) %>% 
    arrange(desc(n)) %>% 
    mutate(cum = cumsum(nn)) %>% 
    arrange(n)

## 136 ORU faculty have 1+ papers
## 120 ORU faculty have 3+ papers
pub_counts %>% 
    filter(auid %in% matched$auid) %>% 
    count(n) %>% 
    arrange(desc(n)) %>% 
    mutate(cum = cumsum(nn)) %>% 
    arrange(n)

## Output ----
write_rds(matched, str_c(data_dir, '03_matched.Rds'))
write_rds(codept, str_c(data_dir, '03_codepartmentals.Rds'))

