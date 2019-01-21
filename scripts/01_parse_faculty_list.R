library(tidyverse)
library(XML)

data_dir = '../data/'

table_names = c('AQRC Senate Faculty', 
                'AQRC Other Academics', 
                'BML/CMSI Senate Faculty', 
                'BML/CMSI Other Academics', 
                'BML/CMSI Senate Faculty Collaborators', 
                'CNPRC Senate Faculty', 
                'CNPRC Other Academics', 
                'CCC Senate Faculty', 
                'CCC Other Academics', 
                'CCC Collaborators', 
                'CHPR Senate Faculty', 
                'CHPR Other Faculty', 
                'CHPR Program Members', 
                'ITS Senate Faculty', 
                'ITS Other Academics', 
                'JMIE Senate Faculty', 
                'JMIE Other Academics', 
                'PICN Senate Faculty', 
                'PICN Other Academics')

tables = readHTMLTable(str_c(data_dir, '00_faculty_list.html'), 
                       stringsAsFactors = FALSE) %>%
    set_names(table_names)

dataf = tables %>% 
    # .[1:3] %>% 
    ## Reformat columns
    map(~ tibble(name = c(.[,1], .[,3]), 
                 department = c(.[,2], .[,4]))) %>% 
    bind_rows(.id = 'table') %>% 
    ## Extract ORU and faculty groups from table name
    separate(col = table, into = c('ORU', 'faculty_group'), 
             sep = ' ', extra = 'merge', remove = FALSE) %>% 
    ## Clean extra whitespace
    mutate_at(vars(name, department), str_squish) %>% 
    filter(name != '')

## Extract departments
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    count(department) %>% 
    write_csv(str_c(data_dir, '01_departments.csv'))

## Manually set canonical department names in `01_departments_canonical.csv`

## Join canonical department names
dataf_clean = dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    left_join(read_csv(str_c(data_dir, '01_departments_canonical.csv')), 
              by = 'department') %>% 
    select(-department, -n) %>% 
    rename(department = canonical)

write_rds(dataf_clean, str_c(data_dir, '01_faculty.Rds'))

stop("Don't run EDA automatically")
## Exploring ----
## There's a little mess in the faculty groups; but "Senate Faculty" seems like the group we want
count(dataf, faculty_group)

ggplot(dataf, aes(ORU, fill = faculty_group)) +
    geom_bar(position = 'stack')


## >_<
count(dataf, department)

## That's a little better
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    count(department) %>% 
    write_csv(str_c(data_dir, '01_departments.csv'))

## Still a bunch of departments
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    left_join(read_csv(str_c(data_dir, '01_departments_canonical.csv')), 
              by = 'department') %>% 
    select(-department, -n) %>% 
    rename(department = canonical) %>% 
    count(department)
    # ggplot(aes(ORU, fill = department)) +
    # geom_bar(position = 'stack')

## What ORUs have both many researchers and few different departments? 
## None
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    left_join(read_csv(str_c(data_dir, '01_departments_canonical.csv')), 
              by = 'department') %>% 
    select(-department, -n) %>% 
    rename(department = canonical) %>% 
    count(ORU, department) %>% 
    group_by(ORU) %>% 
    summarize(n = sum(n), 
              diff_depts = n()) %>% 
    ggplot(aes(n, diff_depts)) +
    geom_text(aes(label = ORU))

## Other direction:  What departments are represented across many ORUs? 
## Civil & Enviro Eng; Public Health Sci; maybe also Ag & Res Econ
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    left_join(read_csv(str_c(data_dir, '01_departments_canonical.csv')), 
              by = 'department') %>% 
    select(-department, -n) %>% 
    rename(department = canonical) %>% 
    count(ORU, department) %>% 
    group_by(department) %>% 
    summarize(n = sum(n), 
              diff_orus = n()) %>% 
    ggplot(aes(n, diff_orus)) +
    geom_text(aes(label = department))

dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    left_join(read_csv(str_c(data_dir, '01_departments_canonical.csv')), 
              by = 'department') %>% 
    select(-department, -n) %>% 
    rename(department = canonical) %>% 
    count(ORU, department) %>% 
    group_by(department) %>% 
    summarize(n = sum(n), 
              diff_orus = n()) %>% 
    arrange(desc(n), desc(diff_orus)) %>% 
    mutate(cumsum = cumsum(n))


## Still only 25 individuals
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    left_join(read_csv(str_c(data_dir, '01_departments_canonical.csv')), 
              by = 'department') %>% 
    select(-department, -n) %>% 
    rename(department = canonical) %>% 
    filter(department %in% c('Civil & Enviro Eng', 'Public Health Sci')) %>% 
    count(department, name) %>% 
    count(department)


## Any individuals represented multiple times? 
## Several dozen if we include all faculty groups
## Only 5 show up twice on Senate Faculty lists
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    count(name) %>% 
    count(n)

## Dan Tancredi is listed as affiliated with JMIE under Earth and Planetary Sciences?? 
dataf %>% 
    filter(faculty_group == 'Senate Faculty') %>% 
    add_count(name) %>% 
    filter(n > 1) %>% 
    arrange(name)
