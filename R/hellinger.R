hellinger = function(dataf, 
                     gamma1 = gamma.x, gamma2 = gamma.y, 
                     id1 = auid.x, id2 = auid.y, 
                     ...) {
    gamma1 = enquo(gamma1)
    gamma2 = enquo(gamma2)
    
    id1 = enquo(id1)
    id2 = enquo(id2)
    
    dataf %>%
        mutate(h_dist_term = sqrt(!!gamma1 * !!gamma2)) %>% 
        group_by(k, !!id1, !!id2, ...) %>%
        summarize(h_dist = sqrt(1 - sum(h_dist_term))) %>%
        ungroup()
}
