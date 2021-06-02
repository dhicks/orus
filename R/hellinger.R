hellinger_old = function(dataf, 
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

#' Using matrix multiplication makes the distance calculations *much* faster
hellinger_ = function(mx1, mx2 = NULL) {
    if (is.null(mx2)) {
        mx2 = t(mx1)
    } else {
        mx2 = t(mx2)
    }
    assert_that(are_equal(ncol(mx1), nrow(mx2)), 
                msg = 'Matrices must have same number of columns')
    
    mx1.2 = sqrt(mx1) %*% sqrt(mx2)
    return(sqrt(1 - mx1.2))
}

#' Convert a tidied topic model dataframe to a matrix
build_matrix = function(topics, id_col, ...) {
    topics %>% 
        filter(...) %>% 
        select(one_of(id_col), topic, gamma) %>% 
        pivot_wider(names_from = 'topic', 
                    values_from = 'gamma') %>% 
        column_to_rownames(id_col) %>% 
        as.matrix()
}

#' Hellinger distance
#' 
#' Hellinger distances, either pairwise within a single topic model df or between two topic model dfs
#' @param {topics1, topics2} Tidied topic model dataframes
#' @param {id1, id2} Document identifiers (auids, ORU name, etc.)
#' @param df Should the function return the matrix of Hellinger distances (default) or a tidy dataframe? 
#' @return matrix (default) or tidy dataframe of Hellinger distances
hellinger = function(topics1, id1, 
                     ...,
                     topics2 = NULL, id2 = NULL, 
                     df = FALSE) {
    matrix1 = build_matrix(topics1, id1, ...)
    if (is.null(id2)) {
        id2 = id1
    }
    if (is.null(topics2)) {
        matrix2 = matrix1
    } else {
        matrix2 = build_matrix(topics2, id2, ...)
    }
    
    hellinger_matrix = hellinger_(matrix1, matrix2)
    hellinger_matrix = replace_na(hellinger_matrix, 0.0)
    
    if (!df) {
        return(hellinger_matrix)
    }
    if (are_equal(id1, id2)) {
        id1 = str_c(id1, '_x')
        id2 = str_c(id2, '_y')
    }
    hellinger_matrix %>% 
        as_tibble(rownames = id1) %>% 
        pivot_longer(-one_of(id1), 
                     names_to = id2,
                     values_to = 'dist')
}