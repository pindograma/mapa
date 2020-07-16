source('acronyms.R')
source('match_shared.R')

# Functions
run_inep_match = function(local_2018_f, escolas_geocoded_inep) {
    j = c(
        'norm_local_esc'='norm_escola_t',
        'norm_cidade'='norm_cidade',
        'uf'='UF')

    matched = inner_join(local_2018_f, escolas_geocoded_inep, by = j) %>%
        remove_ambiguities()
    left = local_2018_f %>% filter(!(ID %in% matched$ID))

    matched2 = sqldf('SELECT *, editdist3(norm_local_esc, norm_escola_t) FROM left
                     INNER JOIN escolas_geocoded_inep ON
                     left.norm_cidade = escolas_geocoded_inep.norm_cidade AND
                     left.uf = escolas_geocoded_inep.UF AND
                     editdist3(left.norm_local_esc, escolas_geocoded_inep.norm_escola_t) <= 1000')
    matched2$norm_cidade_temp = matched2$norm_cidade
    matched2 = subset(matched2, select = -c(norm_cidade))
    matched2$norm_cidade = matched2$norm_cidade_temp

    mmatched2 = matched2 %>%
        rename(dist = `editdist3(norm_local_esc, norm_escola_t)`) %>%
        mutate(l = nchar(norm_local_esc))

    confident2 = mmatched2 %>%
        filter(dist > 0 & dist <= 200) %>%
        remove_ambiguities()

    confident3 = mmatched2 %>%
        filter(dist > 200 & dist <= 400 & l >= 12) %>%
        remove_ambiguities()

    confident4 = mmatched2 %>%
        filter(dist > 400 & dist <= 600 & l >= 18) %>%
        remove_ambiguities()

    confident5 = mmatched2 %>%
        filter(dist > 600 & dist <= 800 & l >= 22) %>%
        remove_ambiguities()

    confident_all = bind_rows(
        matched,
        confident2,
        confident3,
        confident4,
        confident5
    ) %>% distinct(ID, .keep_all = T)

    left2 = local_2018_f %>% filter(!(ID %in% confident_all$ID))

    matched4 = sqldf('SELECT *, editdist3(norm_local_esc_wt, norm_escola_wt) FROM left2
                     INNER JOIN escolas_geocoded_inep ON
                     left2.norm_cidade = escolas_geocoded_inep.norm_cidade AND
                     left2.uf = escolas_geocoded_inep.UF AND
                     editdist3(left2.norm_local_esc_wt, escolas_geocoded_inep.norm_escola_wt) <= 1000')
    matched4$norm_cidade_temp = matched4$norm_cidade
    matched4 = subset(matched4, select = -c(norm_cidade))
    matched4$norm_cidade = matched4$norm_cidade_temp

    mmatched4 = matched4 %>%
        rename(dist = `editdist3(norm_local_esc_wt, norm_escola_wt)`) %>%
        mutate(l = nchar(norm_local_esc_wt))

    confident6 = mmatched4 %>%
        filter(dist <= 200) %>%
        remove_ambiguities()

    confident7 = mmatched4 %>%
        filter(dist > 200 & dist <= 400 & l >= 12) %>%
        remove_ambiguities()

    confident8 = mmatched4 %>%
        filter(dist > 400 & dist <= 600 & l >= 18) %>%
        remove_ambiguities()

    confident9 = mmatched4 %>%
        filter(dist > 600 & dist <= 800 & l >= 22) %>%
        remove_ambiguities()

    confident_all2 = bind_rows(
        confident_all,
        confident6,
        confident7,
        confident8,
        confident9
    ) %>% distinct(ID, .keep_all = T)
    
    if ('Latitude' %in% colnames(confident_all2)) {
        return(confident_all2 %>%
            rename(inep_lat = Latitude) %>%
            rename(inep_lon = Longitude) %>%
            select(ID, Escola, norm_escola_t, norm_escola_wt, inep_lat, inep_lon))
    } else {
        return(confident_all2 %>%
            select(ID, norm_escola_t, norm_escola_wt, local_lat, local_lon))
    }
}
