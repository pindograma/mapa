source('acronyms.R')
source('match_shared.R')

# Functions
run_inep_match = function(local_2018_f, escolas_geocoded_inep) {
    local_2018_f$norm_local_t = local_2018_f$norm_local_esc

    j = c(
        'norm_local_t'='norm_escola_t',
        'norm_cidade'='norm_cidade',
        'SGL_UF'='UF')

    matched = inner_join(local_2018_f, escolas_geocoded_inep, by = j) %>%
        remove_ambiguities()
    left = local_2018_f %>% filter(!(ID %in% matched$ID))

    matched2 = sqldf('SELECT *, editdist3(norm_local_t, norm_escola_t) FROM left
                     INNER JOIN escolas_geocoded_inep ON
                     left.norm_cidade = escolas_geocoded_inep.norm_cidade AND
                     left.SGL_UF = escolas_geocoded_inep.UF AND
                     editdist3(left.norm_local_t, escolas_geocoded_inep.norm_escola_t) <= 1000')
    matched2$norm_cidade_temp = matched2$norm_cidade
    matched2 = subset(matched2, select = -c(norm_cidade))
    matched2$norm_cidade = matched2$norm_cidade_temp

    mmatched2 = matched2 %>%
        rename(dist = `editdist3(norm_local_t, norm_escola_t)`) %>%
        mutate(l = nchar(norm_local_t))

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

    from_p = to4
    to_p = rep(' ', length(to4))

    left2$norm_local_t = mgsub(left2$norm_local_t, from_p, to_p)
    escolas_geocoded_inep$norm_escola_t = mgsub(escolas_geocoded_inep$norm_escola_t, from_p, to_p)

    left2$norm_local_t = str_squish(left2$norm_local_t)
    escolas_geocoded_inep$norm_escola_t = str_squish(escolas_geocoded_inep$norm_escola_t)

    matched4 = sqldf('SELECT *, editdist3(norm_local_t, norm_escola_t) FROM left2
                     INNER JOIN escolas_geocoded_inep ON
                     left2.norm_cidade = escolas_geocoded_inep.norm_cidade AND
                     left2.SGL_UF = escolas_geocoded_inep.UF AND
                     editdist3(left2.norm_local_t, escolas_geocoded_inep.norm_escola_t) <= 1000')
    matched4$norm_cidade_temp = matched4$norm_cidade
    matched4 = subset(matched4, select = -c(norm_cidade))
    matched4$norm_cidade = matched4$norm_cidade_temp

    mmatched4 = matched4 %>%
        rename(dist = `editdist3(norm_local_t, norm_escola_t)`) %>%
        mutate(l = nchar(norm_local_t))

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

    confident_all2 %>%
        rename(inep_lat = Latitude) %>%
        rename(inep_lon = Longitude) %>%
        select(ID, Escola, norm_escola_t, inep_lat, inep_lon)
}