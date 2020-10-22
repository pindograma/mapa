# cnefe_matcher.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

source('acronyms.R')
source('match_shared.R')

extract_good_matches = function(mmatched) {
    confident1 = mmatched %>%
        filter(dist <= 200 & l >= 8) %>%
        remove_ambiguities()

    confident2 = mmatched %>%
        filter(dist > 200 & dist <= 400 & l >= 14) %>%
        remove_ambiguities()

    confident3 = mmatched %>%
        filter(dist > 400 & dist <= 600 & l >= 20) %>%
        remove_ambiguities()

    confident4 = mmatched %>%
        filter(dist > 600 & dist <= 800 & l >= 24) %>%
        remove_ambiguities()

    bind_rows(confident1, confident2, confident3, confident4) %>%
        distinct(ID, .keep_all = T)
}

normalize_address = function(endr) {
    stri_trans_general(str = endr, id = 'Latin-ASCII') %>%
        str_replace_all('\\.', '') %>%
        str_replace_all('ZONA RURAL', '') %>%
        str_replace_all('ZONA URBANA', '') %>%
        str_replace_all('0', 'O') %>%
        str_replace_all('(^|\\s+)D[AEO]S?\\s+', ' ') %>%
        str_replace('[\\s\\-,º]+$', '') %>%
        str_replace('^AV\\s+', 'AVENIDA ') %>%
        str_replace('^R\\s+', 'RUA ') %>%
        str_replace('^ROD\\s+', 'RODOVIA ') %>%
        str_replace('^PCA\\s+', 'PRACA ') %>%
        str_replace('^TRAV\\s+', 'TRAVESSA ') %>%
        str_replace('^TV\\s+', 'TRAVESSA ') %>%
        str_replace('^EST\\s+', 'ESTRADA ') %>%
        str_replace('^ESTR\\s+', 'ESTRADA ') %>%
        str_replace('^PC\\s+', 'PRACA ') %>%
        str_squish()
}

clear_sn = function(endr) {
    endr %>%
        str_replace('SN', '') %>%
        str_replace('S\\/\\s*N', '') %>%
        str_replace('[\\s\\-,º]+$', '') %>%
        str_replace('CENTRO$', '') %>%
        word(1, sep = '-') %>%
        word(1, sep = ',') %>%
        mgsub(from4, to4) %>%
        str_squish()
}

normalize_prefeitura_name = function(x) {
    from = c('^PREFEITURA.*', '^CAMARA MUNICIPAL.*', '^CAMARA DE VEREADORES.*',
             '^CAMARA DOS VEREADORES.*', '^SALAO.*PREFEITURA.*',
             '^PREDIO.*PREFEITURA.*', '^SALAO.*CAMARA',
             '^EDIFICIO.*PREFEITURA.*', '^ATRIO.*PREFEITURA.*',
             'CAMERA')
    to = c('PREFEITURA ', 'CAMARA ', 'CAMARA ',
           'CAMARA ', 'PREFEITURA ',
           'PREFEITURA ', 'CAMARA ',
           'PREFEITURA ', 'PREFEITURA ', 'CAMARA')

    mgsub(x, from, to) %>% str_squish()
}

normalize_ibge_pov_name = function(x, y) {
    x = normalize_simple(x)
    y = normalize_simple(y) %>%
        str_replace_all('\\.', '') %>%
        str_replace('^COL\\s+', '')
    
    from = c('^VILA', '^COMUNIDADE', '^AGROVILA', '^NUCLEO')
    to = c(' ', ' ', ' ', ' ')
    
    ifelse(x == 'POVOADO', paste(x, mgsub(y, from, to)), y) %>%
        str_replace_all('(^|\\s+)D[AEO]S?\\s+', ' ') %>%
        str_squish()
}

get_povoados_com_prefixo = function(base) {
    base %>% filter(
        startsWith(norm_endr, 'POVOADO') |
        startsWith(norm_endr, 'DISTRITO') |
        startsWith(norm_endr, 'COMUNIDADE') |
        startsWith(norm_endr, 'SITIO') |
        startsWith(norm_endr, 'FAZENDA') |
        startsWith(norm_endr, 'ASSENTAMENTO') |
        startsWith(norm_endr, 'LOCALIDADE') |
        startsWith(norm_endr, 'POV ') |
        startsWith(norm_endr, 'ALDEIA') |
        startsWith(norm_endr, 'VILA') |
        startsWith(norm_endr, 'BAIRRO') |
        startsWith(norm_endr, 'COLONIA'))
}

get_povoados_sem_prefixo = function(base, pcp) {
    base %>%
        filter(!(ID %in% pcp$ID)) %>%
        filter(
            !startsWith(norm_endr, 'RUA') &
            !startsWith(norm_endr, 'AVENIDA') &
            !startsWith(norm_endr, 'PRACA') &
            !startsWith(norm_endr, 'ESTRADA') &
            !startsWith(norm_endr, 'RIO') &
            !startsWith(norm_endr, 'LINHA') &
            !startsWith(norm_endr, 'TRAVESSA') &
            !startsWith(norm_endr, 'CORREGO') &
            !startsWith(norm_endr, 'RODOVIA'))
}

get_epsg = function(lat, lon) {
    utm = floor((lon + 180) / 6) + 1
    case_when(
        lat >= 0 & utm == 19 ~ 31973,
        lat >= 0 & utm == 20 ~ 31974,
        lat >= 0 & utm == 21 ~ 31975,
        lat >= 0 & utm == 22 ~ 31976,
        lat < 0 & utm == 18 ~ 31978,
        lat < 0 & utm == 19 ~ 31979,
        lat < 0 & utm == 20 ~ 31980,
        lat < 0 & utm == 21 ~ 31981,
        lat < 0 & utm == 22 ~ 31982,
        lat < 0 & utm == 23 ~ 31983,
        lat < 0 & utm == 24 ~ 31984,
        lat < 0 & utm == 25 ~ 31985,
        T ~ NA_real_
    )
}

# FIXME: We need to deal with the ~9000 NA cases with st_within() at some point.
normalize_cnefe = function(cnefe, mode = 0) {
    all_tracts = read_census_tract(code_tract = 'all', year = 2010) %>%
        mutate(rn = row_number())
    
    if (mode == 1) {
        cnefe = cnefe %>%
            rename(UF = COD_UF, Municipio = COD_MUNICIPIO) %>%
            rename(TipoLogradouro = NOM_TIPO_SEGLOGR) %>%
            rename(TituloLogradouro = NOM_TITULO_SEGLOGR) %>%
            rename(NomeLogradouro = NOM_SEGLOGR) %>%
            rename(NumeroLogradouro = NUM_ENDERECO) %>%
            mutate(IdEstabelecimento = NA) %>%
            rename(Localidade = DSC_LOCALIDADE) %>%
            rename(Lat = LATITUDE, Lon = LONGITUDE) %>%
            st_as_sf(coords = c('Lon', 'Lat'), crs = 4674, remove = F) %>%
            mutate(epsg = get_epsg(Lat, Lon)) %>%
            group_split(epsg) %>%
            map_dfr(function(region) {
                epsg_ = first(region$epsg)
                region %>%
                    st_transform(epsg_) %>%
                    st_join(st_transform(all_tracts, epsg_), st_within) %>%
                    st_drop_geometry()
            }) %>%
            mutate(Distrito = str_sub(code_tract, 8, 9), Subdistrito = str_sub(code_tract, 10, 11), CodSetor = str_sub(code_tract, 12, 15)) %>%
            mutate(CEP = as.character(CEP))
    } else if (mode == 2) {
        coords = st_coordinates(cnefe)

        cnefe = cnefe %>%
            mutate(epsg = get_epsg(coords[,'Y'], coords[,'X'])) %>%
            group_split(epsg) %>%
            map_dfr(function(region) {
                epsg_ = first(region$epsg)
                region %>%
                    st_transform(epsg_) %>%
                    st_join(st_transform(all_tracts, epsg_), st_within) %>%
                    st_drop_geometry()
            }) %>%
            mutate(UF = str_sub(code_tract, 1, 2), Municipio = str_sub(code_tract, 1, 7)) %>%
            rename(TipoLogradouro = NM_TIP_LOG) %>%
            rename(TituloLogradouro = NM_TIT_LOG) %>%
            rename(NomeLogradouro = NM_LOG) %>%
            mutate(NumeroLogradouro = NA, IdEstabelecimento = NA, Localidade = NA, Lat = NA, Lon = NA, CEP = NA) %>%
            mutate(Distrito = str_sub(code_tract, 8, 9), Subdistrito = str_sub(code_tract, 10, 11), CodSetor = str_sub(code_tract, 12, 15))
    }
    
    cnefe %>%
        mutate(compati = as.numeric(paste0(UF, str_pad(Municipio, 5, pad = '0')))) %>%
        mutate(norm_idest = normalize_place(IdEstabelecimento) %>% str_replace_all('0', 'O')) %>%
        mutate(norm_nl = str_replace_all(NomeLogradouro, '(^|\\s+)D[AEO]S?\\s+', '')) %>%
        mutate(norm_tit = recode(TituloLogradouro,
            `SAO` = 'S', `DOUTOR` = 'DR', `SANTA` = 'S', `CORONEL` = 'CEL',
            `PRESIDENTE` = 'PRES', `PADRE` = 'PE', `PROFESSOR` = 'PROF',
            `MARECHAL` = 'MAL', `DOM` = 'D', `SANTO` = 'S', `GENERAL` = 'GAL',
            `BARAO` = 'BR', `NOSSA SENHORA` = 'NS', `VEREADOR` = 'VER',
            `CAPITAO` = 'CAP', `PREFEITO` = 'PREF', `SENADOR` = 'SEN',
            `GOVERNADOR` = 'GOV', `DUQUE` = 'DQ', `MAJOR` = 'MAJ',
            `DEPUTADO` = 'DEP', `DONA` = 'DA', `ENGENHEIRO` = 'ENG',
            `PROFESSORA` = 'PROF', `TENENTE` = 'TEN', `VISCONDE` = 'VISC',
            `FREI` = 'FR', `DESEMBARGADOR` = 'DES', `COMENDADOR` = 'COM')) %>%
        rename(cep = CEP)
}

generate_cnefe_p = function(cnefe) {
    if (nrow(cnefe) == 0) {
        return(tibble())
    }
    
    cnefe %>%
        distinct(compati, TipoLogradouro, norm_tit, norm_nl, Localidade, Distrito, Subdistrito, CodSetor, cep) %>%
        mutate(TipoLogradouro = str_replace_all(TipoLogradouro, '0', 'O') %>% str_squish()) %>%
        mutate(norm_nl = str_replace_all(norm_nl, '0', 'O') %>% str_squish()) %>%
        mutate(addr = paste(TipoLogradouro, norm_nl) %>% str_squish()) %>%
        mutate(full_addr = paste(TipoLogradouro, ifelse(is.na(norm_tit), '', norm_tit), norm_nl) %>% str_squish())
}

match_with_prefix = function(p, cnefe_p, has_prefix, key) {
    l = match_with_prefix_internal(p, cnefe_p, has_prefix, key)
    l[[1]]
}

match_with_prefix_internal = function(p, cnefe_p, has_prefix, key) {
    if (nrow(p) == 0) {
        return(tibble())
    }
    
    ambiguities = tibble()

    p$new_without_fw = ifelse(
        rep(has_prefix, nrow(p)),
        str_replace(p$clear, '.*?\\s', ''),
        p$clear)
    
    stage1 = inner_join(p, cnefe_p, by=c(
        key,
        'clear'='addr'))
    ambiguities = bind_rows(ambiguities, stage1 %>% get_ambiguities(1))
    stage1 = stage1 %>% remove_ambiguities()
    a = p %>% filter(!(ID %in% stage1$ID))

    stage1_1 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='full_addr'))
    ambiguities = bind_rows(ambiguities, stage1_1 %>% get_ambiguities(11))
    stage1_1 = stage1_1 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage1_1$ID))
    
    stage1_2 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='norm_nl'))
    ambiguities = bind_rows(ambiguities, stage1_2 %>% get_ambiguities(12))
    stage1_2 = stage1_2 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage1_2$ID))
    
    stage1_3 = inner_join(a, cnefe_p, by = c(
        key,
        'clear' = 'Localidade'
    ))
    ambiguities = bind_rows(ambiguities, stage1_3 %>% get_ambiguities(13))
    stage1_3 = stage1_3 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage1_3$ID))
    
    stage2 = inner_join(a, cnefe_p, by=c(
        key,
        'new_without_fw'='norm_nl'))
    ambiguities = bind_rows(ambiguities, stage1 %>% get_ambiguities(2))
    stage2 = stage2 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage2$ID))

    stage3 = inner_join(a, cnefe_p, by=c(
        key,
        'new_without_fw'='Localidade'))
    ambiguities = bind_rows(ambiguities, stage3 %>% get_ambiguities(3))
    stage3 = stage3 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage3$ID))
    
    a = a %>% filter(nchar(new_without_fw) > 6)

    stage4 = sqldf(paste0('SELECT *, editdist3(clear, addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage4 %>% get_ambiguities(4))
    stage4 = stage4 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4$ID))
    
    stage4_1 = sqldf(paste0('SELECT *, editdist3(clear, full_addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.full_addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage4_1 %>% get_ambiguities(41))
    stage4_1 = stage4_1 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4_1$ID))
    
    stage4_2 = sqldf(paste0('SELECT *, editdist3(clear, norm_nl) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.norm_nl) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage4_2 %>% get_ambiguities(42))
    stage4_2 = stage4_2 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4_2$ID))
    
    stage4_3 = sqldf(paste0('SELECT *, editdist3(clear, Localidade) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.Localidade) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage4_3 %>% get_ambiguities(43))
    stage4_3 = stage4_3 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4_3$ID))

    stage5 = sqldf(paste0('SELECT *, editdist3(new_without_fw, norm_nl) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.new_without_fw, cnefe_p.norm_nl) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage5 %>% get_ambiguities(5))
    stage5 = stage5 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage5$ID))

    stage6 = sqldf(paste0('SELECT *, editdist3(new_without_fw, Localidade) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.new_without_fw, cnefe_p.Localidade) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage6 %>% get_ambiguities(6))
    stage6 = stage6 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage6$ID))

    list(bind_rows(stage1, stage1_1, stage1_2, stage1_3, stage2, stage3, stage4,
                   stage4_1, stage4_2, stage4_3, stage5, stage6),
         ambiguities)
}

match_sn = function(sn, cnefe_p, key) {
    l = match_sn_internal(sn, cnefe_p, key)
    l[[1]]
}

match_sn_internal = function(sn, cnefe_p, key) {
    a = sn
    ambiguities = tibble()

    stage1 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='norm_nl'))
    ambiguities = bind_rows(ambiguities, stage1 %>% get_ambiguities(1))
    stage1 = stage1 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage1$ID))

    stage2 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='full_addr'))
    ambiguities = bind_rows(ambiguities, stage2 %>% get_ambiguities(2))
    stage2 = stage2 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage2$ID))

    stage3 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='addr'))
    ambiguities = bind_rows(ambiguities, stage3 %>% get_ambiguities(3))
    stage3 = stage3 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage3$ID))

    stage4 = inner_join(a, cnefe_p, by=c(
        key,
        'norm_bairro'='Localidade',
        'clear'='norm_nl'))
    ambiguities = bind_rows(ambiguities, stage4 %>% get_ambiguities(4))
    stage4 = stage4 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4$ID))

    stage5 = inner_join(a, cnefe_p, by=c(
        key,
        'norm_bairro'='Localidade',
        'clear'='full_addr'))
    ambiguities = bind_rows(ambiguities, stage5 %>% get_ambiguities(5))
    stage5 = stage5 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage5$ID))

    stage6 = inner_join(a, cnefe_p, by=c(
        key,
        'norm_bairro'='Localidade',
        'clear'='addr'))
    ambiguities = bind_rows(ambiguities, stage6 %>% get_ambiguities(6))
    stage6 = stage6 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage6$ID))

    a = a %>% filter(nchar(clear) > 6)

    stage7 = sqldf(paste0('SELECT *, editdist3(clear, norm_nl) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.norm_nl) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage7 %>% get_ambiguities(7))
    stage7 = stage7 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage7$ID))
    
    stage8 = sqldf(paste0('SELECT *, editdist3(clear, full_addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.full_addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage8 %>% get_ambiguities(8))
    stage8 = stage8 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage8$ID))

    stage9 = sqldf(paste0('SELECT *, editdist3(clear, addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage9 %>% get_ambiguities(9))
    stage9 = stage9 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage9$ID))

    stage10 = sqldf(paste0('SELECT *, editdist3(clear, norm_nl) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        a.norm_bairro = cnefe_p.Localidade AND
        editdist3(a.clear, cnefe_p.norm_nl) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage10 %>% get_ambiguities(10))
    stage10 = stage10 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage10$ID))

    stage11 = sqldf(paste0('SELECT *, editdist3(clear, full_addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        a.norm_bairro = cnefe_p.Localidade AND
        editdist3(a.clear, cnefe_p.full_addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage11 %>% get_ambiguities(11))
    stage11 = stage11 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage11$ID))

    stage12 = sqldf(paste0('SELECT *, editdist3(clear, addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        a.norm_bairro = cnefe_p.Localidade AND
        editdist3(a.clear, cnefe_p.addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage12 %>% get_ambiguities(12))
    stage12 = stage12 %>% remove_ambiguities()

    list(bind_rows(stage1, stage2, stage3, stage4, stage5, stage6,
         stage7, stage8, stage9, stage10, stage11, stage12),
        ambiguities)
}

match_cn = function(cn, cnefe, cnefe_p, key) {
    l = match_sn_internal(cn, cnefe_p, key)
    success_with_sn = l[[1]]
    missing = cn %>% filter(!(ID %in% success_with_sn$ID))

    sn_ambiguities = l[[2]]
    sn_ambiguities = sn_ambiguities %>% filter(ID %in% missing$ID)

    sn_ambiguities$num = str_extract(sn_ambiguities$norm_endr, '\\d[\\dO]*') %>%
        str_replace_all('O', '0') %>%
        as.numeric()

    num_matched = inner_join(sn_ambiguities, cnefe, by=c(
        key,
        'TipoLogradouro'='TipoLogradouro',
        'norm_tit'='norm_tit',
        'norm_nl'='norm_nl',
        'num'='NumeroLogradouro')) %>%
        remove_ambiguities()

    bind_rows(success_with_sn, num_matched)
}

solve_ambiguities_approx = function(amb) {
    amb %>%
        group_split(stage) %>%
        map_dfr(function(df) {
            df %>% group_by(ID) %>%
                filter(n() <= 3) %>%
                summarize_all(first) %>%
                ungroup()
        })
    
    if (nrow(amb) == 0) { return(tibble()) }
    amb %>% distinct(ID, .keep_all = T)
}

do_address_matching = function(base, cnefe, cnefe_p, key) {
    base$clear = base$norm_endr %>% clear_sn()
    
    povoados_com_prefixo = get_povoados_com_prefixo(base)
    povoados_sem_prefixo = get_povoados_sem_prefixo(base, povoados_com_prefixo)

    ruas = base %>%
        filter(!(ID %in% povoados_com_prefixo$ID)) %>%
        filter(!(ID %in% povoados_sem_prefixo$ID))

    sn = ruas %>% filter(grepl('SN', norm_endr) | grepl('S\\/N', norm_endr) | !grepl('\\d', norm_endr))
    cn = ruas %>% filter(!(ID %in% sn$ID))

    mpcp = match_with_prefix(povoados_com_prefixo, cnefe_p, 1, key)
    mpsp = match_with_prefix(povoados_sem_prefixo, cnefe_p, 0, key)
    msn = match_sn(sn, cnefe_p, key)
    mcn = match_cn(cn, cnefe, cnefe_p, key)

    bind_rows(mpcp, mpsp, msn, mcn)
}

do_address_matching_approx = function(base, cnefe, cnefe_p, key) {
    base$clear = base$norm_endr %>% clear_sn()
    
    povoados_com_prefixo = get_povoados_com_prefixo(base)
    povoados_sem_prefixo = get_povoados_sem_prefixo(base, povoados_com_prefixo)

    ruas = base %>%
        filter(!(ID %in% povoados_com_prefixo$ID)) %>%
        filter(!(ID %in% povoados_sem_prefixo$ID))

    approx_pov_c = match_with_prefix_internal(povoados_com_prefixo, cnefe_p, 1, key)[[2]] %>%
        solve_ambiguities_approx()
    
    approx_pov_s = match_with_prefix_internal(povoados_sem_prefixo, cnefe_p, 0, key)[[2]] %>%
        solve_ambiguities_approx()
    
    approx_ruas = match_sn_internal(ruas, cnefe_p, key)[[2]] %>%
        solve_ambiguities_approx()
    
    bind_rows(approx_pov_c, approx_pov_s, approx_ruas) %>%
        mutate(Lat = NA, Lon = NA)
}

match_with_prefix_ibge = function(pov, agl, has_prefix) {
    pov$new_without_fw = ifelse(
        rep(has_prefix, nrow(pov)),
        str_replace(pov$clear, '.*?\\s', ''),
        pov$clear)
    
    agl$without_fw = str_replace(agl$norm, '.*?\\s', '')
    
    a = pov
    
    stage1 = inner_join(a, agl, by=c(
        'codigo_ibge' = 'code_muni',
        'clear' = 'norm')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage1$ID))

    stage2 = inner_join(a, agl, by=c(
        'codigo_ibge' = 'code_muni',
        'new_without_fw' = 'norm')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage2$ID))
    
    stage3 = inner_join(a, agl, by=c(
        'codigo_ibge' = 'code_muni',
        'new_without_fw' = 'without_fw')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage3$ID))

    a = a %>% filter(nchar(new_without_fw) > 6)

    stage4 = sqldf('SELECT *, editdist3(clear, norm) AS dist FROM a INNER JOIN agl ON
        a.codigo_ibge = agl.code_muni AND
        editdist3(a.clear, agl.norm) <= 200') %>%
        filter(dist > 0) %>%
        remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4$ID))
    
    stage5 = sqldf('SELECT *, editdist3(clear, norm) AS dist FROM a INNER JOIN agl ON
        a.codigo_ibge = agl.code_muni AND
        editdist3(a.new_without_fw, agl.norm) <= 200') %>%
        filter(dist > 0) %>%
        remove_ambiguities()
    a = a %>% filter(!(ID %in% stage5$ID))
    
    stage6 = sqldf('SELECT *, editdist3(clear, without_fw) AS dist FROM a INNER JOIN agl ON
        a.codigo_ibge = agl.code_muni AND
        editdist3(a.new_without_fw, agl.without_fw) <= 200') %>%
        filter(dist > 0) %>%
        remove_ambiguities()
    a = a %>% filter(!(ID %in% stage6$ID))

    bind_rows(stage1, stage2, stage3, stage4, stage5, stage6)
}

run_ibge_agl_match = function(base, agl) {
    base$clear = clear_sn(base$norm_endr)
    agl$norm = normalize_ibge_pov_name(agl$tipoaglomr, agl$nome)
    
    povoados_com_prefixo = get_povoados_com_prefixo(base)
    povoados_sem_prefixo = get_povoados_sem_prefixo(base, povoados_com_prefixo)
    
    bind_rows(
        match_with_prefix_ibge(povoados_com_prefixo, agl, 1),
        match_with_prefix_ibge(povoados_com_prefixo, agl, 0)
    )
}

run_placename_match = function(tg, cnefe) {
    a = tg
    
    cnefe_esc = cnefe %>% filter(EspecieEndereco == 9) %>%
        mutate(norm_escola = future_map_chr(norm_idest, normalize_school_name))

    round3 = inner_join(a, cnefe_esc, by=c(
        'codigo_ibge' = 'compati',
        'norm_local_esc' = 'norm_escola'
    )) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round3$ID))

    round3_1 = inner_join(a, cnefe_esc, by=c(
        'CEP' = 'cep',
        'norm_local_esc' = 'norm_escola'
    )) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round3_1$ID))
    
    round4_prep = sqldf('SELECT *, editdist3(norm_local_esc, norm_escola) AS dist FROM a
                        INNER JOIN cnefe_esc ON
                        a.codigo_ibge = cnefe_esc.compati AND
                        editdist3(a.norm_local_esc, cnefe_esc.norm_escola) <= 1000')
    round4_prep %>% write.csv('round4_backup.csv', row.names=F)
    round4 = extract_good_matches(round4_prep %>% mutate(l = nchar(norm_local_esc)))
    a = a %>% filter(!(ID %in% round4$ID))
    
    round4_1_prep = sqldf('SELECT *, editdist3(norm_local_esc, norm_escola) AS dist FROM a
                        INNER JOIN cnefe_esc ON
                        a.CEP = cnefe_esc.cep AND
                        editdist3(a.norm_local_esc, cnefe_esc.norm_escola) <= 1000')
    round4_1_prep %>% write.csv('round4_1_backup.csv', row.names=F)
    round4_1 = extract_good_matches(round4_1_prep %>% mutate(l = nchar(norm_local_esc)))
    a = a %>% filter(!(ID %in% round4_1$ID))

    cnefe = cnefe %>% filter(EspecieEndereco != 9)
    
    round1 = inner_join(a, cnefe, by=c(
        'codigo_ibge' = 'compati',
        'norm_local' = 'norm_idest'
    )) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round1$ID))

    round1_1 = inner_join(a, cnefe, by=c(
        'CEP' = 'cep',
        'norm_local' = 'norm_idest'
    )) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round1_1$ID))

    # NOTE: This is probably the slowest query in this entire suite of scripts.
    # Hence, we don't attempt to match by CEP. The yield is too small and not
    # worth the time.
    round2_prep = sqldf('SELECT *, editdist3(norm_local, norm_idest) AS dist FROM a
                        INNER JOIN cnefe ON
                        a.codigo_ibge = cnefe.compati AND
                        editdist3(a.norm_local, cnefe.norm_idest) <= 1000')
    round2_prep %>% write.csv('round2_backup.csv', row.names=F)
    round2 = extract_good_matches(round2_prep %>% mutate(l = nchar(norm_local)))
    a = a %>% filter(!(ID %in% round2$ID))

    pref = a %>% filter(
        grepl('PREFEITURA', norm_local, fixed=T) |
        grepl('CAMARA', norm_local, fixed=T))
    pref$norm_local_pref = normalize_prefeitura_name(pref$norm_local)

    cnefe_pref = cnefe %>% filter(
        grepl('PREFEITURA', norm_idest, fixed=T) |
        grepl('CAMERA', norm_idest, fixed=T) |
        grepl('CAMARA', norm_idest, fixed=T))
    cnefe_pref$norm_pref = normalize_prefeitura_name(cnefe_pref$norm_idest)

    round5 = inner_join(pref, cnefe_pref, by=c(
        'codigo_ibge' = 'compati',
        'norm_local_pref' = 'norm_pref'
    )) %>% remove_ambiguities()
    pref = pref %>% filter(!(ID %in% round5$ID))

    round5_1 = inner_join(pref, cnefe_pref, by=c(
        'CEP' = 'cep',
        'norm_local_pref' = 'norm_pref'
    )) %>% remove_ambiguities()
    pref = pref %>% filter(!(ID %in% round5_1$ID))

    l = list(round1, round1_1, round2, round3, round3_1, round4, round4_1, round5, round5_1)
    map_dfr(l, select, -contains('cep')) %>%
        rename(pl_lat = Lat, pl_lon = Lon) %>%
        rename(pl_Distrito = Distrito, pl_Subdistrito = Subdistrito, pl_CodSetor = CodSetor) %>%
        select(ID, IdEstabelecimento, pl_lat, pl_lon, pl_Distrito, pl_Subdistrito, pl_CodSetor)
}

run_address_match = function(tg, cnefe, fun = do_address_matching, is_rural = F) {
    cnefe_p = generate_cnefe_p(cnefe)
    
    if (!is_rural) {
        cnefe_esc = cnefe %>% filter(EspecieEndereco == 9)
        cnefe_saude = cnefe %>% filter(EspecieEndereco == 10)
        
        cnefe_esc_p = generate_cnefe_p(cnefe_esc)
        cnefe_saude_p = generate_cnefe_p(cnefe_saude)
    }
    
    round1 = fun(tg, cnefe, cnefe_p, c('codigo_ibge'='compati'))
    a = tg %>% filter(!(ID %in% round1$ID))

    if (!is_rural) {
        esc = a %>% filter(word(norm_local_esc) %in% school_words)
        round2 = fun(esc, cnefe_esc, cnefe_esc_p, c('codigo_ibge'='compati'))
        a = a %>% filter(!(ID %in% round2$ID))
    
        saude = a %>% filter(grepl('SAUDE', norm_local_esc))
        round3 = fun(saude, cnefe_saude, cnefe_saude_p, c('codigo_ibge'='compati'))
        a = a %>% filter(!(ID %in% round3$ID))
    } else {
        round2 = tibble()
        round3 = tibble()
    }

    round4 = fun(a, cnefe, cnefe_p, c('CEP'='cep'))
    a = a %>% filter(!(ID %in% round4$ID))

    if (!is_rural) {
        round5 = inner_join(a, cnefe_p, by=c('CEP'='cep')) %>% remove_ambiguities()
        a = a %>% filter(!(ID %in% round5$ID))
    
        round6 = inner_join(a, cnefe_p, by=c(
            'CEP'='cep',
            'norm_bairro'='Localidade')) %>% remove_ambiguities()
        a = a %>% filter(!(ID %in% round6$ID))
        
        round7 = inner_join(a, cnefe_p, by=c(
            'codigo_ibge'='compati',
            'norm_bairro'='Localidade')) %>% remove_ambiguities()
    } else {
        round5 = tibble()
        round6 = tibble()
        round7 = tibble()
    }

    l = list(round1, round2, round3, round4, round5, round6, round7)
    map_dfr(l, select, -contains('cep')) %>%
        rename(ad_lat = Lat, ad_lon = Lon) %>%
        rename(ad_Distrito = Distrito, ad_Subdistrito = Subdistrito, ad_CodSetor = CodSetor) %>%
        select(ID, endereco, bairro, TipoLogradouro, norm_tit, norm_nl, Localidade, ad_lat, ad_lon, ad_Distrito, ad_Subdistrito, ad_CodSetor)
}
