# cnefe_matcher.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

source('acronyms.R')
source('match_shared.R')

get_ambiguities = function(x) {
    x %>% group_by(ID) %>%
        filter(n() > 1) %>%
        ungroup()
}

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

normalize_cnefe = function(cnefe) {
    cnefe$compati = paste0(cnefe$UF, str_pad(cnefe$Municipio, 5, pad='0')) %>%
        as.numeric()

    cnefe$norm_idest = normalize_simple(cnefe$IdEstabelecimento) %>%
        str_replace_all('0', 'O')

    cnefe$norm_nl = str_replace_all(cnefe$NomeLogradouro, '(^|\\s+)D[AEO]S?\\s+', '')
    cnefe$norm_tit = recode(cnefe$TituloLogradouro,
        `SAO` = 'S',
        `DOUTOR` = 'DR',
        `SANTA` = 'S',
        `CORONEL` = 'CEL',
        `PRESIDENTE` = 'PRES',
        `PADRE` = 'PE',
        `PROFESSOR` = 'PROF',
        `MARECHAL` = 'MAL',
        `DOM` = 'D',
        `SANTO` = 'S',
        `GENERAL` = 'GAL',
        `BARAO` = 'BR',
        `NOSSA SENHORA` = 'NS',
        `VEREADOR` = 'VER',
        `CAPITAO` = 'CAP',
        `PREFEITO` = 'PREF',
        `SENADOR` = 'SEN',
        `GOVERNADOR` = 'GOV',
        `DUQUE` = 'DQ',
        `MAJOR` = 'MAJ',
        `DEPUTADO` = 'DEP',
        `DONA` = 'DA',
        `ENGENHEIRO` = 'ENG',
        `PROFESSORA` = 'PROF',
        `TENENTE` = 'TEN',
        `VISCONDE` = 'VISC',
        `FREI` = 'FR',
        `DESEMBARGADOR` = 'DES',
        `COMENDADOR` = 'COM')

    cnefe %>% rename(cep = CEP)
}

generate_cnefe_p = function(cnefe) {
    cnefe_p = cnefe %>%
        group_by(compati, TipoLogradouro, norm_tit, norm_nl, Localidade, CodSetor, cep) %>%
        summarize() %>%
        ungroup()
    cnefe_p$TipoLogradouro = str_replace_all(cnefe_p$TipoLogradouro, '0', 'O') %>% str_squish()
    cnefe_p$norm_nl = str_replace_all(cnefe_p$norm_nl, '0', 'O') %>% str_squish()

    cnefe_p$addr = paste(cnefe_p$TipoLogradouro, cnefe_p$norm_nl) %>% str_squish()
    cnefe_p$full_addr = paste(
        cnefe_p$TipoLogradouro,
        ifelse(is.na(cnefe_p$norm_tit), '', cnefe_p$norm_tit),
        cnefe_p$norm_nl) %>% str_squish()

    cnefe_p
}

match_with_prefix = function(p, cnefe_p, has_prefix, key) {
    if (nrow(p) == 0) {
        return(tibble())
    }

    p$new_without_fw = ifelse(
        rep(has_prefix, nrow(p)),
        str_replace(p$clear, '.*?\\s', ''),
        p$clear)
    
    stage1 = inner_join(p, cnefe_p, by=c(
        key,
        'clear'='addr')) %>% remove_ambiguities()
    a = p %>% filter(!(ID %in% stage1$ID))

    stage1_1 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='full_addr')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage1_1$ID))

    stage2 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='norm_nl')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage2$ID))

    stage3 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='Localidade')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage3$ID))

    a = a %>% filter(nchar(new_without_fw) > 6)

    stage4 = sqldf(paste0('SELECT *, editdist3(clear, addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.addr) <= 200')) %>%
        filter(dist > 0) %>%
        remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4$ID))
    
    stage4_1 = sqldf(paste0('SELECT *, editdist3(clear, full_addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.full_addr) <= 200')) %>%
        filter(dist > 0) %>%
        remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4_1$ID))

    stage5 = sqldf(paste0('SELECT *, editdist3(new_without_fw, norm_nl) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.new_without_fw, cnefe_p.norm_nl) <= 200')) %>%
        filter(dist > 0) %>%
        remove_ambiguities()
    a = a %>% filter(!(ID %in% stage5$ID))

    stage6 = sqldf(paste0('SELECT *, editdist3(new_without_fw, Localidade) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.new_without_fw, cnefe_p.Localidade) <= 200')) %>%
        filter(dist > 0) %>%
        remove_ambiguities()
    a = a %>% filter(!(ID %in% stage6$ID))

    bind_rows(stage1, stage1_1, stage2, stage3, stage4, stage4_1, stage5, stage6)
}

match_sn_internal = function(sn, cnefe_p, key) {
    a = sn
    ambiguities = tibble()

    stage1 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='norm_nl'))
    ambiguities = bind_rows(ambiguities, stage1 %>% get_ambiguities())
    stage1 = stage1 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage1$ID))

    stage2 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='full_addr'))
    ambiguities = bind_rows(ambiguities, stage2 %>% get_ambiguities())
    stage2 = stage2 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage2$ID))

    stage3 = inner_join(a, cnefe_p, by=c(
        key,
        'clear'='addr'))
    ambiguities = bind_rows(ambiguities, stage3 %>% get_ambiguities())
    stage3 = stage3 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage3$ID))

    stage4 = inner_join(a, cnefe_p, by=c(
        key,
        'norm_bairro'='Localidade',
        'clear'='norm_nl'))
    ambiguities = bind_rows(ambiguities, stage4 %>% get_ambiguities())
    stage4 = stage4 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage4$ID))

    stage5 = inner_join(a, cnefe_p, by=c(
        key,
        'norm_bairro'='Localidade',
        'clear'='full_addr'))
    ambiguities = bind_rows(ambiguities, stage5 %>% get_ambiguities())
    stage5 = stage5 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage5$ID))

    stage6 = inner_join(a, cnefe_p, by=c(
        key,
        'norm_bairro'='Localidade',
        'clear'='addr'))
    ambiguities = bind_rows(ambiguities, stage6 %>% get_ambiguities())
    stage6 = stage6 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage6$ID))

    a = a %>% filter(nchar(clear) > 6)

    stage7 = sqldf(paste0('SELECT *, editdist3(clear, norm_nl) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.norm_nl) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage7 %>% get_ambiguities())
    stage7 = stage7 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage7$ID))
    
    stage8 = sqldf(paste0('SELECT *, editdist3(clear, full_addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.full_addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage8 %>% get_ambiguities())
    stage8 = stage8 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage8$ID))

    stage9 = sqldf(paste0('SELECT *, editdist3(clear, addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        editdist3(a.clear, cnefe_p.addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage9 %>% get_ambiguities())
    stage9 = stage9 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage9$ID))

    stage10 = sqldf(paste0('SELECT *, editdist3(clear, norm_nl) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        a.norm_bairro = cnefe_p.Localidade AND
        editdist3(a.clear, cnefe_p.norm_nl) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage10 %>% get_ambiguities())
    stage10 = stage10 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage10$ID))

    stage11 = sqldf(paste0('SELECT *, editdist3(clear, full_addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        a.norm_bairro = cnefe_p.Localidade AND
        editdist3(a.clear, cnefe_p.full_addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage11 %>% get_ambiguities())
    stage11 = stage11 %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% stage11$ID))

    stage12 = sqldf(paste0('SELECT *, editdist3(clear, addr) AS dist FROM a INNER JOIN cnefe_p ON
        a.', names(key)[1],' = cnefe_p.', key[[1]], ' AND
        a.norm_bairro = cnefe_p.Localidade AND
        editdist3(a.clear, cnefe_p.addr) <= 200')) %>%
        filter(dist > 0)
    ambiguities = bind_rows(ambiguities, stage12 %>% get_ambiguities())
    stage12 = stage12 %>% remove_ambiguities()

    list(bind_rows(stage1, stage2, stage3, stage4, stage5, stage6,
         stage7, stage8, stage9, stage10, stage11, stage12),
        ambiguities)
}

match_sn = function(sn, cnefe_p, key) {
    l = match_sn_internal(sn, cnefe_p, key)
    l[[1]]
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

do_address_matching = function(base, cnefe, cnefe_p, key) {
    base$clear = base$norm_endr %>% clear_sn()
    
    povoados_com_prefixo = base %>% filter(
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

    povoados_sem_prefixo = base %>%
        filter(!(ID %in% povoados_com_prefixo$ID)) %>% filter(
        !startsWith(norm_endr, 'RUA') &
        !startsWith(norm_endr, 'AVENIDA') &
        !startsWith(norm_endr, 'PRACA') &
        !startsWith(norm_endr, 'ESTRADA') &
        !startsWith(norm_endr, 'RIO') &
        !startsWith(norm_endr, 'LINHA') &
        !startsWith(norm_endr, 'TRAVESSA') &
        !startsWith(norm_endr, 'CORREGO') &
        !startsWith(norm_endr, 'RODOVIA'))

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

run_placename_match = function(tg, cnefe) {
    a = tg

    round1 = inner_join(a, cnefe, by=c(
        'COD_LOCALIDADE_IBGE' = 'compati',
        'norm_local' = 'norm_idest'
    )) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round1$ID))

    round1_1 = inner_join(a, cnefe, by=c(
        'CEP' = 'cep',
        'norm_local' = 'norm_idest'
    )) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round1_1$ID))

    # NOTE: This is probably the slowest query in this entire suite of scripts.
    # It might take half of its entire runtime.  Hence, we don't attempt to match
    # by CEP. The yield is too small and not worth the time. (The same reasoning
    # applies to round4).
    round2_prep = sqldf('SELECT *, editdist3(norm_local, norm_idest) AS dist FROM a
                        INNER JOIN cnefe ON
                        a.COD_LOCALIDADE_IBGE = cnefe.compati AND
                        editdist3(a.norm_local, cnefe.norm_idest) <= 1000')
    round2_prep %>% write.csv('round2_backup.csv', row.names=F)
    round2 = extract_good_matches(round2_prep %>% mutate(l = nchar(norm_local)))
    a = a %>% filter(!(ID %in% round2$ID))

    cnefe_esc = cnefe %>% filter(EspecieEndereco == 9)
    cnefe_esc$norm_escola = future_map(cnefe_esc$norm_idest, normalize_school_name) %>% unlist()

    round3 = inner_join(a, cnefe_esc, by=c(
        'COD_LOCALIDADE_IBGE' = 'compati',
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
                        a.COD_LOCALIDADE_IBGE = cnefe_esc.compati AND
                        editdist3(a.norm_local_esc, cnefe_esc.norm_escola) <= 1000')
    round4_prep %>% write.csv('round4_backup.csv', row.names=F)
    round4 = extract_good_matches(round4_prep %>% mutate(l = nchar(norm_local_esc)))
    a = a %>% filter(!(ID %in% round4$ID))

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
        'COD_LOCALIDADE_IBGE' = 'compati',
        'norm_local_pref' = 'norm_pref'
    )) %>% remove_ambiguities()

    round5_1 = inner_join(pref, cnefe_pref, by=c(
        'CEP' = 'cep',
        'norm_local_pref' = 'norm_pref'
    )) %>% remove_ambiguities()

    l = list(round1, round1_1, round2, round3, round3_1, round4, round5, round5_1)
    map_dfr(l, select, -contains('cep'))
        rename(cnefe_lat = Lat) %>%
        rename(cnefe_lon = Lon) %>%
        select(ID, IdEstabelecimento, CodSetor, cnefe_lat, cnefe_lon)
}

run_address_match = function(tg, cnefe) {
    cnefe_p = generate_cnefe_p(cnefe)
    
    cnefe_esc = cnefe %>% filter(EspecieEndereco == 9)
    cnefe_esc_p = generate_cnefe_p(cnefe_esc)

    cnefe_saude = cnefe %>% filter(EspecieEndereco == 10)
    cnefe_saude_p = generate_cnefe_p(cnefe_saude)
    
    round1 = do_address_matching(tg, cnefe, cnefe_p, c('COD_LOCALIDADE_IBGE'='compati'))
    a = tg %>% filter(!(ID %in% round1$ID))

    esc = a %>% filter(word(norm_local_esc) %in% school_words)
    round2 = do_address_matching(esc, cnefe_esc, cnefe_esc_p, c('COD_LOCALIDADE_IBGE'='compati'))
    a = a %>% filter(!(ID %in% round2$ID))

    saude = a %>% filter(grepl('SAUDE', norm_local_esc))
    round3 = do_address_matching(saude, cnefe_saude, cnefe_saude_p, c('COD_LOCALIDADE_IBGE'='compati'))
    a = a %>% filter(!(ID %in% round3$ID))

    round4 = do_address_matching(a, cnefe, cnefe_p, c('CEP'='cep'))
    a = a %>% filter(!(ID %in% round4$ID))

    round5 = inner_join(a, cnefe_p, by=c('CEP'='cep')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round5$ID))

    round6 = inner_join(a, cnefe_p, by=c(
        'CEP'='cep',
        'norm_bairro'='Localidade')) %>% remove_ambiguities()
    a = a %>% filter(!(ID %in% round6$ID))

    round7 = inner_join(a, cnefe_p, by=c(
        'COD_LOCALIDADE_IBGE'='compati',
        'norm_bairro'='Localidade')) %>% remove_ambiguities()

    l = list(round1, round2, round3, round4, round5, round6, round7)
    map_dfr(l, select, -contains('cep')) %>%
        rename(cnefe_lat = Lat) %>%
        rename(cnefe_lon = Lon) %>%
        select(ID, TipoLogradouro, TituloLogrdouro, NomeLogradouro, NumeroLogradouro, CodSetor, cnefe_lat, cnefe_lon)
}
