# tse_file_reader.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

open_2012 = function() {
    local_2012 <- read_excel("data/local-votacao-2012.xlsx", sheet = "Plan2",
        col_types = c("text", 
            "numeric", "numeric", "numeric", 
            "text", "numeric", "text", "text", 
            "numeric", "text", "text", "skip", 
            "text", "text", "numeric", "text", 
            "text"))
    
    local_2012 %>%
        mutate(ano = 2012) %>%
        rename(local = NM_LOCALVOTACAO) %>%
        rename(codigo_ibge = COD_MUNIC_IBGE) %>%
        rename(uf = UF) %>%
        rename(cidade = NM_MUNIC_IBGE) %>%
        rename(bairro = NM_BAIRRO) %>%
        rename(endereco = ENDERECO_LOCALVOTACAO) %>%
        rename(CEP = NUM_CEP) %>%
        mutate(CEP = as.character(CEP)) %>%
        mutate(tse_lat = NA, tse_lon = NA) %>%
        rename(zona = ZONA, secao = SECAO) %>%
        select(ano, local, codigo_ibge, uf, cidade, bairro, endereco,
               tse_lat, tse_lon, CEP, zona, secao)
}

open_2014 = function() {
    local_2014 = read_delim('data/local-votacao-2014.csv', ';',
        escape_double = F,
        locale = locale(decimal_mark = ','),
        col_types = cols(
            COD_BAIRRO_ECT = col_character(),
            NUM_CEP = col_character()
        ),
        trim_ws = T)
    
    local_2014 %>%
        mutate(ano = 2014) %>%
        rename(local = NM_LOCALVOTACAO) %>%
        rename(codigo_ibge = COD_MUNIC_IBGE) %>%
        rename(uf = UF) %>%
        rename(cidade = NM_MUNIC_IBGE) %>%
        rename(bairro = NM_BAIRRO) %>%
        rename(endereco = ENDERECO_LOCALVOTACAO) %>%
        rename(CEP = NUM_CEP) %>%
        mutate(tse_lat = NA, tse_lon = NA) %>%
        rename(zona = ZONA, secao = SECAO) %>%
        select(ano, local, codigo_ibge, uf, cidade, bairro, endereco,
               tse_lat, tse_lon, CEP, zona, secao)
}

open_2016 = function() {
    correspondencia = read_csv('data/municipios_brasileiros_tse.csv') %>%
        select(codigo_tse, codigo_ibge)
    local_2016 = read_delim('data/local-votacao-2016.csv', ';',
        escape_double = F,
        locale = locale(encoding = 'ISO-8859-1', decimal_mark = ','),
        trim_ws = T)
    
    local_2016 %>%
        mutate(ano = 2016) %>%
        left_join(correspondencia, by = c('CD_LOCALIDADE_TSE' = 'codigo_tse')) %>%
        rename(local = NM_LOCVOT) %>%
        rename(uf = SG_UF) %>%
        rename(cidade = NM_LOCALIDADE) %>%
        rename(bairro = DS_BAIRRO) %>%
        rename(endereco = DS_ENDERECO) %>%
        rename(CEP = NR_CEP) %>%
        mutate(tse_lat = NA, tse_lon = NA) %>%
        rename(zona = NR_ZONA, secao = NR_SECAO) %>%
        select(ano, local, codigo_ibge, uf, cidade, bairro, endereco,
               tse_lat, tse_lon, CEP, zona, secao)
}

open_2018 = function() {
    local_2018 <- read_delim("data/local-votacao-2018-aug.csv",  ";",
        escape_double = F,
        col_types = cols(
            LATITUDE_ZONA = col_double(),
            LONGITUDE_ZONA = col_double(),
            LATITUDE_LOCAL = col_double(),
            LONGITUDE_LOCAL = col_double()
        ),
        locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
        trim_ws = T)
    
    local_2018 %>%
        mutate(ano = 2018) %>%
        rename(local = LOCAL_VOTACAO) %>%
        rename(codigo_ibge = COD_LOCALIDADE_IBGE) %>%
        rename(uf = SGL_UF) %>%
        rename(cidade = LOCALIDADE_LOCAL_VOTACAO) %>%
        rename(bairro = BAIRRO_LOCAL_VOT) %>%
        rename(endereco = ENDERECO) %>%
        rename(tse_lat = LATITUDE_LOCAL, tse_lon = LONGITUDE_LOCAL) %>%
        rename(zona = ZONA, secao = NUM_SECAO) %>%
        select(ano, local, codigo_ibge, uf, cidade, bairro, endereco,
               tse_lat, tse_lon, CEP, zona, secao)
}

open_2020 = function() {
    local_2020 = read_csv('data/local-votacao-2020-jun.csv',
        col_types = cols(COD_OBJETO_PAIS = col_character()),
        locale = locale(decimal_mark = ','))
    
    local_2020 %>%
        mutate(ano = 2020) %>%
        rename(local = NOM_LOCVOT) %>%
        rename(codigo_ibge = COD_LOCALIDADE_IBGE) %>%
        rename(uf = SGL_UF) %>%
        rename(cidade = NOM_LOCALIDADE) %>%
        rename(bairro = NOM_BAIRRO) %>%
        rename(endereco = DES_ENDERECO) %>%
        rename(CEP = NUM_CEP) %>%
        rename(tse_lat = NUM_LATITUDE, tse_lon = NUM_LONGITUDE) %>%
        rename(zona = NUM_ZONA, secao = NUM_SECAO) %>% 
        select(local, codigo_ibge, uf, cidade, bairro, endereco,
               tse_lat, tse_lon, CEP, zona, secao)
}

open_tse = c(
    '2012' = open_2012,
    '2014' = open_2014,
    '2016' = open_2016,
    '2018' = open_2018,
    '2020' = open_2020
) 
