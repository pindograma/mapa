# match_geocoding.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

library(readr)
library(sf)
library(geobr)
library(dplyr)
library(furrr)

plan(multiprocess)

source('tse_file_reader.R')
source('cnefe_matcher.R')
source('match_shared.R')

sfc_as_cols <- function(x, names = c("x","y")) {
    stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
    ret <- sf::st_coordinates(x)
    ret <- tibble::as_tibble(ret)
    stopifnot(length(names) == ncol(ret))
    x <- x[ , !names(x) %in% names]
    ret <- setNames(ret,names)
    dplyr::bind_cols(x,ret)
}

tse_locations = bind_rows(
    open_2018() %>% select(codigo_ibge, local, endereco, tse_lat, tse_lon) %>% mutate(ano = 2018),
    open_2020() %>% select(codigo_ibge, local, endereco, tse_lat, tse_lon) %>% mutate(ano = 2020)
) %>%
    rename(comp_tse_lat = tse_lat, comp_tse_lon = tse_lon) %>%
    filter(!is.na(comp_tse_lat) & !is.na(comp_tse_lon)) %>%
    mutate(norm_local = normalize_place(local)) %>%
    mutate(norm_endr = normalize_address(endereco)) %>%
    select(-local, -endereco) %>%
    distinct() %>%
    arrange(desc(ano))
save(tse_locations, file = 'tse_locations.Rdata')
rm(tse_locations)

escolas_geocoded_inep <- read_delim("escolas-geocoded-inep.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE) %>%
    mutate(norm_cidade = normalize_simple(`Município`)) %>%
    mutate(norm_escola_t = future_map_chr(Escola, normalize_school_name)) %>%
    mutate(norm_escola_wt = remove_titles(norm_escola_t))
save(escolas_geocoded_inep, file = 'escolas_geocoded_inep.Rdata')
rm(escolas_geocoded_inep)

normalize_sp_name = function(x, y) {
    from = c('CLASSE HOSPITALAR', 'CL HOSPITALAR', 'CEEJA',
             'CENTRO DE DETENCAO PROVISORIA ',
             'CENTRO DE PROGRESSAO PENITENCIARIA ')
    to = rep(' ', 5)
    y = mgsub(y, from, to)
    
    remove = c('CEL',
               'FEBEM - UI (CASA À PARTIR DE 2007)',
               'FEBEM - UIP (CASA À PARTIR DE 2007)')
    x = x[!(x %in% remove)]
    
    x = recode(x,
        `CLASSE PENITENCIÁRIA` = '',
        `QUILOMBO` = 'ESCOLA',
        `ÁREA DE ASSENTAMENTO` = '')
    
    paste(x, y) %>% str_squish()
}

read_rio = function(path, cn = 'Nome_Escol') {
    st_read(path) %>%
        st_transform(4326) %>%
        sfc_as_cols(c('local_lon', 'local_lat')) %>%
        st_drop_geometry() %>%
        tibble() %>%
        mutate(UF = 'RJ') %>%
        mutate(norm_cidade = 'RIO DE JANEIRO') %>%
        mutate(norm_escola_t = normalize_school_name(!!as.symbol(cn)))
}

escolas_geocoded_recife = read_sf('recife.geojson') %>%
    st_transform(31985) %>%
    st_centroid() %>%
    st_transform(4326) %>%
    sfc_as_cols(c('local_lon', 'local_lat')) %>%
    st_drop_geometry() %>%
    tibble() %>%
    mutate(escola_tipo = recode(escola_tipo,
        `Cre` = 'CRECHE',
        `Esc` = 'EM'
    )) %>%
    mutate(UF = 'PE') %>%
    mutate(norm_cidade =  'RECIFE') %>%
    mutate(norm_escola_t = paste(escola_tipo, escola_nome) %>% normalize_school_name())

escolas_geocoded_sp = read_delim('estado-saopaulo.csv', ';',
    locale = locale(decimal_mark = ',', encoding = 'ISO-8859-1')) %>%
    rename(local_lat = DS_LATITUDE, local_lon = DS_LONGITUDE) %>%
    mutate(UF = 'SP') %>%
    mutate(norm_cidade = normalize_simple(MUN)) %>%
    mutate(norm_escola_t = normalize_sp_name(TIPOESC, NOMESC) %>% normalize_school_name())

escolas_geocoded_poa = read_delim('portoalegre.csv', ';') %>%
    rename(local_lat = latitude, local_lon = longitude) %>%
    mutate(UF = 'RS') %>%
    mutate(norm_cidade = 'PORTO ALEGRE') %>%
    mutate(norm_escola_t = normalize_school_name(nome))

escolas_geocoded_al = read_csv('alagoas.csv') %>%
    rename(local_lat = `geometry/coordinates/0`, local_lon = `geometry/coordinates/1`) %>%
    mutate(UF = 'AL') %>%
    mutate(norm_cidade = normalize_simple(`properties/Município`)) %>%
    mutate(norm_escola_t = str_replace(`properties/Nome`, '\\(INTEGRAL\\)', '')) %>%
    mutate(norm_escola_t = normalize_school_name(norm_escola_t)) %>%
    filter(!grepl('LOCALIZACAO APROXIMADA', norm_escola_t))

escolas_geocoded_rio_mun = read_rio('escolas-rio-mun/Escolas_Municipais.shp', 'SMEDBOEs33')
escolas_geocoded_rio_est = read_rio('escolas-rio-est/Escolas_Estaduais.shp')
escolas_geocoded_rio_fed = read_rio('escolas-rio-fed/Escolas_Federais.shp')

local_schools_sel = bind_rows(
    escolas_geocoded_recife,
    escolas_geocoded_sp,
    escolas_geocoded_poa,
    escolas_geocoded_al,
    escolas_geocoded_rio_mun,
    escolas_geocoded_rio_est,
    escolas_geocoded_rio_fed,
) %>% select(UF, norm_cidade, norm_escola_t, local_lat, local_lon)
save(local_schools_sel, file = 'local_schools_sel.Rdata')
rm(local_schools_sel)

cnefe_rural = read_delim('cnefe_agro/all.csv', ';', col_types = cols(
    NOM_COMP_ELEM4=col_character(),
    VAL_COMP_ELEM4=col_character(),
    NOM_COMP_ELEM5=col_character(),
    VAL_COMP_ELEM5=col_character(),
    NOM_TITULO_SEGLOGR=col_character())) %>%
    filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
    normalize_cnefe(T)
save(cnefe_rural, file = 'cnefe_rural.Rdata')
rm(cnefe_rural)

mun = read_municipality(code_muni = 'all', 2017) %>%
    mutate(rn = row_number())
ibge_agl = st_read('ibge/loc_aglomerado_rural_isolado_p.shp') %>%
    st_transform(4674) %>%
    mutate(mun = st_within(., mun) %>% st_unlist()) %>%
    inner_join(mun %>% st_drop_geometry(), by = c('mun' = 'rn')) %>%
    sfc_as_cols(c('ibge_approx_lon', 'ibge_approx_lat')) %>%
    st_drop_geometry() %>%
    tibble()
save(ibge_agl, file = 'ibge_agl.Rdata')
rm(ibge_agl)

cnefe_with_ids = read_csv('cnefe_with_ids.csv',
    col_types = cols(
        CEP = col_character(),

        Lat = col_character(),
        Lon = col_character(),

        Elemento2 = col_character(),
        Elemento3 = col_character(),
        Elemento4 = col_character(),
        Elemento5 = col_character(),
        Elemento6 = col_character(),
        Valor2 = col_character(),
        Valor3 = col_character(),
        Valor4 = col_character(),
        Valor5 = col_character(),
        Valor6 = col_character()
    )) %>%
        filter(EspecieEndereco != 8) %>%
        normalize_cnefe()
save(cnefe_with_ids, file = 'cnefe_with_ids.Rdata')
rm(cnefe_with_ids)

cnefe_all = read_csv('cnefe_filtered.csv',
    col_types = cols(
        CEP = col_character(),

        Lat = col_character(),
        Lon = col_character(),

        Elemento2 = col_character(),
        Elemento3 = col_character(),
        Elemento4 = col_character(),
        Elemento5 = col_character(),
        Elemento6 = col_character(),
        Valor2 = col_character(),
        Valor3 = col_character(),
        Valor4 = col_character(),
        Valor5 = col_character(),
        Valor6 = col_character()
    )) %>% normalize_cnefe()
save(cnefe_all, file = 'cnefe_all.Rdata')
rm(cnefe_all)
