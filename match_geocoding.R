# match_geocoding.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

# Imports
library(readr)
library(stringi)
library(dplyr)
library(stringr)
library(mgsub)
library(sqldf)
library(purrr)
library(furrr)
library(sf)
library(geobr)
library(readxl)

source('cnefe_matcher.R')
source('inep_matcher.R')
source('google_matcher.R')
source('tse_file_reader.R')

# Configuration
google_full_reset = F
options('sqldf.dll' = '/home/arch/spellfix.so')
plan(multiprocess)

# Code
addr_current = read_csv('ADDR_CURRENT.csv', col_types = cols(
    ad_lat = col_character(),
    ad_lon = col_character()
)) %>%
    mutate(norm_local = normalize_place(local))

local_2018_f = open_2012() %>%
    filter(uf != 'ZZ') %>%
    mutate(ID = row_number()) %>%
    mutate(norm_cidade = normalize_simple(cidade)) %>%
    mutate(norm_cidade = recode(norm_cidade,
         `SANTA ISABEL DO PARA` = 'SANTA IZABEL DO PARA',
         `GRACCHO CARDOSO` = 'GRACHO CARDOSO',
         `ELDORADO DOS CARAJAS` = 'ELDORADO DO CARAJAS',
         `QUINJINGUE` = 'QUIJINGUE',
         `GRAO-PARA` = 'GRAO PARA',
         `SAO LUIS DO PARAITINGA` = 'SAO LUIZ DO PARAITINGA',
         `SEM PEIXE` = 'SEM-PEIXE'
    )) %>%
    mutate(norm_local = normalize_place(local)) %>%
    mutate(norm_bairro = normalize_simple(bairro)) %>%
    mutate(norm_endr = normalize_address(endereco)) %>%
    anti_join(addr_current, by = c(
        'codigo_ibge' = 'codigo_ibge',
        'norm_local' = 'norm_local'
    )) %>%
    distinct(codigo_ibge, local, endereco, .keep_all = T) %>%
    mutate(norm_local_esc = future_map_chr(local, normalize_school_name)) %>%
    mutate(norm_google_endr = revert_unidecode_damage(endereco)) %>%
    mutate(norm_google_bairro = revert_unidecode_damage(bairro)) %>%
    mutate(norm_google_cidade = revert_unidecode_damage(cidade))

load('tse_locations.Rdata')
tse_match = inner_join(local_2018_f, tse_locations, by = c(
    'codigo_ibge' = 'codigo_ibge',
    'norm_local' = 'norm_local',
    'norm_endr' = 'norm_endr'
)) %>%
    group_by(ID) %>%
    filter(n() == 1 | (n() == 2 & n_distinct(ano) == 2)) %>%
    ungroup() %>%
    distinct(codigo_ibge, norm_local, norm_endr, .keep_all = T)
rm(tse_locations)

load('escolas_geocoded_inep.Rdata')
inep = run_inep_match(
    local_2018_f %>% filter(uf != 'DF'),
    escolas_geocoded_inep)

conflicts_solved = read_csv("solve_backup.csv")
sc = local_2018_f %>%
    inner_join(conflicts_solved, by = c(
        'norm_cidade' = 'norm_cidade',
        'local' = 'LOCAL_VOTACAO'
    )) %>%
    inner_join(escolas_geocoded_inep, by = c(
        'uf' = 'UF',
        'norm_cidade' = 'norm_cidade',
        'Escola' = 'Escola'
    )) %>%
    rename(inep_lat = Latitude, inep_lon = Longitude) %>%
    remove_ambiguities()
rm(escolas_geocoded_inep)

load('local_schools_sel.Rdata')
local_schools = run_inep_match(
    local_2018_f %>% filter(uf != 'DF'),
    local_schools_sel)
rm(local_schools_sel)

load('cnefe_with_ids.Rdata')
cnefe_pl = run_placename_match(
    local_2018_f %>% filter(uf != 'DF'),
    cnefe_with_ids)
rm(cnefe_with_ids)

load('cnefe_rural.Rdata')
cnefe_rural_addr = run_address_match(local_2018_f, cnefe_rural, is_rural = T) %>%
    rename(rural_Distrito = ad_Distrito) %>%
    rename(rural_Subdistrito = ad_Subdistrito) %>%
    rename(rural_CodSetor = ad_CodSetor)
rm(cnefe_rural)

load('cnefe_all.Rdata')
cnefe_addr = run_address_match(
    local_2018_f %>% filter(uf != 'DF'),
    cnefe_all)

load('ibge_agl.Rdata')
ibge_addr_approx = run_ibge_agl_match(local_2018_f, ibge_agl)
rm(ibge_agl)

matched1 = bind_rows(
    local_2018_f %>% filter(!is.na(tse_lat) & !is.na(tse_lon)),
    tse_match,
    inner_join(
        local_2018_f,
        inep %>% filter(!is.na(inep_lat) & !is.na(inep_lon)),
        by = c('ID' = 'ID')),
    inner_join(
        local_2018_f,
        local_schools %>% filter(!is.na(local_lat) & !is.na(local_lon)),
        by = c('ID' = 'ID')
    ),
    inner_join(local_2018_f, cnefe_pl, by = c('ID' = 'ID')),
    inner_join(local_2018_f, cnefe_addr, by = c('ID' = 'ID')),
    inner_join(local_2018_f, cnefe_rural_addr, by = c('ID' = 'ID')),
    sc %>% filter(!is.na(inep_lat) & !is.na(inep_lon)),
    ibge_addr_approx
)

fo = function(x) { first(x, order_by=x) }
matched_grouped_1 = matched1 %>%
    group_by(ID) %>%
    summarize(
        uf = fo(uf),
        cidade = fo(cidade),
        bairro = fo(bairro),
        endereco = fo(endereco),
        
        norm_google_cidade = fo(norm_google_cidade),
        norm_google_bairro = fo(norm_google_bairro),
        norm_google_endr = fo(norm_google_endr),
        
        tse_lat = fo(tse_lat),
        comp_tse_lat = fo(comp_tse_lat),
        inep_lat = fo(inep_lat),
        local_lat = fo(local_lat),
        pl_CodSetor = fo(pl_CodSetor),
        ad_CodSetor = fo(ad_CodSetor),
        rural_CodSetor = fo(rural_CodSetor))

left = local_2018_f %>% filter(!(ID %in% matched_grouped_1$ID))

matched_grouped_1 = matched_grouped_1 %>% mutate(results =
    (!is.na(tse_lat) | !is.na(comp_tse_lat)) +
    (!is.na(inep_lat)) +
    (!is.na(pl_CodSetor)) +
    (!is.na(ad_CodSetor)) +
    (!is.na(rural_CodSetor)) +
    (!is.na(local_lat)))

to_geocode_1 = matched_grouped_1 %>%
    filter(results == 1) %>%
    generate_addr_export()
to_geocode_2 = left %>% generate_addr_export()
to_geocode = bind_rows(to_geocode_1, to_geocode_2)

legacy_tried = tibble()
google_tried = tibble()
if (!google_full_reset) {
    legacy_tried = read_csv('data/google/legacy/original_to_geocode.csv') %>%
        mutate(jnd = paste(ENDERECO, BAIRRO_LOCAL_VOT, LOCALIDADE_LOCAL_VOTACAO, SGL_UF) %>% str_squish())
    
    files = list.files(
        path = 'data/google/geocoder-filtered-output',
        pattern = '*.csv',
        full.names = T,
        recursive = F)
    google_tried = reduce(files, function(acc, x) {
        geocoded = read_csv(x, col_types = cols())
        bind_rows(acc, geocoded)
    }, .init = tibble()) %>%
        mutate(jnd = paste(endr_orig, bairro_orig, cidade, uf) %>% str_squish())
    
    to_geocode = to_geocode %>%
        mutate(jnd = paste(endereco, bairro, cidade, uf) %>% str_squish()) %>%
        mutate(jnd2 = paste(norm_google_endr, bairro, cidade, uf) %>% str_squish()) %>%
        filter(!(jnd %in% legacy_tried$jnd)) %>%
        filter(!(jnd2 %in% google_tried$jnd))
}
to_geocode %>% write.csv('EXPORT_GOOGLE_ADDR_PHASE_1.csv', row.names = F)

google_geocoded = tibble()
google_legacy = tibble()

if (!google_full_reset) {
    google_legacy = read_csv('data/google/legacy/geocoded.csv') %>%
        select(-ID)
    
    files = list.files(
        path = 'data/google/geocoder-filtered-output',
        pattern = '*_okay.csv',
        full.names = T,
        recursive = F)
    google_geocoded = map_dfr(files, read_csv)
}

gmatch_legacy = match_geocoded_legacy(local_2018_f, google_legacy)
gmatch = match_geocoded(local_2018_f, google_geocoded)

matched2 = bind_rows(matched1, gmatch_legacy, gmatch)

local_2018_f = local_2018_f %>%
    mutate(pl_without_damage = revert_unidecode_damage(local)) %>%
    mutate(pl_jnd = paste(pl_without_damage, norm_google_cidade, uf) %>% str_squish())

gpl_tried = tibble()
to_place = local_2018_f %>% filter(!(ID %in% matched2$ID))

if (!google_full_reset) {
    pfiles = list.files(
        path = 'data/google/places-filtered-output',
        pattern = '*.csv',
        full.names = T,
        recursive = F)
    gpl_tried = map_dfr(pfiles, read_csv) %>%
        mutate(jnd = paste(placename_orig, cidade, uf) %>% str_squish())

    to_place = to_place %>%
        filter(!(pl_jnd %in% gpl_tried$jnd))
}
to_place %>% write.csv('EXPORT_GOOGLE_PLACES.csv', row.names = F)

gpl = read_csv('gplaces2.csv', col_types = cols( ID = col_skip() )) %>%
    filter(matched == 1)

pmatch = bind_rows(
    inner_join(local_2018_f, gpl, by = c(
        'uf' = 'uf',
        'norm_google_cidade' = 'cidade',
        'pl_without_damage' = 'placename_orig',
        'norm_endr' = 'endr_orig'
    )),
    inner_join(local_2018_f, gpl, by = c(
        'uf' = 'uf',
        'norm_google_cidade' = 'cidade',
        'pl_without_damage' = 'placename_orig',
        'endereco' = 'endr_orig'
    ))) %>%
        distinct(ID, .keep_all = T) %>%
        rename(places_lat = lat, places_lon = lon)

matched3 = bind_rows(matched2, pmatch)
left = local_2018_f %>% filter(!(ID %in% matched3$ID))

cnefe_addr_approx = run_address_match(left, cnefe_all, do_address_matching_approx) %>%
    rename(approx_ad_Distrito = ad_Distrito, approx_ad_Subdistrito = ad_Subdistrito,
           approx_ad_CodSetor = ad_CodSetor)
rm(cnefe_all)

to_geocode_2 = left %>% generate_addr_export(T)
if (!google_full_reset) {
    to_geocode_2 = to_geocode_2 %>%
        mutate(jnd = paste(endereco, bairro, cidade, uf) %>% str_squish()) %>%
        mutate(jnd2 = paste(revert_unidecode_damage(endereco), bairro, cidade, uf) %>% str_squish()) %>%
        filter(!(jnd %in% legacy_tried$jnd)) %>%
        filter(!(jnd2 %in% google_tried$jnd))
}
to_geocode_2 %>% write.csv('EXPORT_GOOGLE_ADDR_PHASE_2.csv', row.names = F)

google_approx = tibble()
legacy_approx = tibble()

if (!google_full_reset) {
    approx_files = list.files(
        path = 'data/google/geocoder-filtered-output',
        pattern = '*_approx.csv',
        full.names = T,
        recursive = F)
    google_approx = reduce(approx_files, function(acc, x) {
        geocoded = read_csv(x)
        bind_rows(acc, geocoded)
    }, .init = tibble())
    legacy_approx = read_csv('data/google/legacy/approx.csv') %>% select(-ID)
}

gmatch_legacy_approx = match_geocoded_legacy(left, legacy_approx)
gmatch_approx = match_geocoded(left, google_approx)
gapprox = bind_rows(gmatch_legacy_approx, gmatch_approx) %>%
    rename(google_approx_lat = google_lat, google_approx_lon = google_lon)

matched4 = bind_rows(
    matched3,
    inner_join(left, cnefe_addr_approx, by = c('ID' = 'ID')),
    gapprox)

matched_grouped_2 = matched4 %>%
    group_by(ID) %>%
    summarize(
        codigo_ibge = fo(codigo_ibge),
        uf = fo(uf),
        local = fo(local),
        cidade = fo(cidade),
        bairro = fo(bairro),
        endereco = fo(endereco),
        
        tse_lat = fo(tse_lat),
        tse_lon = fo(tse_lon),
        comp_tse_lat = fo(comp_tse_lat),
        comp_tse_lon = fo(comp_tse_lon),
        inep_lat = fo(inep_lat),
        inep_lon = fo(inep_lon),
        ad_lat = fo(ad_lat),
        ad_lon = fo(ad_lon),
        pl_lat = fo(pl_lat),
        pl_lon = fo(pl_lon),
        google_lat = fo(google_lat),
        google_lon = fo(google_lon),
        local_lat = fo(local_lat),
        local_lon = fo(local_lon),
        places_lat = fo(places_lat),
        places_lon = fo(places_lon),
        
        pl_Distrito = fo(pl_Distrito),
        pl_Subdistrito = fo(pl_Subdistrito),
        pl_CodSetor = fo(pl_CodSetor),
        
        ad_Distrito = fo(ad_Distrito),
        ad_Subdistrito = fo(ad_Subdistrito),
        ad_CodSetor = fo(ad_CodSetor),
        
        rural_Distrito = fo(rural_Distrito),
        rural_Subdistrito = fo(rural_Subdistrito),
        rural_CodSetor = fo(rural_CodSetor),
        
        approx_ad_Distrito = fo(approx_ad_Distrito),
        approx_ad_Subdistrito = fo(approx_ad_Subdistrito),
        approx_ad_CodSetor = fo(approx_ad_CodSetor),
        
        google_approx_lat = fo(google_approx_lat),
        google_approx_lon = fo(google_approx_lon),
        
        ibge_approx_lon = fo(ibge_approx_lon),
        ibge_approx_lat = fo(ibge_approx_lat))

matched_grouped_2 = matched_grouped_2 %>% mutate(results =
    (!is.na(tse_lat) | !is.na(comp_tse_lat)) +
    (!is.na(inep_lat)) +
    (!is.na(pl_CodSetor)) +
    (!is.na(ad_CodSetor)) +
    (!is.na(rural_CodSetor)) +
    (!is.na(local_lat)) +
    (!is.na(google_lat)) +
    (!is.na(places_lat)))

matched_grouped_2 %>% write.csv('OUTPUT_2012.csv', row.names = F)