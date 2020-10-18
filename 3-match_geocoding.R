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
library(docopt)

source('cnefe_matcher.R')
source('inep_matcher.R')
source('tse_file_reader.R')

# Configuration
options('sqldf.dll' = '/home/arch/spellfix.so')
plan(multiprocess)

doc = 'match_geocoding.R

Usage:
  match_geocoding.R create <year> [--recycle=<rf>]

Options:
  -h --help         Show this screen.
  --recycle=<rf>    File to borrow addresses from.
'

arguments = docopt(doc)

# Code
if (exists(arguments$rf)) {
  addr_current = read_csv(arguments$rf, col_types = cols(
      ad_lat = col_character(),
      ad_lon = col_character()
  )) %>%
    mutate(norm_local = normalize_place(local))
}

print('Normalizing TSE address data...')
local_2018_f = open_tse[arguments$year] %>%
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

print('Matching with TSE data...')
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

print('Matching with INEP data...')
load('escolas_geocoded_inep.Rdata')
inep = run_inep_match(
    local_2018_f %>% filter(uf != 'DF'),
    escolas_geocoded_inep)
save(inep, file = 'inep_last_backup.Rdata')

conflicts_solved = read_csv("data/solve_backup.csv")
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

print('Matching with local school data...')
load('local_schools_sel.Rdata')
local_schools = run_inep_match(
    local_2018_f %>% filter(uf != 'DF'),
    local_schools_sel)
rm(local_schools_sel)

print('Matching with CNEFE 2010 placename data...')
load('cnefe_with_ids.Rdata')
cnefe_pl = run_placename_match(
    local_2018_f %>% filter(uf != 'DF'),
    cnefe_with_ids)
rm(cnefe_with_ids)
save(cnefe_pl, file = 'cnefe_pl_last_backup.Rdata')

print('Matching with CNEFE 2017 address data...')
load('cnefe_rural.Rdata')
cnefe_rural_addr = run_address_match(local_2018_f, cnefe_rural, is_rural = T) %>%
    rename(rural_Distrito = ad_Distrito) %>%
    rename(rural_Subdistrito = ad_Subdistrito) %>%
    rename(rural_CodSetor = ad_CodSetor)
rm(cnefe_rural)
save(cnefe_pl, file = 'cnefe_rural_last_backup.Rdata')

print('Matching with CNEFE 2010 address data...')
load('cnefe_all.Rdata')
cnefe_addr = run_address_match(
    local_2018_f %>% filter(uf != 'DF'),
    cnefe_all)
save(cnefe_pl, file = 'cnefe_addr_last_backup.Rdata')

print('Matching with IBGE approximate data...')
load('ibge_agl.Rdata')
ibge_addr_approx = run_ibge_agl_match(local_2018_f, ibge_agl)
rm(ibge_agl)

print('Matching with CNEFE 2019 address data...')
load('malha2019.Rdata')
cnefe_malha = run_address_match(
    local_2018_f %>% filter(uf != 'DF'),
    malha)
save(cnefe_malha, file = 'cnefe_malha_last_backup.Rdata')

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

matched_grouped_1 = matched_grouped_1 %>% mutate(results =
    (!is.na(tse_lat) | !is.na(comp_tse_lat)) +
    (!is.na(inep_lat)) +
    (!is.na(pl_CodSetor)) +
    (!is.na(ad_CodSetor)) +
    (!is.na(rural_CodSetor)) +
    (!is.na(local_lat)))

save(matched_grouped_1, file = paste0('output_', arguments$year, '_without_google.Rdata'))
