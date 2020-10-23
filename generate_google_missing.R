# generate_google_missing.R
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
library(purrr)
library(docopt)

source('tse_file_reader.R')
source('google_matcher.R')

doc = 'generate_google_missing.R

Usage:
  generate_google_missing.R export_maps_cn --data=<data> --local=<local> [--ignorepast]
  generate_google_missing.R export_maps_sn --data=<data> --local=<local> [--ignorepast]
  generate_google_missing.R export_places --data=<data> --local=<local> [--ignorepast]
  generate_google_missing.R create <year> --data=<data> --local=<local>

Options:
  -h --help                 Show this screen.
  --data=<data>             R data file geocoded by match_geocoding.R
  --local=<local>           R data file with polling places to geocode
  --ignorepast              Ignore previous attempts to geocode Google results.
'

arguments = docopt(doc)
print(arguments)

load(arguments$local)
load(arguments$data)

left = local_2018_f %>%
    filter(!(ID %in% matched_grouped_1$ID))

to_geocode_1 = matched_grouped_1 %>%
    filter(results == 1) %>%
    generate_addr_export()
to_geocode_2 = generate_addr_export(left)
to_geocode = bind_rows(to_geocode_1, to_geocode_2)

legacy_tried = tibble()
google_tried = tibble()

if (!arguments$ignorepast & (arguments$export_map_cn | arguments$export_map_sn)) {
    if (file.exists('data/google/legacy/original_to_geocode.csv')) {
        legacy_tried = read_csv('data/google/legacy/original_to_geocode.csv') %>%
            mutate(jnd = paste(ENDERECO, BAIRRO_LOCAL_VOT, LOCALIDADE_LOCAL_VOTACAO, SGL_UF) %>% str_squish())
    }

    if (file.exists('data/google/geocoder-filtered-output')) {
        files = list.files(
            path = 'data/google/geocoder-filtered-output',
            pattern = '*.csv',
            full.names = T,
            recursive = F)
        
        google_tried = map_dfr(files, read_csv) %>%
            mutate(jnd = paste(endr_orig, bairro_orig, cidade, uf) %>% str_squish())
    } 
}

if (arguments$export_maps_cn) {
    if (!arguments$ignorepast) {
        to_geocode = to_geocode %>%
            mutate(jnd = paste(endereco, bairro, cidade, uf) %>% str_squish()) %>%
            mutate(jnd2 = paste(norm_google_endr, bairro, cidade, uf) %>% str_squish()) %>%
            filter(!(jnd %in% legacy_tried$jnd)) %>%
            filter(!(jnd2 %in% google_tried$jnd))
    }
    
    to_geocode %>% write.csv('EXPORT_GOOGLE_ADDR_PHASE_1.csv', row.names = F)
    stop('Generated file successfully (R will not let me stop execution without saying this is an error, but it is not an error).')
}

google_geocoded = tibble()
google_legacy = tibble()

google_legacy = read_csv('data/google/legacy/geocoded.csv') %>%
    select(-ID)

files = list.files(
    path = 'data/google/geocoder-filtered-output',
    pattern = '*_okay.csv',
    full.names = T,
    recursive = F)
google_geocoded = map_dfr(files, read_csv)

gmatch_legacy = match_geocoded_legacy(local_2018_f, google_legacy)
gmatch = match_geocoded(local_2018_f, google_geocoded)

matched2 = bind_rows(matched1, gmatch_legacy, gmatch)

local_2018_f = local_2018_f %>%
    mutate(pl_without_damage = revert_unidecode_damage(local)) %>%
    mutate(pl_jnd = paste(pl_without_damage, norm_google_cidade, uf) %>% str_squish())

gpl_tried = tibble()
to_place = local_2018_f %>% filter(!(ID %in% matched2$ID))

if (arguments$export_places) {
    if (!arguments$ignorepast) {
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
    stop('Generated file successfully (R will not let me stop execution without saying this is an error, but it is not an error).')
}

gpl = read_csv('data/google/gplaces2.csv', col_types = cols( ID = col_skip() )) %>%
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

if (arguments$export_maps_sn) {
    to_geocode_2 = generate_addr_export(left, allow_sn = T)
    if (!google_full_reset) {
        to_geocode_2 = to_geocode_2 %>%
            mutate(jnd = paste(endereco, bairro, cidade, uf) %>% str_squish()) %>%
            mutate(jnd2 = paste(revert_unidecode_damage(endereco), bairro, cidade, uf) %>% str_squish()) %>%
            filter(!(jnd %in% legacy_tried$jnd)) %>%
            filter(!(jnd2 %in% google_tried$jnd))
    }
    to_geocode_2 %>% write.csv('EXPORT_GOOGLE_ADDR_PHASE_2.csv', row.names = F)
    stop('Generated file successfully (R will not let me stop execution without saying this is an error, but it is not an error).')
}

google_approx = tibble()
legacy_approx = tibble()

if (!google_full_reset) {
    approx_files = list.files(
        path = 'data/google/geocoder-filtered-output',
        pattern = '*_approx.csv',
        full.names = T,
        recursive = F)
    google_approx = map_dfr(approx_files, read_csv)
    legacy_approx = read_csv('data/google/legacy/approx.csv') %>% select(-ID)
}

gmatch_legacy_approx = match_geocoded_legacy(left, legacy_approx)
gmatch_approx = match_geocoded(left, google_approx)
gapprox = bind_rows(gmatch_legacy_approx, gmatch_approx) %>%
    rename(google_approx_lat = google_lat, google_approx_lon = google_lon)

matched4 = bind_rows(matched3, gapprox)

fo = function(x) { first(x, order_by=x) }
matched_grouped_2 = matched4 %>%
    group_by(ID) %>%
    summarize(
        uf = fo(uf),
        cidade = fo(cidade),
        bairro = fo(bairro),
        endereco = fo(endereco),
        
        local = fo(local),
        codigo_ibge = fo(codigo_ibge),
        
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

save(list = c('matched4', 'matched_grouped_2'), file = paste0('output_', arguments$year, '_with_google.Rdata'))
