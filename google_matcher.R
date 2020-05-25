# google_matcher.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

filter(!(ENDERECO %in% gall$ENDERECO))

generate_addr_export = function(addrs) {
    addrs$norm = normalize_simple(addrs$ENDERECO)
    addrs %>%
        filter(grepl('\\d', norm) &
               !grepl('S\\/N', norm) &
               !grepl('[,\\s]+SN', norm) &
               !grepl('KM', norm) &
               !grepl('^ESTRADA\\s+GERAL', norm) &
               !grepl('RURAL', norm) &
               !grepl('^RIO', norm) &
               !grepl('^SITIO', norm) &
               !grepl('^RANCHO', norm) &
               !grepl('^POVOADO', norm) &
               !grepl('^POV\\s+', norm) &
               !grepl('^SERINGAL', norm) &
               !grepl('^DISTRITO', norm) &
               !grepl('^LOCALIDADE', norm) &
               !grepl('^ZONA', norm) &
               !grepl('^REGIAO', norm) &
               !grepl('^COMUNIDADE', norm) &
               !grepl('^ALDEIA', norm) &
               !grepl('^BAIRRO', norm) &
               !grepl('^COLONIA', norm) &
               !grepl('^FAZENDA', norm) &
               !grepl('^ASSENTAMENTO', norm)) %>%
        select(ID, ENDERECO, BAIRRO_LOCAL_VOT, LOCALIDADE_LOCAL_VOTACAO, SGL_UF)
}

match_geocoded = function(local_2018_f, google) {
    inner_join(local_2018_f, google, by = c('ID' = 'ID')) %>%
        rename(google_lat = lat, google_lon = lon)
}

match_geocoded_legacy = function(local_2018_f, google) {
    google$endr_orig_0 = str_replace(google$endr, paste0(' ', google$uf), '') %>%
        str_replace(paste0(' ', google$cidade), '') %>%
        str_replace(paste0(' ', normalize_simple(google$bairro_orig)), '') %>%
        str_trim()
    google$endr_orig = ifelse(is.na(google$endr_orig), google$endr_orig_0, google$endr_orig)

    local_2018_f$endr_google = normalize_simple(local_2018_f$ENDERECO)
    local_2018_f$endr_google_2 = str_replace(
        local_2018_f$endr_google,
        '\\s*-\\s*ZONA URBANA', '') %>% str_replace('º', 'O')
    
    google$endr_orig = str_squish(google$endr_orig)
    google$endr_orig_2 = str_replace(google$endr_orig, '\\s+-\\s+ZONA URBANA', '') %>%
        str_squish()

    inner_join(local_2018_f, google, by = c('endr_google_2' = 'endr_orig_2')) %>%
        remove_ambiguities() %>%
        rename(google_lat = lat, google_lon = lon)
}

import_placenamed = function(){}

import_placenamed_legacy = function(){}
