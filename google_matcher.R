# google_matcher.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

generate_addr_export = function(addrs, allow_sn = F) {
    addrs = addrs %>%
        mutate(norm = normalize_simple(endereco)) %>%
        filter(!grepl('KM', norm) &
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
               !grepl('^VILA', norm) &
               !grepl('^BAIRRO', norm) &
               !grepl('^LINHA', norm) &
               !grepl('^ASSENTAMENTO', norm))
    
    if (!allow_sn) {
        addrs = addrs %>% filter(
            grepl('\\d', norm) &
            !grepl('S\\/N', norm) &
            !grepl('[,\\s]+SN', norm))
    }
    
    addrs %>% select(ID, endereco, bairro, cidade, uf,
                     norm_google_endr, norm_google_bairro, norm_google_cidade)
}

revert_unidecode_damage = function(x) {
    normalize_simple(x) %>%
        str_replace_all('º', 'O') %>%
        str_replace_all('ª', 'A')
}

match_geocoded = function(local_2018_f, google) {
    inner_join(
        local_2018_f,
        google %>% select(-ID) %>% distinct(),
        by = c(
            'uf' = 'uf',
            'cidade' = 'cidade',
            'bairro' = 'bairro_orig',
            'norm_google_endr' = 'endr_orig'
        )) %>%
            distinct(ID, .keep_all = T) %>%
            rename(google_lat = lat, google_lon = lon)
}

match_geocoded_legacy = function(local_2018_f, google) {
    google$endr_orig_0 = str_replace(google$endr, paste0(' ', google$uf), '') %>%
        str_replace(paste0(' ', google$cidade), '') %>%
        str_replace(paste0(' ', normalize_simple(google$bairro_orig)), '') %>%
        str_trim()
    google$endr_orig = ifelse(is.na(google$endr_orig), google$endr_orig_0, google$endr_orig)

    local_2018_f$endr_google = normalize_simple(local_2018_f$endereco)
    local_2018_f$endr_google_2 = str_replace(
        local_2018_f$endr_google,
        '\\s*-\\s*ZONA URBANA', '') %>% revert_unidecode_damage()
    
    google$endr_orig = str_squish(google$endr_orig)
    google$endr_orig_2 = str_replace(google$endr_orig, '\\s+-\\s+ZONA URBANA', '') %>%
        str_squish()

    inner_join(local_2018_f, google, by = c(
        'uf' = 'uf',
        'cidade' = 'cidade',
        'bairro' = 'bairro_orig',
        'endr_google_2' = 'endr_orig_2'
    )) %>%
        distinct(ID, .keep_all = T) %>%
        rename(google_lat = lat, google_lon = lon)
}