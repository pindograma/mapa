# generate_areas.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

# Imports
library(geobr)
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(Rcpp)

# Definitions
party_colors <- c("#CB454A", "#2E74C0")

st_unlist = function(x) {
    is.na(x) = lengths(x) == 0
    unlist(x)
}

make_points_sfc = function(lat, lon) {
    map2(lat,
         lon,
         function(x, y) { st_point(x = c(y, x)) }) %>%
    st_sfc(crs = 4326) %>%
    st_transform(31983)
}

gen_tract = function(d, s, c) {
    paste0(
        city_id,
        str_pad(d, 2, pad = '0'),
        str_pad(s, 2, pad = '0'),
        str_pad(c, 4, pad = '0'))
}

city_name = 'RIO DE JANEIRO'
city_id = 3304557
city_csv = 'rj_2018.csv'

# Datasets
all_secoes <- read_delim("~/pindograma/dados-externos/mapa/local-votacao-08-08-2018.csv", 
    ";", escape_double = FALSE,
    col_types = cols(
        LATITUDE_ZONA = col_double(),
        LONGITUDE_ZONA = col_double(),
        LATITUDE_LOCAL = col_double(),
        LONGITUDE_LOCAL = col_double()
    ),
    locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
    trim_ws = TRUE)

geocoded_secoes = read_csv('data/OUTPUT.csv', col_types = cols(
    ad_lat = col_character(),
    ad_lon = col_character()
))

votacoes_sp <- read_csv(city_csv)
#segundo_turno = votacoes_sp %>% filter(NR_TURNO == 2)
#haddad = segundo_turno %>% filter(NR_VOTAVEL == 50) %>% rename(haddad = QT_VOTOS)
#bolsonaro = segundo_turno %>% filter(NR_VOTAVEL == 10) %>% rename(bolsonaro = QT_VOTOS)

votos_sel = votacoes_sp %>%
    filter(NR_VOTAVEL >= 1000 & NR_VOTAVEL < 9999) %>%
    group_by(NR_ZONA, NR_SECAO) %>%
    summarize(total = sum(QT_VOTOS),
              freixo = sum(QT_VOTOS[NR_VOTAVEL == 5050]),
              helio = sum(QT_VOTOS[NR_VOTAVEL == 1720])) %>%
    ungroup()

sp_orig = read_census_tract(code_tract = city_id, simplified = F) %>%
    st_transform(31983)
sp_orig$Distrito = str_sub(sp_orig$code_district, start = -2) %>%
    as.numeric()
sp_orig$Subdistrito = str_sub(sp_orig$code_subdistrict, start = -2) %>%
    as.numeric()
sp_orig$CodSetor = str_sub(sp_orig$code_tract, start = -4) %>%
    as.numeric()

# Filtering
geocoded_secoes_sp = geocoded_secoes %>% filter(LOCALIDADE_LOCAL_VOTACAO == city_name)
all_secoes_sp = all_secoes %>% filter(COD_LOCALIDADE_IBGE == city_id %>% as.character())

sp = sp_orig# %>% filter(name_subdistrict == 'Botafogo' |
            #            name_subdistrict == 'Copacabana' |
            #            name_subdistrict == 'Lagoa' |
            #            name_subdistrict == 'Rocinha')

# Processing
geocoded_secoes_sp = geocoded_secoes_sp %>% mutate(
    lat = case_when(
        !is.na(LATITUDE_LOCAL) ~ LATITUDE_LOCAL,
        !is.na(inep_lat) ~ inep_lat,
        !is.na(google_lat) ~ google_lat,
        T ~ NA_real_),
    lon = case_when(
        !is.na(LONGITUDE_LOCAL) ~ LONGITUDE_LOCAL,
        !is.na(inep_lon) ~ inep_lon,
        !is.na(google_lon) ~ google_lon,
        T ~ NA_real_))

geocoded_secoes_sp = geocoded_secoes_sp %>% mutate(code_tract = case_when(
    !is.na(pl_Distrito) & !is.na(pl_Subdistrito) & !is.na(pl_CodSetor) ~
        gen_tract(pl_Distrito, pl_Subdistrito, pl_CodSetor),
    !is.na(ad_Distrito) & !is.na(ad_Subdistrito) & !is.na(ad_CodSetor) ~
        gen_tract(ad_Distrito, ad_Subdistrito, ad_CodSetor),
    T ~ NA_character_))

points = make_points_sfc(geocoded_secoes_sp$lat, geocoded_secoes_sp$lon)
geocoded_secoes_sp$setor_rn = st_within(points, sp) %>%
    st_unlist()

sp = sp %>% mutate(rn = row_number())
sp_joined_0 = inner_join(sp, geocoded_secoes_sp, by = c('rn' = 'setor_rn')) %>%
    select(-code_tract.y) %>% rename(code_tract = code_tract.x)
sp_joined_1 = inner_join(sp, geocoded_secoes_sp %>% filter(is.na(lat) | is.na(lon)),
                         by = c('code_tract' = 'code_tract')) %>%
    select(-setor_rn)
sp_joined = rbind(sp_joined_0, sp_joined_1)

# TODO: VERIFY COUNT MISMATCHES!!!!
geocoded_all_secoes_sp_0 = inner_join(
    sp_joined,
    all_secoes_sp %>% select(LOCAL_VOTACAO, ENDERECO, ZONA, NUM_SECAO),
    by = c('LOCAL_VOTACAO' = 'LOCAL_VOTACAO', 'ENDERECO' = 'ENDERECO'))
geocoded_all_secoes_sp = left_join(
    geocoded_all_secoes_sp_0,
    votos_sel,
    by = c('ZONA'='NR_ZONA', 'NUM_SECAO'='NR_SECAO'))

geocoded_all_secoes_sp$helio = ifelse(
    is.na(geocoded_all_secoes_sp$helio),
    0,
    geocoded_all_secoes_sp$helio
)
geocoded_all_secoes_sp$freixo = ifelse(
    is.na(geocoded_all_secoes_sp$freixo),
    0,
    geocoded_all_secoes_sp$freixo
)

points = make_points_sfc(sp_joined$lat, sp_joined$lon)
points2 = st_centroid(sp_joined_1)

spg = geocoded_all_secoes_sp %>%
    group_by(code_tract) %>%
    summarize(rn = first(rn), helio = sum(helio)/sum(total), freixo = sum(freixo)/sum(total)) %>%
    ungroup()
#spg$bolsonaro_p = spg$bolsonaro / (spg$bolsonaro + spg$haddad)

spt = spg %>% filter(!is.na(freixo)) %>% mutate(main = rn)

all_vec = spt$main
st_intersects_sp = function(x) { st_intersects(x, sp) }

walk_aggregate = function(x) {
    x %>% group_by(main) %>% summarize(
        geom = st_union(geom) %>% st_cast('MULTIPOLYGON'),
        bolsonaro_p = mean(bolsonaro_p, na.rm = T)
    )
}

sptf = mapwalk(spt, sp, all_vec)
sptf_b = sptf %>% st_buffer(dist = 1)
sptf_ag = walk_aggregate(sptf_b)

ggplot() +
    geom_sf(data = sptf_ag, aes(fill = bolsonaro_p), lwd = 0) +
    scale_fill_gradient(low = 'white', high = '#cc0000')
    #scale_fill_gradient2(low = '#cc0000', mid = '#e0fff0', high = '#2c9764', midpoint = 0.5) +
    #geom_sf(data = points)
    #scale_fill_manual(values = party_colors)

verify_ibge_accuracy = function(spj, sp) {
    spj = spj %>% filter(!is.na(pl_CodSetor))
    
    lhs = inner_join(spj, sp %>% as.data.frame(), by = c(
        'pl_Distrito' = 'Distrito',
        'pl_Subdistrito' = 'Subdistrito',
        'pl_CodSetor' = 'CodSetor'
    ))
    rhs = lhs$`geom.y` %>% st_sfc(crs = 31983)
    
    spj %>% mutate(dist = st_distance(st_centroid(lhs), st_centroid(rhs), by_element = T) %>% st_unlist())
}
