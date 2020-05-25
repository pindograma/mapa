# match_geocoding.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

library(readr)
library(stringi)
library(dplyr)
library(stringr)
library(mgsub)
library(sqldf)
library(purrr)
library(furrr)

options('sqldf.dll' = '/home/arch/spellfix.so')
plan(multiprocess)

source('cnefe_matcher.R')
source('inep_matcher.R')
source('google_matcher.R')

local_2018 <- read_delim("local-votacao-08-08-2018.csv", 
    ";", escape_double = FALSE,
    col_types = cols(
        LATITUDE_ZONA = col_double(),
        LONGITUDE_ZONA = col_double(),
        LATITUDE_LOCAL = col_double(),
        LONGITUDE_LOCAL = col_double()
    ),
    locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
    trim_ws = TRUE)

local_2018_f = local_2018 %>%
    select(-NUM_SECAO, -SECAO_AGREGADORA, -SECAO_AGREGADA) %>%
    distinct(COD_LOCALIDADE_IBGE, LOCAL_VOTACAO, ENDERECO, .keep_all = T) %>%
    filter(SGL_UF != 'ZZ')
local_2018_f$ID = seq.int(nrow(local_2018_f))
rm(local_2018)

local_2018_f$norm_cidade = normalize_simple(local_2018_f$LOCALIDADE_LOCAL_VOTACAO)
local_2018_f$norm_cidade = recode(local_2018_f$norm_cidade,
                 `SANTA ISABEL DO PARA` = 'SANTA IZABEL DO PARA',
                 `GRACCHO CARDOSO` = 'GRACHO CARDOSO',
                 `ELDORADO DOS CARAJAS` = 'ELDORADO DO CARAJAS',
                 `QUINJINGUE` = 'QUIJINGUE',
                 `GRAO-PARA` = 'GRAO PARA',
                 `SAO LUIS DO PARAITINGA` = 'SAO LUIZ DO PARAITINGA',
                 `SEM PEIXE` = 'SEM-PEIXE')

local_2018_f$norm_local = normalize_simple(local_2018_f$LOCAL_VOTACAO)
local_2018_f$norm_bairro = normalize_simple(local_2018_f$BAIRRO_LOCAL_VOT)
local_2018_f$norm_endr = normalize_address(local_2018_f$ENDERECO)
local_2018_f$norm_local_esc = future_map(local_2018_f$LOCAL_VOTACAO, normalize_school_name) %>%
    unlist()

escolas_geocoded_inep <- read_delim("escolas-geocoded-inep.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)

escolas_geocoded_inep$norm_cidade = normalize_simple(escolas_geocoded_inep$`Município`)
escolas_geocoded_inep$norm_escola_t = future_map(escolas_geocoded_inep$Escola, normalize_school_name) %>%
    unlist()

inep = run_inep_match(
    local_2018_f %>% filter(SGL_UF != 'DF'),
    escolas_geocoded_inep)

conflicts_solved = read_csv("solve_backup.csv")
sc_stage1 = inner_join(
    local_2018_f,
    conflicts_solved,
    by=c( 'norm_cidade'='norm_cidade', 'LOCAL_VOTACAO'='LOCAL_VOTACAO'))
sc = inner_join(
    sc_stage1,
    escolas_geocoded_inep,
    by=c('SGL_UF'='UF', 'norm_cidade.x'='norm_cidade', 'Escola'='Escola')) %>%
        rename(norm_cidade = norm_cidade.x) %>%
        rename(inep_lat = Latitude) %>%
        rename(inep_lon = Longitude) %>%
        remove_ambiguities()
rm(escolas_geocoded_inep)

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
    )) %>% normalize_cnefe()

cnefe_pl = run_placename_match(
    local_2018_f %>% filter(SGL_UF != 'DF'),
    cnefe_with_ids)
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

cnefe_addr = run_address_match(
    local_2018_f %>% filter(SGL_UF != 'DF'),
    cnefe_all)
rm(cnefe_all)

matched = bind_rows(
    local_2018_f %>% filter(!is.na(LATITUDE_LOCAL) & !is.na(LONGITUDE_LOCAL)),
    inep %>% filter(!is.na(inep_lat) & !is.na(inep_lon)),
    cnefe_pl,
    cnefe_addr,
    sc %>% filter(!is.na(inep_lat) & !is.na(inep_lon))
)

fo = function(x) { first(x, order_by=x) }
matched_grouped = matched %>%
    group_by(ID) %>%
    summarize(
        SGL_UF = fo(SGL_UF),
        LOCALIDADE_LOCAL_VOTACAO = fo(LOCALIDADE_LOCAL_VOTACAO),
        BAIRRO_LOCAL_VOT = fo(BAIRRO_LOCAL_VOT),
        ENDERECO = fo(ENDERECO),
        
        LATITUDE_LOCAL = fo(LATITUDE_LOCAL),
        LONGITUDE_LOCAL = fo(LONGITUDE_LOCAL),
        inep_lat = fo(inep_lat),
        inep_lon = fo(inep_lon),
        cnefe_ad_lat = fo(cnefe_ad_lat),
        cnefe_ad_lon = fo(cnefe_ad_lon),
        cnefe_pl_lat = fo(cnefe_pl_lat),
        cnefe_pl_lon = fo(cnefe_pl_lon),

        pl_CodSetor = fo(pl_CodSetor),
        ad_CodSetor = fo(ad_CodSetor))



# export all Google stuff when desired

# import Google
# legacy JOIN
