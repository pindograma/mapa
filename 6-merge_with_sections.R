# merge_with_sections.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

library(tidyverse)
library(stringi)
library(readxl)

source('tse_file_reader.R')
source('match_shared.R')

all_years = bind_rows(
    open_2008(),
    open_2010(),
    open_2012(),
    open_2014(),
    open_2016(),
    open_2018(),
    open_2020()
) %>%
    mutate(norm_local = normalize_place(local)) %>%
    select(ano, codigo_ibge, norm_local, uf, zona, secao) %>%
    mutate(ID_n = row_number())

addr_current = readRDS('merged.rda') %>%
    mutate(norm_local = normalize_place(local)) %>%
    select(-ID, -uf, -cidade, -bairro, -local)

geocoded_secoes_orig = inner_join(all_years, addr_current, by = c(
    'codigo_ibge' = 'codigo_ibge',
    'norm_local' = 'norm_local'
)) %>%
    group_by(ID_n) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    select(-norm_local, -ID_n, -endereco)

saveRDS(geocoded_secoes_orig, 'final_output.rda')

