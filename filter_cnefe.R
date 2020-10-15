# filter_cnefe.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)

f <- args[1]
o <- args[2]

df <- read_fwf(file = f,
               locale = locale(encoding = 'Latin1'),
               col_positions = fwf_widths(
                c(2, 5, 2, 2, 4, 1, 20, 30, 60, 8, 7, 20, 10, 20, 10, 20,
                          10, 20, 10, 20, 10, 20, 10, 15, 15, 60, 60, 2, 40, 1,
                          30, 3, 3, 8),
               c('UF', 'Municipio', 'Distrito', 'Subdistrito', 'CodSetor',
                             'SituacaoSetor', 'TipoLogradouro', 'TituloLogradouro',
                             'NomeLogradouro', 'NumeroLogradouro', 'ModificadorNumero',
                             'Elemento1', 'Valor1', 'Elemento2', 'Valor2',
                             'Elemento3', 'Valor3', 'Elemento4', 'Valor4', 'Elemento5',
                             'Valor5', 'Elemento6', 'Valor6', 'Lat', 'Lon', 'Localidade',
                             'Nulo', 'EspecieEndereco', 'IdEstabelecimento',
                             'IndicadorEndereco', 'IdDomicilioColetivo', 'Quadra',
                             'Face', 'CEP')),
               col_types = cols(Lat = col_character(), Lon = col_character(),
                                TituloLogradouro = col_character(),
                                Elemento1 = col_character(), Valor1 = col_character(),
                                Elemento2 = col_character(), Valor2 = col_character(),
                                Elemento3 = col_character(), Valor3 = col_character(),
                                Elemento4 = col_character(), Valor4 = col_character(),
                                Elemento5 = col_character(), Valor5 = col_character(),
                                Elemento6 = col_character(), Valor6 = col_character(),
                                EspecieEndereco = col_double())) %>%
        filter(EspecieEndereco != 1 & EspecieEndereco != 2)

save(df, file = o)
