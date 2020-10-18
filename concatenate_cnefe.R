library(tidyverse)

source('match_shared.R')
source('cnefe_matcher.R')

files = list.files(path = 'data', pattern = '*.Rdata', full.names = T)

cnefe_all = map_dfr(files, function(x) {
    load(x)
    df %>%
        mutate(CEP = as.character(CEP)) %>%
        select(-IdDomicilioColetivo)
}) %>%
    normalize_cnefe()
save(cnefe_all, file = 'cnefe_all.Rdata')

cnefe_with_ids = cnefe_all %>%
    filter(!is.na(IdEstabelecimento)) %>%
    filter(EspecieEndereco != 8) %>%
    normalize_cnefe()
save(cnefe_with_ids, file = 'cnefe_with_ids.Rdata')
