library(tidyverse)

files = list.files(
    path = '.',
    pattern = 'output.*_with_google.Rdata',
    full.names = T,
    recursive = F)

merged = map_dfr(files, function(x) {
    load(x)

    matched_grouped_2 %>%
        mutate(ano = word(x, 2, sep = '_'))
})

saveRDS(merged, file = 'merged.rda')
