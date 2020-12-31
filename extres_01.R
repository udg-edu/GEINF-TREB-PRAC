library(tidyverse)
library(lubridate)

load('randomisation.RData')

create_group_in_cluster = function(IDS, TGRUP, treballs, grups){
  TCLUSTER = pull(filter(treballs, estudiant %in% IDS))
  grup_ = tibble(
    tgrup = TGRUP,
    estudiant = IDS
  )
  grups_new = bind_rows(grups, grup_)
  treballs_new = mutate(treballs, tgrup = if_else(estudiant %in% IDS, TGRUP, tgrup))
  list(treballs = treballs_new, grups = grups_new)
}
INFO  = jsonlite::read_json("extres_01.json")
G1 = unlist(INFO$G1)
G2 = unlist(INFO$G2)
out_1 = create_group_in_cluster(G1,
                                as_datetime("2020-12-31 09:19:56 CET"),
                                treballs, grups)

out_2 = create_group_in_cluster(G2,
                                as_datetime("2020-12-31 09:24:52 CET"),
                                out_1$treballs, out_1$grups)
treballs_01 = out_2$treballs
grups_01 = out_2$grups

# Treballs assignats
treballs_a_assignar = correccions %>% 
  pivot_longer(matches('treb'), values_to = 'treball') %>%
  semi_join(filter(treballs_01, !estudiant %in% c(G1,G2)), by = 'treball') %>%
  count(treball) %>%
  arrange(n) %>%
  slice(1:10) %>%
  pull(treball)
crea_correccio = function(estudiant, treballs){
  tibble(estudiant = estudiant, treballs = treballs) %>%
    mutate(id = 1:5) %>%
    pivot_wider(names_from = id, values_from = treballs, names_prefix = "treb")
}
corr1 = crea_correccio(INFO$c1, treballs_a_assignar[1:5])
corr2 = crea_correccio(INFO$c2, treballs_a_assignar[6:10])

correccions_01 = bind_rows(correccions, corr1, corr2)

save(grups_01, treballs_01, correccions_01, file = 'extres_01.RData')

## Es generen informes
correccions_extra = correccions_01 %>%
  anti_join(correccions, by = 'estudiant')

for(I in 1:nrow(correccions_extra)){
  info = slice(correccions_extra, I)
  render('index_alumne.Rmd', output_dir = 'corr', output_file = sprintf("index_%s.html", info$estudiant))
}
