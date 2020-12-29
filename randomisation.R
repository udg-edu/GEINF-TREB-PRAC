library(tidyverse)
library(readxl)

estudiants = read_excel("3105G070072020 Estadística Qualificacions.xlsx") %>%
  transmute(
    nom = str_c(Cognoms, Nom, sep = " "),
    estudiant = str_c('u', `Número ID`))

grups = read_excel("GEINF-GRUPS.xlsx") %>%
  rename(tgrup = Timestamp, u1 = `...2`, u2 = `...3`) %>%
  pivot_longer(u1:u2, values_to = 'estudiant') %>%
  semi_join(estudiants, by = 'estudiant') %>%
  group_by(estudiant) %>%
  top_n(1, tgrup) %>%
  group_by(tgrup) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  select(-name)

clusters = read_excel("GEINF-CLUSTERS.xlsx") %>%
  rename(tcluster = Timestamp, u1 = `...2`, u2 = `...3`, u3 = `...4`, u4 = `...5`) %>%
  pivot_longer(u1:u4, values_to = 'estudiant') %>%
  semi_join(estudiants, by = 'estudiant') %>%
  group_by(estudiant) %>%
  top_n(1, tcluster) %>%
  group_by(tcluster) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(-name)

library(digest)
treballs = unzip("3105G070072020 Estadística-Entrega del treball pràctic-973703.zip", list = TRUE) %>%
  as_tibble() %>%
  transmute(
    nom = str_replace(Name, "_\\d{7}_assignsubmission_file_/.+", ""),
    fitxer = Name) %>%
  left_join(estudiants, by = "nom") %>%
  left_join(grups, by = 'estudiant') %>%
  group_by(tgrup) %>%
  filter(is.na(tgrup) | 1:n() == 1) %>%
  ungroup() %>%
  mutate(
    treball = str_sub(map_chr(estudiant, sha1), 1, 5)
  ) %>%
  left_join(clusters, by = 'estudiant')

treballs_cluster = treballs %>%
  filter(!is.na(tcluster)) %>%
  group_by(tcluster) %>%
  summarise(cluster = list(unique(treball)))

treballs_corr = treballs %>%
  rename(estudiant_treball = estudiant) %>%
  left_join(grups, by = 'tgrup') %>%
  mutate(estudiant = if_else(is.na(estudiant), estudiant_treball, estudiant)) %>%
  select(estudiant, treball, tcluster) %>%
  left_join(treballs_cluster, by = 'tcluster')

id_treballs = pull(treballs, treball)

set.seed(2)
repeat{
  treballs_corr = treballs_corr %>%
    mutate(
      treb1 = sample(c(id_treballs, sample(id_treballs, nrow(treballs_corr) - nrow(treballs)))),
      test1 = treb1 != treball,
      test2 = map2_lgl(treb1, cluster, ~!.x %in% .y)
    ) 
  errors = treballs_corr %>% 
    summarise(test1 = sum(!test1), test2 = sum(!test2)) %>%
    unlist() %>%
    max()
  if(errors == 0) break
}
repeat{
  treballs_corr = treballs_corr %>%
    mutate(
      treb2 = sample(c(id_treballs, sample(id_treballs, nrow(treballs_corr) - nrow(treballs)))),
      test1 = treb2 != treball,
      test2 = treb2 != treb1,
      test3 = map2_lgl(treb2, cluster, ~!.x %in% .y)
    ) 
  errors = treballs_corr %>% 
    summarise(test1 = sum(!test1), test2 = sum(!test2), test3 = sum(!test3)) %>%
    unlist() %>%
    max()
  if(errors == 0) break
}
repeat{
  treballs_corr = treballs_corr %>%
    mutate(
      treb3 = sample(c(id_treballs, sample(id_treballs, nrow(treballs_corr) - nrow(treballs)))),
      test1 = treb3 != treball,
      test2 = treb3 != treb1,
      test3 = treb3 != treb2,
      test4 = map2_lgl(treb3, cluster, ~!.x %in% .y)
    ) 
  errors = treballs_corr %>% 
    summarise(test1 = sum(!test1), test2 = sum(!test2), test3 = sum(!test3), test4 = sum(!test4)) %>%
    unlist() %>%
    max()
  if(errors == 0) break
}
repeat{
  treballs_corr = treballs_corr %>%
    mutate(
      treb4 = sample(c(id_treballs, sample(id_treballs, nrow(treballs_corr) - nrow(treballs)))),
      test1 = treb4 != treball,
      test2 = treb4 != treb1,
      test3 = treb4 != treb2,
      test4 = treb4 != treb3,
      test5 = map2_lgl(treb4, cluster, ~!.x %in% .y)
    ) 
  errors = treballs_corr %>% 
    summarise(test1 = sum(!test1), test2 = sum(!test2), test3 = sum(!test3), test4 = sum(!test4), test5 = sum(!test5)) %>%
    unlist() %>%
    max()
  if(errors == 0) break
}
repeat{
  treballs_corr = treballs_corr %>%
    mutate(
      treb5 = sample(c(id_treballs, sample(id_treballs, nrow(treballs_corr) - nrow(treballs)))),
      test1 = treb5 != treball,
      test2 = treb5 != treb1,
      test3 = treb5 != treb2,
      test4 = treb5 != treb3,
      test5 = treb5 != treb4,
      test6 = map2_lgl(treb5, cluster, ~!.x %in% .y)
    ) 
  errors = treballs_corr %>% 
    summarise(test1 = sum(!test1), test2 = sum(!test2), test3 = sum(!test3), test4 = sum(!test4), test5 = sum(!test5), test6 = sum(!test6)) %>%
    unlist() %>%
    max()
  if(errors == 0) break
}

correccions = treballs_corr %>%
  select(estudiant, matches('treb.$'))

save(estudiants, grups, clusters, treballs, correccions, file = 'randomisation.RData')
