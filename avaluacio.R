library(tidyverse)

load('randomisation.RData')
notes_alumnes = select(correccions, estudiant)

treballs_invalids = scan(file = 'treballs_invalids', what = 'character')

correccions_assignades = correccions %>%
  pivot_longer(starts_with('treb'), values_to = 'treball') %>%
  filter(!treball %in% treballs_invalids)

load('correccions.RData')
correccions_fetes = correccions %>%
  filter(!treball %in% treballs_invalids)

correccio_final = correccions_fetes %>%
  transmute(
    estudiant, treball,
    preg_1 = ( p1_good + pmin(8 - p1_good - p1_bad, p1_almost) / 2 ) / 8,
    preg_2 = pmin(10, 4 * p2_i1 + 2 * p2_i2 + 2 * p2_i3 + 4 * p2_i4 + 2 * p2_i5) / 10,
    preg_3 = 1/5 * (p3_1_i1 + p3_1_i2 + p3_1_i3 + p3_1_i4) / 4 +
             1/5 * (p3_2_i1 + p3_2_i2 + p3_2_i3) / 3 +
             1/5 * (p3_3_i1 + p3_3_i2 + p3_3_i3 + p3_3_i4 + p3_3_i5) / 5 +
             1/5 * (p3_4_i1 + p3_4_i2 + p3_4_i3 + p3_4_i4 + p3_4_i5) / 5 +
             1/5 * (p3_5_i1 + p3_5_i2 + p3_5_i3) / 3,
    preg_4 = 1/3 * (p4_1_i1 + p4_1_i2 + p4_1_i3) / 3 +
             1/3 * (p4_2_i1 + p4_2_i2 + p4_2_i3 + p4_2_i4 + p4_2_i5) / 5 +
             1/3 * (p4_3_i1 + p4_3_i2 + p4_3_i3) / 3,
    preg_5 = (p5_i1 + p5_i2 + p5_i3 + p5_i4 + p5_i5) / 5,
    preg_6 = (p6_i1 + p6_i2 + p6_i3 + p6_i4) / 4,
    preg_estil = (pres + claredat) / 10 ) %>%
  pivot_longer(starts_with("preg")) %>%
  group_by(treball, name) %>%
  summarise(.groups = 'drop', n = n(), nota = median(value)) %>%
  pivot_wider(names_from = name, values_from = nota) %>%
  mutate(
    nota = 10 * (preg_1 + preg_2 + preg_3 + preg_4 + preg_5 + preg_6 + preg_estil) / 8) %>%
  bind_rows(tibble(treball = treballs_invalids)) %>%
  replace_na(list(nota = 0))

ggplot(data = correccio_final) + 
  geom_histogram(aes(x = nota), bins = 8, col = 'white') +
  scale_x_continuous(breaks = 0:10) +
  theme_minimal() +
  labs(title = 'Distribució de la nota dels treballs', y = "Freqüències", x = "Nota")

n_treballs = correccions_assignades %>%
  left_join(select(correccions_fetes, estudiant, treball, temps), by = c('estudiant', 'treball')) %>%
  group_by(estudiant) %>%
  summarise(.groups = 'drop', 
            n_treb = n(),
            n_corr = sum(!is.na(temps)))

agreement_x = function(i, n, x){
  v = unique(x[-i])
  if(length(v) > 1) return(TRUE)
  return(x[i] == v)
}
disagreement = correccions_fetes %>%
  select(estudiant, treball, matches("i.$")) %>%
  pivot_longer(matches("i.$")) %>%
  group_by(treball, name) %>%
  mutate(
    n = n(),
    id = 1:n(),
    agreement = map2_dbl(id, n, agreement_x, value)) %>%
  ungroup() %>%
  filter(agreement == 0) %>%
  count(estudiant, name = 'disagreement') %>%
  mutate(penalitzacio = floor((pmax(0, disagreement-5))/10))

notes = notes_alumnes %>%
  left_join(grups, by = 'estudiant') %>%
  left_join(select(treballs, estudiant, treball), by = 'estudiant') %>%
  group_by(tgrup) %>%
  fill(treball, .direction = 'downup') %>%
  left_join(correccio_final, by = 'treball') %>%
  left_join(n_treballs, by = 'estudiant') %>%
  left_join(disagreement, by = 'estudiant') %>%
  replace_na(list(penalitzacio = 0)) %>%
  mutate(nota_final = pmax(0, nota - 0.5 * (n_treb-n_corr) - penalitzacio)) %>%
  arrange(nota_final)

ggplot(data = notes) + 
  geom_histogram(aes(x = nota_final), bins = 8, col = 'white') +
  scale_x_continuous(breaks = 0:10) +
  theme_minimal() +
  labs(title = 'Distribució de la nota dels treballs després penalització', y = "Freqüències", x = "Nota")

  
