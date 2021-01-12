library(tidyverse)
library(readxl)
ev = read_excel("GEINF-AVAL.xlsx")

sols = function(var, n, preg) map(var, ~lapply(1:n, 
                                               function(i, text, preg) 
                                                 as.integer(grepl(paste0(i,".-"), text)), .x, preg) %>%
                                    setNames(sprintf("p%s_i%d", preg, 1:n)) %>% as_tibble())

correccions = ev %>%
  transmute(
    temps = `Timestamp`,
    estudiant = `El teu codi d'estudiant (amb la \"u\" inicial)`,
    treball = str_sub(`Codi del treball (5 caràcters)`, 1, 5),
    treball = if_else(treball == "0.0", "0e683", treball),  # Google Form llegeix el valor com 0 per 10 a la 683
    p1_good = replace_na(`Quantes de les 8 instruccions a explicar s'han explicat correctament?`, 0),
    p1_almost =  replace_na(`Quantes de les 8 instruccions a explicar falta algun detall per explicar?`, 0),
    p1_bad =  replace_na(`Quantes de les 8 instruccions a explicar no s'ha explicat gens o malament?`, 0),
    sols(`Marca en cas afirmatiu....7`, 5, "2"),
    sols(`Per la variable numèrica`, 4, "3_1"),
    sols(`Per la variable categòrica`, 3, "3_2"),
    sols(`Per les dues variables categòriques`, 5, "3_3"),
    sols(`Per les dues variables numèriques`, 5, "3_4"),
    sols(`Per la variable categòrica i la numèrica`, 3, "3_5"),
    "n3" = `Des d'un punt de vista subjectiu, amb una nota de 0 a 10, com valoraries aquesta pregunta?...18`,
    sols(`Per les dues variables numèriques.`, 3, "4_1"),
    sols(`Per la variable numèrica i categòrica.`, 5, "4_2"),
    sols(`Marca en cas afirmatiu si aplica per estudiar la relació de dues variables categòriques.`, 3, "4_3"),
    "n4" = `Des d'un punt de vista subjectiu, amb una nota de 0 a 10, com valoraries aquesta pregunta?...19`,
    sols(`Marca en cas afirmatiu....16`, 5, "5"),
    "n5" = `Des d'un punt de vista subjectiu, amb una nota de 0 a 10, com valoraries aquesta pregunta?...20`,
    sols(`Marca en cas afirmatiu....17`, 4, "6"),
    "n6" = `Des d'un punt de vista subjectiu, amb una nota de 0 a 10, com valoraries aquesta pregunta?...21`,
    "pres" = `De 0 a 10, com valoraries la maquetació del treball?`,
    "claredat" =  replace_na(`De 0 a 10, com valoraries el redactat del treball?`, 0)
  ) %>% 
  unnest(cols = everything())

# S'agafa la darrera correcció feta a un treball
correccions = correccions %>%
  group_by(estudiant, treball) %>%
  slice_max(temps, n = 1) %>%
  ungroup()

save(correccions, file = 'correccions.RData')
