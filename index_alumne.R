library(tidyverse)

load('randomisation.RData')
library(kableExtra)
library(rmarkdown)

for(I in 1:nrow(correccions)){
  info = slice(correccions, I)
  info$estudiant
  render('index_alumne.Rmd', output_dir = 'corr', output_file = sprintf("index_%s.html", info$estudiant))
}

info = treballs %>%
  mutate(treb = 1:n()) %>%
  select(treb, treball) %>%
  pivot_wider(names_from = treb, values_from = treball, names_prefix = 'treb')
render('index_alumne.Rmd', output_dir = 'corr', output_file = "index_all.html")
