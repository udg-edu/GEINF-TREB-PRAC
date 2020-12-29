library(tidyverse)

load('randomisation.RData')
dir.create(file.path("files", "_files"), recursive = TRUE)

unzip("3105G070072020 Estadística-Entrega del treball pràctic-973703.zip", exdir = file.path("files", "_files"))

copia = function(fitxer, treball){
  file.copy(file.path("files", "_files", fitxer), file.path("files", str_c(treball, ".html")), overwrite = TRUE)
}

with(treballs, walk2(fitxer, treball, copia)  )
unlink(file.path("files", "_files"), recursive = TRUE)
