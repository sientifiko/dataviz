
# lapply(c("ggplot2", "tidyverse"), install.packages)

library(ggplot2); library(tidyverse)

# IMPORTAR DATA
casen <- read.delim("casen2017_set.csv", sep = ";")

# ASIGNAR NOMBRE MÁS CORTO A LAS REGIONES
reg_short <- c("Tarapaca", "Antofagasta","Atacama","Coquimbo", "Valparaiso",
               "Ohiggins", "Maule", "Biobio", "Ñuble", "Araucania", "Los lagos",
               "Aysen", "Magallanes", "RM", "Los rios", "Arica y Parinacota")

casen$regs <- factor(casen$region, levels = unique(casen$region),
                    labels = reg_short)

rm(reg_short) # remueve el vector

# ================ GRAFICAR UNA VARIABLE




