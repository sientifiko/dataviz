
# lapply(c("ggplot2", "tidyverse", "ggthemes"), install.packages)

library(ggplot2); library(tidyverse); library(ggthemes)

# IMPORTAR DATA
casen <- read.delim("casen2017_set.csv", sep = ";")

# ASIGNAR NOMBRE MÁS CORTO A LAS REGIONES
reg_short <- c("Tarapaca", "Antofagasta","Atacama","Coquimbo", "Valparaiso",
               "Ohiggins", "Maule", "Biobio", "Ñuble", "Araucania", "Los lagos",
               "Aysen", "Magallanes", "RM", "Los rios", "Arica y Parinacota")

casen$regs <- factor(casen$region, levels = unique(casen$region),
                    labels = reg_short)

rm(reg_short) # remueve el vector

casen$ingreso <- as.numeric(paste0(casen$yautcor)) # factores, la pesadilla de R

casen_mayores <- casen[c(which(casen$edad>=20)),] # quitamos a menores de edad

# ================ GRAFICAR UNA VARIABLE ===================

# variable categórica, gráfica de barras de frecuencia
region <- casen_mayores %>% group_by(regs) %>%
  summarize(frec = n())

# desordenado
ggplot(region, aes(regs, frec)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y="Frecuencia")

# ordenado
ggplot(region, aes(reorder(regs, frec), frec)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y="Frecuencia")

# variable cuantitativa, histograma, forma horrible
ggplot(casen_mayores, aes(ingreso)) +
  geom_histogram(binwidth = 10000) +
  theme_bw() +
  labs(x="Ingresos", y="Frecuencia")

# forma más linda
ggplot(casen_mayores, aes(log(ingreso) )) +
  geom_histogram() +
  theme_bw() +
  labs(x="Ingresos (escala logaritmica)", y="Frecuencia")

# forma como densidad, recomendado para variables contínuas
ggplot(casen_mayores, aes(log(ingreso) )) +
  geom_density() +
  theme_hc() +
  labs(x="Ingresos (escala logaritmica)", y="Frecuencia")


# ============GRAFICAR DOS VARIABLES CATEGÓRICAS =======

# tabla de contingencia
t(prop.table(table(casen_mayores$qaut, casen_mayores$regs), margin = 2) * 100)

# generando tabla para plotear
quint_by_regs <- casen_mayores %>%
  na.omit() %>%
  group_by(regs, qaut) %>%
  summarize(n=n()) %>%
  mutate(perc= n/sum(n))

# columnas apiladas
ggplot(quint_by_regs, aes(regs, perc, fill=qaut)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="", y="frecuencia", fill= "Quintil ingreso\nautonomo")

# poniendolo bn bellakhoooo
ggplot(quint_by_regs, aes(regs, perc, fill=qaut)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label=scales::percent(perc)),
            position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="frecuencia", fill= "Quintil ingreso\nautonomo")



# ============= GRAFICAR DOS VARIABLES CUANTITATIVAS =========



# ======= GRAFICAR UNA VARIABLE CATEGÓRICA Y UNA CUANTITATIVA======



# ================== GRAFICAR UNA INTERACCIÓN ===========




# ================= OTRAS GRAFICAS CHIDAS ===============







