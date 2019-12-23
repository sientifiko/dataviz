
# lapply(c("tidyverse", "ggthemes"), install.packages)

library(tidyverse); library(ggthemes); library(scales)

options(scipen = 999)

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
ggplot(region, aes(x= regs, y= frec)) +
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
  geom_histogram(binwidth = 1000000) +
  theme_bw() +
  labs(x="Ingresos", y="Frecuencia")

# forma más linda
ggplot(casen_mayores, aes(log(ingreso))) +
  geom_histogram() +
  theme_bw() +
  labs(x="Ingresos (escala logaritmica)", 
       y="Frecuencia")

# forma como densidad, recomendado para variables contínuas
ggplot(casen_mayores, aes(log(ingreso) )) +
  geom_density() +
  theme_hc() +
  labs(x="Ingresos (escala logaritmica)", y="Frecuencia")


# ============GRAFICAR DOS VARIABLES CATEGÓRICAS =======

# tabla de contingencia
t(prop.table(table(casen_mayores$qaut, 
                   casen_mayores$regs), margin = 2) * 100)

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

# graficar escolaridad e ingresos
ggplot(casen_mayores, aes(esc, log(ingreso) )) +
  geom_point() +
  theme_bw() +
  labs(x="Escolaridad en años", y="Ingresos (escala logaritmica)")

# ponerle más estilo y detalles
ggplot(na.omit(casen_mayores), aes(esc, log(ingreso))) +
  geom_point(aes(size=numper, color=sexo), alpha = 1/3) +
  theme_bw() +
  geom_smooth(method = "lm") +
  labs(x="Escolaridad en años", y="Ingresos (escala logaritmica)",
       size="Tamaño de familia", color="Sexo")

# incluir no linealidad
ggplot(na.omit(casen_mayores), aes(esc, log(ingreso))) +
  geom_point(aes(size=numper, color=sexo), alpha = 1/3) +
  theme_bw() +
  geom_smooth() +
  labs(x="Escolaridad en años", y="Ingresos (escala logaritmica)",
       size="Tamaño de familia", color="Sexo")
  

# ======= GRAFICAR UNA VARIABLE CATEGÓRICA Y UNA CUANTITATIVA======

# Cajón y bigotes
ggplot(casen_mayores, aes(sexo, esc)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="", y="Escolaridad en años")

# Cajón y bigotes, con más detalles
ggplot(casen_mayores, aes(sexo, esc)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous(breaks = c(1:22)) +
  labs(x="", y="Escolaridad en años")

# plotear ingresos
ggplot(casen_mayores, aes(regs, log(ingreso))) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(0, max(log(casen_mayores$ingreso), 
                                         na.rm = T),1)) +
  labs(x="", y="Ingresos (escala logaritmica)")


# agregando variable sexo
ggplot(na.omit(casen_mayores), aes(regs, log(ingreso))) +
  geom_boxplot(outlier.alpha = 0, aes(color=sexo)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(0, max(log(casen_mayores$ingreso), 
                                         na.rm = T),1)) +
  labs(x="", y="Ingresos (escala logaritmica)")



# ================== GRAFICAR UNA INTERACCIÓN ===========

# Interacción ruralidad y sexo en educación
ggplot(casen_mayores, aes(zona, esc, color=sexo, group=sexo)) +
  geom_point(stat = "summary", fun.y=mean) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x="", y="Media de escolaridad")


# ================= OTRAS GRAFICAS CHIDAS ===============

# el cleveland plot!!!

# un set de datas para plotear
reg_sex_incom <- casen_mayores %>% 
  group_by(regs, sexo) %>%
  summarize(mean_income = mean(ingreso, na.rm = T))

# Cleveland dotplot
ggplot(reg_sex_incom, aes(mean_income, regs)) +
  geom_line(aes(group=regs)) +
  geom_point(aes(color=sexo), size=3) +
  theme_bw() + 
  geom_vline(xintercept = mean(reg_sex_incom$mean_income)) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "top",
        legend.text = element_text(face = "bold",
                                   size = 15)) +
  scale_x_continuous(breaks = seq(0, 
                                  max(reg_sex_incom$mean_income),50000),
                     labels = dollar_format(prefix = "$") ) +
  labs(x="Media ingresos", y="", color="Sexo")


# =============== SOBRE DESIGUALDAD EN CHILE ===========
casen_mayores$yautcor <- as.numeric(as.character(casen_mayores$yautcor))

# ploteo box plot por edad como factor
ggplot(casen_mayores, aes(as.factor(edad), log(yautcor), 
                          fill=as.factor(edad))) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none") +
  labs(x="Edad", y= "Log ingreso autónomo corregido")

# creo un subset con quantiles 20-80 de ingreso
group_edad <- casen_mayores %>% group_by(edad) %>%
  summarize(q20 = quantile(yautcor, .2, na.rm = T),
            q80 = quantile(yautcor, .8, na.rm = T))

# calculo el diferencial entre éstos
group_edad$dif <- group_edad$q80 - group_edad$q20

# ploteo diferencial
ggplot(group_edad, aes(as.factor(edad), dif, fill=as.factor(edad))) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none") +
  geom_hline(yintercept = 242879.2, colour = "red",
             size = 1) +
  labs(x="Edad", y= "Diferencia ingreso aut. corregido q_20 y q_80")


# package inequality y foreign pa importar data de extensión .dta
library(ineq); library(foreign)

# dataset con Gini 2017
gini_edad_2017 <- casen_mayores %>% group_by(edad) %>%
  summarize(gini = ineq(ypch, type = "Gini"))

# importo casen de 1990
casen1990 <- read.dta("casen1990.dta")

# dataset gini 1990
gini_edad_1990 <- casen1990 %>% 
  filter(edad >= 20) %>%
  group_by(edad) %>%
  summarize(gini = ineq(ypchaj, type = "Gini"))

# asigno años
gini_edad_2017$anno <- 2017
gini_edad_1990$anno <- 1990

# fusiono
gini_edad <- rbind(gini_edad_1990, gini_edad_2017)
gini_edad$gini <- gini_edad$gini * 100

# ploteo
ggplot(gini_edad, aes(as.factor(edad), gini, fill = as.factor(edad))) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   size = 12)) +
  scale_x_discrete(breaks = seq(20,117, 5)) +
  scale_y_continuous(limits = c(0,100)) +
  facet_wrap(~anno) +
  labs(x="Edad", y = "Ìndice Gini")
  
# importar set histórico del gini
gini_data <- read.delim("gini_data.csv", sep = "\t")

# plotear desigualdad
ggplot(gini_data, aes(anno, gini)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(limits = c(0,100)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Edad", y = "Índice Gini nacional")












