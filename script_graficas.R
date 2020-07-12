
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

# plotear desigualdad con intérvalo natural
ggplot(gini_data, aes(anno, gini)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(limits = c(0,100)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Edad", y = "Índice Gini nacional", 
       title = "Sin eje Y manipulado")

# plotear desigualdad sin intérvalo natural
ggplot(gini_data, aes(anno, gini)) +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(limits = c(0,100)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Edad", y = "Índice Gini nacional",
       title = "Cón eje Y manipulado")


# ====================== VISUALIZAR TEXTO ================

# Nube de palabras (el código es largo, pero es porque está desarmado para que 
# lo entiendan, cuando sean más secos pueden meterlo en una pura función)

library(XML);library(RCurl)
library(tm)
library(wordcloud)

texto <-  "texto_ejemplo_manifiestocomunista.txt" # poner aca el path o url del texto, debe ser un txt

txt <- readLines(texto ,encoding="UTF-8")
txt = iconv(txt, to="ASCII//TRANSLIT")

# construye un corpus
corpus <- Corpus(VectorSource(txt))

# lleva a minisculas
d  <- tm_map(corpus, tolower)

# quita espacios en blanco
d  <- tm_map(d, stripWhitespace)

# remueve la puntuaci?n
d <- tm_map(d, removePunctuation)

# remove numbers
d <- tm_map(d, removeNumbers)

# remove certain words
vector <- c("ano", "columna", "anos", "refiere", "via","torno","nota",
            "forma","formas","area", "mas", "vez") # palabras a remover de manera personalizada
d <- tm_map(d, removeWords, vector)


# carga mi archivo de palabras vacias personalizada y lo convierte a ASCII
sw <- readLines("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", encoding="UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")

# remueve palabras vacias genericas
d <- tm_map(d, removeWords, stopwords("spanish"))

# remueve palabras vac?as personalizadas
d <- tm_map(d, removeWords, sw)

# crea matriz de terminos
tdm <- TermDocumentMatrix(d)

# la convierte en objeto matriz
m <- as.matrix(tdm)

# la ordena por frecuencia
v <- sort(rowSums(m),decreasing=TRUE)

# la convierte a data frame
df <- data.frame(word = names(v),freq=v)

# par(mfrow = c(2,2)) -- > esta es otra forma de ajustar resolucion
# correr este comando para ajustar la resolucion
# a la hora de plotear si queda feo
# par(mar = rep(1,1))

set.seed(1234) # para replicabilidad de la gráfica
wordcloud(df$word, df$freq, min.freq= 5,
          #  max.words = x, 
          random.order = F,
          rot.per = 0.2,
          #    fixed.asp = T ,
          #   use.r.layout = T,
          colors = brewer.pal(8, "Dark2"))


# ==================== HAGAMOS GIFS!!!! ===============================

library(gganimate); library(transformr); library(gifski)

# vamos a ver si existe alguna relacion entre la complejidad económica de un país
# y su nivel de democracia. Para ello usaremos el índice de complejidad económica (ECI) del MIT
# que mide que tan intensiva en conocimiento es la industria de un país
# https://oec.world/en/rankings/country/eci/

# La hipótesis detrás?? Eso se los dejo a ustedes, yo solo estoy aquí para enseñarles
# a hacer visualizaciones bonitas

# para democracia usaremos el dataset del V-Dem
# partamos con este dataset, y saquemos los datos que nos interesan, 
# en este caso sacaremos el índice de democracia igualitaria, porque es
# intersante, pueden ver el codebook para más detalles de qué mide el índice

vdem <- readRDS("V-Dem-CY-Full+Others-v10.rds")

vdem2 <- vdem %>% select(country_name, country_text_id, year,
                         v2x_egaldem)

# luego importaremos el ECI
eci <- read.delim("eci_country_rankings.csv", sep = ";")

# vamos a hacer algunos arreglos, como pasar textos a mayúsculas, y construir
# una llave foránea para juntar distintos datasets

eci$pais_iso <- toupper(eci$pais_iso) # convierte en mayúsculas

# construimos la llave foránea juntando el año y el nombre corto de cada país
# en cada dataset

eci$llave <- paste0(eci$Year, eci$pais_iso)
vdem2$llave <- paste0(vdem2$year, vdem2$country_text_id)

# juntamos los dataset aplicando el homólogo de SQL en R, que nos proporciona
# tiydiverse, es una maravilla si me lo preguntan

data <- vdem2 %>% inner_join(select(eci, ECI, continente, llave), by = "llave")

# me molesta como están los continentes, así que los pasaré a nombre completo
data$continente_largo <- case_when(
  data$continente == "na" ~ "Norte America",
  data$continente == "af" ~ "Africa",
  data$continente == "as" ~ "Asia",
  data$continente == "eu" ~ "Europa",
  data$continente == "oc" ~ "Oceania",
  data$continente == "sa" ~ "América del sur"
)

# con esto estamos listos para nuestra gráfica. Para ello primero le damos forma en
# ggplot
plot <- ggplot(data, aes(ECI, v2x_egaldem, colour = continente_largo)) +
  geom_text(aes(label = country_text_id)) +
  theme_light() +
  scale_y_continuous(limits = c(0,1)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = .5) +
  facet_wrap(.~continente_largo, ncol = 3, nrow = 2) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 20),
        title = element_text(face = "bold", size = 30),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Indice de Complejidad Economica",
       y="Indice de Democracia Igualitaria", 
       colour = "Continente",
       title = "Año: {frame_time}") +
  transition_time(as.integer(year)) 

# Por último con estas sentencias damos forma al gif y lo exportamos
# Noten que pueden editar duración del gif, su tamaño, entre otros parámetros
animate(plot,  duration = 30, width = 900)
anim_save("eci_vs_edi.gif")


# hagamos otro gif ploteando la relación entre contribuciones sociales
# y niveles de desigualdad, esta vez aplicando imputación de datos por
# interpolación lineal, con la librería imputeTS
library(imputeTS); library(readxl)

# partamos limpiando los datos, importamos solo con las columnas que queremos
gini_world <- read_xls("gini_world.xls") %>%
  select(c(2, 5:64))

# pivoteamos la tabla para que las columnas pasen a filas
gini_world <- gini_world %>% gather("ano", "gini", 2:ncol(gini_world))

# hacemos lo mismo con el dataset de contribuciones sociales
contribuciones <- read_xls("contribuciones sociales.xls") 
contribuciones <- contribuciones %>% select(2, 4:63) 
contribuciones <- contribuciones %>% gather("ano", "contrib", 2:ncol(contribuciones))

# escribimos una función para automatizar la interpolación lineal
interpolation <- function(data, variable){
  
  #' @param data el df cuyos datos imputaremos
  #' @param variable el nombre de la variable que queremos imputar
  
  # creamos un vector con los países
  paises <- unique(contribuciones[["Country Code"]])
  
  for (i in paises) {
    # la función na_interpolate requiere como mínimo 2 valores no nulos
    # imputamos pobreza
    if(sum(!is.na(data[[variable]][data$`Country Code`==i]))>2){
      data[[variable]][data$`Country Code`==i] <- data[[variable]][data$`Country Code`==i] %>%
        na_interpolation() # esta linea interpola los datos
    } else{
      # si el país tiene muy pocos datos, lo sacamos del df
      data <- data[!data$`Country Code`==i,]
    }
  }
  return(data)
} # fin función interpolation()

# generamos la imputación
gini_world2 <- interpolation(gini_world, "gini")
contribuciones2 <- interpolation(contribuciones, "contrib")

# vamos a plotear si más desigualdad implica mayor distribución
# y vamos a mirar los niveles de democracia igualitaria, todo en uno
# pero antes crearemos sus respectivas llaves

gini_world2$llave <- paste0(gini_world2$ano, gini_world2$`Country Code`)
contribuciones2$llave <- paste0(contribuciones2$ano, contribuciones2$`Country Code`)

# juntamos todo
full_data <- data %>%
  inner_join(select(gini_world2, gini, llave), by = "llave") %>%
  inner_join(select(contribuciones2, contrib, llave), by= "llave")

# sacaremos a mozambique porque le falta muchos datos
full_data <- full_data %>% filter(!country_text_id=="MOZ")


# y estamos listos pa plotear
plot2 <- ggplot(full_data, aes(gini, contrib, colour = continente_largo)) +
  geom_text(aes(label = country_text_id, size= v2x_egaldem)) +
  theme_classic() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100)) +
  facet_wrap(.~continente_largo, ncol = 3, nrow = 2) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold", angle = 90),
        strip.text = element_text(size = 20),
        title = element_text(face = "bold", size = 30),
        plot.title = element_text(hjust = 0.5, vjust = -15),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Gini",
       y="Contribución social (% recaudación)", 
       colour = "Continente",
       subtitle = "(Tamaño indica nivel de democracia)",
       title = "{frame_time}") +
  transition_time(as.integer(year))
  
animate(plot2,  duration = 30, width = 900)
anim_save("gini_vs_contrib.gif")




















