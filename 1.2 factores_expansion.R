library(tidyverse)
library(survey)
library(convey)
library(patchwork)
library(readxl)
library(RColorBrewer)

options(scipen = 999)

# =============== TRABAJANDO CON ENCUESTAS ==============

# Hasta ahora he trabajado de forma muy burda con las encuestas
# la verdad es que estas tienen un "ponderador" conocidos como factores
# de expansión.
# En las encuestas aleatorias, cada caso recibe una ponderación matemática
# respecto del segmento que representa, así por ejemplo los ingresos de
# una persona encuestada, en teoría representa los de N otras personas
# esto puede tener un enorme impacto en las estimaciones que se saquen
# asi que en este script incluiremos tales ponderadores con la librería
# survey. Es un tema que estoy estudiando aún, así que pueden existir 
# formas más limpias de hacer lo mismo que reproduciré acá, pero que por 
# ahora desconozco


# grafiquemos las llamadas curvas de Lorenz usando la Casen

# importamos los datos igual que siempre
data <- read.delim("casen2017_set.csv", sep = ";")

# filtramos a los adultos de 20 años o más, para tener una idea más
# clara de la percepción de ingresos
data_adults <- data %>% filter(edad >= 20)

# aquí creamos un objeto encuesta
data2 <- svydesign(data = data_adults, # -Los datos de la encuesta 
                   ids = ~region,      # -El nivel de estratificación 
                                       # (lean documentación de encuesta, si no
                                       #  hay estratificacion, usen "~1")
                   nest = T,           # -Si los casos están anidados en su
                                       # estrato (en este caso tenemos 
                                       # estraficación regional)
                   weights = ~expf)    # -El peso de cada persona

# convertimos la encuesta en un objeto que permite el trabajo con
# percentiles
data3 <- convey_prep(data2)

# generamos un objeto con los percentiles que queremos (nos dará
# automático una curva con base r, ignorémosla ya que la haremos con
# ggplot para que quede más bonita)
lorenz <- svylorenz(~yautcor,   # el vector de ingresos
                    data3,      # el objeto previo
                    quantiles = seq(0,1,.01 ),  # los quantiles que queremos
                    na.rm = TRUE)  # removemos valores nulos

# convertimos el objeto en data frame
lorenz.data <- lorenz$quantiles %>% #apuntamos a que queremos
  t() %>%  # trasponemos la tabla, porque está pivoteada
  as.data.frame() %>%  # la pasamos a df
  rownames_to_column(var = "quantiles") # le indicamos que pase los nombres
                                        # de filas a columnas

# los datos venían cmo texto, así que los parseamos
lorenz.data$quantiles <- as.numeric(paste0(lorenz.data$quantiles))

# y listo, graficada la curva
ggplot(lorenz.data, aes(quantiles)) +
  theme_classic() +
  geom_line(aes(y= yautcor)) +
  geom_line(aes(y= quantiles, color = "red")) +
  theme(legend.position = "none") +
  labs(x= "Quantiles", y = "Proporción de ingreso capturada")


# === OTRO EJEMPLO CON EL WVS

# El Word Value Survey de Ronald Inglehart, es una reconocida encuesta que
# intenta medir cambios culturales en las sociedades a nivel mundial
# tiene una rica fuente de datos que podemos usar, y que recomiendo estudiar
# para este ejemplo construiremos una gráfica de confianza en el caso de Chile
# si revisan la documentación, los datos de la WVS ola 7, o WVS7, verán que
# para Chile los datos son del 2018, el 2019 fue el llamado "Estallido social"
# asi que los datos pueden haber cambiado mucho de estos, pero puede reflejar
# el contexto hasta ese entonces


# partimos con lo típico
data <- read_xlsx("wvs.xlsx")

# evaluaremos la confianza en las instituciones por nivel de ingresos auto
# informado (ver documentación)

# las variables que nos interesan son las siguientes
# Q288 nivel de ingresos
# q69 pacos 
# q71 gobierno 
# q72 partidos
# q73 congreso

# cada una está rankeada de 1 = mucha a 4 = nada

# creamos un subset solo con las variables que nos interesan
data2 <- data %>% 
  select(`W_WEIGHT: Weight`, 
         `Q288: Scale of incomes`, 
         `Q69: Confidence: The Police`,
         `Q71: Confidence: The Government`, 
         `Q72: Confidence: The Political Parties`, 
         `Q73: Confidence: Parliament`) %>%
  as.data.frame() # se pasa a tibble, y es mejor trabajarlo como df

# les cambiamos los nombres a las columnas
colnames(data2) <- c("fe", "ses", "Pacos", 
                     "Gobierno", "Partidos", "Parlamento")

# convertimos los valores nulos (-1 y -2) en, bueno, valores nulos
data2$ses[data2$ses==-1|data2$ses==-2] <- NA
data2$Pacos[data2$Pacos==-1|data2$Pacos==-2] <- NA
data2$Gobierno[data2$Gobierno==-1|data2$Gobierno==-2] <- NA
data2$Partidos[data2$Partidos==-1|data2$Partidos==-2] <- NA
data2$Parlamento[data2$Parlamento==-1|data2$Parlamento==-2] <- NA

# dado que la escala likert esta como al revés, 1= mucha (?), las daremos 
# vuelta usando brujería matemática, no explicaré como lo hice, 
# estudien matemáticas!
data2$Pacos <- sqrt((data2$Pacos-4)^2)+1

data2$Gobierno <- sqrt((data2$Gobierno-4)^2)+1

data2$Partidos <- sqrt((data2$Partidos-4)^2)+1

data2$Parlamento <- sqrt((data2$Parlamento-4)^2)+1


# pivoteamos la tabla para que quede todo en columnas y no en filas
likert <- data2 %>% 
  gather("instituciones", "escala", 3:6)


# pasamos la casen al objeto encuesta, en la mísma lógica
likert2 <- svydesign(data = likert, 
                     weights = ~fe, # estos son los factores de expansion
                     ids = ~1) # acá no hay estratificación, por ende

# ok, esto es más complejo, pero para que entiendan la intuición, lo que 
# estamos haciendo es sacar la tabla de contingencia pero donde el recuento
# está ponderado por el factor de expansión, y es una tabla de 3
# interacciones, el nivel de ingresos, las
likert3 <- svytable(~ses+instituciones+escala, design= likert2) %>% 
  as.data.frame()

# sacamos el porcentaje correspondiente por nivel de ingresos
likert4 <- likert3 %>% 
  group_by(ses, instituciones, escala) %>%
  summarize(n= sum(Freq)) %>%
  mutate(p= n/ sum(n))

# cambiamos los numeros por la escala
likert4$escala <- factor(likert4$escala, 
                         levels = 1:4,
                         labels = c("Nada", "Poca", "Algo", "Mucha"))


# finalmente graficamos
ggplot(likert4, aes(escala, 
                    fct_rev(as.factor(ses)), 
                    fill= as.numeric(escala))) +
  theme_classic() +
  geom_tile(colour = "grey50") +
  scale_fill_gradient(low = "red4", 
                      high = "slateblue") +
  geom_text(aes(label=scales::percent(round(p,2))), color= "white") +
  scale_x_discrete(position = "top") +
  facet_wrap(~instituciones, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.text.y = element_text(size=8),
        legend.position = "none", 
        title = element_text(face = "bold", size = 20)) +
  labs(x="", 
       y="Nivel de ingreso", 
       title = "Confianza en las instituciones WVS7")


