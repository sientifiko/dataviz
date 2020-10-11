
library(tidyverse)
library(gganimate)
library(imputeTS)

# el siguiente es un código para producir un gif de la serie
# histórica de la curva de Phillips


options(scipen = 999)

data <- read_delim("phillips.csv", delim = ";")

data <- data %>% filter(Time >= 1987)

interpolation <- function(data, variable){
  
  #' @param data el df cuyos datos imputaremos
  #' @param variable el nombre de la variable que queremos imputar
  
  # creamos un vector con los países
  paises <- unique(data[["Country Code"]])
  
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

data <- interpolation(data, "Unemployment")

g <- ggplot(data, aes(Inflation, Unemployment)) +
  theme_classic() +
  geom_jitter() +
  scale_x_continuous(trans = "log10") +
  theme( axis.title.x = element_text(size = 20, face = "bold"),
         axis.title.y = element_text(size = 18, face = "bold", angle = 90),
         title = element_text(face = "bold", size = 30)) +
  geom_smooth() +
  labs(x="Inflación %", y = "Desempleo %", title = "{frame_time}") +
  transition_time(as.integer(Time))

animate(g,  duration = 30, width = 900)


