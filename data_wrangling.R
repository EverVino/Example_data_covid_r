setwd("/home/ever/rpractice")

# install.packages("pacman") Para instalar la biblioteca pacman que es un gestor de paquetes
library(pacman)

# Abriendo bibliotecas que serán usadas en este ejemplo
p_load("readr")
p_load("dplyr")
p_load("ggplot2")

# Importando nuestros datos del archivo .csv
covid_data <- read_csv("owid-covid-data.csv")

# Usando filter() y select() para obtener datos solamente del continente sudamericano
covid_sudamerica <-
  covid_data %>% 
  filter(continent == "South America" & !is.na(new_cases_smoothed)) %>% 
  select(location, date, new_cases_smoothed)

# Filtrando nuestros datos para obtener datos de países de la región
paises_andinos = c("Bolivia", "Peru", "Chile", "Ecuador")
covid_paises_region <-
  covid_data %>% 
  filter(location %in% paises_andinos) %>% 
  select(location, date, new_cases_smoothed)

ggplot(covid_paises_region) + 
  geom_line(aes(x = date, y = new_cases_smoothed, color = location), size = 0.5) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  labs(
    x = "",
    y = "",
    title = "Registro de contagios por COVID-19 en países de la región ",
    subtitle = "(Datos suavizados)",
    caption = "Fuente: Our World In Data",
    color = "Países"
  )

# Guardando la imágen generada por ggplot()
ggsave(
  filename = "covid_region.jpeg",
  path = "./",
  scale = 1,
  device = "png",
  dpi = 300,
  bg = "white"
)

# Ejemplo de uso de summarise() para obtener el total de contagios en la región
summarise(
  covid_paises_region,
  total = sum(new_cases_smoothed, na.rm = TRUE),
  dias = n_distinct(date)
)

# Usando summarise() y group_by() para obtener el total de contagios 
total_covid_paises_region <-
  covid_paises_region %>% 
  group_by(location) %>% 
  summarise(total_contagios = sum(new_cases_smoothed, na.rm = TRUE))

total_covid_paises_region

# Obteniendo los datos de contagios por continentes
covid_continentes <-
  covid_data %>% 
  group_by(continent, date) %>% 
  summarise(total_contagios = sum(new_cases_smoothed, na.rm = TRUE)) %>% 
  filter(!is.na(continent))

# Graficando covid_continentes
ggplot(covid_continentes) + 
  geom_line(aes(x = date, y = total_contagios, color = continent), size = 0.8) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  labs(
    x = "",
    y = "",
    title = "Registro de contagios COVID-19 por Continente",
    subtitle = "(Datos suavizados)",
    caption = "Fuente: Our World In Data",
    color = "Continente"
  )

# Guardando la imágen generada por ggplot()
ggsave(
  filename = "covid_continentes.png",
  path = "./",
  scale = 1,
  device = "png",
  dpi = 300,
  bg = "white"
)

# Obteniendo una nueva columan indicador usando mutate()
covid_continentes_indicador <-
  covid_data %>% 
  group_by(continent, date) %>% 
  summarise(nuevos_casos = sum(new_cases_smoothed, na.rm = TRUE), nuevas_muertes = sum(new_deaths_smoothed, na.rm = TRUE)) %>% 
  filter(!is.na(continent) & nuevos_casos != 0 & nuevas_muertes != 0)

covid_continentes_indicador <- 
  covid_continentes_indicador %>% 
  mutate(indicador = nuevas_muertes/nuevos_casos*1000)

# Graficando covid_continentes_indicador
ggplot(covid_continentes_indicador) + 
  geom_line(aes(x = date, y = indicador, color = continent), size = 0.8) +
  scale_color_brewer(palette = "Dark2") + theme_bw() + 
  labs(
    x = "",
    y = "",
    title = "Relación diaria de (muertes)/(1000 contagiados) COVID-19",
    subtitle = "(Datos suavizados)",
    caption = "Fuente: Our World In Data",
    color = "Continente"
  )

# Guardando la imágen generada por ggplot()
ggsave(
  filename = "covid_continentes_indicador.png",
  path = "./",
  scale = 1,
  device = "png",
  dpi = 300,
  bg = "white"
)

# Cerrando las bibliotecas abiertas 
p_unload(all)

