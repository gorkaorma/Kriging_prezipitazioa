# Cargar librerias
library(gstat)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
library(httr)
library(tidyverse)
library(climaemet)

# Set working directory
setwd("~/R /Kriging_prezipitazioa")

# Cargar el shapefile de España
eh <- st_read("euskal_herria.shp")
eh <- st_set_crs(eh, 25830)
eh <- st_transform(eh, 4326)

herrialdeak <- st_read("herrialdeak.shp")
herrialdeak <- st_set_crs(herrialdeak, 25830)
herrialdeak <- st_transform(herrialdeak, 4326)

# Descarga de datos mediante web scraping
###########################################
# First, meteo navarra data
url_naf <- "https://meteo.navarra.es/estaciones/mapasdatosprecipitacion.cfm"

# Erabaki eguna (hilabetea)
res <- GET(url_naf, query = list(
  fecha_sel = "28/03/2026",
  IDParam_sel = "1"
))

html <- content(res, "text", encoding = "UTF-8")

pattern <- "'([^']+ GN)'.*?>([0-9]+\\.?[0-9]*)<"
matches <- str_match_all(html, pattern)[[1]]

datos_naf <- data.frame(
  estacion = matches[,2],
  precipitacion_mes = as.numeric(matches[,3])
)

print(datos_naf)

# Añadir coordenadas a los datos de precipitación 
station_data <- st_read("METEOR_Sym_EstMetAuto.shp")
head(station_data)
staciones_latlon <- st_transform(station_data, crs = 4326)
coords <- st_coordinates(staciones_latlon)

staciones_latlon$lon <- coords[,1]
staciones_latlon$lat <- coords[,2]

# Unir los datos de precipitacion con las coordenadas
datos_naf <- left_join(datos_naf, staciones_latlon, by = c("estacion" = "ESTACION"))


##################### EUSKALMET DATA ################
# Descargar datos de Euskalmet mediante web scraping
library(httr)
library(jsonlite)
library(dplyr)

# Convertir a data frame
url <- "https://www.euskalmet.euskadi.eus/vamet/stations/readings/C051/2026/03/webmet00-monthSummaryData.json"

json_data <- fromJSON(url, flatten = TRUE)
names(json_data)

d <- json_data[["_items"]][["_dailySummaries"]][[1]][["_total"]]
sum(d)

# Obtener estaciones de euskalmet
url_st <- "https://www.euskalmet.euskadi.eus/vamet/stations/stationList/webmet00-stationList.json"

txt <- readLines(url_st, warn = FALSE)
txt <- iconv(txt, from = "latin1", to = "UTF-8")
txt <- paste(txt, collapse = "\n")

eusk_st <- fromJSON(txt, flatten = TRUE)

library(stringr)
# Filtrar las que su id empieza por C (precipitación)
eusk_st <- eusk_st %>% filter(str_starts(id, "C"))

library(jsonlite)
library(purrr)
# Repetir para todas las estaciones de Euskalmet y obtener sus datos de precipitación
fecha <- "2026/03"

euskalmet_data <- data.frame()

for (estacion in eusk_st$id) {
  
  cat("Procesando estación:", estacion, "\n")
  
  url <- paste0(
    "https://www.euskalmet.euskadi.eus/vamet/stations/readings/",
    estacion,
    "/",
    fecha,
    "/webmet00-monthSummaryData.json"
  )
  
  json_data <- try(fromJSON(url, flatten = TRUE), silent = TRUE)
  
  if (inherits(json_data, "try-error")) next
  
  lista <- json_data$`_items`$`_dailySummaries`
  
  if (is.null(lista)) next
  
  matches <- keep(lista, function(df) {
    "_measureId._id" %in% names(df) &&
      any(df$`_measureId._id` == "precipitation")
  })
  
  if (length(matches) == 0) next
  
  precip <- matches[[1]]
  
  sum_prec <- sum(precip$`_total`, na.rm = TRUE)
  
  euskalmet_data <- rbind(
    euskalmet_data,
    data.frame(
      estacion = estacion,
      izena = eusk_st$name[eusk_st$id == estacion],
      precipitacion_mes = sum_prec,
      altitud = eusk_st$altitude[eusk_st$id == estacion],
      latitud = eusk_st$y[eusk_st$id == estacion],
      longitud = eusk_st$x[eusk_st$id == estacion]
    )
  )
}

###################### JOIN DATA ######################
# Cambiar nombre de columna en datos_naf para que coincida con euskalmet_data (izena -> estacion)
# y renombrar lon y lat a longitud y latitud respectivamente
datos_naf <- datos_naf %>% rename(izena = estacion) %>%
  rename(longitud = lon, latitud = lat) %>% rename(altitud = ALTITUD)
  
# Unir los datos de Euskalmet con los de Navarra
data <- bind_rows(datos_naf, euskalmet_data) %>%
  select(izena, altitud, latitud, longitud, precipitacion_mes)

#################### PLOT DATA #####################
# Plotear las estaciones de datos de precipitación en un mapa de Euskal Herria con el shapefile de Euskal Herria como fondo, usando ggplot2. 
# El color de los puntos representará la cantidad de precipitación mensual. 

ggplot() +
  geom_point(data = data, aes(x = longitud, y = latitud, color = precipitacion_mes), size = 3) +
  geom_sf(data = eh, fill = NA, color = "black") +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Precipitación mensual en estaciones de Euskal Herria",
       x = "Longitud", y = "Latitud", color = "Precipitación (mm)") +
  theme_minimal() 

#################### KRIGING INTERPOLATION #####################
####################### Normal Kriging #########################
# Excluir estaciones con datos faltantes
data <- data %>% filter(!is.na(latitud) & !is.na(longitud) & !is.na(precipitacion_mes))

# Crear un objeto sf con los datos de precipitación
data_sf <- st_as_sf(data, coords = c("longitud", "latitud"), crs = 4326) 

# Realizar kriging para interpolar la precipitación en toda la región
# Primero, necesitamos crear una cuadrícula de puntos para la interpolación
grid <- st_make_grid(eh, n = c(100, 100)) # Crear
grid_sf <- st_sf(geometry = grid) 
grid_sf <- st_transform(grid_sf, st_crs(data_sf))

# Realizar kriging
kriging_model <- gstat(formula = precipitacion_mes ~ 1, locations = data_sf)
kriging_result <- predict(kriging_model, newdata = grid_sf)
class(kriging_result)

# Mask the kriging result to the shape of Euskal Herria
kriging_result <- st_set_crs(kriging_result, 4326)
kriging_crop <- st_intersection(kriging_result, eh)

# Crear paleta de colores
my_palette <- colorRampPalette(c("white", "#ffffd9", "#c8e9b4", "#41b6c4", 
                                 "#1b91c0", "#225ea8","#0d2c84", "#d100d1"))
cols <- my_palette(100)   # 100 colores

# Define ranges for breaks
breaks_vals <- seq(0, 300, by = 50)

# Plotear el resultado del kriging
p <- ggplot() +
  geom_sf(data = kriging_crop, aes(fill = var1.pred), color = NA, lwd = 0) +
  geom_sf(data = herrialdeak, fill = NA, color = "black", lwd = 0.05) +
  geom_sf(data = eh, fill = NA, color = "black", lwd = 0.4) +
  scale_fill_gradientn(colors = cols, 
                       breaks = breaks_vals, 
                       limits = c(0, 300), 
                       oob = scales::squish,
                       name = "(mm)") +
  labs(title = "Prezipitazioa Martxoan", subtitle = "Kriging arrunta") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.5, "cm"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold", margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(t = 0, b = 5))
        )

# Guardar el plot
ggsave(
  filename = "kriging_arrunta.png",           # nombre del archivo
  plot = p,                                   # plot a guardar
  width = 8,                                  # ancho en pulgadas
  height = 10,                                # alto en pulgadas
  dpi = 300                                   # resolución (para imprimir/publicar)
)
