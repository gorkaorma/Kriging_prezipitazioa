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

###################### Iparralde Data ######################
# Descargar estaciones disponibles
library(httr)
library(jsonlite)
library(dplyr)

url <- "https://www.infoclimat.fr/stations-meteo/getList.php?pays=FR"
res <- GET(url)
txt <- content(res, "text", encoding = "UTF-8")
json <- fromJSON(txt, flatten = TRUE)
stations_ipa <- json$data
stations_ipa <- as.data.frame(stations)
head(stations_ipa)

iparralde <- stations_ipa %>% filter(dept == "64")


# Try a single station to see how the data is structured
library(rvest)
station_id <- iparralde$stationid[10]
station_name <- iparralde$name[10]
url_data <- paste0("https://www.infoclimat.fr/climatologie/annee/2026/", station_name, "/valeurs/", station_id, ".html")
# Read the HTML content of the page
res_data <- GET(url_data)
html_data <- content(res_data, "text", encoding = "UTF-8")
page <- read_html(html_data)

tables <- html_table(page, fill = TRUE)
tables[[1]]

# Extraer precipitacion (CumulPrécips) de Marzo (columna mars2026)
precip_mars <- tables[[1]]$mars2026[14] # La fila 13 o 14 corresponde a CumulPrécips

library(rvest)
# Repetir para todas las estaciones de iparralde y obtener sus datos de precipitación de marzo
iparralde_data <- data.frame()

year <- 2026

for (i in 1:nrow(iparralde)) {
  station_id <- iparralde$stationid[i]
  station_name <- iparralde$name[i]
  
  station_name_clean <- URLencode(station_name, reserved = TRUE)
  
  url_data <- paste0(
    "https://www.infoclimat.fr/climatologie/annee/",
    year, "/",
    station_name_clean,
    "/valeurs/",
    station_id,
    ".html"
  )
  
  res_data <- GET(url_data)
  html_data <- content(res_data, "text", encoding = "UTF-8")
  page <- read_html(html_data)
  
  tables <- html_table(page, fill = TRUE)
  
  if (length(tables) > 0) {
    
    tabla <- tables[[1]]
    
    # 🔍 Buscar fila "Cumul"
    fila_cumul <- which(grepl("Cumul|Total|Somme", tabla[[1]], ignore.case = TRUE))
    
    if (length(fila_cumul) > 0) {
      precip_mars <- tabla$mars2026[fila_cumul[1]]
      print(paste("Estación:", station_name, "- Precipitación marzo:", precip_mars))
      
      # 🧹 Limpieza
      precip_mars <- gsub(",", ".", precip_mars)
      precip_mars <- gsub("[^0-9.]", "", precip_mars)
      precip_mars <- as.numeric(precip_mars)
      
    } else {
      warning(paste("No se encontró Cumul en", station_name))
      precip_mars <- NA
    }

    iparralde_data <- rbind(
      iparralde_data,
      data.frame(
        estacion = station_name,
        precipitacion_mes = precip_mars,
        latitud = iparralde$latitude[i],
        longitud = iparralde$longitude[i]
      )
    )
  }
}


###################### JOIN DATA ######################
# Cambiar nombre de columna en datos_naf para que coincida con euskalmet_data (izena -> estacion)
# y renombrar lon y lat a longitud y latitud respectivamente
datos_naf <- datos_naf %>% rename(izena = estacion) %>%
  rename(longitud = lon, latitud = lat) %>% rename(altitud = ALTITUD)

iparralde_data <- iparralde_data %>% rename(izena = estacion) %>%
  mutate(
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud),
    precipitacion_mes = as.numeric(precipitacion_mes)
  )
   
  
# Unir los datos de Euskalmet, Meteo Navarra e Iparralde en un solo data frame
data <- bind_rows(
  euskalmet_data %>% select(izena, precipitacion_mes, latitud, longitud),
  datos_naf %>% select(izena, precipitacion_mes, latitud, longitud),
  iparralde_data %>% select(izena, precipitacion_mes, latitud, longitud)
 )

# Eliminar filas con datos faltantes
data <- data %>% filter(!is.na(latitud) & !is.na(longitud) & !is.na(precipitacion_mes))


#################### PLOT DATA #####################
# Plotear las estaciones de datos de precipitación en un mapa de Euskal Herria con el shapefile de Euskal Herria como fondo, usando ggplot2. 
# El color de los puntos representará la cantidad de precipitación mensual. 
library(tidyverse)

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
  geom_sf(data = herrialdeak, fill = NA, color = "black", lwd = 0.075) +
  geom_sf(data = eh, fill = NA, color = "black", lwd = 0.6) +
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
p

# Guardar el plot
ggsave(
  filename = "kriging_arrunta.png",           # nombre del archivo
  plot = p,                                   # plot a guardar
  width = 8,                                  # ancho en pulgadas
  height = 10,                                # alto en pulgadas
  dpi = 300                                   # resolución (para imprimir/publicar)
)


############# KLIMATOLOGIA ETA ANOMALIA #####################
library(terra)
# Cargar climatologias de WordClim  de marzo en carpeta wc2
climatologia <- rast("wc2/wc2.1_30s_prec_03.tif")

# Recortar la climatología al área de Euskal Herria
climatologia_crop <- crop(climatologia, eh)
climatologia_masked <- mask(climatologia_crop, eh)

# Plotear la climatología con ggplot
climatologia_df <- as.data.frame(climatologia_masked, xy = TRUE, na.rm = TRUE)
# Rename column to prec_03
names(climatologia_df)[3] <- "prec"

# Crear paleta de colores
my_palette <- colorRampPalette(c("white", "#ffffd9", "#c8e9b4", "#41b6c4", 
                                 "#1b91c0", "#225ea8","#0d2c84", "#d100d1"))
cols <- my_palette(100)   # 100 colores

# Define ranges for breaks
breaks_vals <- seq(0, 300, by = 50)

p_klima <- ggplot() +
  geom_raster(data = climatologia_df, aes(x = x, y = y, fill = prec)) +
  geom_sf(data = herrialdeak, fill = NA, color = "black", lwd = 0.075) +
  geom_sf(data = eh, fill = NA, color = "black", lwd = 0.6) +
  scale_fill_gradientn(colors = cols, 
                       breaks = breaks_vals, 
                       limits = c(0, 300), 
                       oob = scales::squish,
                       name = "(mm)") +
  labs(title = "Batez besteko prezipitazioa", subtitle = "Martxoa (1970-2000) | WorldClim") +
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

p_klima

################ Cokriging #########################
library(terra)
library(gstat)
library(sf)
library(sp)

# cargar DEM de Euskal Herria
dem <- rast("eh_dem.tif")
ext(dem)
crs(dem)
plot(dem)

# Excluir estaciones con datos faltantes
data <- data %>% filter(!is.na(latitud) & !is.na(longitud) & !is.na(precipitacion_mes))

# Crear un objeto sf con los datos de precipitación
data_sf <- st_as_sf(data, coords = c("longitud", "latitud"), crs = 4326) 

# Extraer altitud para cada estación de datos de precipitación
coords <- vect(data_sf)
data_sf$altitud_dem <- terra::extract(dem, coords)[,2]
data_clean <- data_sf %>% filter(!is.na(altitud_dem))

# Convertir a SpatialPointsDataFrame para gstat
data_sp <- as(data_clean, "Spatial")

# Crear el model de cokriging usando la variable de precipitación y la altitud como covariable.
g <- gstat(NULL, id = "prec", formula = precipitacion_mes ~ 1, data = data_sp)
g <- gstat(g, id = "alt", formula = altitud_dem ~ 1, data = data_sp)

# Variograma y ajuste
vg <- variogram(g)

vg_model <- vgm(1, "Sph", 10000, 0.1)
vg_fit <- fit.lmc(vg, g, model = vg_model)

# Crear grid para predicción
new_grid <- as.data.frame(climatologia_masked, xy = TRUE, na.rm = TRUE)
names(new_grid)[3] <- "altitud_dem"
coordinates(new_grid) <- ~x+y
proj4string(new_grid) <- crs(climatologia_masked)

# Realizar cokriging con la nueva grid
cokriging_result_2 <- predict(vg_fit, newdata = new_grid)

# Convertir resultado a raster
df_co2 <- as.data.frame(cokriging_result_2)
cokriging_raster_2 <- rast(df_co2[, c("x", "y", "prec.pred")], type = "xyz")
crs(cokriging_raster_2) <- crs(climatologia_masked)
plot(cokriging_raster_2)
lines(eh, col = "black")

# Plotear el resultado del cokriging con la nueva grid
cokriging_df2 <- as.data.frame(cokriging_raster_2, xy = TRUE, na.rm = TRUE) 

# Crear paleta de colores
palette1 <- colorRampPalette(c("white", "#ffffd9", "#c8e9b4", "#41b6c4", 
                              "#1b91c0", "#225ea8","#0d2c84", "#d100d1"))
cols1 <- palette1(100)   # 100 colores

# Define ranges for breaks
breaks_vals <- seq(0, 300, by = 50)

p3 <- ggplot() +
  geom_raster(data = cokriging_df2, aes(x = x, y = y, fill = prec.pred)) +
  geom_sf(data = herrialdeak, fill = NA, color = "black", lwd = 0.3) +
  geom_sf(data = eh, fill = NA, color = "black", lwd = 0.6) +
  scale_fill_gradientn(colors = cols1, 
                       breaks = breaks_vals, 
                       limits = c(0, 300), 
                       oob = scales::squish,
                       name = "(mm)") +
  labs(title = "Prezipitazioa Martxoan", subtitle = "Cokriging") +
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

p3

# Guardar el plot
ggsave( 
  filename = "cokriging_martxoa2.png",         # nombre del archivo
  plot = p3,                                  # plot a guardar
  width = 8,                                  # ancho en pulgadas
  height = 10,                                # alto en pulgadas
  dpi = 300                                   # resolución (para imprimir/publicar)
)



################### Anomalia #####################
library(scales)
# Calcular la anomalía restando la climatología al resultado del cokriging
anomalia <- cokriging_raster_2 / climatologia_masked * 100

# Plotear la anomalía
anomalia_df <- as.data.frame(anomalia, xy = TRUE, na.rm = TRUE)
names(anomalia_df)[3] <- "anomalia"

# Crear paleta de colores para la anomalía que sea discreta 
breaks <- c(0, 20, 40, 60, 80, 120, 140, 160, 180, 200)
cols <- c("#330000", "#663301", "#996633", "#cc9965", "#ffffff", "#ccccff", "#9999ff", "#3366ff", "#000099")

# Plotear la anomalía con ggplot y la paleta de colores creada, con límites de -100% a 100% y con una leyenda que indique que los valores representan el porcentaje de anomalía respecto a la climatología.
p_anomalia <- ggplot() +
  geom_raster(data = anomalia_df, aes(x = x, y = y, fill = anomalia)) +
  geom_sf(data = herrialdeak, fill = NA, color = "black", lwd = 0.3) +
  geom_sf(data = eh, fill = NA, color = "black", lwd = 0.6) +
  scale_fill_stepsn(
            colors = cols,
            breaks = breaks,
            limits = c(0, 200),
            oob = scales::squish,
            name = "Anomalia (%)") +
  labs(title = "Prezipitazioaren anomalia", subtitle = "2026ko Martxoa") +
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
p_anomalia

# Guardar el plot de la anomalía
ggsave( 
  filename = "anomalia_martxoa2.png",         # nombre del archivo
  plot = p_anomalia,                                  # plot a guardar
  width = 8,                                  # ancho en pulgadas
  height = 10,                                # alto en pulgadas
  dpi = 300                                   # resolución (para imprimir/publicar)
)

# Guardar los dos plots juntos en un mismo archivo
library(gridExtra)
combined_plot <- grid.arrange(p3, p_anomalia, ncol = 2)

ggsave(
  filename = "pilatutakoa_eta_anomalia_martxoa.png", # nombre del archivo
  plot = combined_plot,                       # plot a guardar
  width = 16,                                # ancho en pulgadas
  height = 10,                                # alto en pulgadas
  dpi = 300                                   # resolución (para imprimir/publicar)
)
