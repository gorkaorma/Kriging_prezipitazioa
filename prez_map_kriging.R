# En este script se realizara un mapa de kriging 
# para interpolar los datos de precipitacion en el territorio de Nafarroa, 
# utilizando el paquete gstat. Para ello, se descargaran datos de Aemet con
# el paquete climaemet, se prepararan los datos para el kriging, se realizara la interpolacion
# y se visualizara el resultado con ggplot2.

# Cargar librerias
library(climaemet)
library(gstat)
library(ggplot2)
library(sf)
library(terra)

# Set working directory
setwd("~/R /Kriging_prez_aemet")

# Cargar el shapefile de España
herrialdeak_shapefile <- st_read("Herrialdeak.shp")
herrialdeak_sf <- st_as_sf(herrialdeak_shapefile)
st_crs(herrialdeak_sf) <- 4326


# Set api key
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJncmtvcm1hZXR4ZWFAZ21haWwuY29tIiwianRpIjoiYWEyNWI2NGQtMDM4Ni00ZmY4LWEzMTEtNzZjNjkyYzRlNjYwIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE3NzQ0Njg2NTEsInVzZXJJZCI6ImFhMjViNjRkLTAzODYtNGZmOC1hMzExLTc2YzY5MmM0ZTY2MCIsInJvbGUiOiIifQ.t_GiEByAomvO80CDad4nL58WtxOc3pUHR-C4kmUPmV4")

# Descargar datos de precipitacion de Aemet para Nafarroa y Pais vasco con aemet_daily_clim
stations <- aemet_stations()
stations <- stations[stations$provincia %in% c("NAVARRA", "BIZKAIA", "GIPUZKOA", "ARABA/ALAVA"), ]

# Descargar datos los indicativos de stations del dia 8 de enero del 2026 utilizando aemet_daily_clim y guardarlos en un dataframe llamado df
df <- aemet_daily_clim(stations$indicativo, start = "2026-01-08", end = "2026-01-08")
    
# Preparar un data frame con las coordenadas de las estaciones y los datos de precipitacion
df <- df[, c("indicativo", "prec")]
df <- merge(df, stations[, c("indicativo", "latitud", "longitud", "nombre")], by = "indicativo")
df$prec <- as.numeric(gsub(",", ".", df$prec))
df <- df[!is.na(df$prec), ] 

# Plotear los datos de df
ggplot(df, aes(x = longitud, y = latitud, color = prec)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Precipitación el 8 de enero del 2026",
       x = "Longitud",
       y = "Latitud",
       color = "Precipitación (mm)") +
  theme_minimal() +
  coord_sf()

# Plotear el mapa de Nafarroa con los puntos de las estaciones
ggplot() + 
  geom_sf(data = herrialdeak_sf, fill = "lightblue", color = "black") +
  geom_point(data = df, aes(x = longitud, y = latitud, color = prec), size = 3) +
  scale_color_viridis_c() +
  labs(title = "Precipitación el 8 de enero del 2026",
       x = "Longitud",
       y = "Latitud",
       color = "Precipitación (mm)") +
  theme_minimal() +
  coord_sf()



