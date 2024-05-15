#P. Rojas Sept. 2023
#Este script da formato a los datos de cr2 para el ADA


library(lubridate)
library(dplyr)
#read the metadata
metadata <- read.csv("cr2_prAmon_2018_stations_ghcn.txt", header = TRUE, sep = ",")
precipitacion <- read.csv("cr2_prAmon_2018_ghcn.txt", header = FALSE, sep = ",")
#seleccionamos desde primeros 16 elementos del dataframe precipitacion
precipitacion2 <- precipitacion[16:nrow(precipitacion),]
# Convierte la columna "fecha" al formato de fecha
precipitacion2$V1 <- as.Date(paste(precipitacion2$V1, "-01", sep = ""), format = "%Y-%m-%d")
#-9999 a precipitation
precipitacion2[precipitacion2 == -9999] <- NA
#subset con fecha superior 1969 e inferior a 2018
precipitacion3 <- subset(precipitacion2, V1 >= "1981-01-01" & V1 <= "2010-12-01")



#vamos a NAN estaciones que tengan años con mas de 4 NAN
for (j in 1981:2010) {
  test <- precipitacion3[lubridate::year(precipitacion3$V1) == j,]
  na_count_by_year <- test %>%  summarise_all(~sum(is.na(.)))
  test[,na_count_by_year >= 4] <- NA
  precipitacion3[lubridate::year(precipitacion3$V1) == j,] <- test
}


#seleccionar columnas de precipitacion
precipitacion4 <- precipitacion3
test <- precipitacion3[,2:ncol(precipitacion3)]
#aseguramos el numerico
test2 <- apply(as.matrix(test), 2, as.numeric)

#borrar columnas con na, igual a 0 seria sin datos faltantes
test3 <- test2[,colSums(is.na(test2)) < 120]

#este es un fix rapido y mal pensando
precipitacion <- precipitacion[,colnames(precipitacion4[,c("V1",colnames(test3))])]
precipitacion3 <- precipitacion4[,c("V1",colnames(test3))]

#reciclado del codigo del atlas
codigo <- precipitacion[1,2:ncol(precipitacion)]
lat <- precipitacion[6,2:ncol(precipitacion)]
lon  <-  precipitacion[7,2:ncol(precipitacion)]

colnames(precipitacion3) <- c("fecha",codigo)

BaseDatosRegistros=data.frame(id_station=rep(as.character(codigo[1][1]),30),
                              Year=1981:2010,
                              matrix(unlist(precipitacion3[2]),ncol=12,byrow=T))

for (k in 2:length(codigo)){
  bd=data.frame(id_station=rep(as.character(codigo[k][1]),30),
                Year=1981:2010,
                matrix(unlist(precipitacion3[k+1]),ncol=12,byrow=T))
  BaseDatosRegistros=rbind(BaseDatosRegistros,bd)
}
colnames(BaseDatosRegistros)[3:14]=month.abb

zero_correction <- function(BaseDatosRegistros){
  for (i in 3:14){
    baseline.index <- which(BaseDatosRegistros[,i] == 0)
    noise <- runif(length (  baseline.index ))
    BaseDatosRegistros[baseline.index,i] <- as.numeric(BaseDatosRegistros[baseline.index,i]) + noise
  }
  return(BaseDatosRegistros)
}

BaseDatosRegistros <- zero_correction(BaseDatosRegistros)

write.csv(BaseDatosRegistros,"BaseDatosRegistros.csv",row.names=FALSE)

BaseDatosEstaciones=data.frame(id_station=unlist(codigo),
                               lat=unlist(lat),
                               lon=unlist(lon))

write.csv(BaseDatosEstaciones,"BaseDatosEstaciones.csv", row.names=FALSE)

#promedio
Mediaconcero <- terra::rast("Mediaconcero.tif")
terra::NAflag(Mediaconcero) <- -999

#raster rcp85
cr2mma_futuro_cercano <- terra::rast("cr2mma_futuro_cercano-cr2-regcm4-10k-rcp85.tif")
cr2mma_futuro_intermedio_cr2 <- terra::rast("cr2mma_futuro_intermedio_cr2-regcm4-10k-rcp85.tif")
cr2mma_futuro_lejano <- terra::rast("cr2mma_futuro_lejano-cr2-regcm4-10k-rcp85.tif")
#raster rcp25
cr2mma_futuro_cercano_25 <- terra::rast("futuro_lejano_rcp26-cimp5.tif")
cr2mma_futuro_intermedio_cr2_25 <- terra::rast("futuro_intermedio_rcp26-RegCM4-10k.tif")
cr2mma_futuro_lejano_25 <- terra::rast("futuro_cercano_rcp26-RegCM4-10k.tif")

#resample 85
cr2mma_futuro_cercano <- terra::resample(cr2mma_futuro_cercano, Mediaconcero, method="bilinear")
cr2mma_futuro_intermedio_cr2 <- terra::resample(cr2mma_futuro_intermedio_cr2, Mediaconcero, method="bilinear")
cr2mma_futuro_lejano <- terra::resample(cr2mma_futuro_lejano, Mediaconcero, method="bilinear")

#resample 25
cr2mma_futuro_cercano_25 <- terra::resample(cr2mma_futuro_cercano_25, Mediaconcero, method="bilinear")
cr2mma_futuro_intermedio_cr2_25 <- terra::resample(cr2mma_futuro_intermedio_cr2_25, Mediaconcero, method="bilinear")
cr2mma_futuro_lejano_25 <- terra::resample(cr2mma_futuro_lejano_25, Mediaconcero, method="bilinear")


#escala y multiplicacion
cr2mma_futuro_cercano <- cr2mma_futuro_cercano/100
cr2mma_futuro_intermedio_cr2 <- cr2mma_futuro_intermedio_cr2/100
cr2mma_futuro_lejano <- cr2mma_futuro_lejano/100

cr2mma_futuro_cercano_25 <- cr2mma_futuro_cercano_25/100
cr2mma_futuro_intermedio_cr2_25 <- cr2mma_futuro_intermedio_cr2_25/100
cr2mma_futuro_lejano_25 <- cr2mma_futuro_lejano_25/100

#total
#pp_faltante
pp_totalcr2mma_futuro_cercano <- Mediaconcero +  cr2mma_futuro_cercano*Mediaconcero
pp_totalcr2mma_futuro_intermedio_cr2 <- Mediaconcero +  cr2mma_futuro_intermedio_cr2*Mediaconcero
pp_totalcr2mma_futuro_lejano <- Mediaconcero + cr2mma_futuro_lejano*Mediaconcero

pp_totalcr2mma_futuro_cercano_25 <- Mediaconcero + cr2mma_futuro_cercano_25*Mediaconcero
pp_totalcr2mma_futuro_intermedio_cr2_25 <-  Mediaconcero + cr2mma_futuro_intermedio_cr2_25*Mediaconcero
pp_totalcr2mma_futuro_lejano_25 <-  Mediaconcero + cr2mma_futuro_lejano_25*Mediaconcero


#exportar
terra::writeRaster(pp_totalcr2mma_futuro_cercano_25, "pp_total_pp_totalcr2mma_futuro_cercano_25.tif", overwrite=TRUE)
terra::writeRaster(x = pp_totalcr2mma_futuro_intermedio_cr2_25, filename = "pp_total_pp_totalcr2mma_futuro_intermedio_cr2_25.tif", overwrite=TRUE)
terra::writeRaster(x = pp_totalcr2mma_futuro_lejano_25, filename = "pp_total_pp_totalcr2mma_futuro_lejano_25.tif", overwrite=TRUE)

terra::writeRaster(pp_totalcr2mma_futuro_cercano_25, "pp_total_cr2mma_futuro_cercano_25.tif", overwrite=TRUE)
terra::writeRaster(x = pp_totalcr2mma_futuro_lejano_25, filename = "pp_total_cr2mma_futuro_lejano_25.tif", overwrite=TRUE)
terra::writeRaster(x = pp_totalcr2mma_futuro_intermedio_cr2_25, filename = "pp_total_cr2mma_futuro_intermedio_cr2_25.tif", overwrite=TRUE)

BaseDatosEstaciones$lon <- as.numeric(BaseDatosEstaciones$lon)
BaseDatosEstaciones$lat <- as.numeric(BaseDatosEstaciones$lat)
v <- terra::vect(BaseDatosEstaciones, geom=c("lon", "lat"))
terra::plot(v)
data <- read.csv("BaseDatosRegistros.csv", header = TRUE, sep = ",", dec = ".")

generarpp <- function (nombre = "nombredearchivo", cr2mma_futuro_cercano){

  e <- terra::extract(cr2mma_futuro_cercano, v)
  BaseDatosEstaciones$e <- e[,2]
  lista <- c()
  for(j in BaseDatosEstaciones$id_station){
    if(!is.na(BaseDatosEstaciones[BaseDatosEstaciones$id_station == j,]$e)){
    data[data$id_station == j,3:14] <- data[data$id_station == j,3:14] + data[data$id_station == j,3:14] * BaseDatosEstaciones[BaseDatosEstaciones$id_station == j,]$e
  }else{
    lista <- c(lista,j)
  } 
  }
  dataprocesada <- data[!(data$id_station %in% lista),]
  
  #guardamos el archivo data en un archivo csv
  write.csv(dataprocesada, file = paste(nombre,"_cr2mma_futuro_cercanoBaseDatosRegistros.csv"), row.names = FALSE)
  
  write.csv(dataprocesada, file = paste(nombre,"_cr2mma_futuro_cercanoBaseDatosRegistros.csv"), row.names = FALSE)
  
  BaseDatosEstaciones2 <- BaseDatosEstaciones[BaseDatosEstaciones$id_station %in% lista == FALSE,]   
  write.csv(BaseDatosEstaciones2,paste(nombre,"BaseDatosEstaciones.csv"), row.names=FALSE)
  
}

generarpp(nombre = "cr2mma_futuro_cercano", cr2mma_futuro_cercano)
generarpp(nombre = "cr2mma_futuro_intermedio_cr2", cr2mma_futuro_intermedio_cr2)
generarpp(nombre = "cr2mma_futuro_lejano", cr2mma_futuro_lejano)

generarpp(nombre = "cr2mma_futuro_cercano_25", cr2mma_futuro_cercano_25)
generarpp(nombre = "cr2mma_futuro_intermedio_cr2_25", cr2mma_futuro_intermedio_cr2_25)
generarpp(nombre = "cr2mma_futuro_lejano_25", cr2mma_futuro_lejano_25)

