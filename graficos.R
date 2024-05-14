#https://dieghernan.github.io/tidyterra/
library(tidyterra)
library(terra)
library(ggplot2)
# Temperatures


graficador <- function (rastername, titulo, subtitulo, units){
#en algún momento tenían función distinta
#es por ahora
rasternamefile <- rastername
#raster rcp85
cr2mma_futuro_cercano <- terra::rast(paste("rcp85-futuro_cercano/Mapas/",rasternamefile,".tif",sep=""))
cr2mma_futuro_cercano <- cr2mma_futuro_cercano %>%
  rename(Futuro_Cercano_RCP85 = rastername)


cr2mma_futuro_intermedio_cr2 <- terra::rast(paste("rcp85-futuro_intermedio/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_intermedio_cr2 <- cr2mma_futuro_intermedio_cr2 %>%
  rename(Futuro_Intermedio_RCP85 = rastername)


cr2mma_futuro_lejano <- terra::rast(paste("rcp85-futuro_lejano/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_lejano <- cr2mma_futuro_lejano %>%
  rename(Futuro_Lejano_RCP85 = rastername)

mediaconcero <- terra::rast(paste("CHILE_base_original/Mapas/",rasternamefile,".tif", sep=""))
mediaconcero <- mediaconcero %>%
  rename(Histórico = rastername )


#raster rcp25
cr2mma_futuro_cercano_RCP25 <- terra::rast(paste("cr2mma_futuro_cercano_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_cercano_RCP25 <- cr2mma_futuro_cercano_RCP25 %>%
  rename(Futuro_Cercano_RCP25 = rastername)

cr2mma_futuro_intermedio_cr2_RCP25 <- terra::rast(paste("cr2mma_futuro_intermedio_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_intermedio_cr2_RCP25 <- cr2mma_futuro_intermedio_cr2_RCP25 %>%
  rename(Futuro_Intermedio_RCP25 = rastername)

cr2mma_futuro_lejano_RCP25 <- terra::rast(paste("cr2mma_futuro_lejano_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_lejano_RCP25 <- cr2mma_futuro_lejano_RCP25 %>%
  rename(Futuro_Lejano_RCP25 = rastername)

#shape
# prov <- vect("cr2mma_futuro_lejano_25/CHILE.shp")


#stack
rastertemporal <- c(cr2mma_futuro_cercano, cr2mma_futuro_intermedio_cr2, cr2mma_futuro_lejano, cr2mma_futuro_cercano_RCP25, cr2mma_futuro_intermedio_cr2_RCP25, cr2mma_futuro_lejano_RCP25)



# define number of classes
no_classes <- 9
quantiles <-  as.vector(quantile(values(rastertemporal), probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE))


# With discrete values
discreto <- terra::classify(rastertemporal, round(quantiles,0))

ggplot() +   facet_wrap(~lyr, ncol = 3) +
  geom_spatraster(data = discreto) +
  scale_fill_whitebox_d( palette = "bl_yl_rd", direction = -1, guide = guide_legend(reverse = TRUE))+  theme_minimal()+ labs(
    fill = units,
    title = titulo,
    subtitle = subtitulo, na.translate=FALSE
  )+theme(legend.text = element_text(size=16), axis.title=element_text(size=20,face="bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),   legend.box.margin=margin(10,10,10,30) )+xlim(-80, -66.41667)+scale_x_continuous(breaks=c(-80,-75,-70))
#+scale_fill_discrete(na.translate=FALSE)
#axis.text=element_text(size=24),
#
}

graficador(rastername = "EstQuant_1_in_100yr", titulo = "Precipitación Esperable", subtitulo = "Cada 100 años", units = "mm/año")
ggsave(file="EstQuant_1_in_100yr.png", width = 297, height = 210, units = "mm")

graficador(rastername = "EstQuant_1_in_50yr", titulo = "Precipitación Esperable", subtitulo = "Cada 50 años", units = "mm/año")
ggsave(file="EstQuant_1_in_50yr.png", width = 297, height = 210, units = "mm")

graficador(rastername = "RP07", titulo = "Cambios en Periodo de Retorno (RP07)", subtitulo = "30% Bajo la media", units = "años")
ggsave(file="RP07yr.png", width = 297, height = 210, units = "mm")

#graficador(rastername = "RP09", titulo = "Cambios en Periodo de Retorno (RP09)", subtitulo = "10% Bajo la media")

graficador(rastername = "RP09", titulo = "Cambios en Periodo de Retorno (RP09)", subtitulo = "30% Bajo la media", units = "años")
ggsave(file="RP09yr.png", width = 297, height = 210, units = "mm")




graficador2 <- function (rastername, titulo, subtitulo){
  #en algún momento tenían función distinta
  #es por ahora
  rasternamefile <- rastername
  #raster rcp85
  cr2mma_futuro_cercano <- terra::rast(paste("rcp85-futuro_cercano/Mapas/",rasternamefile,".tif",sep=""))
  cr2mma_futuro_cercano <- cr2mma_futuro_cercano %>%
    rename(Futuro_Cercano_RCP85 = rastername)
  
  
  cr2mma_futuro_intermedio_cr2 <- terra::rast(paste("rcp85-futuro_intermedio/Mapas/",rasternamefile,".tif", sep=""))
  cr2mma_futuro_intermedio_cr2 <- cr2mma_futuro_intermedio_cr2 %>%
    rename(Futuro_Intermedio_RCP85 = rastername)
  
  
  cr2mma_futuro_lejano <- terra::rast(paste("rcp85-futuro_lejano/Mapas/",rasternamefile,".tif", sep=""))
  cr2mma_futuro_lejano <- cr2mma_futuro_lejano %>%
    rename(Futuro_Lejano_RCP85 = rastername)
  
  mediaconcero <- terra::rast(paste("CHILE_base_original/Mapas/",rasternamefile,".tif", sep=""))
  mediaconcero <- mediaconcero %>%
    rename(Histórico = rastername )
  
  
  #raster rcp25
  cr2mma_futuro_cercano_RCP25 <- terra::rast(paste("cr2mma_futuro_cercano_25/Mapas/",rasternamefile,".tif", sep=""))
  cr2mma_futuro_cercano_RCP25 <- cr2mma_futuro_cercano_RCP25 %>%
    rename(Futuro_Cercano_RCP25 = rastername)
  
  cr2mma_futuro_intermedio_cr2_RCP25 <- terra::rast(paste("cr2mma_futuro_intermedio_25/Mapas/",rasternamefile,".tif", sep=""))
  cr2mma_futuro_intermedio_cr2_RCP25 <- cr2mma_futuro_intermedio_cr2_RCP25 %>%
    rename(Futuro_Intermedio_RCP25 = rastername)
  
  cr2mma_futuro_lejano_RCP25 <- terra::rast(paste("cr2mma_futuro_lejano_25/Mapas/",rasternamefile,".tif", sep=""))
  cr2mma_futuro_lejano_RCP25 <- cr2mma_futuro_lejano_RCP25 %>%
    rename(Futuro_Lejano_RCP25 = rastername)
  
  #shape
  # prov <- vect("cr2mma_futuro_lejano_25/CHILE.shp")
  
  
  #stack
  rastertemporal <- c(cr2mma_futuro_cercano, cr2mma_futuro_intermedio_cr2, cr2mma_futuro_lejano, cr2mma_futuro_cercano_RCP25, cr2mma_futuro_intermedio_cr2_RCP25, cr2mma_futuro_lejano_RCP25)
  
  
  
  # define number of classes
  no_classes <- 9
  quantiles <-  as.vector(quantile(values(rastertemporal), probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE))
  
  
  # With discrete values
  discreto <- terra::classify(rastertemporal, round(quantiles,0))
  
  
  mediaconcero <- terra::rast(paste("CHILE_base_original/Mapas/",rasternamefile,".tif", sep=""))
  mediaconcero <- mediaconcero %>%
    rename(Histórico = rastername )
  
  
  # With discrete values
  discreto <- terra::classify(mediaconcero, round(quantiles,0))
  
  ggplot() +   
    geom_spatraster(data = discreto) +
    scale_fill_whitebox_d( palette = "bl_yl_rd", direction = -1, guide = guide_legend(reverse = TRUE))+  theme_minimal()+ labs(
      fill = "",
      title = titulo,
      subtitle = subtitulo
    )+theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlim(-80, -66.41667)+scale_x_continuous(breaks=c(-80,-75,-70))
  
  # 
  # 
  # ggplot() +   facet_wrap(~lyr, ncol = 3) +
  #   geom_spatraster(data = discreto) +
  #   scale_fill_whitebox_d( palette = "bl_yl_rd", direction = -1, guide = guide_legend(reverse = TRUE))+  theme_minimal()+ labs(
  #     fill = "mm/año",
  #     title = titulo,
  #     subtitle = subtitulo, na.translate=FALSE
  #   )+theme(legend.text = element_text(size=16), axis.title=element_text(size=20,face="bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),   legend.box.margin=margin(10,10,10,30) )+xlim(-80, -66.41667)+scale_x_continuous(breaks=c(-80,-75,-70))
  # #+scale_fill_discrete(na.translate=FALSE)
  # #axis.text=element_text(size=24),
  # #
}
graficador2(rastername = "EstQuant_1_in_100yr", titulo = "Precipitación Esperable", subtitulo = "Cada 100 años")
ggsave(file="HIST_EstQuant_1_in_100yr.png", width = 297, height = 210, units = "mm")

graficador2(rastername = "EstQuant_1_in_50yr", titulo = "Precipitación Esperable", subtitulo = "Cada 50 años")
ggsave(file="HIST_EstQuant_1_in_50yr.png", width = 297, height = 210, units = "mm")

#PR

graficador2(rastername = "RP07", titulo = "RP07", subtitulo = "")
ggsave(file="HIST_RP07.png", width = 297, height = 210, units = "mm")


graficador2(rastername = "RP09", titulo = "RP09", subtitulo = "")
ggsave(file="HIST_RP09.png", width = 297, height = 210, units = "mm")






mediaconcero <- terra::rast(paste("CHILE_base_original/Mapas/","RP07",".tif", sep=""))
mediaconcero <- mediaconcero %>%
  rename(Histórico = "RP07" )

discreto <- terra::classify(mediaconcero, round(quantiles,0))

ggplot() +   
  geom_spatraster(data = discreto) +
  scale_fill_whitebox_d( palette = "bl_yl_rd", direction = -1, guide = guide_legend(reverse = TRUE))+  theme_minimal()+ labs(
    fill = "",
    title = "Periodo de Retorno (RP07)",
    subtitle = "30% de la media"
  )




mediaconcero <- terra::rast(paste("CHILE_base_original/Mapas/","RP09",".tif", sep=""))
mediaconcero <- mediaconcero %>%
  rename(Histórico = "RP09" )

discreto <- terra::classify(mediaconcero, round(quantiles,0))

ggplot() +   
  geom_spatraster(data = discreto) +
  scale_fill_whitebox_d( palette = "bl_yl_rd", direction = -1, guide = guide_legend(reverse = TRUE))+  theme_minimal()+ labs(
    fill = "",
    title = "Periodo de Retorno (RP09)",
    subtitle = "10% bajo la media"
  )


# ggplot() +
#   geom_spatraster(data = rastertemporal) +
#   facet_wrap(~lyr, ncol = 3) +
#   scale_fill_whitebox_b(
#     palette = "bl_yl_rd", direction = -1,
#     breaks = as.numeric(quantiles),
#     labels = labels,
#     guide = guide_legend(reverse = TRUE),
#     limits = c(0,max(values(rastertemporal), na.rm=TRUE)+1)
#   ) +  theme_minimal()+
#   labs(
#     fill = "",
#     title = "Precipitación Esperada",
#     subtitle = "En 100 años"
#   )





#Variacion %
anomalia = (rastertemporal - mediaconcero)/mediaconcero*100
#list <- as.list(anomalia)
#anomalia <- c(list[[2]], list[[3]], list[[4]],list[[5]],list[[6]],list[[7]] )
quantiles <- quantile(values(anomalia), probs = seq(0, 1, by = 0.1), na.rm = TRUE)

ggplot(prov) +
  geom_spatraster(data = anomalia) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_whitebox_c(
    palette =  "bl_yl_rd", direction = -1,
    breaks = as.numeric(quantiles),
    labels = scales::label_number(suffix = "%"),
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_minimal()+
  labs(
    fill = "",
    title = "Variación de Precipitación",
    subtitle = rastername
  )




#%
anomalia = (rastertemporal)/mediaconcero*100
list <- as.list(anomalia)
anomalia <- c(list[[2]], list[[3]], list[[4]],list[[5]],list[[6]],list[[7]] )

ggplot(prov) +
  geom_spatraster(data = anomalia) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_whitebox_c(
    palette =  rev("bl_yl_rd"), direction = -1,
    labels = scales::label_number(suffix = "%"),
    n.breaks = 20,
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_minimal()+
  labs(
    fill = "",
    title = "% de Precipitación Media",
    subtitle = rastername
  )



rasternamefile ="RP07" 
rastername = "RP07"

#raster rcp85
cr2mma_futuro_cercano <- terra::rast(paste("rcp85-futuro_cercano/Mapas/",rasternamefile,".tif",sep=""))
cr2mma_futuro_cercano <- cr2mma_futuro_cercano %>%
  rename(Futuro_Cercano_RCP85 = rastername)


cr2mma_futuro_intermedio_cr2 <- terra::rast(paste("rcp85-futuro_intermedio/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_intermedio_cr2 <- cr2mma_futuro_intermedio_cr2 %>%
  rename(Futuro_Intermedio_RCP85 = rastername)


cr2mma_futuro_lejano <- terra::rast(paste("rcp85-futuro_lejano/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_lejano <- cr2mma_futuro_lejano %>%
  rename(Futuro_Lejano_RCP85 = rastername)

mediaconcero <- terra::rast(paste("CHILE_base_original/Mapas/",rasternamefile,".tif", sep=""))
mediaconcero <- mediaconcero %>%
  rename(Histórico = rastername )


#raster rcp25
cr2mma_futuro_cercano_RCP25 <- terra::rast(paste("cr2mma_futuro_cercano_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_cercano_RCP25 <- cr2mma_futuro_cercano_RCP25 %>%
  rename(Futuro_Cercano_RCP25 = rastername)

cr2mma_futuro_intermedio_cr2_RCP25 <- terra::rast(paste("cr2mma_futuro_intermedio_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_intermedio_cr2_RCP25 <- cr2mma_futuro_intermedio_cr2_RCP25 %>%
  rename(Futuro_Intermedio_RCP25 = rastername)

cr2mma_futuro_lejano_RCP25 <- terra::rast(paste("cr2mma_futuro_lejano_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_lejano_RCP25 <- cr2mma_futuro_lejano_RCP25 %>%
  rename(Futuro_Lejano_RCP25 = rastername)

#shape
prov <- vect("cr2mma_futuro_lejano_25/CHILE.shp")


#stack
rastertemporal <- c(cr2mma_futuro_cercano, cr2mma_futuro_intermedio_cr2, cr2mma_futuro_lejano, cr2mma_futuro_cercano_RCP25, cr2mma_futuro_intermedio_cr2_RCP25, cr2mma_futuro_lejano_RCP25)

quantiles <- quantile(values(rastertemporal), probs = seq(0, 1, by = 0.8), na.rm = TRUE)


# Facet all layers
library(ggplot2)

ggplot(prov) +
  geom_spatraster(data = rastertemporal) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd", direction = -1,
    breaks = c(round(as.numeric(quantiles),1),30.2),
    labels = scales::label_number(suffix = "years"),
    guide = "coloursteps",
  ) +  theme_minimal()+
  labs(
    fill = "",
    title = "Periodo de Retorno (RP07)",
    subtitle = "Disminución de un 30% de la precipitación media"
  )





rasternamefile ="RP09" 
rastername = "RP09"

#raster rcp85
cr2mma_futuro_cercano <- terra::rast(paste("rcp85-futuro_cercano/Mapas/",rasternamefile,".tif",sep=""))
cr2mma_futuro_cercano <- cr2mma_futuro_cercano %>%
  rename(Futuro_Cercano_RCP85 = rastername)


cr2mma_futuro_intermedio_cr2 <- terra::rast(paste("rcp85-futuro_intermedio/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_intermedio_cr2 <- cr2mma_futuro_intermedio_cr2 %>%
  rename(Futuro_Intermedio_RCP85 = rastername)


cr2mma_futuro_lejano <- terra::rast(paste("rcp85-futuro_lejano/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_lejano <- cr2mma_futuro_lejano %>%
  rename(Futuro_Lejano_RCP85 = rastername)

mediaconcero <- terra::rast(paste("CHILE_base_original/Mapas/",rasternamefile,".tif", sep=""))
mediaconcero <- mediaconcero %>%
  rename(Histórico = rastername )


#raster rcp25
cr2mma_futuro_cercano_RCP25 <- terra::rast(paste("cr2mma_futuro_cercano_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_cercano_RCP25 <- cr2mma_futuro_cercano_RCP25 %>%
  rename(Futuro_Cercano_RCP25 = rastername)

cr2mma_futuro_intermedio_cr2_RCP25 <- terra::rast(paste("cr2mma_futuro_intermedio_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_intermedio_cr2_RCP25 <- cr2mma_futuro_intermedio_cr2_RCP25 %>%
  rename(Futuro_Intermedio_RCP25 = rastername)

cr2mma_futuro_lejano_RCP25 <- terra::rast(paste("cr2mma_futuro_lejano_25/Mapas/",rasternamefile,".tif", sep=""))
cr2mma_futuro_lejano_RCP25 <- cr2mma_futuro_lejano_RCP25 %>%
  rename(Futuro_Lejano_RCP25 = rastername)

#shape
prov <- vect("cr2mma_futuro_lejano_25/CHILE.shp")


#stack
rastertemporal <- c(cr2mma_futuro_cercano, cr2mma_futuro_intermedio_cr2, cr2mma_futuro_lejano, cr2mma_futuro_cercano_RCP25, cr2mma_futuro_intermedio_cr2_RCP25, cr2mma_futuro_lejano_RCP25)
quantiles <- quantile(values(rastertemporal), probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Facet all layers
library(ggplot2)

ggplot(prov) +
  geom_spatraster(data = rastertemporal) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd", direction = -1,
    breaks = as.numeric(quantiles),
    labels = scales::label_number(suffix = "years"),
    guide = guide_legend(reverse = TRUE)
  ) +  theme_minimal()+
  labs(
    fill = "",
    title = "RP07",
    subtitle = rastername
  )





















#stack
rastertemporal <- c(mediaconcero, cr2mma_futuro_cercano, cr2mma_futuro_intermedio_cr2, cr2mma_futuro_lejano)

ggplot(prov) +
  geom_spatraster(data = rastertemporal) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_whitebox_c(
    palette = rev("bl_yl_rd"),
    labels = scales::label_number(suffix = "mm"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +   theme_minimal() +
  labs(
    fill = "",
    title = "Precipitation",
    subtitle = "EstQuant_1_in_100yr"
  )



#raster rcp25
cr2mma_futuro_cercano <- terra::rast("cr2mma_futuro_cercano_25/Mapas/EstQuant_1_in_100yr.tif")
cr2mma_futuro_cercano <- cr2mma_futuro_cercano %>%
  rename(futuro_cercano = EstQuant_1_in_100yr)


cr2mma_futuro_intermedio_cr2 <- terra::rast("cr2mma_futuro_intermedio_25/Mapas/EstQuant_1_in_100yr.tif")
cr2mma_futuro_intermedio_cr2 <- cr2mma_futuro_intermedio_cr2 %>%
  rename(futuro_intermedio = EstQuant_1_in_100yr)


cr2mma_futuro_lejano <- terra::rast("cr2mma_futuro_lejano_25/Mapas/EstQuant_1_in_100yr.tif")
cr2mma_futuro_lejano <- cr2mma_futuro_lejano %>%
  rename(futuro_lejano = EstQuant_1_in_100yr)

mediaconcero <- terra::rast("rcp85-futuro_lejano/Mapas/Mediaconcero.tif")
mediaconcero <- mediaconcero %>%
  rename(media = Mediaconcero )
#stack
rastertemporal <- c(mediaconcero, cr2mma_futuro_cercano, cr2mma_futuro_intermedio_cr2, cr2mma_futuro_lejano)

anomalia <- rastertemporal - mediaconcero
ggplot(prov) +
  geom_spatraster(data = rastertemporal) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_whitebox_c(
    palette = rev("bl_yl_rd"),
    labels = scales::label_number(suffix = "mm"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +   theme_minimal()+
  labs(
    fill = "",
    title = "Precipitation",
    subtitle = "EstQuant_1_in_100yr"
  )

rastertemporal <- c(mediaconcero, cr2mma_futuro_cercano, cr2mma_futuro_intermedio_cr2, cr2mma_futuro_lejano)

#% variacion
anomalia <- (rastertemporal-mediaconcero)/mediaconcero*100
list <- as.list(anomalia)
anomalia <- c(list[[2]], list[[3]], list[[4]])
ggplot(prov) +
  geom_spatraster(data = anomalia) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    labels = scales::label_number(suffix = "%"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = "Precipitation",
    subtitle = "EstQuant_1_in_100yr"
  )













