##############################################################
#                 Mapa 57: Mapa de deforestacion             #
#                     Año: 2023                              #
#                       Gorky Florez Castillo                #
#                  Parte 1: Datos raster                     #
##############################################################

#Librerias----------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(cptcity)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(grid)
library(RStoolbox)

Cuenca_Utcubamba = st_read("SHP/Cuenca_Utcubamba.shp")  %>% st_as_sf()
Cuenca_Utcubamb <- st_transform(Cuenca_Utcubamba , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Defores = stack("Raster/Deforesta.tif")

Cuenca_Utcubamb_defores    <- crop(Defores , Cuenca_Utcubamb)                           #
Cuenca_Utcubamb_defores    <- Cuenca_Utcubamb_defores  <- mask(Cuenca_Utcubamb_defores , Cuenca_Utcubamb)


Defores_tbl  <-  rasterToPoints(Cuenca_Utcubamb_defores)
Defores_df   <-  data.frame(Defores_tbl)
colnames(Defores_df) = c("x", "y", "Año")


library(dplyr)

Defores_df= Defores_df%>%
  subset(Año<= 19 & Año> 0)  %>%
  mutate(Años = 2000 +Año)

Defores_df_2001_6= Defores_df%>%
  subset(Año<= 6 & Año> 0)  %>%
  mutate(Años = 2000 +Año)


Defores_df_2000 = Defores_df%>% subset(Año<= 1 & Año> 0)  %>% mutate(Años = 2000 +Año)
Defores_df_2001 = Defores_df%>% subset(Año<= 2 & Año> 1)  %>% mutate(Años = 2000 +Año)
Defores_df_2002 = Defores_df%>% subset(Año<= 3 & Año> 2)  %>% mutate(Años = 2000 +Año)
Defores_df_2003 = Defores_df%>% subset(Año<= 4 & Año> 3)  %>% mutate(Años = 2000 +Año)
Defores_df_2004 = Defores_df%>% subset(Año<= 5 & Año> 4)  %>% mutate(Años = 2000 +Año)
Defores_df_2005 = Defores_df%>% subset(Año<= 6 & Año> 5)  %>% mutate(Años = 2000 +Año)



library(elevatr)
library(ggnewscale)
elev = get_elev_raster(Cuenca_Utcubamb, z=11)
Poligo_alt    <- crop(elev, Cuenca_Utcubamb)                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Cuenca_Utcubamb)


slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

summary(Geo_data_frame$alt)


#-------------
A= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cuenca_Utcubamb, fill=NA, color="black", size=1)+
  new_scale_fill()+
  geom_tile(data= Defores_df_2000, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "red")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "a) 2001", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  

  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)

B= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cuenca_Utcubamb, fill=NA, color="black", size=1)+
  new_scale_fill()+
  geom_tile(data= Defores_df_2001, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "red")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "b) 2002", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  
  
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)


C= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cuenca_Utcubamb, fill=NA, color="black", size=1)+
  new_scale_fill()+
  geom_tile(data= Defores_df_2002, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "red")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "c) 2003", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  
  
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)


D= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cuenca_Utcubamb, fill=NA, color="black", size=1)+
  new_scale_fill()+
  geom_tile(data= Defores_df_2003, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "red")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "d) 2004", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  
  
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)

E= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cuenca_Utcubamb, fill=NA, color="black", size=1)+
  new_scale_fill()+
  geom_tile(data= Defores_df_2004, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "red")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "e) 2005", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  
  
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)

FF= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cuenca_Utcubamb, fill=NA, color="black", size=1)+
  new_scale_fill()+
  geom_tile(data= Defores_df_2005, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "red")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(tag = "f) 2006", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  
  
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = "Elevacion \nmsnm",
    
    nrow = 9,
    keywidth = 0.5,
    keyheight = 0.7,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)
           
#-------------
colores<- c("#fed0bb", "#ffee32",  "#f2c078","#ff5400" ,"#c81d25")

General= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data= Defores_df_2001_6, aes(x = x, y =  y,  fill=Años)) +
  scale_fill_gradientn(colours = colores, 
                       breaks = c(2000,2001, 2002,2003,2004,2005),
                       na.value = 'white',
                       
                       name='Deforestacion')+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  geom_sf(data = Cuenca_Utcubamb, fill=NA, color="black", size=1)+
  labs(tag = "Total)", x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag.position = "top",
        plot.tag = element_text(size=14, face='bold'),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"),
        legend.position = c(0.25,0.15)
  )+
  
  
  scale_x_continuous(breaks = c(-78.4, -78, -77.6))+
  scale_y_continuous(breaks = c(-6.8, -6.2, -5.6))+
  guides(fill = guide_legend(
    title = " Deforestacion",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  annotate(geom = "text", x = -77.6, y = -6.8, hjust = 0, vjust = 1, 
           label = "Cuenca Utcubamba.",size = 4, family="serif", color = 
             "black",  fontface="italic", angle=90)
           

tbl = rasterToPoints(Cuenca_Utcubamb_defores, spatial = F)
tbl = as_tibble(tbl)
tbl = setNames(tbl, c("x", "y", "year"))
tbl = filter(tbl, year > 0)


summ = tbl %>%
  group_by(year)%>%
  summarise(count =n())%>%
  ungroup ()%>%
  mutate( year= 2000 + year)

summ = mutate(summ, meters =count *900, has = meters /10000)
summ = dplyr::select(summ, year,has)


library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# current verison
packageVersion("hrbrthemes")
## [1] '0.8.6'
update_geom_font_defaults(font_rc_light)

Estadis = ggplot(data = summ, aes(x=year, y=has)) +
  geom_col(fill= "#ce4257") +
  scale_y_comma(limits=c(0,3000)) +
  labs(x="Periodo 2000 - 2019 (años)",
       y="Hectareas (ha)",
       title="Deforestacion entre 2000 - 2019 en la Cuenca Utcubamba") + 
  theme_ipsum_rc(grid="X")+
  geom_text(aes(label=paste0(round(has,0), "ha"), hjust=0.5) , position = position_dodge(0.90), size = 3,
            vjust=-1, hjust=0.5)+
  theme(plot.background = element_rect(fill = "white"),
        plot.subtitle = element_text(face = "italic", family="serif"),
        axis.text.y  = element_text(angle = 90, size=8),
        axis.text.x  = element_text( color="black", size=8),
        plot.caption = element_text(size =6, hjust = 0.95, family="serif", face = "italic"))+
  scale_x_continuous(breaks = c(2001:2019)) 

Estadis

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 37), ylim = c(0, 30), expand = FALSE) +
  
  
  draw_plot(A , width = 10, height = 10,x = 0, y = 20)+
  draw_plot(B , width = 10, height = 10,x = 7.5, y = 20)+
  draw_plot(C , width = 10, height = 10,x = 0, y = 10)+
  draw_plot(D , width = 10, height = 10,x = 7.5, y = 10)+
  draw_plot(E , width = 10, height = 10,x = 0, y = 0)+
  draw_plot(FF , width = 10, height = 10,x = 7.5, y = 0)+
  draw_plot(FF , width = 20, height = 20,x = 13.5, y = 10)+
  draw_plot(Estadis , width = 20, height = 10,x = 16, y = 0)+
  
  theme(panel.background = element_rect(fill = "white"))




ggsave(plot=Expo ,"Mapa de DEFORESTACION.png",units = "cm",width = 37, #ancho
       height = 30, #alargo
       dpi=1200)












































