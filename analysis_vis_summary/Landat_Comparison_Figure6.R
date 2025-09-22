library(raster)
library(ggplot2)
library(sf)
library(ggspatial)
library(dplyr)
library(tidyr)
library(cowplot)
library(scales)
setwd(".../imgs_for_figure/")

S2color = '#FFDB53'
Pekelcolor = '#FFA93D'
Pickenscolor = '#E92100'
############    ###################
###         AKCP 
############    ###################

##########
### LOAD IN DATA
#########

#### shapefiles
AKCP_dry_pekel <- st_read("AKCP_pekel_month_2021.shp") 
AKCP_dry_pickens <- st_read("AKCP_pickens_month_2021.shp")
AKCP_dry_S2 <- st_read("AKCP_S2max_2021.shp")
AKCP_wet_pekel <- st_read("AKCP_pekel_annual_2019.shp") 
AKCP_wet_pickens <- st_read("AKCP_pickens_month_2019.shp")
AKCP_wet_S2 <- st_read("AKCP_S2max_2019.shp")

#### rasters
AKCP_rgb_dry_landsat <- brick("AKCP_rgb_Landsat_dry.tif")
AKCP_rgb_wet_landsat <- brick("AKCP_rgb_Landsat_wet.tif")
AKCP_ndwi_dry_landsat <- raster("AKCP_ndwi_Landsat_dry.tif")
AKCP_ndwi_wet_landsat <- raster("AKCP_ndwi_Landsat_wet.tif")

AKCP_rgb_dry_S2<-brick("AKCP_rgb_S2_dry.tif")
AKCP_rgb_wet_S2 <- brick("AKCP_rgb_S2_wet.tif")
AKCP_ndwi_dry_S2 <- raster("AKCP_ndwi_S2_dry.tif")
AKCP_ndwi_wet_S2 <- raster("AKCP_ndwi_S2_wet.tif")

##########
### CONVERT TO DATAFRAMES, DEAL WITH CRSs
#########
## RGB
AKCP_dry_rgb_landsat<-as.data.frame(AKCP_rgb_dry_landsat, xy=TRUE) %>% 
                      drop_na() %>%
                        mutate(
                          # Rescale each channel to range from 0 to 1 for full contrast
                          red_scaled = rescale(red, to = c(0, 1)),
                          green_scaled = rescale(green, to = c(0, 1)),
                          blue_scaled = rescale(blue, to = c(0, 1)),
                          ### add hex
                          hex = rgb(
                            red = red_scaled,
                            green = green_scaled,
                            blue = blue_scaled,
                            maxColorValue = 1))
AKCP_wet_rgb_landsat<-as.data.frame(AKCP_rgb_wet_landsat, xy=TRUE) %>% 
                      drop_na() %>%
                      mutate(
                      # Rescale each channel to range from 0 to 1 for full contrast
                            red_scaled = rescale(red, to = c(0, 1)),
                          green_scaled = rescale(green, to = c(0, 1)),
                          blue_scaled = rescale(blue, to = c(0, 1)),
                          ### add hex
                          hex = rgb(
                            red = red_scaled,
                            green = green_scaled,
                            blue = blue_scaled,
                            maxColorValue = 1))
## NDWI
AKCP_ndwi_dry_landsat<-as.data.frame(AKCP_ndwi_dry_landsat, xy=TRUE)
AKCP_ndwi_wet_landsat<-as.data.frame(AKCP_ndwi_wet_landsat, xy=TRUE)

## FALSE
AKCP_wet_false_landsat<-as.data.frame(AKCP_rgb_wet_landsat, xy=TRUE) %>% 
                          drop_na() %>%filter(nir>0) %>%
                          mutate(hex = rgb(red=nir,  green=green, blue=blue, maxColorValue = 0.3))
AKCP_dry_false_landsat<-as.data.frame(AKCP_rgb_dry_landsat, xy=TRUE) %>% 
                        drop_na() %>% filter(nir>0) %>%
                        mutate(hex = rgb(red=nir,  green=green, blue=blue, maxColorValue =0.3))
###### Sentinel-2
## RGB
AKCP_dry_rgb_S2<-as.data.frame(AKCP_rgb_dry_S2, xy=TRUE) %>% 
                      drop_na() %>%
                      mutate(
                        # Rescale each channel to range from 0 to 1 for full contrast
                        red_scaled = rescale(B4, to = c(0, 1)),
                        green_scaled = rescale(B3, to = c(0, 1)),
                        blue_scaled = rescale(B2, to = c(0, 1)),
                        ### add hex
                        hex = rgb(
                          red = red_scaled,
                          green = green_scaled,
                          blue = blue_scaled,
                          maxColorValue = 1))
AKCP_wet_rgb_S2<-as.data.frame(AKCP_rgb_wet_S2, xy=TRUE) %>% 
                      drop_na() %>%
                    mutate(
                      # Rescale each channel to range from 0 to 1 for full contrast
                      red_scaled = rescale(B4, to = c(0, 1)),
                      green_scaled = rescale(B3, to = c(0, 1)),
                      blue_scaled = rescale(B2, to = c(0, 1)),
                      ### add hex
                        hex = rgb(
                        red = red_scaled,
                        green = green_scaled,
                        blue = blue_scaled,
                        maxColorValue = 1))
## NDWI
AKCP_ndwi_dry_S2<-as.data.frame(AKCP_ndwi_dry_S2, xy=TRUE)
AKCP_ndwi_wet_S2<-as.data.frame(AKCP_ndwi_wet_S2, xy=TRUE)


## FALSE
AKCP_wet_false_S2<-as.data.frame(AKCP_rgb_wet_S2, xy=TRUE) %>% 
                    drop_na() %>%
                    mutate(hex = rgb(red=B8,  green=B3, blue=B2, maxColorValue = 0.7))
AKCP_dry_false_S2<-as.data.frame(AKCP_rgb_dry_S2, xy=TRUE) %>% 
                      drop_na() %>% 
                      mutate(hex = rgb(red=B8,  green=B3, blue=B2, maxColorValue =1))

#### get everything in the same CRS
AKCP_dry_pekel<-st_transform(AKCP_dry_pekel, crs(AKCP_rgb_dry_landsat))
AKCP_dry_pickens<-st_transform(AKCP_dry_pickens, crs(AKCP_rgb_dry_landsat))
AKCP_dry_S2<-st_transform(AKCP_dry_S2, crs(AKCP_rgb_dry_landsat))

AKCP_wet_pickens<-st_transform(AKCP_wet_pickens, crs(AKCP_rgb_wet_landsat))
AKCP_wet_pekel<-st_set_crs(AKCP_wet_pekel,4326)
AKCP_wet_S2<-st_transform(AKCP_wet_S2, crs(AKCP_rgb_wet_landsat))


##########
### PLOTTING
#########

###### LANDSAT
## RGB
p_AKCP_l_d_rgb<-ggplot() +
                  geom_raster(data = AKCP_dry_rgb_landsat, aes(x = x, y = y, fill = hex)) +
                  scale_fill_identity() + 
                  geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                  theme_void() +
                  labs(title ='Landsat RGB') +
                  theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))
p_AKCP_l_w_rgb<-ggplot() +
                  geom_raster(data = AKCP_wet_rgb_landsat, aes(x = x, y = y, fill = hex)) +
                  scale_fill_identity() +   
                  geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                  theme_void()# +
               #   labs(y = 'Wet') +
                  #theme(axis.title.y = element_text(family = "Helvetica", face = "bold", size = 14, color = "black"))
## NDWI
p_AKCP_l_d_ndwi<-ggplot() +
                geom_raster(data = AKCP_ndwi_dry_landsat, aes(x = x, y = y, fill = NDWI)) +
                scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                                     values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                                     limits = c(-1, 1))+   
                geom_sf(data = AKCP_dry_pickens, fill = NA, color = NA, linewidth = 0.5) +
                guides(fill="none")+              
                theme_void() +
                labs(title ='Landsat NDWI') +
                theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black",hjust=0.5))

p_AKCP_l_w_ndwi<-ggplot() +
                  geom_raster(data = AKCP_ndwi_wet_landsat, aes(x = x, y = y, fill = NDWI)) +
                  scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                       values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                       limits = c(-1, 1))+  
                  geom_sf(data = AKCP_wet_pickens, fill = NA, color = NA, linewidth = 0.5) +
                #  geom_sf(data = AKCP_wet_pekel, fill = NA, color = "black", linewidth = 0.5) +
                  guides(fill="none")+
                  theme_void()

## false color
p_AKCP_l_d_false<-ggplot() +
                    geom_raster(data = AKCP_dry_false_landsat, aes(x = x, y = y, fill = hex)) +
                    scale_fill_identity() + 
                    geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                    theme_void()
p_AKCP_l_w_false<-ggplot() +
                    geom_raster(data = AKCP_wet_false_landsat, aes(x = x, y = y, fill = hex)) +
                    scale_fill_identity() +   
                    geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                    theme_void()

###### Sentinel-2
## RGB
p_AKCP_s2_d_rgb<-ggplot() +
                geom_raster(data = AKCP_dry_rgb_S2, aes(x = x, y = y, fill = hex)) +
                scale_fill_identity() +   
                theme_void() +
                geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                labs(title ='Sentinel-2 RGB') +
                theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))
  
p_AKCP_s2_w_rgb<-ggplot() +
                geom_raster(data = AKCP_wet_rgb_S2, aes(x = x, y = y, fill = hex)) +
                scale_fill_identity() + 
                geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                theme_void()
## NDWI
p_AKCP_S2_d_ndwi<-ggplot() +
                geom_raster(data = AKCP_ndwi_dry_S2, aes(x = x, y = y, fill = NDWI)) +
                scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                                     values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                                     limits = c(-1, 1))+ 
                geom_sf(data = AKCP_dry_S2, fill = NA, color =NA, linewidth = 0.5) +
                guides(fill="none")+
                theme_void() +
                labs(title ='Sentinel-2 NDWI') +
                theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))

p_AKCP_S2_w_ndwi<-ggplot() +
                geom_raster(data = AKCP_ndwi_wet_S2, aes(x = x, y = y, fill = NDWI)) +
                scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                                     values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                                     limits = c(-1, 1))+ 
                geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                guides(fill="none")+
                theme_void()
## FALSE
p_AKCP_s2_w_false<-ggplot() +
                    geom_raster(data = AKCP_wet_false_S2, aes(x = x, y = y, fill = hex)) +
                    scale_fill_identity() + 
                    geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                    theme_void()
p_AKCP_s2_d_false<-ggplot() +
                      geom_raster(data = AKCP_dry_false_S2, aes(x = x, y = y, fill = hex)) +
                      scale_fill_identity() + 
                      geom_sf(data = AKCP_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
                      theme_void()

### with shapefiles
p_AKCP_dry_shp_L<-p_AKCP_l_d_rgb+ 
          geom_sf(data = AKCP_dry_pickens, fill = NA, color = Pickenscolor, linewidth = 0.5) +
          geom_sf(data = AKCP_dry_pekel, fill = NA, color = Pekelcolor, linewidth = 0.5) +
          guides(fill="none")+
          labs(title ='Landsat RGB', x = ' ', y = ' ') +
          theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))
p_AKCP_wet_shp_L<-p_AKCP_l_w_rgb+ 
            geom_sf(data = AKCP_wet_pickens, fill = NA, color = Pickenscolor, linewidth = 0.5) +
            geom_sf(data = AKCP_wet_pekel, fill = NA, color = Pekelcolor, linewidth = 0.5) +
            guides(fill="none")

p_AKCP_dry_shp_S<-p_AKCP_s2_d_rgb + 
              geom_sf(data = AKCP_dry_S2, fill = NA, color = S2color, linewidth = 0.5) +
              guides(fill="none")+
              labs(title ='Sentinel-2 RGB', x = ' ', y = ' ') +
              theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))
p_AKCP_wet_shp_S<-p_AKCP_s2_w_rgb + 
          geom_sf(data = AKCP_wet_S2, fill = NA, color = S2color, linewidth = 0.5) +
          guides(fill="none")+
          xlab(" ") + ylab(" ")  

#### arrange grid
AKCP_dry <- plot_grid(p_AKCP_dry_shp_L,p_AKCP_dry_shp_S,
                      p_AKCP_l_d_ndwi, p_AKCP_S2_d_ndwi,
                      nrow=1, align = "hv")

AKCP_dry_wlabel<-ggdraw() +
                draw_label("Dry", x = 0, y = 0.5, hjust = 0.5, angle = 90, size = 14, fontface = "bold") +
                draw_plot(AKCP_dry, x = 0.01 , y = 0, width = 0.98, height = 1) +
                theme(plot.margin = margin(0, 10, 0, 10))

AKCP_wet <- plot_grid(p_AKCP_wet_shp_L,p_AKCP_wet_shp_S,
                      p_AKCP_l_w_ndwi, p_AKCP_S2_w_ndwi,
                      nrow=1, align = "hv")
AKCP_wet_wlabel<-ggdraw() +
                draw_label("Wet", x = 0, y = 0.5, hjust = 0.5, angle = 90, size = 14, fontface = "bold") +
                draw_plot(AKCP_wet, x = 0.01 , y = 0, width = 0.98, height = 1) +
                theme(plot.margin = margin(0, 10, 0, 10))


### labels

NDWI_legend<-get_legend(ggplot() +
                          geom_raster(data = AKCP_ndwi_wet_S2, aes(x = x, y = y, fill = NDWI)) +
                          scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                                               values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                                               limits = c(-1, 1)) +
                          theme( legend.direction = "horizontal",        
                                 legend.title.position = "top", 
                                 legend.title = element_text(hjust = 0.5),
                                 legend.box = "horizontal")) 
class_legend<-get_legend( ggplot() + 
                  geom_sf(data = AKCP_wet_pickens, aes( fill = 'GLAD')) +
                  geom_sf(data = AKCP_wet_pekel,  aes( fill = 'GSWO')) +
                  geom_sf(data = AKCP_wet_S2, aes( fill = 'Sentinel-2'))+
                  scale_fill_manual(
                      name = "Data Source",
                      values = c("GLAD" = Pickenscolor, "GSWO" = Pekelcolor, "Sentinel-2" = S2color))+
                    guides(fill = guide_legend(
                      override.aes = list(
                        shape = 3, color= NA,
                        size = 3
                      )))+
                    theme( legend.direction = "horizontal",        
                           legend.title.position = "top", 
                           legend.title = element_text(hjust = 0.5),
                           legend.box = "horizontal")) 

#### put everything together
AKCP_both<-plot_grid(AKCP_dry_wlabel, NULL,AKCP_wet_wlabel, nrow=3, align='v', rel_heights=c(1,-0.15,1))

AKCP_both_wlabel<-ggdraw() +
  draw_label("Alaskan Coastal Plain", x = 0.98, y = 0.5, hjust = 0.5, angle = 270, size = 14, fontface = "bold") +
  draw_plot(AKCP_both, x = 0.01 , y = 0, width = 0.98, height = 1) +
  theme(plot.margin = margin(0, 10, 0, 0))

#legends_both<-plot_grid(NDWI_legend, class_legend, nrow=1, align = 'h')
#AKCP_finalplot<-plot_grid(AKCP_both, legends_both, nrow=2, rel_heights = c(1,0.2))

#ggsave('Rasters_2by4.jpg', AKCP_finalplot,width=300, height = 120, units= 'mm', dpi = 500)

############    ###################
###         YKF 
############    ###################

##########
### LOAD IN DATA
#########

#### shapefiles
YKF_dry_pekel <- st_read("YKF_pekel_month_2016.shp") 
YKF_dry_pickens <- st_read("YKF_pickens_month_2016.shp")
YKF_dry_S2 <- st_read("YKF_S2max_2016.shp")
YKF_wet_pekel <- st_read("YKF_pekel_month_2020.shp") 
YKF_wet_pickens <- st_read("YKF_pickens_month_2020.shp")
YKF_wet_S2 <- st_read("YKF_S2max_2020.shp")

#### rasters
YKF_rgb_dry_landsat <- brick("YKF_rgb_Landsat_dry.tif")
YKF_rgb_wet_landsat <- brick("YKF_rgb_Landsat_wet.tif")
YKF_ndwi_dry_landsat <- raster("YKF_ndwi_Landsat_dry.tif")
YKF_ndwi_wet_landsat <- raster("YKF_ndwi_Landsat_wet.tif")

YKF_rgb_dry_S2<-brick("YKF_rgb_S2_dry.tif")
YKF_rgb_wet_S2 <- brick("YKF_rgb_S2_wet.tif")
YKF_ndwi_dry_S2 <- raster("YKF_ndwi_S2_dry.tif")
YKF_ndwi_wet_S2 <- raster("YKF_ndwi_S2_wet.tif")

##########
### CONVERT TO DATAFRAMES, DEAL WITH CRSs
#########
## RGB
YKF_dry_rgb_landsat<-as.data.frame(YKF_rgb_dry_landsat, xy=TRUE) %>% 
  drop_na() %>%
  mutate(
    # Rescale each channel to range from 0 to 1 for full contrast
    red_scaled = rescale(red, to = c(0, 1)),
    green_scaled = rescale(green, to = c(0, 1)),
    blue_scaled = rescale(blue, to = c(0, 1)),
    ### add hex
    hex = rgb(
      red = red_scaled,
      green = green_scaled,
      blue = blue_scaled,
      maxColorValue = 1))
YKF_wet_rgb_landsat<-as.data.frame(YKF_rgb_wet_landsat, xy=TRUE) %>% 
  drop_na() %>%
  mutate(
    # Rescale each channel to range from 0 to 1 for full contrast
    red_scaled = rescale(red, to = c(0, 1)),
    green_scaled = rescale(green, to = c(0, 1)),
    blue_scaled = rescale(blue, to = c(0, 1)),
    ### add hex
    hex = rgb(
      red = red_scaled,
      green = green_scaled,
      blue = blue_scaled,
      maxColorValue = 1))
## NDWI
YKF_ndwi_dry_landsat<-as.data.frame(YKF_ndwi_dry_landsat, xy=TRUE)
YKF_ndwi_wet_landsat<-as.data.frame(YKF_ndwi_wet_landsat, xy=TRUE)

## FALSE
YKF_wet_false_landsat<-as.data.frame(YKF_rgb_wet_landsat, xy=TRUE) %>% 
  drop_na() %>%filter(nir>0) %>%
  mutate(hex = rgb(red=nir,  green=green, blue=blue, maxColorValue = 0.3))
YKF_dry_false_landsat<-as.data.frame(YKF_rgb_dry_landsat, xy=TRUE) %>% 
  drop_na() %>% filter(nir>0) %>%
  mutate(hex = rgb(red=nir,  green=green, blue=blue, maxColorValue =0.3))
###### Sentinel-2
## RGB
YKF_dry_rgb_S2<-as.data.frame(YKF_rgb_dry_S2, xy=TRUE) %>% 
  drop_na() %>%
  mutate(
    # Rescale each channel to range from 0 to 1 for full contrast
    red_scaled = rescale(B4, to = c(0, 1)),
    green_scaled = rescale(B3, to = c(0, 1)),
    blue_scaled = rescale(B2, to = c(0, 1)),
    ### add hex
    hex = rgb(
      red = red_scaled,
      green = green_scaled,
      blue = blue_scaled,
      maxColorValue = 1))
YKF_wet_rgb_S2<-as.data.frame(YKF_rgb_wet_S2, xy=TRUE) %>% 
  drop_na() %>%
  mutate(
    # Rescale each channel to range from 0 to 1 for full contrast
    red_scaled = rescale(B4, to = c(0, 1)),
    green_scaled = rescale(B3, to = c(0, 1)),
    blue_scaled = rescale(B2, to = c(0, 1)),
    ### add hex
    hex = rgb(
      red = red_scaled,
      green = green_scaled,
      blue = blue_scaled,
      maxColorValue = 1))
## NDWI
YKF_ndwi_dry_S2<-as.data.frame(YKF_ndwi_dry_S2, xy=TRUE)
YKF_ndwi_wet_S2<-as.data.frame(YKF_ndwi_wet_S2, xy=TRUE)


## FALSE
YKF_wet_false_S2<-as.data.frame(YKF_rgb_wet_S2, xy=TRUE) %>% 
  drop_na() %>%
  mutate(hex = rgb(red=B8,  green=B3, blue=B2, maxColorValue = 1))
YKF_dry_false_S2<-as.data.frame(YKF_rgb_dry_S2, xy=TRUE) %>% 
  drop_na() %>% 
  mutate(hex = rgb(red=B8,  green=B3, blue=B2, maxColorValue =1))

#### get everything in the same CRS
YKF_dry_pekel<-st_transform(YKF_dry_pekel, crs(YKF_rgb_dry_landsat))
YKF_dry_pickens<-st_transform(YKF_dry_pickens, crs(YKF_rgb_dry_landsat))
YKF_dry_S2<-st_transform(YKF_dry_S2, crs(YKF_rgb_dry_landsat))

YKF_wet_pickens<-st_transform(YKF_wet_pickens, crs(YKF_rgb_wet_landsat))
YKF_wet_pekel<-st_transform(YKF_wet_pekel, crs(YKF_rgb_wet_landsat))
YKF_wet_S2<-st_transform(YKF_wet_S2, crs(YKF_rgb_wet_landsat))


##########
### PLOTTING
#########

###### LANDSAT
## RGB
p_YKF_l_d_rgb<-ggplot() +
  geom_raster(data = YKF_dry_rgb_landsat, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() + 
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  theme_void() +  
  xlab(" ") + ylab(" ")  
 # labs(title ='Landsat RGB') +
  #theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))
p_YKF_l_w_rgb<-ggplot() +
  geom_raster(data = YKF_wet_rgb_landsat, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() +   
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  theme_void()# +
#   labs(y = 'Wet') +
#theme(axis.title.y = element_text(family = "Helvetica", face = "bold", size = 14, color = "black"))
## NDWI
p_YKF_l_d_ndwi<-ggplot() +
  geom_raster(data = YKF_ndwi_dry_landsat, aes(x = x, y = y, fill = NDWI)) +
  scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                       values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                       limits = c(-1, 1))+   
  geom_sf(data = YKF_dry_pickens, fill = NA, color = NA, linewidth = 0.5) +
  guides(fill="none")+              
  theme_void() +
  xlab(" ") + ylab(" ")  
#  labs(title ='Landsat NDWI') +
  #theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black",hjust=0.5))

p_YKF_l_w_ndwi<-ggplot() +
  geom_raster(data = YKF_ndwi_wet_landsat, aes(x = x, y = y, fill = NDWI)) +
  scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                       values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                       limits = c(-1, 1))+  
  geom_sf(data = YKF_wet_pickens, fill = NA, color = NA, linewidth = 0.5) +
  #  geom_sf(data = YKF_wet_pekel, fill = NA, color = "black", linewidth = 0.5) +
  guides(fill="none")+
  theme_void()

## false color
p_YKF_l_d_false<-ggplot() +
  geom_raster(data = YKF_dry_false_landsat, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() + 
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  theme_void()
p_YKF_l_w_false<-ggplot() +
  geom_raster(data = YKF_wet_false_landsat, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() +   
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  theme_void()

###### Sentinel-2
## RGB
p_YKF_s2_d_rgb<-ggplot() +
  geom_raster(data = YKF_dry_rgb_S2, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() +   
  theme_void() +
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  xlab(" ") + ylab(" ")  
 # labs(title ='Sentinel-2 RGB') +
  #theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))

p_YKF_s2_w_rgb<-ggplot() +
  geom_raster(data = YKF_wet_rgb_S2, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() + 
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  theme_void()
## NDWI
p_YKF_S2_d_ndwi<-ggplot() +
  geom_raster(data = YKF_ndwi_dry_S2, aes(x = x, y = y, fill = NDWI)) +
  scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                       values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                       limits = c(-1, 1))+ 
  geom_sf(data = YKF_dry_S2, fill = NA, color =NA, linewidth = 0.5) +
  guides(fill="none")+
  theme_void() +
  xlab(" ") + ylab(" ")  
  #labs(title ='Sentinel-2 NDWI') +
 # theme( plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))

p_YKF_S2_w_ndwi<-ggplot() +
  geom_raster(data = YKF_ndwi_wet_S2, aes(x = x, y = y, fill = NDWI)) +
  scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                       values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                       limits = c(-1, 1))+ 
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  guides(fill="none")+
  theme_void()
## FALSE
p_YKF_s2_w_false<-ggplot() +
  geom_raster(data = YKF_wet_false_S2, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() + 
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  theme_void()
p_YKF_s2_d_false<-ggplot() +
  geom_raster(data = YKF_dry_false_S2, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() + 
  geom_sf(data = YKF_wet_S2, fill = NA, color = NA, linewidth = 0.5) +
  theme_void()


### with shapefiles
p_YKF_dry_shp_L<-p_YKF_l_d_rgb+ 
  geom_sf(data = YKF_dry_pickens, fill = NA, color = Pickenscolor, linewidth = 0.5) +
  geom_sf(data = YKF_dry_pekel, fill = NA, color = Pekelcolor, linewidth = 0.5) +
  guides(fill="none")+
  xlab(" ") + ylab(" ")  
 # labs(title ='Landsat RGB', x = ' ', y = ' ') +
  #theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))
p_YKF_wet_shp_L<-p_YKF_l_w_rgb+ 
  geom_sf(data = YKF_wet_pickens, fill = NA, color = Pickenscolor, linewidth = 0.5) +
  geom_sf(data = YKF_wet_pekel, fill = NA, color = Pekelcolor, linewidth = 0.5) +
  guides(fill="none")

p_YKF_dry_shp_S<-p_YKF_s2_d_rgb + 
  geom_sf(data = YKF_dry_S2, fill = NA, color = S2color, linewidth = 0.5) +
  guides(fill="none")+
  xlab(" ") + ylab(" ")  
# labs(title ='Sentinel-2 RGB', x = ' ', y = ' ') +
 # theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 14, color = "black", hjust=0.5))
p_YKF_wet_shp_S<-p_YKF_s2_w_rgb + 
  geom_sf(data = YKF_wet_S2, fill = NA, color = S2color, linewidth = 0.5) +
  guides(fill="none")+
  xlab(" ") + ylab(" ")  

#### arrange grid
YKF_dry <- plot_grid(p_YKF_dry_shp_L,p_YKF_dry_shp_S,
                      p_YKF_l_d_ndwi, p_YKF_S2_d_ndwi,
                      nrow=1, align = "hv")

YKF_dry_wlabel<-ggdraw() +
  draw_label("Dry", x = 0, y = 0.5, hjust = 0.5, angle = 90, size = 14, fontface = "bold") +
  draw_plot(YKF_dry, x = 0.01 , y = 0, width = 0.98, height = 1) +
  theme(plot.margin = margin(0, 10, 0, 10))

YKF_wet <- plot_grid(p_YKF_wet_shp_L,p_YKF_wet_shp_S,
                      p_YKF_l_w_ndwi, p_YKF_S2_w_ndwi,
                      nrow=1, align = "hv")
YKF_wet_wlabel<-ggdraw() +
  draw_label("Wet", x = 0, y = 0.5, hjust = 0.5, angle = 90, size = 14, fontface = "bold") +
  draw_plot(YKF_wet, x = 0.01 , y = 0, width = 0.98, height = 1) +
  theme(plot.margin = margin(0, 10, 0, 10))


### labels

NDWI_legend<-get_legend(ggplot() +
                          geom_raster(data = YKF_ndwi_wet_S2, aes(x = x, y = y, fill = NDWI)) +
                          scale_fill_gradientn(colors = c("#06402b","white", "blue"),
                                               values = scales::rescale(c(-1, 0, 1)),  # Ensure 0 is at the center
                                               limits = c(-1, 1)) +
                          theme( legend.direction = "horizontal",        
                                 legend.title.position = "top", 
                                 legend.title = element_text(hjust = 0.5),
                                 legend.box = "horizontal")) 
class_legend<-get_legend( ggplot() + 
                            geom_sf(data = YKF_wet_pickens, aes( fill = 'GLAD')) +
                            geom_sf(data = YKF_wet_pekel,  aes( fill = 'GSWO')) +
                            geom_sf(data = YKF_wet_S2, aes( fill = 'Sentinel-2'))+
                            scale_fill_manual(
                              name = "Data Source",
                              values = c("GLAD" = Pickenscolor, "GSWO" = Pekelcolor, "Sentinel-2" = S2color))+
                            guides(fill = guide_legend(
                              override.aes = list(
                                shape = 3, color= NA,
                                size = 3
                              )))+
                            theme( legend.direction = "horizontal",        
                                   legend.title.position = "top", 
                                   legend.title = element_text(hjust = 0.5),
                                   legend.box = "horizontal")) 

#### put everything together
YKF_both<-plot_grid(YKF_dry_wlabel,NULL, YKF_wet_wlabel, nrow=3, align='v', rel_heights = c(1,-0.5,1))
YKF_both_wlabel<-ggdraw() +
  draw_label("Yukon Flats", x = 0.98, y = 0.5, hjust = 0.5, angle = 270, size = 14, fontface = "bold") +
  draw_plot(YKF_both, x = 0.01 , y = 0, width = 0.98, height = 1) +
 theme(plot.margin = margin(0, 10, 0, 0))


legends_both<-plot_grid(class_legend,NDWI_legend,  nrow=1, align = 'h')
#YKF_finalplot<-plot_grid(YKF_both_wlabel, legends_both, nrow=2, rel_heights = c(1,0.2))

############    ###################
###         BOTH REGIONS 
############    ###################

bothregions<-plot_grid(AKCP_both_wlabel,NULL,YKF_both_wlabel, nrow=3, align='v',
                       rel_heights=c(0.75,-0.18,0.8))
finalplot<-plot_grid(bothregions,NULL, legends_both, nrow=3, rel_heights = c(1,-0.09,0.07))
ggsave('LandsatComparison_Figure4.jpg', finalplot,width=300, height = 240, units= 'mm', dpi = 500)

