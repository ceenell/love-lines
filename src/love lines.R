# remembering places with lines -------------------------------------------

library(tidyverse)
library(terrainr)
library(elevatr)
library(sf)
library(raster)

# plot styling
library(MetBrewer)
library(scico)
library(ggfx)
library(extrafont)
library(geomtextpath)

# fix fonts on mac
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()
fonts()
loadfonts()


# site --------------------------------------------------------------------

vortex <- data.frame(lat = 34.8552, long = -111.7799)

# meadow/ridge SE of RMBL
em_will <- data.frame(lat = 38.880267, long = -106.770207)
ew_pt_sf <- em_will %>% st_as_sf(coords = c('long','lat'), crs = 4326)
ew_sf <- ew_pt_sf %>%
  st_buffer(1000)

# create polygon from buffer
ew_bbox <- ew_sf %>% st_bbox 
ew_bbox_sf <- ew_bbox %>% 
  st_as_sfc()

# get elevation raster
ew_el <- get_elev_raster(ew_sf, 14)

# crop to bbox
ew_el_crop <- crop(ew_el, ew_bbox)

# prep to ggplot
ew_df <- as.data.frame(ew_el_crop, xy = TRUE)
colnames(ew_df) <- c('x','y','z')

ew_df %>%
  ggplot()+
  geom_raster(aes(x, y, fill = z)) +
  geom_sf(data = ew_bbox_sf, 
          fill = NA,
          color = "white"
  ) +
  theme_void() +
  theme(
    legend.position = "none"
    ) +
  coord_sf() +
  #scale_fill_scico(
  #  palette = "devon",
  #  direction = -1
  #  )+
  scale_fill_gradientn(
    colors=met.brewer("Peru1")#, n = 30, type = "continuous"
    )


# contouring --------------------------------------------------------------

ew_df %>%
  ggplot(aes(x, y))+
  geom_raster(aes(fill = z)) +
  geom_contour(aes(z = z),
               bins = 100,
               color = "white",
               size = 0.51,
               alpha = 0.25) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  coord_sf() +
  scale_fill_gradientn(
    colors=met.brewer("Peru1")#, n = 30, type = "continuous"
  )

## with colored bands
n_bins <- 50
ew_df %>%
  ggplot()+
  geom_contour(aes(x, y, z = z, color = ..level..),
               bins = n_bins,
               #color = z,
               size = 0.35) +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helsinki", size = 30)
  ) +
  geom_sf_text(data=ew_pt_sf, label = "x", 
               family = "Helsinki", 
               size = 8) +
  geom_sf_text(data=ew_pt_sf, label = "Em     Will\n\n", 
               family = "Helsinki", 
               color = "black",
               size = 10) +
  geom_sf_text(data=ew_pt_sf, label = "and         \n\n\n\n", 
               family = "Helsinki", 
               size = 4) +
  geom_sf_text(data=ew_pt_sf, label = paste0(em_will$lat,', ', em_will$long, '\n\n'), 
               family = "Noto Sans", 
               size = 4) +
  coord_sf() +
  scale_color_gradientn(
    #colors=met.brewer("Peru1")
    colors = colorRampPalette(c("dodgerblue", "darkslateblue", "royalblue", "turquoise"))(10)
    #colors = colorRampPalette(c("orange", "darkmagenta", "turquoise"))(10)
  )

ggsave("out/em_will_blues.png", width = 200, height = 200, units = "mm", dpi = 300)

fonts()


# with the fxn ------------------------------------------------------------

source('src/make_contours.R')

ew <- make_contours(lat = em_will$lat, long = em_will$long)
ew <- make_contours(lat = 34.790039, long = -111.765866)

fonts()
family <- 'Marquee Moon'

ew$df %>%
  ggplot()+
  geom_contour(aes(x, y, z = z, color = ..level..),
               bins = n_bins,
               alpha=0.8,
               size = 0.45) +
  theme_contour() +
  geom_sf_text(data= ew$pt_sf, 
               label = "x", 
               angle = 0,
               family = "Helsinki", 
               color = "black",
               size = 12) +
  geom_sf_text(data=ew$pt_sf, label = "Will", 
               family = family, 
               hjust = 1.1,
               vjust = -1.2,
               color = "black",
               size = 30) +
  geom_sf_text(data=ew$pt_sf, label = "Em", 
               family = family, 
               hjust = 1.15,
               vjust = -0.1,
               color = "black",
               size = 30) +
  geom_sf_text(data=ew$pt_sf, label = "&", 
               vjust = -1.45,
               hjust = 3.75,
               family = "Monospace", 
               color = "black",
               size = 20) +
  geom_sf_text(data=ew$pt_sf, 
               label = paste0('    ', em_will$lat,', ', em_will$long), 
               family = "Helsinki", 
               hjust = 1.125,
               vjust = 1.2,
               color = "black",
               size = 4) +
  coord_sf() +
  theme(plot.background = element_rect(fill="white"))+
  scale_color_gradientn(
    #colors=rev(met.brewer("Navajo"))
    #colors = colorRampPalette(c("darkslateblue", "orangered", "gold3","turquoise"))(10)
    colors = colorRampPalette(c("#800080", '#BAB86C','cadetblue3','deepskyblue','goldenrod1','orangered'))(10)
    #colors = scico(n=10, palette = "bamako", direction = -1, end = 0.8)
  )

ggsave("out/em_will_light_greens.png", width = 200, height = 200, units = "mm", dpi = 300)

## cee anjali
ew <- make_contours(lat = 34.790039, long = -111.765866, buffer = 300)
ew<-curry
n_bins <- 35
ew$df %>%
  ggplot()+
  geom_contour(aes(x, y, z = z, color = ..level..),
               bins = n_bins,
               alpha=0.8,
               size = 0.45) +
  theme_contour() +
  #geom_sf_text(data= ew$pt_sf, 
  #             label = "p", 
  #             angle = 0,
  #             alpha = 0.5,
  #             family = "BonusHearts", 
  #             color = "orangered",
  #             size = 15) +
  geom_sf_text(data= ew$pt_sf, 
               label = "n", 
               angle = 0,
               alpha = 0.5,
               family = "MF Love Dings", 
               color = "#FF1493",
               size = 15) +
  geom_sf_text(data=ew$pt_sf, label = "Anjali", 
               family = family, 
               hjust = 1.2,
               vjust = 1.4,
               color = "black",
               size = 30) +
  geom_sf_text(data=ew$pt_sf, label = "Cee", 
               family = family, 
               hjust = 0,
               vjust = 1.4,
               color = "black",
               size = 30) +
  geom_sf_text(data=ew$pt_sf, label = "&", 
               vjust = 3,
               hjust = 1.1,
               family = "Monospace", 
               color = "black",
               size = 20) +
  geom_sf_text(data=ew$pt_sf, 
               label = paste0(' ', 34.790039,',\n', -111.765866), 
               family = "Helsinki", 
               hjust = -0.3,
               vjust = 0.4,
               color = "black",
               size = 4) +
  coord_sf() +
  theme(plot.background = element_rect(fill="white", color=NA))+
  scale_color_gradientn(
    #colors=rev(met.brewer("Navajo"))
    #colors = colorRampPalette(c("darkslateblue", "orangered", "gold3","turquoise"))(10)
    #colors = colorRampPalette(c("#800080","darkslateblue", '#BAB86C','deepskyblue','goldenrod1','orangered'))(10)
    colors = scico(n=10, palette = "lajolla", direction = 1, end = 0.9, begin = 0.2)
  )

ggsave("out/cee_anj_over.png", width = 200, height = 200, units = "mm", dpi = 300)


# ridge map ---------------------------------------------------------------

library(ggridges)

ew <- make_contours(lat = em_will$lat, long = em_will$long, buffer = 10000)
ew$rast_crop

bell <- make_contours(lat = vortex$lat, long = vortex$long, buffer = 5000)
ew <- make_contours(lat = 34.790039, long = -111.765866, buffer = 3000)
peru <- make_contours(lat = -12.112308, long = -77.038177, buffer = 3000)

## take every nth value for grouping
n <- 5
grps <- ew$df %>% arrange(y) %>% pull(y) %>% unique
grp_n <- grps[seq(1, length(grps), n)]

ew$df %>%
  filter(y %in% grp_n) %>%
  #transform(y = factor(y)) %>%
  ggplot()+
  geom_density_ridges_gradient(aes(x=x, y=y, 
                     group = y,
                     height = z,
                     fill = stat(x),
                     color = y
                     ),
                 stat="identity",
                 fill = NA,
                 scale = length(grp_n)
                 )+
  theme_classic()+
  theme(
        axis.text=element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks=element_blank(),
        legend.position = "none")+
  scale_color_gradientn(
    #colors=rev(met.brewer("Navajo"))
    #colors = colorRampPalette(c("darkslateblue", "orangered", "gold3","turquoise"))(10)
    colors = colorRampPalette(c("#800080", '#BAB86C','cadetblue3','deepskyblue','goldenrod1','orangered'))(10)
    #values = scico(n=length(grp_n), palette = "batlow", direction = 1)
  )+
  coord_cartesian(clip = "off") 
## need to add location to map - cannot go elevation gradient like this though, but spacing seems correct

ew$df %>%
  filter(y %in% grp_n) %>%
  transform(y = factor(y)) %>%
  ggplot()+
  geom_path(aes(x=x, y=z, color=y))+
  theme_void()+
  theme(legend.position="none",
        plot.background = element_rect(fill=NA, color=NA))+
  #facet_grid(~y)+
    scale_color_manual(
      #colors=rev(met.brewer("Navajo"))
      #colors = colorRampPalette(c("darkslateblue", "orangered", "gold3","turquoise"))(10)
      values = colorRampPalette(c("#800080", '#BAB86C','cadetblue3','deepskyblue','goldenrod1','orangered'))(length(grp_n))
      #values = scico(n=length(grp_n), palette = "batlow", direction = 1)
    )

ggsave("out/cee_anj_horizon_na.png", width = 600, height = 180, units = "mm", dpi = 300)


ew$df_crop %>%
  filter(y %in% grp_n) %>%
  mutate(height =z) %>%
  transform(y = factor(y)) %>%
  ggplot()+
  geom_point(aes(x=x, y=height, color=height, group = y),
             size = 0.3,
             alpha=0.5
             )+
  theme_void()+
  theme(legend.position="none",
        panel.spacing.y = unit(-0.1*(length(grp_n)), "lines"),
        strip.text = element_blank())+
  facet_grid(y~.)+
  #scale_color_scico(palette="batlow")
  scale_color_gradientn(
    #colors=rev(met.brewer("Navajo"))
    #colors = colorRampPalette(c("darkslateblue", "orangered", "gold3","turquoise"))(10)
    colors = colorRampPalette(c("#800080", '#BAB86C','cadetblue3','deepskyblue','goldenrod1','orangered'))(length(grp_n))
    #colors = scico(n=palette = "lajolla", direction = 1)
  )
  ggsave("out/cee_anj_flotsam.png", width = 600, height = 400, units = "mm", dpi = 300)
  


# peru --------------------------------------------------------------------
  
  peru <- make_contours(lat = -12.112308, long = -77.038177, buffer = 25000)
  n <- 5
  grps <- peru$df %>% arrange(y) %>% pull(y) %>% unique
  grp_n <- grps[seq(1, length(grps), n)]  
  
  peru$df %>%
    filter(y %in% grp_n) %>%
    mutate(height =z) %>%
    transform(y = factor(y)) %>%
    ggplot()+
    geom_point(aes(x=x, y=height, color=height, group = y),
               size = 0.3,
               alpha=0.5
    )+
    theme_void()+
    theme(legend.position="none",
          panel.spacing.y = unit(-0.1*(length(grp_n)), "lines"),
          strip.text = element_blank())+
    facet_grid(y~.)+
    #scale_color_scico(palette="batlow")
    scale_color_gradientn(
      #colors=rev(met.brewer("Navajo"))
      #colors = colorRampPalette(c("darkslateblue", "orangered", "gold3","turquoise"))(10)
      colors = colorRampPalette(c("#800080", '#BAB86C','cadetblue3','deepskyblue','goldenrod1','orangered'))(length(grp_n))
      #colors = scico(n=palette = "lajolla", direction = 1)
    )
  ggsave("out/peru_flotsam.png", width = 600, height = 400, units = "mm", dpi = 300)
  
  peru$df %>%
    filter(y %in% grp_n) %>%
    #transform(y = factor(y)) %>%
    ggplot()+
    geom_density_ridges_gradient(aes(x=x, y=y, 
                                     group = y,
                                     height = z,
                                     fill = stat(x),
                                     color = y
    ),
    stat="identity",
    fill = NA,
    scale = length(grp_n)
    )+
    theme_contour()+
    scale_color_gradientn(
      #colors=rev(met.brewer("Navajo"))
      #colors = colorRampPalette(c("darkslateblue", "orangered", "gold3","turquoise"))(10)
      colors = colorRampPalette(c("#800080", '#BAB86C','cadetblue3','deepskyblue','goldenrod1','orangered'))(10)
      #values = scico(n=length(grp_n), palette = "batlow", direction = 1)
    )+
    coord_cartesian(clip = "off") 
