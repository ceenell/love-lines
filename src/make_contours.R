make_contours <- function(lat, long, buffer = 1000, zoom = 14, crs = 4326){
  pt <- data.frame(lat = lat, long = long)
  pt_sf <- pt %>% st_as_sf(coords = c('long','lat'), crs = crs)
  pt_sf_buff <- pt_sf %>%
    st_buffer(buffer)
  
  # create polygon from buffer
  pt_bbox <- pt_sf_buff %>% st_bbox 
  pt_bbox_sf <- pt_bbox %>% 
    st_as_sfc()
  
  # get elevation raster
  #pt_el <- get_elev_raster(pt_sf, zoom)
  pt_el_buff <- get_elev_raster(pt_sf_buff, zoom)
  
  # crop to bbox
  pt_el_crop <- crop(pt_el_buff, pt_bbox)
  
  # prep to ggplot
  pt_df <- as.data.frame(pt_el_buff, xy = TRUE)
  colnames(pt_df) <- c('x','y','z')
  pt_df_crop <- as.data.frame(pt_el_crop, xy = TRUE)
  colnames(pt_df_crop) <- c('x','y','z')
  
  return(list('pt_sf' = pt_sf, # orig pt as sf
              'rast' = pt_el_buff, # elevation raster 
              'rast_crop' = pt_el_crop, # elevation raster cropped to buffer
              'df_crop' = pt_df_crop,
              'df' = pt_df )) # for plotting with ggplot
}

theme_contour <- function(family = "Helsinki", size = 30){
  theme_void() +
    theme(
      legend.position = "none",
      text = element_text(family = family, size = size)
    ) 
}

prep_shading <- function(lat, long){
  
}