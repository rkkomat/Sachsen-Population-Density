library(rayshader)
library(rayrender)
library(sf)
library(tigris)
library(rgl)
library(sp)
library(raster)
library(tigris)
library(tidyverse)
library(stars)
library(MetBrewer)
library(colorspace)
library(rnaturalearth)
library(dplyr)
library(devtools)
library(usethis)
library(eurostat)

remotes::install_github("tylermorganwall/rayrender")
remotes::install_github("tylermorganwall/rayshader")


map <- "DEU"
data <- st_read("data/german_population/kontur_population_DE_20220630.gpkg")

st <- get_eurostat_geospatial(resolution = "20", nuts_level = 1, year = 2021)

sachsen <- st %>%
  filter(NUTS_NAME == "Sachsen") %>%
  st_transform(crs = st_crs(data))

sachsen %>%
  ggplot() +
  geom_sf()

state_sachsen <- st_intersection(data, sachsen)

bb <- st_bbox(state_sachsen)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(data))

sachsen %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left, color = "green") +
  geom_sf(data = bottom_right, color = "red")


width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

size <- 4000

# convert to raster so that we can convert to matrix

final_raster <- st_rasterize(state_sachsen, 
                             nx = floor(size* as.numeric(w_ratio)),
                             ny = floor(size*as.numeric(h_ratio)))




# convert to matrix

matrix_final <- matrix(final_raster$population, 
                       nrow = floor(size* w_ratio),
                       ncol = floor(size*h_ratio))



# color pallete with metbrewer and colorspace packages
color_map <- met.brewer('Tam')
swatchplot(color_map)



texture <- grDevices::colorRampPalette(color_map, bias=2.5)(256)
swatchplot(texture)



# plot that map on 3d with rgl object 

rgl:: rgl.close()           # to close the rgl window after test render 




matrix_final %>%  
  height_shade(texture = texture) %>% 
  plot_3d(heightmap = matrix_final,
          zscale = 100/4,
          solid = FALSE,
          shadowdepth = 0)


render_camera(theta = -15, phi = 45, zoom = .8)


# test plot 

# render_highquality(filename = 'images/test_plot.png',
#                    interactive = FALSE,
#                    lightdirection = 280,
#                    lightaltitude = c(20, 80),
#                    lightcolor = c(color_map[2], "white"),
#                    lightintensity = c(600, 100))


# choose a path for the final render image
outfile <- "images/finalplot1.png"


{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c(color_map[1], "white"),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}




