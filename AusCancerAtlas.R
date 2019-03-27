library(sugarbag)
library(absmapsdata)
library(tidyverse)


# Create centroids set
centroids <- create_centroids(sa22011, "sa2_5dig_2011")
# save(centroids, file = "data/sa22011/centroids.rda")

# Create hexagon location grid
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 5)
# save(grid, file = "data/sa22011/grid.rda")

# Allocate polygon centroids to hexagon grid points
allocated <- allocate(centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_5dig_2011",
  hex_size = 0.2, # same size used in create_grid
  hex_filter = 15,
  width = 30,
  focal_points = capital_cities,
  verbose = TRUE) 

#save(allocated, file = "data/sa22011/allocated.rda")

hex_allocated <- create_hexmap(sa22011, sf_id = "sa2_5dig_2011",
  focal_points = capital_cities,
  verbose = TRUE)

#save(hex_allocated, file = "data/sa22011/hex_allocated.rda")


# same column used in create_centroids
hexagons <- fortify_hexagon(data = allocated, sf_id = "sa2_5dig_2011", hex_size = 0.2)
polygons <- fortify_sfc(sa22011, keep = 0.1)

ggplot(hexagons) + geom_polygon(aes(x=long, y = lat, group = sa2_5dig_2011))