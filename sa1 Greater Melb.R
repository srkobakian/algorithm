
# create a hexagon map
library(sugarbag)
library(sf)
library(absmapsdata)
library(ggplot2)

## sa1
## areas from absmapsdata package
sa1 <- absmapsdata::sa12011 %>% 
  filter(state_name_2011 != "Other Territories") %>% 
  filter(gcc_name_2011 == "Greater Melbourne") %>% 
  st_transform(., crs = "+proj=longlat +datum=WGS84")

# Create centroids set
centroids <- create_centroids(sa1, "sa1_7dig_2011")

# choropleth map display
ggplot(sa1) + 
  geom_sf(aes(fill = albers_sqkm)) +
  geom_point(aes(x = longitude, y = latitude), 
             colour = "grey", size = 0.5, data = centroids) +
  scale_fill_distiller(type = "seq", palette = "BuGn",
                       direction = 1, na.value = "light grey") + 
  theme_void() +
  coord_sf(crs = "+proj=longlat +datum=WGS84") +
  theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))


# Create hexagon location grid
grid <- create_grid(centroids = centroids, hex_size = 0.02, buffer_dist = 1)

# Allocate polygon centroids to hexagon grid points
sa1_melb <- allocate(centroids = centroids,
                          hex_grid = grid,
                          sf_id = "sa1_7dig_2011",
                          hex_size = 0.02, # same size used in create_grid
                          hex_filter = 10,
                          focal_points = capital_cities,
                          width = 30, verbose = TRUE) # same column used in create_centroids

save(hex_allocated, file = "hex_allocated.rda")
load("hex_allocated.rda")






ggsave(filename = "figures/aus_sa1.png", plot = sa1_map,
       device = "png", bg = "transparent", dpi = 300,  width = 10, height = 8)

