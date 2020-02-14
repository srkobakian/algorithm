
###########################################################
## LIBRARIES
## sugarbag to create tesellated hexagon map
## tidyverse for tidy data manipulation, not necessary

library(sugarbag)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(sf)
library(readr)

###########################################################
## DATA
## load data files from sugarbag
data(capital_cities)

sa2 <- absmapsdata::sa22011  %>% 
  filter(state_name_2011 == "Tasmania")

# Join with cancer data from Australian Cancer Atlas
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 < 0.5 ~ "#33809d",
    sir_p50 >= 0.5 & sir_p50 < 0.75 ~ "#aec6c7",
    sir_p50 >= 0.75 & sir_p50 < 1.5 ~ "#fff4bc",
    sir_p50 >= 1.25 & sir_p50 < 1.5 ~ "#ff9a64",
    sir_p50 >= 1.5 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}

SIR <- read_csv("kobakian_sugarbag_jss/data/SIR Downloadable Data.csv") %>% 
  dplyr::select(Cancer_name, SA2_name, Sex_name, p50) %>% 
  filter(Cancer_name == "Lung", Sex_name == "Males") %>% 
  filter(SA2_name %in% sa2$sa2_name_2011) %>% 
  mutate(SIR = map_chr(p50, aus_colours)) 

# No data for "Mount Wellington", "Wilderness - East", "Wilderness - West" 
###########################################################
## CENTROIDS
## Use the create_centroids function to find polygon centroids:
centroids <- create_centroids(shp_sf = sa2, sf_id = "sa2_name_2011")

## Join the data from the original data set to the set of centroids by sf_id
centroids <- left_join(centroids,
  sf::st_drop_geometry(sa2), by = "sa2_name_2011")

###########################################################
## CENTROID PLOTS
## Fortify the underlying geographical areas for plotting
fort_sa2 <- fortify_sfc(sa2)

cents <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", size = 0.2, colour = "grey") + 
  geom_point(aes(x = longitude, y = latitude), shape=21, fill = "#b2df8a", data = centroids) + 
  theme_void() + coord_equal()

ggsave(file = "figures/1centroids.png", cents, width = 10, height = 5, units = "in", dpi = 300)


###########################################################
## INTERNAL ALLOCATE CODE

## create the bounding box from the set of polygon centroids:
bbox <- tibble::tibble(min = c(min(centroids$longitude, na.rm = TRUE), min(centroids$latitude, na.rm = TRUE)),
  max = c(max(centroids$longitude, na.rm = TRUE), max(centroids$latitude, na.rm = TRUE)))

# hexagon size calculation:
hex_size <- (bbox$max[1] - bbox$min[1])/(bbox$max[2] - bbox$min[2]) / 5

# buffer distance calculation (distance to extend beyond min and max centroid points)
buffer_dist <- max((bbox$max[1] - bbox$min[1]), (bbox$max[2] - bbox$min[2]))*0.3


###########################################################
## GRID CREATION
###########################################################

grid <- tibble::as_tibble(
  expand.grid(
    hex_long = seq(bbox$min[1] - buffer_dist, # minimum
      bbox$max[1] + buffer_dist, # maximum
      hex_size), # distance between hexagons
    hex_lat = seq(bbox$min[2] - buffer_dist, # minimum
      bbox$max[2] + buffer_dist, # maximum
      hex_size))) # distance between hexagons

grid1 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", size = 0.2, colour = "grey") +
  geom_point(aes(x = longitude, y = latitude), data = centroids, shape = 21, fill = "#b2df8a", colour = "black") + theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = grid, size = 0.75) + labs(tag = "a")


# Find every second latitude
shift_lat <- grid %>% dplyr::select(hex_lat) %>%
  dplyr::distinct() %>%
  dplyr::filter(dplyr::row_number() %% 2 == 1) %>% unlist()

# Shift the longitude of every second latitude to the right to make hex structure
grid <- grid %>%
  dplyr::mutate(hex_long = ifelse(hex_lat %in% shift_lat, hex_long,
    hex_long + (hex_size / 2))) %>%
  dplyr::mutate(id=1:NROW(.)) %>%
  dplyr::mutate(assigned=FALSE)

grid2 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", size = 0.2, colour = "grey") + 
     geom_point(aes(x = longitude, y = latitude), shape=21, fill = "#b2df8a", colour = "grey", data = centroids) + theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = grid, size = 0.75) + labs(tag = "b")

full_grid <- grid <- grid %>%
  mutate(hex_long_int = dense_rank(hex_long)-1,
    hex_lat_int = dense_rank(hex_lat)-1)

# Fortify the possible hexagon locations for plotting
fort_grid <- grid

g2 <- arrangeGrob(grid1, grid2, nrow = 1)
ggsave(file = "2grid.png", g2, width = 10, height = 5, units = "in", dpi = 300)


###########################################################
## ROLLING WINDOWS

# Amount of longitude columns and latitude rows
nlong <- length(unique(grid$hex_long))
nlat <- length(unique(grid$hex_lat))

# Find the closest row and column for each centroid
centroids <- centroids %>%
  mutate(long_int = round((longitude-min(grid$hex_long))/(max(grid$hex_long)-min(grid$hex_long))*nlong, 0),
    lat_int = round((latitude-min(grid$hex_lat))/(max(grid$hex_lat)-min(grid$hex_lat))*nlat, 0))

# Amount of longitude columns and latitude rows in each group
long_size = round(nlong/20,0)
lat_size = round(nlat/20,0)

## FIRST ROLLING WINDOW

# make a list of groups, these windows are defined manually
nlong_list <- purrr::map2(seq(1:nlong), long_size + seq(1:nlong), c)
nlat_list <- purrr::map2(seq(1:nlat), lat_size + seq(1:nlat), c)

nlong_table <- map2_dfr(nlong_list, seq(1:length(nlong_list)), .f = function(window = .x,  id = .y){
  left <- window[1]
  right <- window[2]
  top <- (id %% 4)*(-1)
  bottom <- (id %% 4)*(-1) -0.8
  tib <- tibble(
    window = c(id,id,id,id),
    order = seq(1,4),
    x = c(left, left, right, right),
    y = c(top, bottom, bottom, top))
  return(tib)
})


nlat_table <- map2_dfr(nlat_list, seq(1:length(nlat_list)), .f = function(window = .x,  id = .y){
  top <- window[2]
  bottom <- window[1]
  right <- (id %% 4)*(-1)
  left <- (id %% 4)*(-1) - 0.8
  tib <- tibble(
    window = c(id,id,id,id),
    order = seq(1,4),
    x = c(right, left, left, right),
    y = c(top, top, bottom, bottom))
  return(tib)
})



lat_window <- function(x, cents = centroids, maximum = nlat){
  max_int = min(x[2],maximum)
  
  cents_in <- filter(cents, between(lat_int, x[1], max_int))
  return(cents_in)
}

long_window <- function(x, cents = centroids, maximum = nlong){
  max_int = x[2]
  while (max_int > maximum){
    max_int = max_int - 1
  }
  
  cents_in <- filter(cents, between(long_int, x[1], max_int))
  return(cents_in)
}


# LATITUDE ROWS FILTER
# amount of latitude in sliding window
lat_windows <- purrr::map(.x = nlat_list, .f = lat_window)
# LONGITUDE COLS FILTER
long_windows <- purrr::map(.x = nlong_list, .f = long_window)

# find the min and max longitude for each latitude
range_rows <- purrr::map_dfr(.x = lat_windows,
  .f = function(x) {x %>%
      dplyr::summarise(
        long_min = ifelse(purrr::is_empty(long_int), NA, min(x$long_int)),
        long_max = ifelse(purrr::is_empty(long_int), NA, max(x$long_int))
      )}
)

# find the min and max longitude for each latitude
range_cols <- purrr::map_dfr(.x = long_windows, .f = function(x) { x %>%
    dplyr::summarise(
      lat_min = ifelse(purrr::is_empty(lat_int), NA, min(x$lat_int)),
      lat_max = ifelse(purrr::is_empty(lat_int), NA, max(x$lat_int))
    )}
)


## SECOND ROLLING WINDOW

# smooth the minimums
av_range_rows <- purrr::map_dfr(.x = nlat_list, .f = function(x, rows = range_rows) {
  rows[x[1]:min(x[2], NROW(rows)),] %>%
    dplyr::summarise(mean_long_min = mean(long_min, na.rm=T), mean_long_max = mean(long_max, na.rm=T))
}) %>%
  bind_cols(lat_id = c(seq(1:nlat) + lat_size), .)


# smooth the minimums
av_range_cols <- purrr::map_dfr(.x = nlong_list, .f = function(x, cols = range_cols) {
  cols[x[1]:min(x[2], NROW(cols)),] %>%
    dplyr::summarise(mean_lat_min = mean(lat_min, na.rm=T), mean_lat_max = mean(lat_max, na.rm=T))
}) %>%
  bind_cols(long_id = c(seq(1:nlong) + long_size + round(long_size/2)), .)


###########################################################

# APPLY A BUFFER
# change buffer to amount of hexagons (ints) either side
hex_buffer <- floor(buffer_dist/hex_size)

grid <- grid %>%
  left_join(., av_range_rows, by = c("hex_lat_int" = "lat_id")) %>%
  left_join(., av_range_cols, by = c("hex_long_int" = "long_id")) %>%
  rowwise() %>%
  mutate(long_buffer = ifelse(between(hex_long_int,mean_long_min - hex_buffer,
    mean_long_max + hex_buffer), "in", "out")) %>%
  mutate(lat_buffer = ifelse(between(hex_lat_int,mean_lat_min - hex_buffer,
    mean_lat_max + hex_buffer), "in", "out")) %>%
  filter(lat_buffer =="in" | long_buffer == "in")


grid3 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "white", size = 0.2, colour = "grey") +
  geom_point(aes(x = longitude, y = latitude), data = centroids, shape = 21, colour = "grey", fill = "#b2df8a") + theme_void() + coord_equal()  +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#c3d4e2", data = fort_grid, size = 0.5) + 
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = grid, size = 0.75) + labs(tag = "c")

ggsave(file = "3grid.png", g3, width = 5, height = 5, units = "in", dpi = 300)

###########################################################
## CLOSEST FOCAL POINT

# Find the closest focal point (capital city) for each centroid
centroids <- centroids %>% 
  tidyr::nest(data = c(longitude, latitude)) %>% 
  mutate(closest = map(data, closest_focal_point, focal_points = capital_cities %>% filter(points == "Hobart"))) %>% 
  tidyr::unnest(c(data, closest)) %>% 
  arrange(focal_distance)


###########################################################
## ALLOCATE


s_centroids <- split(centroids, centroids[["focal_distance"]])

# Set up allocation data frame
centroid_allocation <- NULL


hex_filter = 10
width = 35
# keep value to reset expanded distances
expand_dist <- hex_filter


# filter for only the available hex grid points
hex_grid <- grid %>% filter(!assigned)

filter_dist <- hex_filter*hex_size

# filter grid for avaiable points
centroid1 <- centroids %>% head(1)

flong <- centroid1$longitude
flat <- centroid1$latitude

hex_grid <- hex_grid %>% ungroup() %>%
  filter(flat - filter_dist < hex_lat & hex_lat < flat + filter_dist) %>%
  filter(flong - filter_dist < hex_long & hex_long < flong + filter_dist)


buff1 <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "white", size = 0.2, colour = "grey") + 
  geom_point(aes(x = longitude, y = latitude), data = centroids, shape = 21, colour = "grey", fill = "#b2df8a")  + 
  geom_point(aes(x = longitude, y = latitude), data = centroid1, colour = "orange") + theme_void() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#c3d4e2", data = fort_grid, size = 0.5) +  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = hex_grid, size = 1) +
  theme_void() + coord_equal() + labs(tag = "d")


hex_grid <- hex_grid %>%
  rowwise %>%
  mutate(
    hex_lat_c = hex_lat - flat,
    hex_long_c = hex_long - flong) %>%
  mutate(hyp = ((hex_lat_c^2) + (hex_long_c^2))^(1/2))


f_angle <- centroid1 %>%
  mutate(atan = atan2(
    latitude - focal_latitude, 
    longitude - focal_longitude),
    angle = (atan * 180 / pi),
    pangle = ifelse(angle < 0, angle + 360, angle)) %>% pull()


hex_grid <- hex_grid %>%
  # create circle of radius: hex_filter
  filter(hyp < filter_dist) %>%
  mutate(
    # geosphere takes a long time
    angle = f_angle,
    angle_plus = (angle + width) %% 360,
    angle_minus = (angle - width) %% 360,
    atan = atan2(hex_lat_c, hex_long_c),
    hex_angle = (atan * 180 / pi),
    hex_angle = ifelse(hex_angle < 0, hex_angle + 360, hex_angle))



buff2 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "white", size = 0.2, colour = "grey") + 
  geom_point(aes(x = longitude, y = latitude), data = centroids, shape = 21, colour = "grey", fill = "#b2df8a")  + 
  geom_point(aes(x = longitude, y = latitude), data = centroid1, colour = "orange") + theme_void() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#c3d4e2", data = fort_grid, size = 0.5) +  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = hex_grid, size = 1) +
  theme_void() + coord_equal() + labs(tag = "e")



g4 <- arrangeGrob(buff1, buff2, nrow = 1)
ggsave(file = "4grid.png", g4, width = 10, height = 5, units = "in", dpi = 300)


# Filter for angle within circle
if (hex_grid$angle_minus[1] < hex_grid$angle_plus[1]) {
  hex_grid <- hex_grid %>%
    # create slice of 60 degrees from centroid
    filter(angle_minus < hex_angle & hex_angle < angle_plus)
} else {
  hex_grid <- hex_grid %>%
    # create slice of 60 degrees from centroid
    filter(hex_angle < angle_plus | angle_minus > hex_angle)
}

buff3 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "white", size = 0.2, colour = "grey") +  
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "white", size = 0.2, colour = "grey") + 
  geom_point(aes(x = longitude, y = latitude), data = centroids, shape = 21, colour = "grey", fill = "#b2df8a")  + 
  geom_point(aes(x = focal_longitude, y = focal_latitude), data = centroid1, colour = "red") +
  geom_point(aes(x = longitude, y = latitude), data = centroid1, colour = "orange") +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#c3d4e2", data = fort_grid, size = 0.5) + 
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = hex_grid, size = 1) +
  theme_void() + coord_equal() + labs(tag = "f")


# Choose first available point
cent <- centroid1 %>% dplyr::rename(focal_point = points, focal_dist = focal_distance, focal_angle = angle)

# Filter should give one hex point
hex <- hex_grid %>% 
  ungroup %>% 
  filter(hyp == min(hyp)) %>%
  select(hex_long, hex_lat, hex_id = id)

# Update grid to show this centroid as assigned
hex_grid[which(hex_grid$id == hex$hex_id),]$assigned <- TRUE

# Add to table of allocated centroids
centroid_allocation <- bind_rows(centroid_allocation, dplyr::bind_cols(cent, hex)) %>% as_tibble()


hex_points_df <- centroid_allocation %>% 
  fortify_hexagon(hex_size = hex_size,
    sf_id = "sa2_name_2011")

# All filter steps
library(gridExtra)
library(grid)
arrangeGrob(
  grid1, grid2, 
  grid3, buff1, 
  buff2, buff3, ncol = 2) %>% 
ggsave(filename = "figures/filter.png", plot = ., width = 8, height = 12, units="in", dpi = 300)


# Create centroids set
centroids <- create_centroids(sa2, "sa2_name_2011")
# Create hexagon location grid
grid <- create_grid(centroids = centroids,
  hex_size = 0.2,
  buffer_dist = 1.2)
# Allocate polygon centroids to hexagon grid points
hex_allocated <- allocate(
  centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  # same column used in create_centroids
  hex_size = 0.2,
  # same size used in create_grid
  hex_filter = 10,
  use_neighbours = sa2,
  focal_points = capital_cities %>% filter(points == "Hobart"),
  width = 35,
  verbose = FALSE
)

h1 <- hex_allocated %>%
  fortify_hexagon(hex_size = 0.2, 
    sf_id = "sa2_name_2011") %>% 
  left_join(., sa2)


p1 <- fortify_sfc(sa2)
end_hex <- ggplot() +
  geom_polygon(data = p1, aes(x = long, lat, group = interaction(sa2_name_2011, polygon)), fill = "grey", size = 0.2, colour = "grey") +
  geom_polygon(aes(x = long, y = lat, group = sa2_name_2011), data = h1, colour = "grey", fill = "#b2df8a") + theme_void() + coord_equal()

g6 <- arrangeGrob(cents, end_hex, nrow = 1)
ggsave(file = "6allocate.png", g6, width = 10, height = 5, units = "in", dpi = 300)



# Join with cancer data from Australian Cancer Atlas
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 < 0.80 ~ "#33809d",
    sir_p50 >= 0.80 & sir_p50 < 0.9 ~ "#aec6c7",
    sir_p50 >= 0.9 & sir_p50 < 1.2 ~ "#fff4bc",
    sir_p50 >= 1.2 & sir_p50 < 1.5 ~ "#ff9a64",
    sir_p50 >= 1.5 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}


# Lung Cancer Example
library(readr)
SIR <- read_csv("kobakian_sugarbag_jss/data/SIR Downloadable Data.csv") %>% 
  dplyr::select(Cancer_name, SA2_name, Sex_name, p50) %>% 
  filter(Cancer_name == "Lung", Sex_name == "Males") %>% 
  filter(SA2_name %in% sa2$sa2_name_2011) %>% 
  mutate(SIR = map_chr(p50, aus_colours)) 

sa2_SIR <- left_join(fort_sa2, SIR, by = c("sa2_name_2011" = "SA2_name"))

sf_SIR <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, 
    group = interaction(sa2_name_2011, polygon), fill = SIR), data = sa2_SIR, size = 0.2, colour = "grey") + 
  theme_void() + coord_equal() +
  scale_fill_identity()
sf_SIR

h1_SIR <- left_join(h1, SIR, by = c("sa2_name_2011" = "SA2_name"))

hex_SIR <- ggplot() +
  geom_polygon(data = p1, aes(x = long, lat, group = interaction(sa2_name_2011, polygon)), fill = "white", size = 0.2, colour = "grey") +
  geom_polygon(aes(x = long, y = lat, group = sa2_name_2011, fill = SIR), data = h1_SIR, colour = "black") + theme_void() + coord_equal() +
  scale_fill_identity()


g7 <- arrangeGrob(sf_SIR, hex_SIR, nrow = 1)
ggsave(file = "7SIR.png", g7, width = 10, height = 5, units = "in", dpi = 300)

