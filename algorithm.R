## ----setup, echo=FALSE, message=FALSE, warning=FALSE, comment = FALSE----
library(knitcitations)
library(RefManageR)
library(sf)
library(sugarbag)
library(kableExtra)
 
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
options("citation_format" = "pandoc")
BibOptions(check.entries = FALSE, style = "markdown")


## ----eval=FALSE-------------------------------------------------------
## install.packages("sugarbag")


## ----eval=FALSE-------------------------------------------------------
## devtools::install_github("srkobakian","sugarbag")


## ---------------------------------------------------------------------
library(sugarbag)


## ----full_hexmapcode, echo = TRUE, warning = FALSE, message = FALSE, eval=FALSE----
## ## Load data
## qld_sa2 <- absmapsdata::sa22011 %>%
##   filter(state_name_2011 == "Queensland")
## 
## ## Create centroids set
## centroids <- create_centroids(qld_sa2, "sa2_name_2011")
## 
## ## Create hexagon grid
## grid <- create_grid(centroids = centroids,
##                     hex_size = 0.12,
##                     buffer_dist = 1.2)
## 
## ## Allocate polygon centroids to hexagon grid points
## hex_allocated <- allocate(
##   centroids = centroids,
##   hex_grid = grid,
##   sf_id = "sa2_name_2011",
##   ## same column used in create_centroids
##   hex_size = 0.12,
##   ## same size used in create_grid
##   hex_filter = 10,
##   use_neighbours = qld_sa2,
##   focal_points = capital_cities %>%
##     filter(points == "Brisbane"),
##   width = 35,
##   verbose = FALSE
## )
## 
## ## Prepare to plot
## fort_hex <- fortify_hexagon(data = hex_allocated,
##                             sf_id = "sa2_name_2011",
##                             hex_size = 0.12)
## 
## fort_qld <- qld_sa2 %>%
##   fortify_sfc()
## 
## ## Make a plot
## library(ggplot2)
## qld_hexmap <- ggplot() +
##   geom_polygon(aes(
##       x = long,
##       y = lat,
##       group = interaction(sa2_name_2011, polygon)
##     ),
##     fill = "white",
##     colour = "lightgrey",
##     data = fort_qld
##   ) +
##   geom_polygon(aes(
##     x = long,
##     y = lat,
##     group = hex_id,
##     fill = rownumber
##   ),
##   data = fort_hex) +
##   scale_fill_distiller("", palette = "PRGn") +
##   coord_equal() + theme_void()
## qld_hexmap


## ----capital_cities---------------------------------------------------
data(capital_cities)


## ----cents------------------------------------------------------------
centroids <- create_centroids(shp_sf = tas_sa2, sf_id = "SA2_NAME16")


## ----grid-------------------------------------------------------------
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)


## ----centroids, eval = FALSE------------------------------------------
## hexmap_allocation <- allocate(
##   centroids = centroids %>% select(SA2_NAME16, longitude, latitude),
##   sf_id = "SA2_NAME16",
##   hex_grid = grid,
##   hex_size = 0.2, ## same size used in create_grid
##   hex_filter = 10,
##   width = 35,
##   focal_points = capital_cities,
##   verbose = TRUE)

