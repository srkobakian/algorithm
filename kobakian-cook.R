## ----setup, echo=FALSE, message=FALSE, warning=FALSE, comment = FALSE---------
library(knitcitations)
library(kableExtra)
library(RefManageR)
library(sp)
library(sf)
library(sugarbag)
library(absmapsdata)
library(tidyverse)
library(cartogram)
library(ggthemes)
library(cowplot)

knitr::opts_chunk$set(warning = FALSE, message = FALSE, eval = FALSE)
options("citation_format" = "pandoc")
BibOptions(check.entries = FALSE, style = "markdown")


invthm <- theme_map() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "white")
  )


# function to allocate colours to regions
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 <  0.74 ~ "#33809d",
    sir_p50 >= 0.74 & sir_p50 < 0.98 ~ "#aec6c7",
    sir_p50 >= 0.98 & sir_p50 < 1.05 ~ "#fff4bc",
    sir_p50 >= 1.05 & sir_p50 < 1.45 ~ "#ff9a64",
    sir_p50 >= 1.45 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}



## -----------------------------------------------------------------------------
states <- absmapsdata::state2011 %>% 
  rmapshaper::ms_simplify(keep = 0.25, keep_shapes = TRUE) %>% 
  filter(!state_name_2011 == "Other Territories")

sa2 <- absmapsdata::sa22011 %>% 
  filter(state_name_2011 == "New South Wales") %>% 
  filter(!st_is_empty(geometry))

# sa2 <- sa2 %>% rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)

SIR <- read_csv("data/SIR Downloadable Data.csv") %>% 
  filter(SA2_name %in% sa2$sa2_name_2011) %>% 
  dplyr::select(Cancer_name, SA2_name, Sex_name, p50) %>% 
  filter(Cancer_name == "Thyroid", Sex_name == "Females")
ERP <- read_csv("data/ERP.csv") %>%
  filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) %>% 
  dplyr::select(Region, Value)

# Alternative maps
# Join with sa2 sf object
sa2thyroid_ERP <- SIR %>% 
  left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) %>%
  left_join(., ERP %>% 
              dplyr::select(Region, 
              Population = Value), by = c("sa2_name_2011"= "Region")) %>% 
  filter(!st_is_empty(geometry))

sa2thyroid_ERP <- sa2thyroid_ERP %>% 
  filter(!is.na(Population)) %>% 
  mutate(SIR = map_chr(p50, aus_colours)) %>% 
  st_as_sf() 

aus_ggchoro <- ggplot(sa2thyroid_ERP) + 
  #geom_sf(fill = "lightgrey", data = states) + 
  geom_sf(aes(fill = SIR)) + 
  scale_fill_identity()


## ----alternatives-------------------------------------------------------------

###############################################################################

sa2thyroid_ERP <- st_transform(sa2thyroid_ERP, 3112)
sa2thyroid_ERP <- sa2thyroid_ERP %>% 
  mutate(sva = sqrt(as.numeric(Population)/as.numeric(albers_sqkm)))


###############################################################################
# Contiguous Cartograms

cont <- sa2thyroid_ERP %>% 
  mutate(Population = Population + 1) %>% 
  cartogram_cont(., weight = "Population", itermax = 20) %>%
  st_as_sf()

save(cont, file = "data/cont.rda")
load("data/cont.rda")

aus_ggcont <- ggplot(cont) + 
  geom_sf(aes(fill = SIR), colour = NA) + 
  scale_fill_identity() + 
  invthm + guides(fill=FALSE)
aus_ggcont

ggsave(filename = "figures/aus_ggcont.png", plot = aus_ggcont,
       device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)



## -----------------------------------------------------------------------------

###############################################################################
# Non - Contiguous Cartograms

ncont <- cartogram_ncont(sa2thyroid_ERP, k = 1/10,
                         weight = "Population") %>% st_as_sf()

save(ncont, file = "data/ncont.rda")
load("data/ncont.rda")

aus_ggncont <- ggplot(ncont) + 
  geom_sf(aes(fill = SIR), colour = NA) + 
  scale_fill_identity() + 
  invthm + guides(fill=FALSE)
aus_ggncont

ggsave(filename = "figures/aus_ggncont.png", plot = aus_ggncont,
       device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)



## -----------------------------------------------------------------------------

###############################################################################
# Non - Contiguous Dorling Cartograms

dorl <- sa2lung_ERP %>%
  mutate(pop = (Population/max(Population))*10) %>% 
  cartogram_dorling(., k = 0.01, weight = "pop", m_weight = 1) %>% st_as_sf()
d <- st_bbox(dorl)
aus_ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = SIR)) + 
  scale_fill_identity()+
  invthm + guides(fill = FALSE)
aus_ggdorl

ggsave(filename = "figures/aus_ggdorl.png", plot = aus_ggdorl,
       device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)



## ----full_hexmapcode----------------------------------------------------------
## Load data
tas_sa2 <- absmapsdata::sa22011 %>% 
  filter(state_name_2011 == "New South Wales")

## Create centroids set
centroids <- create_centroids(tas_sa2, "sa2_name_2011")

## Create hexagon grid
grid <- create_grid(centroids = centroids,
                    hex_size = 0.12,
                    buffer_dist = 1.2)

## Allocate polygon centroids to hexagon grid points
hex_allocated <- allocate(
  centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  ## same column used in create_centroids
  hex_size = 0.12,
  ## same size used in create_grid
  hex_filter = 10,
  use_neighbours = tas_sa2,
  focal_points = capital_cities %>%
    filter(points == "Hobart"),
  width = 35,
  verbose = FALSE
)

## Prepare to plot
fort_hex <- fortify_hexagon(data = hex_allocated,
                            sf_id = "sa2_name_2011",
                            hex_size = 0.12)

fort_tas <- tas_sa2 %>%
  fortify_sfc()

## Make a plot
library(ggplot2)
tas_hexmap <- ggplot() +
  geom_polygon(aes(
      x = long,
      y = lat,
      group = interaction(sa2_name_2011, polygon)
    ),
    fill = "white",
    colour = "lightgrey",
    data = fort_tas
  ) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = hex_id,
    fill = rownumber
  ),
  data = fort_hex) +
  scale_fill_distiller("", palette = "PRGn") +
  coord_equal() + theme_void()
tas_hexmap


## ----mapgrid------------------------------------------------------------------

###############################################################################

aus_grid <- gridExtra::grid.arrange(aus_ggchoro, aus_ggcont, aus_ggncont, aus_ggdorl, nrow = 2)

aus_grid
ggsave(filename = "figures/aus_grid.png", plot = aus_grid,
  device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)


## ----capital_cities-----------------------------------------------------------
data(capital_cities)


## ----cents--------------------------------------------------------------------
centroids <- create_centroids(shp_sf = tas_sa2, sf_id = "SA2_NAME16")


## ----grid---------------------------------------------------------------------
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)


## ----centroids, eval = FALSE--------------------------------------------------
#> hexmap_allocation <- allocate(
#>   centroids = centroids %>% select(SA2_NAME16, longitude, latitude),
#>   sf_id = "SA2_NAME16",
#>   hex_grid = grid,
#>   hex_size = 0.2, ## same size used in create_grid
#>   hex_filter = 10,
#>   width = 35,
#>   focal_points = capital_cities,
#>   verbose = TRUE)


## ----eval=FALSE---------------------------------------------------------------
#> install.packages("sugarbag")


## ----eval=FALSE---------------------------------------------------------------
#> devtools::install_github("srkobakian","sugarbag")


## -----------------------------------------------------------------------------
library(sugarbag)

