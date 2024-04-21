#################
# Depth data preparation 
#################

## P0: Set wd 
# setwd("/home/erqu0001/Public_Eros/larval-sizes")

## P1: Load libraries 
library(tidyverse)
library(marmap)
library(ggpubr)

## Load data 
larvae <- readr::read_csv("data/clean/larval_size.csv")

## P2: Extract depth  
# Extract data from NOAA via marmap
depthStudyArea <- getNOAA.bathy(
  lon1 = 7, 
  lon2 = 13,
  lat1 = 55, 
  lat2 = 60, 
  resolution = 1
  )

# extract depths at the locations observed
depthObservations <- get.depth(depthStudyArea, x=larvae$lon, y=larvae$lat, locator=F)

# Scale the depth at observations
depthObservations <- depthObservations %>% 
  mutate(
    depth_positive = depth * (-1), 
    depth_scaled = (depth_positive-mean(depth_positive))/sd(depth_positive), 
  )

## P3: Wrangle the results
depthStudyArea <- as.xyz(depthStudyArea) %>% 
  dplyr::rename(
    lon = V1, 
    lat = V2, 
    depth = V3
  )

## P4: Plot the results  

# Import country data
country <- ne_countries(scale = "large", returnclass = "sf")

# Prepare a theme 
depth_theme <- theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10), 
    legend.key.height = unit(.5, 'cm'), 
    legend.key.width = unit(2, 'cm')
  )

# Plot the spatial distribution of depth in the study area
depthStudyArea_plot <- ggplot() + 
  geom_tile(
    data = depthStudyArea %>% filter(depth<=0), 
    aes(x = lon, y = lat, fill = depth)
    ) +
  geom_contour(
    data = depthStudyArea, 
    aes(x = lon, y = lat, z = depth),
    binwidth = 25, 
    color = "grey85", 
    size = 0.1
    ) +
  geom_sf(data = country) +
  coord_sf(
    xlim = c(7, 13), 
    ylim = c(56, 59)
    ) + 
  depth_theme

# Plot the spatial distribution of points and associate depth 
depthObservations_plot <- ggplot() + 
  geom_contour(
    data = depthStudyArea, 
    aes(x = lon, y = lat, z = depth),
    binwidth = 25, 
    color = "gray50", 
    size = .1
  ) +
  geom_point(
    data = cbind(larvae, depthObservations["depth"]), 
    aes(x = lon, y = lat, color = depth), 
    size = 1
  ) + 
  geom_sf(data = country) +
  coord_sf(
    xlim = c(7, 13), 
    ylim = c(56, 59)
  ) + 
  depth_theme 


# Gather the two plots together
depthOverview_plot <- ggarrange(
  depthStudyArea_plot, 
  depthObservations_plot,
  common.legend = TRUE, 
  legend="bottom"
)

# Save plot
ggsave(
  "depthOverview_plot.pdf",
  plot = depthOverview_plot,
  device = "pdf",
  path = "figures",
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300
)

# Save depth vector 
write.csv(depthObservations[,c("depth", "depth_scaled")], "data/covariates/depth/depthVector.csv", row.names = F)
