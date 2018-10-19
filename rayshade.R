library(AFM)
library(tidyverse)
library(rayshader)
library(rgl)
library(lattice)

peakForce_matrix %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

heatmap(height_matrix, Rowv=NA, Colv=NA, col = grey.colors(256))

levelplot(height_matrix, col.regions = grey.colors(256))

rayshader::plot_3d
AFM::displayIn3D


