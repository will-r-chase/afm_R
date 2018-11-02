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


height_matrix %>%
  sphere_shade(texture = "desert",progbar = FALSE) %>%
  add_shadow(ray_shade(height_matrix,zscale=1,maxsearch = 300,progbar = FALSE),0.7) %>%
  plot_3d(height_matrix,zscale=1,fov=0,theta=135,zoom=0.9,phi=45, windowsize = c(800,800))
