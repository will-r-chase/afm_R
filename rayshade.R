library(tidyverse)
library(rayshader)
library(rgl)
library(lattice)

##rayshader stuff
peakForce_matrix %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

heatmap(height_scaled, Rowv=NA, Colv=NA, col = grey.colors(256))

levelplot(height_matrix, col.regions = grey.colors(256))

rayshader::plot_3d
AFM::displayIn3D

height_matrix <- read_test$maps$Height_Sensor_matrix

height_matrix %>%
  sphere_shade(texture = "desert",progbar = FALSE) %>%
  add_shadow(ray_shade(height_matrix,zscale=1,maxsearch = 300,progbar = FALSE),0.7) %>%
  plot_3d(height_matrix,zscale=1,fov=0,theta=135,zoom=0.9,phi=45, windowsize = c(800,800))

theta = seq(0, 358, by=2)
for(i in 1:180) {
  height_matrix %>%
    sphere_shade(texture = "desert",progbar = FALSE) %>%
    add_shadow(ray_shade(height_matrix,zscale=1,maxsearch = 300,progbar = FALSE),0.7) %>%
    plot_3d(height_matrix,zscale=1,fov=0,theta=theta[i],zoom=0.9,phi=45, windowsize = c(800,800))
  rgl::snapshot3d(paste0("filename",i,".png"))
}

movie3d( spin3d(), duration=6, dir=getwd(), clean=FALSE)

