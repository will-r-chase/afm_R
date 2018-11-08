library(tidyverse)
library(rayshader)
library(rgl)
library(lattice)
library(abind)

##rayshader stuff
height_matrix %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

heatmap(height_matrix, Rowv=NA, Colv=NA, col = grey.colors(256))

levelplot(height_matrix, col.regions = grey.colors(256))

rayshader::plot_3d
AFM::displayIn3D

height_matrix <- read_test$maps$Height_Sensor_matrix

height_rot <- rotate(x = height_img, 0)
height_nanoscope <- colormap(height_rot, nanoscope_palette(256))
display(height_nanoscope, method = "raster")
height_color_matrix <- height_nanoscope@.Data

levelplot(height_matrix, col.regions = grey.colors(256))

rayshade <-
  height_matrix %>%
  sphere_shade(texture = "desert",progbar = FALSE) %>%
  add_shadow(ray_shade(height_matrix,zscale=0.8,maxsearch = 300,progbar = FALSE),0.7) %>%
  add_overlay(., rgb_array, alphacolor = "#000000", alphalayer = 0.3) %>%
  plot_3d(height_matrix, zscale=0.8,fov=0,theta=0,zoom=0.9,phi=45, windowsize = c(800,800), solid = TRUE)

raw_height <- read_test$scan_data$`Height_Sensor(nm)`
height_order <- tibble(raw = raw_height)
histogram(raw_height)
bins <- cut(raw_height, 256, labels = 1:256)
height_tibble <- 
  tibble(raw = raw_height, bins = as.integer(bins)) %>%
  arrange(bins)
colors_tibble <-
  tibble(hex = nanoscope_palette(256), bins = 1:256)
height_colors <- 
  left_join(height_tibble, colors_tibble, by = "bins") 

height_colors <- height_colors[match(raw_height, height_colors$raw), ]


#test_df <- tibble(raw = c(-400, -300, -450), bins = c(2, 3, 1), colors = c("#000000", "#6E2300", "#FAFAF5"))

hex_df <- tibble(hex = nanoscope_palette(256))

colors_df <- as.tibble(t(col2rgb(hex_df$hex))) %>%
  cbind(., hex_df) %>%
  distinct()

height_colors_rgb <- left_join(height_colors, colors_df, by = "hex")

red_mat <- matrix(height_colors_rgb$red, ncol = 512, nrow = 512, byrow = TRUE)
green_mat <- matrix(height_colors_rgb$green, ncol = 512, nrow = 512, byrow = TRUE)
blue_mat <- matrix(height_colors_rgb$blue, ncol = 512, nrow = 512, byrow = TRUE)

rgb_array <- array(c(red_mat, green_mat, blue_mat), dim = c(512, 512, 3))



theta = seq(0, 358, by=2)
for(i in 1:180) {
  height_matrix %>%
    sphere_shade(texture = "desert",progbar = FALSE) %>%
    add_shadow(ray_shade(height_matrix,zscale=1,maxsearch = 300,progbar = FALSE),0.7) %>%
    plot_3d(height_matrix,zscale=1,fov=0,theta=theta[i],zoom=0.9,phi=45, windowsize = c(800,800))
  rgl::snapshot3d(paste0("filename",i,".png"))
}

movie3d( spin3d(), duration=6, dir=getwd(), clean=FALSE)

