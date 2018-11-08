library(tidyverse)
library(EBImage)
library(viridis)
library(png)

nanoscope_colors <- c("#000000", "#0A0000", "#140000", "#1E0000", "#280000", "#320000", "#3C0000", "#460000", "#500000", "#5A0400", "#641500", "#6E2300", "#783400", "#824300", "#8C5100", "#966100", "#A07102", "#AA801B", "#B49037", "#BE9F52", "#C8AD6C", "#D2BD87", "#DCCCA3", "#E6DCBF", "#F0ECDB", "#FAFAF5", "#FFFFFF")

nanoscope_colors2 <- c("#000000", "#000000", "#0A0000", "#140000", "#1E0000", "#280000", "#320000", "#3C0000", "#460000", "#500000", "#5A0400", "#641500", "#6E2300", "#783400", "#824300", "#8C5100", "#966100", "#A07102", "#AA801B", "#B49037", "#BE9F52", "#C8AD6C", "#D2BD87", "#DCCCA3", "#E6DCBF", "#F0ECDB", "#FAFAF5", "#FFFFFF", "#FFFFFF")

nanoscope_palette <- colorRampPalette(nanoscope_colors2)
nanoscope_palette(256)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

height_matrix <- read_test$maps$Height_Sensor_matrix
height_scaled <- height_matrix + abs(min(height_matrix))

height_scaled <- range01(height_matrix)

height_img <- Image(height_scaled)
height_rot <- rotate(x = height_img, 90)
display(height_rot, method = "raster")

height_nanoscope <- colormap(height_rot, nanoscope_palette(256))
display(height_nanoscope, method = "raster")

height_color_matrix <- height_nanoscope@.Data

pf_matrix <- read_test$maps$Peak_Force_Error_matrix
pf_scaled <- pf_matrix + abs(min(pf_matrix))

pf_image <- Image(pf_scaled)
display(pf_image, method = "raster", all = TRUE)

pf_rot <- rotate(x = pf_image, -90)

display(pf_rot, method = "raster")

viridis_pal <- viridis(256)

pf_viridis <- colormap(pf_rot, viridis_pal)
pf_nanoscope <- colormap(pf_rot, nanoscope_palette(256))
pf_grey <- colormap(pf_rot, grey.colors(256))

display(pf_viridis, method = "raster")
display(pf_nanoscope, method = "raster")
display(pf_grey, method = "raster")

fhi = matrix(1, nrow = 3, ncol = 3)
fhi[1, 1] = -8
fhi2 = matrix(c(0, 1, 0, 1, -4, 1, 0, 1, 0), nrow = 3, ncol = 3)
img_fhi = filter2(pf_rot, fhi)
EBImage::display(img_fhi, method = "raster")

hist(pf_grey)
eq_pf_grey <- equalize(pf_grey)
display(eq_pf_grey, method = "raster")

hist(pf_nanoscope)
eq_pf_nanoscope <- equalize(pf_nanoscope)
display(eq_pf_nanoscope, method = "raster")

hist(eq_pf_grey)

fhi = matrix(1, nrow = 3, ncol = 3)
fhi[2, 2] = -7
img_fhi2 = filter2(eq_pf_grey, fhi)
display(img_fhi2, method = "raster")


img_ff <- fft(pf_rot) #fftw2d


###################################################
###################################################
# FFT SHIFT
fftshift <- function(img_ff, dim = -1) {
  
  rows <- dim(img_ff)[1]    
  cols <- dim(img_ff)[2]    
  
  swap_up_down <- function(img_ff) {
    rows_half <- ceiling(rows/2)
    return(rbind(img_ff[((rows_half+1):rows), (1:cols)], img_ff[(1:rows_half), (1:cols)]))
  }
  
  swap_left_right <- function(img_ff) {
    cols_half <- ceiling(cols/2)
    return(cbind(img_ff[1:rows, ((cols_half+1):cols)], img_ff[1:rows, 1:cols_half]))
  }
  
  if (dim == -1) {
    img_ff <- swap_up_down(img_ff)
    return(swap_left_right(img_ff))
  }
  else if (dim == 1) {
    return(swap_up_down(img_ff))
  }
  else if (dim == 2) {
    return(swap_left_right(img_ff))
  }
  else {
    stop("Invalid dimension parameter")
  }
}

ifftshift <- function(img_ff, dim = -1) {
  
  rows <- dim(img_ff)[1]    
  cols <- dim(img_ff)[2]    
  
  swap_up_down <- function(img_ff) {
    rows_half <- floor(rows/2)
    return(rbind(img_ff[((rows_half+1):rows), (1:cols)], img_ff[(1:rows_half), (1:cols)]))
  }
  
  swap_left_right <- function(img_ff) {
    cols_half <- floor(cols/2)
    return(cbind(img_ff[1:rows, ((cols_half+1):cols)], img_ff[1:rows, 1:cols_half]))
  }
  
  if (dim == -1) {
    img_ff <- swap_left_right(img_ff)
    return(swap_up_down(img_ff))
  }
  else if (dim == 1) {
    return(swap_up_down(img_ff))
  }
  else if (dim == 2) {
    return(swap_left_right(img_ff))
  }
  else {
    stop("Invalid dimension parameter")
  }
}
###################################################
###################################################
# FFT SHIFT


# Magnitude and Phase
magntd <- sqrt(Re(img_ff)^2+Im(img_ff)^2)
phase  <- atan(Im(img_ff)/Re(img_ff))

img_fftsh <- fftshift(magntd)

EBImage::display(phase,title="FFT", method = "raster")



