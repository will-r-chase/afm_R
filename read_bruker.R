library(tidyverse)

"\Scan Size: 500 nm"
"\Samps/line: 512"
"\Lines: 512"
"\QNM scan line shift: 8.192"
"\Scan Rate: 0.497119"
"\Capture direction: Down"
"\Lift Height: 71.0466"
"\Sync Distance: 146.1"
"\Sync Distance New: 146.1"
"\Sync Distance QNM: 145.222"
"\Peak Force Engage Amplitude: 100"
"\Peak Force Engage Setpoint: 0.15"
"\Tip Radius: 1"
"\Cantilever Angle: 12"
"\Sample Poisson's Ratio: 0.3"
"\Tip Half Angle: 0.261799"
"\Line Direction: Retrace"
"\Plane fit: 0 0 0 5"
"\@2:Z scale: 24150.5 nm"
"\*File list end"

afm_text <- read_lines("500nm_1.txt")
afm_header <- afm_text[str_detect(afm_text, "\\\\")]
afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]

extract_numeric_parameter <- function(parameter){
  parameter_regex <- paste0("\\\\", parameter, ":")
  parameter_text <- afm_header[str_detect(afm_header, parameter_regex)][1]
  parameter_value <- readr::parse_number(parameter_text)
  return(parameter_value)
}

scan_size <- extract_numeric_parameter("Scan Size")
samps_line <- extract_numeric_parameter("Samps/line")
afm_lines <- extract_numeric_parameter("Lines")

afm_df <- afm_data %>%
  str_squish() %>%
  as.tibble() %>%
  slice(-1) %>%
  tidyr::separate(., col = value, 
                  into = c("Height_Sensor(nm)", "Peak_Force_Error(nN)", "DMTModulus(MPa)", "LogDMTModulus(log(Pa))", "Adhesion(nN)", "Deformation(nm)", "Dissipation(eV)", "Height(nm)"),
                  sep = " ")

#height_matrix <- matrix(as.numeric(afm_df$`Height_Sensor(nm)`), ncol = 512, nrow = 512, byrow = T)
#peakForce_matrix <- matrix(as.numeric(afm_df$`Peak_Force_Error(nN)`), ncol = 512, nrow = 512, byrow = T)

afm_experiment <- 
  
afm_matrix <- function(data, channel){
  
}