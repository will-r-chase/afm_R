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

extract_numeric_parameter <- function(header, parameter){
  parameter_regex <- paste0("\\\\", parameter, ":")
  parameter_text <- header[str_detect(header, parameter_regex)][1]
  parameter_value <- readr::parse_number(parameter_text)
  return(parameter_value)
}

extract_scan_points <- function(data){
  afm_df <- data %>%
    str_squish() %>%
    as.tibble() %>%
    slice(-1) %>%
    tidyr::separate(., col = value, 
                    into = c("Height_Sensor(nm)", "Peak_Force_Error(nN)", "DMTModulus(MPa)", "LogDMTModulus(log(Pa))", "Adhesion(nN)", "Deformation(nm)", "Dissipation(eV)", "Height(nm)"),
                    sep = " ") %>%
    mutate_all(funs(as.numeric(.)))
}

afm_matrix <- function(data, samps_line, afm_lines, channel = ""){
  data %>%
    pull(channel) %>%
    matrix(., ncol = samps_line, nrow = afm_lines, byrow = TRUE)
}

afm_scan <- function(scan_points, height_matrix, scan_size, samps_per_line, afm_lines, ...){
  arguments <- list(...)
  print(arguments)
  lapply(arguments, function(x){
    if(class(x)!="martix"){
      opt_params <- list(x)
    }
  })
  
  data <- list(
    params = list(scan_size = scanSize, samps_per_line = sampsPerLine, afm_lines = afmLines),
    scan_data = scan_points,
    height_map = height_matrix
  )
  
  class(data) <- "afm_scan"
  return(data)
} 

afm_read_bruker <- function(file){
  afm_text <- read_lines(file)
  afm_header <- afm_text[str_detect(afm_text, "\\\\")]
  afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]
  
  scanSize <- extract_numeric_parameter(afm_header, "Scan Size")
  sampsPerLine <- extract_numeric_parameter(afm_header, "Samps/line")
  afmLines <- extract_numeric_parameter(afm_header, "Lines")
  
  scan_points <- extract_scan_points(afm_data)
  height_matrix <- afm_matrix(scan_points, sampsPerLine, afmLines, channel = "Height_Sensor(nm)")
  peakforce_matrix <- afm_matrix(scan_points, sampsPerLine, afmLines, channel = "Peak_Force_Error(nN)")
  modulus_matrix <- afm_matrix(scan_points, sampsPerLine, afmLines, channel = "DMTModulus(MPa)")
  
  scan_object <- afm_scan()
}

afm_read_bruker("500nm_1.txt")

afm_read_bruker <- function(file){
  afm_text <- read_lines(file)
  afm_header <- afm_text[str_detect(afm_text, "\\\\")]
  afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]
  return(afm_header)
}
