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

extract_parameter <- function(header, parameter){
  parameter_regex <- paste0("\\\\", parameter, ":")
  parameter_text <- header[str_detect(header, parameter_regex)][1]
  tryCatch({readr::parse_number(parameter_text)}, 
           warning = function(w){str_extract(parameter_text, pattern = '(?<=: )(.*)(?=")')})
}

parameter_regex <- paste0("\\\\", "Line Direction", ":")
parameter_text <- afm_header[str_detect(afm_header, parameter_regex)][1]
str_extract(parameter_text, pattern = '(?<=: )(.*)(?=")')


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

afm_scan <- function(scan_points, scanSize, sampsPerLine, afmLines, ...){
  arguments <- list(...)
  maps <- Filter(is.matrix, arguments)
  opt_params <- arguments[!(names(arguments)%in%names(maps))]
  
  data <- list(
    params = list(scan_size = scanSize, samps_per_line = sampsPerLine, afm_lines = afmLines),
    scan_data = scan_points,
    opt_params = opt_params,
    maps = maps
  )
  
  class(data) <- "afm_scan"
  return(data)
} 

afm_read_bruker <- function(file, maps = "all", opt_params = list()){
  afm_text <- read_lines(file)
  afm_header <- afm_text[str_detect(afm_text, "\\\\")]
  afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]
  
  scanSize <- extract_parameter(afm_header, "Scan Size")
  sampsPerLine <- extract_parameter(afm_header, "Samps/line")
  afmLines <- extract_parameter(afm_header, "Lines")
  scan_points <- extract_scan_points(afm_data)
  
  if(maps == "all"){
    all_channels <- colnames(scan_points)
    map_names <- paste(str_extract(all_channels, pattern = ".+?(?=\\()"), "matrix", sep = "_")
    all_maps <- map(.x = all_channels, ~afm_matrix(scan_points1, sampsPerLine1, afmLines1, channel = .x))
    names(all_maps) <- map_names
  } else{
    all_channels <- colnames(scan_points)
    select_channels <- maps[maps%in%all_channels]
    map_names <- paste(str_extract(select_channels, pattern = ".+?(?=\\()"), "matrix", sep = "_")
    select_maps <- map(.x = select_channels, ~afm_matrix(scan_points1, sampsPerLine1, afmLines1, channel = .x))
    names(select_maps) <- map_names
  }
  
  ##here deal with opt_params
  
  scan_object <- afm_scan()
}

afm_read_bruker("500nm_1.txt")



#### test
afm_text <- read_lines("500nm_1.txt")
afm_header <- afm_text[str_detect(afm_text, "\\\\")]
afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]

scanSize1 <- extract_numeric_parameter(afm_header, "Scan Size")
sampsPerLine1 <- extract_numeric_parameter(afm_header, "Samps/line")
afmLines1 <- extract_numeric_parameter(afm_header, "Lines")

scan_points1 <- extract_scan_points(afm_data)
height_matrix <- afm_matrix(scan_points, sampsPerLine1, afmLines1, channel = "Height_Sensor(nm)")
peakforce_matrix <- afm_matrix(scan_points, sampsPerLine1, afmLines1, channel = "Peak_Force_Error(nN)")
modulus_matrix <- afm_matrix(scan_points, sampsPerLine1, afmLines1, channel = "DMTModulus(MPa)")

test_scan <- afm_scan(scan_points1, scanSize1, sampsPerLine1, afmLines1)

scanRate <- extract_numeric_parameter(afm_header, "Scan Rate")

test_scan2 <- afm_scan(scan_points1, scanSize1, sampsPerLine1, afmLines1, scan_rate = scanRate, height = height_matrix)

arguments <- list(scan_rate = scanRate, height = height_matrix)
print(arguments)
opt_params <- list(arguments[is.matrix==TRUE)])
maps <- list()
for(i in 1:length(arguments)){
  if(class(arguments[[i]])!="matrix"){
    opt_params[[i]] <- arguments[[i]]
    names(opt_params[[i]]) <- names(arguments[[i]])
  } else if(class(arguments[[i]])=="matrix"){
    maps[[i]] <- arguments[[i]]
  }
}

matrix_test <- map(arguments, ~if(class(.x)=="matrix"){
  arguments[.x]})

test<-map_if(arguments, is.matrix, "[")

map(arguments, 1)

extract_matrix <- function(x){
  matricies <- list()
  for(i in 1:length(x)){
    if(class(x[[i]])=="Numeric"){
      matricies[[i]]<-x[[i]]
    }
  }
  return(matricies)
}

class(arguments$scan_rate)
class(arguments$height)

all_channels <- colnames(scan_points)
map_names <- paste(str_extract(all_channels, pattern = ".+?(?=\\()"), "matrix", sep = "_")
maps_test <- map(.x = all_channels, ~afm_matrix(scan_points1, sampsPerLine1, afmLines1, channel = .x))
