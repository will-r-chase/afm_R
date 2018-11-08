library(tidyverse)

#####example of header parameters#####
# "\Scan Size: 500 nm"
# "\Samps/line: 512"
# "\Lines: 512"
# "\QNM scan line shift: 8.192"
# "\Scan Rate: 0.497119"
# "\Capture direction: Down"
# "\Lift Height: 71.0466"
# "\Sync Distance: 146.1"
# "\Sync Distance New: 146.1"
# "\Sync Distance QNM: 145.222"
# "\Peak Force Engage Amplitude: 100"
# "\Peak Force Engage Setpoint: 0.15"
# "\Tip Radius: 1"
# "\Cantilever Angle: 12"
# "\Sample Poisson's Ratio: 0.3"
# "\Tip Half Angle: 0.261799"
# "\Line Direction: Retrace"
# "\Plane fit: 0 0 0 5"
# "\@2:Z scale: 24150.5 nm"

#extract parameters from the header
#parameter must be character vector that exactly matches the description in the header ie. "Tip Radius"
#if no match is found in the header, the parameter value is returned directly
#this allows users to store values that may not be in the header but are associated with the experiment
#such as "cell number" "treatment" etc.
afm_extract_parameter <- function(header = NULL, parameter = NULL){
  parameter_regex <- paste0("\\\\", parameter, ":")
  parameter_text <- header[str_detect(header, parameter_regex)][1]
  if (is.na(parameter_text)) {
    warning(paste(parameter, "was not found in the header, returning", parameter, "directly"))
    return(parameter)
  } else {
  tryCatch({readr::parse_number(parameter_text)}, 
           warning = function(w){str_extract(parameter_text, pattern = '(?<=: )(.*)(?=")')})
  }
}

#reads the scan data into a dataframe
#add later: automatic naming of columns
#add later: safety to check that all lines are complete
afm_extract_scan_points <- function(data){
  assert_that(is.character(data))
  
  afm_df <- data %>%
    str_squish() %>%
    as.tibble() 
  
  names <- unlist(strsplit(x = as.character(afm_df[1, ]), split = " "))
  assert_that(is.character(names))
  
  afm_df %>%
    slice(-1) %>%
    {
      tryCatch(
        {tidyr::separate(., col = value, into = names, sep = " ", convert = TRUE)},
        warning = function(w) {stop("The rows of your scan data had an unequal 
                                    number of columns. Check that all lines of 
                                    your scan data are complete.")}
      )    
    }
}

#formats a data channel as a matrix for visualization
#provide channel argument as character vector that matches the colname in scan_points exactly
afm_matrix <- function(data, samps_line, afm_lines, channel = NULL){
  assert_that(!is.null(channel))
  
  data %>%
    pull(channel) %>%
    matrix(., ncol = samps_line, nrow = afm_lines, byrow = TRUE)
}

#takes required inputs (scan size, scan points, samps per line, lines)
#also takes optional params and maps (image matrices)
#formats as afm_scan object
afm_scan <- function(scan_points = NULL, scanSize = NULL, sampsPerLine = NULL, afmLines = NULL, maps = list(), optional_params = list()){
  assert_that(!is.null(scan_points), !is.null(scanSize), !is.null(sampsPerLine), !is.null(afmLines))
  assert_that(is.numeric(scanSize), is.numeric(sampsPerLine), is.numeric(afmLines))
  
  if(is.tibble(scan_points) == FALSE & is.data.frame(scan_points) == FALSE) {
    stop("scan_points must be a tibble or dataframe")
  }
  
  data <- list(
    params = list(scan_size = scanSize, samps_per_line = sampsPerLine, afm_lines = afmLines),
    scan_data = scan_points,
    opt_params = optional_params,
    maps = maps
  )
  
  class(data) <- "afm_scan"
  return(data)
} 

afm_scan(scan_points = scan_points1, scanSize = scanSize1, sampsPerLine = sampsPerLine1, afmLines = afmLines1)

#reads a bruker text file that has a header and scan data from any number of channels
afm_read_bruker <- function(file = NULL, maps = "all", opt_params = list(), scan_size = "Scan Size", samps_per_line = "Samps/line", afm_lines = "Lines"){
  assert_that(!is.null(file))
  
  afm_text <- read_lines(file)
  afm_header <- afm_text[str_detect(afm_text, "\\\\")]
  afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]
  
  assert_that(length(afm_header) > 0, msg = "No header found, your file must have a header")
  assert_that(length(afm_data) > 0, msg = "No scan data found, your file must have scan data")
  
  scanSize <- afm_extract_parameter(afm_header, scan_size)
  assert_that(is.numeric(scanSize), msg = "scan_size is not numeric, 
                                          check that it is present in 
                                          the header and that you correctly 
                                          specified the name of the parameter")
  sampsPerLine <- afm_extract_parameter(afm_header, samps_per_line)
  assert_that(is.numeric(sampsPerLine), msg = "samps_per_line is not numeric, 
                                          check that it is present in 
                                          the header and that you correctly 
                                          specified the name of the parameter")
  afmLines <- afm_extract_parameter(afm_header, afm_lines)
  assert_that(is.numeric(afmLines), msg = "afm_lines is not numeric, 
                                          check that it is present in 
                                          the header and that you correctly 
                                          specified the name of the parameter")
  scan_points <- afm_extract_scan_points(afm_data)
  
  if(maps == "all"){
    all_channels <- colnames(scan_points)
    map_names <- paste(str_extract(all_channels, pattern = ".+?(?=\\()"), "matrix", sep = "_")
    afm_maps <- map(.x = all_channels, ~afm_matrix(scan_points, sampsPerLine, afmLines, channel = .x))
    names(afm_maps) <- map_names
  } else{
    all_channels <- colnames(scan_points)
    select_channels <- maps[maps%in%all_channels]
    map_names <- paste(str_extract(select_channels, pattern = ".+?(?=\\()"), "matrix", sep = "_")
    afm_maps <- map(.x = select_channels, ~afm_matrix(scan_points, sampsPerLine, afmLines, channel = .x))
    names(afm_maps) <- map_names
  }
  
  optional_params <- purrr::map(opt_params, ~afm_extract_parameter(afm_header, .x))
  
  afm_scan(scan_points, scanSize, sampsPerLine, afmLines, maps = afm_maps, optional_params)
}

read_test<-afm_read_bruker("500nm_1.txt", c("Height_Sensor(nm)", "Peak_Force_Error(nN)"), list(scan_rate = "Scan Rate", cap_dir = "Capture direction", num = 4, cell = "cell1"))
two_um_scan <- afm_read_bruker("2um_1.txt")

#### test
afm_text <- read_lines("500nm_1.txt")
afm_header <- afm_text[str_detect(afm_text, "\\\\")]
afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]

afm_text_bad <- read_lines("500nm_1_bad.txt")
afm_header_bad <- afm_text_bad[str_detect(afm_text_bad, "\\\\")]
afm_data_bad <- afm_text_bad[((2*length(afm_header_bad))+1):length(afm_text_bad)]

scanSize1 <- extract_parameter(afm_header, "Scan Size")
sampsPerLine1 <- extract_parameter(afm_header, "Samps/line")
afmLines1 <- extract_parameter(afm_header, "Lines")

scan_points1 <- extract_scan_points(afm_data)
height_matrix <- afm_matrix(scan_points1, sampsPerLine1, afmLines1, channel = "Height_Sensor(nm)")
peakforce_matrix <- afm_matrix(scan_points1, sampsPerLine1, afmLines1, channel = "Peak_Force_Error(nN)")
modulus_matrix <- afm_matrix(scan_points1, sampsPerLine1, afmLines1, channel = "DMTModulus(MPa)")

test_scan <- afm_scan(scan_points1, scanSize1, sampsPerLine1, afmLines1)

scanRate <- extract_numeric_parameter(afm_header, "Scan Rate")

test_scan2 <- afm_scan(scan_points1, scanSize1, sampsPerLine1, afmLines1, scan_rate = scanRate, height = height_matrix)

arguments <- list(scan_rate = scanRate, height = height_matrix)
print(arguments)

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
