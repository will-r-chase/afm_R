library(AFM)
library(tidyverse)
library(rayshader)
library(data.table)

half_micron <- importFromNanoscope("500nm_1.txt")

fullfilename <- "500nm_1.txt"

filename = basename(fullfilename)
headerEndString <- "Height(nm)"
wholeFile <- fread(fullfilename, quote = "")
wholeFile <- unlist(wholeFile)
headerSizeWhich <- which(wholeFile == headerEndString) + 
  1
hdrs <- read.table(fullfilename, nrows = headerSizeWhich, 
                   skip = 2, comment.char = "", strip.white = TRUE, check.names = TRUE)
newhdrs <- sapply(hdrs, function(x) {
  x <- str_replace_all(x, pattern = "[^0-9a-zA-Z,.:]+", 
                       replacement = "")
})
unlistedNewHdrs = unlist(strsplit(newhdrs, ":", fixed = TRUE))
oneSamplesperline = which(tolower(unlistedNewHdrs) == "sampsline")
Samplesperline = as.numeric(unlistedNewHdrs[oneSamplesperline + 
                                              1][1])
oneScanSize = which(tolower(unlistedNewHdrs) == "scansize")
ScanSize = unlistedNewHdrs[oneScanSize + 1][1]
ScanSize = as.numeric(substr(ScanSize, 1, nchar(ScanSize) - 
                               2))
oneLines = which(tolower(unlistedNewHdrs) == "lines")
Lines = as.numeric(unlistedNewHdrs[oneLines + 1][1])
print(paste(ScanSize, Samplesperline, Lines))
nM <- read.table(fullfilename, skip = headerSizeWhich)
nM <- unlist(nM)
scanSizeFromZero <- ScanSize - 1
scanby <- ScanSize/Samplesperline
endScan <- ScanSize * (1 - 1/Samplesperline)
print(paste("imported ", filename, "...", sep = ""))
return(new("AFMImage", data = data.table(x = rep(seq(0, endScan, 
                                                     by = scanby), times = Lines), y = rep(seq(0, endScan, 
                                                                                               by = scanby), each = Samplesperline), h = nM), samplesperline = Samplesperline, 
           lines = Lines, hscansize = ScanSize, vscansize = ScanSize, 
           scansize = ScanSize, fullfilename = fullfilename))