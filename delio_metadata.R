library(stringr)
library(tools)
library(magrittr)

# path to files
setwd("~/Documents/projects/delio/anti-paysage_two_compositions/anti-paysage_II")

# execute Bash commands and store the output in vectors
fileTypes <- system("find . -type f -print0 | xargs -0 file", intern = TRUE)
fileTypes <- strsplit(fileTypes, split = ":")

fileSizes <- system("find . -type f -print0 | xargs -0 du -h", intern = TRUE)
fileSizes <- strsplit(fileSizes, split = "\t")

files <- system("find . -type f", intern = TRUE)

# load and subset exif metadata
# exifTool version 10.11
system("exiftool -r -csv * > ~/Documents/projects/delio/exif.csv")
Sys.sleep(2)

exifData <- read.csv("~/Documents/projects/delio/exif.csv", header = TRUE, stringsAsFactors = FALSE)

exifData <- exifData[ , c("SourceFile", "AvgBytesPerSec", "BitsPerSample", "Comment", "Duration", "NumChannels", "SampleRate", "Software")]

# create vectors for repeat data values
n = length(files)

creator <- rep("Thomas J. Delio", n)
src <- rep("Derived from Anti-Paysage by Thomas J. Delio. Score 1990. Recording 1995. Recording published on Neuma 450-90.", n)
date <- rep("ca. 2003-2007", n)
rights <- rep("Copyright Thomas J. Delio", n)
access <- rep("Open access.", n)
license <- rep("This work is licensed under a Creative Commons Attribution 4.0 International License.", n)

# create empty vectors (for types and sizes)
fTypes <- vector(mode = "character", length = n)
fSizes <- vector(mode = "character", length = n)

# populate the types and sizes vectors
for (i in 1:length(fileTypes)) {
  fTypes[i] <- str_trim(fileTypes[[i]][2], side = c("both"))
  fSizes[i] <- str_trim(fileSizes[[i]][1], side = c("both"))
}

# get file directories (for tags)
fileDirs <- dirname(files) %>%
  sub("^\\./", "", .) %>%
  gsub("_", " ", .)

# extract file names (for titles)
titles <- basename(files) %>%
  file_path_sans_ext %>%
  gsub("_", " ", .)

# convert file names to server locations (for files)
serverLocations <- paste0("http://rds.lib.umd.edu/delio/", basename(files))

# copy exif 'Comment' data into Software variable and annotate (for Software)
exifData$Software[exifData$Comment == "Made with ACID Pro 7.0"] <- "Made with ACID Pro 7.0 [Verbatim extract from file metatda. Originally stored in 'Comment' field]"

exifData$Software[exifData$Comment == "check out www.acidplanet.com!"] <- "check out www.acidplanet.com! [Verbatim extract from file metatda. Originally stored in 'Comment' field]"

# add annotation to Software variable (for Software)
exifData$Software[grepl("Forge", exifData$Software)] <- paste(exifData$Software[grepl("Forge", exifData$Software)], "[Verbatim extract from file metatda. Originally stored in 'Software' field]", sep = " ")

# drop comment column
exifData$Comment <- NULL

# build data frame
results <- data.frame(SourceFile = files, files = serverLocations, tags = fileDirs, titles = titles, creator = creator, src = src, date = date, rights = rights, access = access, license = license, types = fTypes, sizes = fSizes, stringsAsFactors = FALSE)

# drop any rows representing hidden files
results <- results[!(grepl("^\\.", (results$titles))), ]

# strip leading period from source file path
results$SourceFile <- sub("^\\.\\/", "", results$SourceFile)

# annotate the 'data' data types
results$types[file_ext(results$SourceFile) == "pkf"] <- ".sfk: Sound Forge data file"

results$types[file_ext(results$SourceFile) == "wav"] <- paste(".wav:", results$types[file_ext(results$SourceFile) == "wav"], sep = " ")

results$types[file_ext(results$SourceFile) == "acd"] <- ".acd: ACID data file"

results$types[file_ext(results$SourceFile) == "sfk"] <- ".sfk: Sound Forge data file"

results$types[file_ext(results$SourceFile) == "ana"] <- ".ana: ??? data file"

results$types[file_ext(results$SourceFile) == "grn"] <- ".grn: ??? data file"

# annotate the file sizes
results$sizes[file_ext(results$SourceFile) == "pkf"] <- paste(".pkf:", results$sizes[file_ext(results$SourceFile) == "pfk"], sep = " ")

results$sizes[file_ext(results$SourceFile) == "wav"] <- paste(".wav:", results$sizes[file_ext(results$SourceFile) == "wav"], sep = " ")

results$sizes[file_ext(results$SourceFile) == "acd"] <- paste(".acd:", results$sizes[file_ext(results$SourceFile) == "acd"], sep = " ")

results$sizes[file_ext(results$SourceFile) == "sfk"] <- paste(".sfk:", results$sizes[file_ext(results$SourceFile) == "sfk"], sep = " ")

results$sizes[file_ext(results$SourceFile) == "ana"] <- paste(".ana:", results$sizes[file_ext(results$SourceFile) == "ana"], sep = " ")

results$sizes[file_ext(results$SourceFile) == "grn"] <- paste(".grn:", results$sizes[file_ext(results$SourceFile) == "grn"], sep = " ")

# merge descriptive and technical metadata
results <- merge(results, exifData, by = "SourceFile", all.x = TRUE)

# resolve unique entities and collapse variant files, data types, and file sizes into single character values
for (title in unique(results$titles)) {
  len <- length(results$titles[results$titles == title])
  
  # files
  f <- vector(mode = "character", length = len)
  f <- results$files[results$titles == title]
  results$files[results$titles == title] <- paste(f, collapse = "|")
  
  # types
  t <- vector(mode = "character", length = len)
  t <- results$types[results$titles == title]
  results$types[results$titles == title] <- paste(t, collapse = "|")
  
  #sizes
  s <- vector(mode = "character", length = len)
  s <- results$sizes[results$titles == title]
  results$sizes[results$titles == title] <- paste(s, collapse = "|")
}

# remove duplicate entity rows
# problem is to remove only duplicates that are not .wav files
dupTitles <- unique(results$titles[duplicated(results$titles)])

redundant <- as.integer(row.names(results[(results$titles %in% dupTitles) & (!grepl("wav", results$SourceFile)), ]))

results <- results[-redundant, ]

# check that duplicates have been deleted from rows with duplicate titles
# for (title in dupTitles) {
#   cat("\n")
#   print(title)
#   print(results$SourceFile[results$titles == title])
# }

# drop SourceFile column
results$SourceFile <- NULL

# produce Omeka-compliant column names
colnames(results) <- c("{files}", "{tags}", "{dc}:{title}", "{dc}:{creator}", "{dc}:{source}", "{dc}:{date}", "{dc}:{rights}", "{dc}:{access}", "{dc}:{license}", "{dc}:{format}", "{sound}:{size}", "{sound}:{avgBytesPerSec}", "{sound}:{bitsPerSample}", "{sound}:{duration}", "{sound}:{channels}", "{sound}:{sampleRate}", "{sound}:{software}")

# write csv file
write.csv(results, file = "~/Documents/projects/delio/delio_metadata.csv", quote = TRUE, row.names = FALSE)

resultsExFiles <- results[ , -1]

write.csv(resultsExFiles, file = "~/Documents/projects/delio/delio_metadata_ex_files.csv", quote = TRUE, row.names = FALSE)

