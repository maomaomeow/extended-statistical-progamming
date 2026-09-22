# Group35 Members: Iris YAN, Ruijing Li, Lufan Chen
# Contributions:

music_raw <- readLines("palestrina.txt")    # Read file: palestrina.txt
head(music_raw, 20)    # View the first 20 lines of the file

# Data pre-processing
preprocess <- function(music){
  # Remove the first three lines containing details about the composer and the data source.
  music_clean <- music[-(1:3)]
  
  # Remove the lines begin with "#"
  retain_line <- !startsWith(music_clean, "#")    # Search for lines that do not begin with "#"
  music_clean <- music_clean[retain_line]    # Retain the lines that do not begin with "#"
  
  # Indicate the end of a piece
  end_line <- which(music_clean == "") - 1    # Find the end line of each piece
  music_clean[end_line] <- paste0(music_clean[end_line], " ||")    # Add double bar-line at the end of each piece
  
  # Indicate the end of a measure
  normal_line <- -c(end_line, end_line + 1)    # Find the normal lines
  music_clean[normal_line] <- paste0(music_clean[normal_line], " |")    # Add bar-line at the end of each measure
  
  # Split each measure into individual notes
  music_clean <- unlist(strsplit(music_clean, " "))
  
  return(music_clean)
}

music_clean <- preprocess(music_raw)    # Process raw data
tail(music_clean, n = 10)    # View the last 10 entries after processing
length(music_clean)    # View the length of the returned vector
