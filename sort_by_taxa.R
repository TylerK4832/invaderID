# Tyler Kempton
# 7/1/2020

# load necessary packages
library(filesstrings)
library(tidyverse)
library(rdrop2)
library(httpuv)
library(magick)
library(shiny)

# define necessary functions
# actually i don't even think this is used but i'll leave it in here anyways
appendVector <- function(vec, value) {
  vec <- c(vec, value)
  return(vec)
}

# define necessary lists/ vectors/ strings
morphoVecList <- list(algVec, anemVec, barnVec, bivVec, brbryoVec, coltuniVec, 
                      enbryoVec, nothingVec, soltuniVec, spngVec, tubewmVec)

morphoNameVec <- c("Alg", "Anem", "Barn", "Biv", "BrBryo", "ColTuni",
                   "EnBryo", "Nothing", "SolTuni", "Spng", "TubeWm", 
                   "NONE")

folderToSort <- "Individual Tiles from 2018 Launch - Overlayed"

# collectors to import data
int <- col_integer()
fac <- col_factor(levels = morphoNameVec)
chr <- col_character()

# read data from csv
dataCSV <- read_csv("New4Agreement_FixedIssue_Pulled13July2020.csv",
                    skip = 1,
                    col_names = c("SheetID", "SubjectID", "Alg", "Anem", "Barn",
                                  "Biv", "BrBryo", "ColTuni", "EnBryo", 
                                  "Nothing", "SolTuni", "Spng", "TubeWm", 
                                  "NoIDs", "TotalIDs", "zname", "PlateID", 
                                  "FinalIDs1", "FinalIDs2", "FinalIDs3"),
                    col_types = list(int, int, int, int, int, int, int, int, 
                                     int, int, int, int, int, int, int, chr, 
                                     chr, fac, fac, fac))

# restrict dataset to only necessary columns/variables
finalIDs <- dataCSV[c(2, 16, 17, 18, 19, 20)]

# create data frame with tile entries
subjects <- finalIDs %>% 
  group_by(SubjectID) %>% 
  summarize() %>% 
  arrange()

# initialize empty vectors to hold subject IDs of corresponding identifications
algVec <- c()
anemVec <- c()
barnVec <- c()
bivVec <- c()
brbryoVec <- c()
coltuniVec <- c()
enbryoVec <- c()
nothingVec <- c()
soltuniVec <- c()
spngVec <- c()
tubewmVec <- c()

# sort identifications into vectors (above) based on FinalID1, FinalID2, FinalID3
# very inefficient, needs improvement
# i will attempt to shorten the code by using functions
# i hate this
for (subject in subjects$SubjectID) {
  currentRow <- finalIDs %>% 
    filter(SubjectID == subject)
    morpho <- as.character(currentRow$FinalIDs1)
  if (morpho != "NONE") {
    if (morpho == "Alg") {
      algVec <- c(algVec, subject)
    }
    else if (morpho == "Anem") {
      anemVec <- c(anemVec, subject)
    }
    else if (morpho == "Barn") {
      barnVec <- c(barnVec, subject)
    }
    else if (morpho == "Biv") {
      bivVec <- c(bivVec, subject)
    }
    else if (morpho == "BrBryo") {
      brbryoVec <- c(brbryoVec, subject)
    }
    else if (morpho == "ColTuni") {
      coltuniVec <- c(coltuniVec, subject)
    }
    else if (morpho == "EnBryo") {
      enbryoVec <- c(enbryoVec, subject)
    }
    else if (morpho == "Nothing") {
      nothingVec <- c(nothingVec, subject)
    }
    else if (morpho == "SolTuni") {
      soltuniVec <- c(soltuniVec, subject)
    }
    else if (morpho == "Spng") {
      spngVec <- c(spngVec, subject)
    }
    else if (morpho == "TubeWm") {
      tubewmVec <- c(tubewmVec, subject)
    }
  }
  morpho <- as.character(currentRow$FinalIDs2)
  if (morpho != "NONE") {
    if (morpho == "Alg") {
      algVec <- c(algVec, subject)
    }
    else if (morpho == "Anem") {
      anemVec <- c(anemVec, subject)
    }
    else if (morpho == "Barn") {
      barnVec <- c(barnVec, subject)
    }
    else if (morpho == "Biv") {
      bivVec <- c(bivVec, subject)
    }
    else if (morpho == "BrBryo") {
      brbryoVec <- c(brbryoVec, subject)
    }
    else if (morpho == "ColTuni") {
      coltuniVec <- c(coltuniVec, subject)
    }
    else if (morpho == "EnBryo") {
      enbryoVec <- c(enbryoVec, subject)
    }
    else if (morpho == "Nothing") {
      nothingVec <- c(nothingVec, subject)
    }
    else if (morpho == "SolTuni") {
      soltuniVec <- c(soltuniVec, subject)
    }
    else if (morpho == "Spng") {
      spngVec <- c(spngVec, subject)
    }
    else if (morpho == "TubeWm") {
      tubewmVec <- c(tubewmVec, subject)
    }
  }
  morpho <- as.character(currentRow$FinalIDs3)
  if (morpho != "NONE") {
    if (morpho == "Alg") {
      algVec <- c(algVec, subject)
    }
    else if (morpho == "Anem") {
      anemVec <- c(anemVec, subject)
    }
    else if (morpho == "Barn") {
      barnVec <- c(barnVec, subject)
    }
    else if (morpho == "Biv") {
      bivVec <- c(bivVec, subject)
    }
    else if (morpho == "BrBryo") {
      brbryoVec <- c(brbryoVec, subject)
    }
    else if (morpho == "ColTuni") {
      coltuniVec <- c(coltuniVec, subject)
    }
    else if (morpho == "EnBryo") {
      enbryoVec <- c(enbryoVec, subject)
    }
    else if (morpho == "Nothing") {
      nothingVec <- c(nothingVec, subject)
    }
    else if (morpho == "SolTuni") {
      soltuniVec <- c(soltuniVec, subject)
    }
    else if (morpho == "Spng") {
      spngVec <- c(spngVec, subject)
    }
    else if (morpho == "TubeWm") {
      tubewmVec <- c(tubewmVec, subject)
    }
  }
}

# create list of all photo names
photosList <- list.files(folderToSort)

# create and add photos to corresponding folder
# idek
dir.create("Sorted Images")
for (i in 1:11) {
  directory <- paste("Sorted Images/", morphoNameVec[i], sep = "")
  dir.create(directory)
  manifest <- data.frame(image = character(),
                         plate = character(),
                         stringsAsFactors = FALSE)
  for (subject in morphoVecList[[i]]) {
    currentRow <- finalIDs %>% 
      filter(SubjectID == subject)
    z <- currentRow$zname
    plate <- currentRow$PlateID
    for (photo in photosList) {
      if (grepl(plate, photo, fixed = TRUE) && !(plate %in% manifest$plate)) {
        fromString <- paste(folderToSort, "/", photo, sep = "")
        file.copy(fromString, directory)
        appendManifest <- data.frame("image" = photo,
                                     "plate" = plate,
                                     stringsAsFactors = FALSE)
        manifest <- rbind(manifest, appendManifest)
      }
    }
  }
  
  # create image manifest file for each folder
  csvPath <- paste(directory, "/", morphoNameVec[i], "_manifest.csv", sep = "")
  write_csv(manifest, csvPath)
}

# done
# woohoo