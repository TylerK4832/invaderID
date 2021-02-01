# load necessary packages
library(filesstrings)
library(tidyverse)

morphoNameVec <- c("Alg", "Anem", "Barn", "Biv", "BrBryo", "ColTuni",
                   "EnBryo", "Nothing", "SolTuni", "Spng", "TubeWm", "EnBryo_Watersipora", "ColTuni_Botrylid", "BrBryo_Amanthia", "BrBryo_Bugula",
                   "NONE")

# collectors to import data
int <- col_integer()
fac <- col_factor(levels = morphoNameVec)
chr <- col_character()

# import original identifications dataset
allIDs <- read_csv("New4Agreement_FixedIssue_Pulled13July2020.csv",
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
allIDs<- allIDs[c(2, 16, 17, 18, 19, 20)]

# import target taxa zooniverse dataset
targetTaxaIDs <- read_csv("23July2020_invader-id-target-taxa-classifications_DataSplitApart_Watersipora.csv")

# iterate through allIDs
for (row in 1:nrow(targetTaxaIDs)) {
  if (targetTaxaIDs[row, "present"] == "Yes") {
    for(i in seq(from=7, to=104, by=2)) {
      if (is.na(targetTaxaIDs[[row, i]])) {
        break
      } else {
        options(digits = 10)
        x <- as.double(targetTaxaIDs[[row, i]])
        y <- as.double(targetTaxaIDs[[row, i+1]])
        z <- paste("z", ((7 *(ceiling(x/146.2857) - 1)) + ceiling(y/146.2857)), sep = "")
        plate <- targetTaxaIDs[[row, "plate_id"]]
        aRow <- as.data.frame(allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), ])
        if (nrow(aRow) != 0) {
          if ((!(is.na(aRow[["SubjectID"]]))) && !("EnBryo_Watersipora" %in% c(as.character(aRow[["FinalIDs1"]]),as.character(aRow[["FinalIDs2"]]),as.character(aRow[["FinalIDs3"]])))) {
            if (as.character(aRow[["FinalIDs1"]]) == "EnBryo" || as.character(aRow[["FinalIDs1"]]) == "Nothing" || as.character(aRow[["FinalIDs1"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs1"] = "EnBryo_Watersipora"
            } else if (as.character(aRow[["FinalIDs2"]]) == "EnBryo" || as.character(aRow[["FinalIDs2"]]) == "Nothing" || as.character(aRow[["FinalIDs2"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs2"] = "EnBryo_Watersipora"
            } else if (as.character(aRow[["FinalIDs3"]]) == "EnBryo" || as.character(aRow[["FinalIDs3"]]) == "Nothing" || as.character(aRow[["FinalIDs3"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs3"] = "EnBryo_Watersipora"
            }
          }
        } else {
          newRow <- data.frame("SubjectID" = NA, "zname" = z, "PlateID" = plate, 
                               "FinalIDs1" = "EnBryo_Watersipora", "FinalIDs2" = "NONE", "FinalIDs3" = "NONE")
          allIDs <- rbind(allIDs, newRow)
        }
      }
    }
  }
}

# import target taxa zooniverse dataset
targetTaxaIDs <- read_csv("23July2020_invader-id-target-taxa-classifications_DataSplitApart_Botrylid.csv")

# iterate through allIDs
for (row in 1:nrow(targetTaxaIDs)) {
  if (targetTaxaIDs[row, "present"] == "Yes") {
    for(i in seq(from=7, to=104, by=2)) {
      if (is.na(targetTaxaIDs[[row, i]])) {
        break
      } else {
        options(digits = 10)
        x <- as.double(targetTaxaIDs[[row, i]])
        y <- as.double(targetTaxaIDs[[row, i+1]])
        z <- paste("z", ((7 *(ceiling(x/146.2857) - 1)) + ceiling(y/146.2857)), sep = "")
        plate <- targetTaxaIDs[[row, "plate_id"]]
        aRow <- as.data.frame(allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), ])
        if (nrow(aRow) != 0) {
          if ((!(is.na(aRow[["SubjectID"]]))) && !("ColTuni_Botrylid" %in% c(as.character(aRow[["FinalIDs1"]]),as.character(aRow[["FinalIDs2"]]),as.character(aRow[["FinalIDs3"]])))) {
            if (as.character(aRow[["FinalIDs1"]]) == "ColTuni" || as.character(aRow[["FinalIDs1"]]) == "Nothing" || as.character(aRow[["FinalIDs1"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs1"] = "ColTuni_Botrylid"
            } else if (as.character(aRow[["FinalIDs2"]]) == "ColTuni" || as.character(aRow[["FinalIDs2"]]) == "Nothing" || as.character(aRow[["FinalIDs2"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs2"] = "ColTuni_Botrylid"
            } else if (as.character(aRow[["FinalIDs3"]]) == "ColTuni" || as.character(aRow[["FinalIDs3"]]) == "Nothing" || as.character(aRow[["FinalIDs3"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs3"] = "ColTuni_Botrylid"
            }
          }
        } else {
          newRow <- data.frame("SubjectID" = NA, "zname" = z, "PlateID" = plate, 
                               "FinalIDs1" = "ColTuni_Botrylid", "FinalIDs2" = "NONE", "FinalIDs3" = "NONE")
          allIDs <- rbind(allIDs, newRow)
        }
      }
    }
  }
}

# import target taxa zooniverse dataset
targetTaxaIDs <- read_csv("23July2020_invader-id-target-taxa-classifications_DataSplitApart_Bugula.csv")

# iterate through allIDs
for (row in 1:nrow(targetTaxaIDs)) {
  if (targetTaxaIDs[row, "present"] == "Yes") {
    for(i in seq(from=7, to=104, by=2)) {
      if (is.na(targetTaxaIDs[[row, i]])) {
        break
      } else {
        options(digits = 10)
        x <- as.double(targetTaxaIDs[[row, i]])
        y <- as.double(targetTaxaIDs[[row, i+1]])
        z <- paste("z", ((7 *(ceiling(x/146.2857) - 1)) + ceiling(y/146.2857)), sep = "")
        plate <- targetTaxaIDs[[row, "plate_id"]]
        aRow <- as.data.frame(allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), ])
        if (nrow(aRow) != 0) {
          if ((!(is.na(aRow[["SubjectID"]]))) && !("BrBryo_Bugula" %in% c(as.character(aRow[["FinalIDs1"]]),as.character(aRow[["FinalIDs2"]]),as.character(aRow[["FinalIDs3"]])))) {
            if (as.character(aRow[["FinalIDs1"]]) == "BrBryo" || as.character(aRow[["FinalIDs1"]]) == "Nothing" || as.character(aRow[["FinalIDs1"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs1"] = "BrBryo_Bugula"
            } else if (as.character(aRow[["FinalIDs2"]]) == "BrBryo" || as.character(aRow[["FinalIDs2"]]) == "Nothing" || as.character(aRow[["FinalIDs2"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs2"] = "BrBryo_Bugula"
            } else if (as.character(aRow[["FinalIDs3"]]) == "BrBryo" || as.character(aRow[["FinalIDs3"]]) == "Nothing" || as.character(aRow[["FinalIDs3"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs3"] = "BrBryo_Bugula"
            }
          }
        } else {
          newRow <- data.frame("SubjectID" = NA, "zname" = z, "PlateID" = plate, 
                               "FinalIDs1" = "BrBryo_Bugula", "FinalIDs2" = "NONE", "FinalIDs3" = "NONE")
          allIDs <- rbind(allIDs, newRow)
        }
      }
    }
  }
}

# import target taxa zooniverse dataset
targetTaxaIDs <- read_csv("23July2020_invader-id-target-taxa-classifications_DataSplitApart_Amanthia.csv")

# iterate through allIDs
for (row in 1:nrow(targetTaxaIDs)) {
  if (targetTaxaIDs[row, "present"] == "Yes") {
    for(i in seq(from=7, to=104, by=2)) {
      if (is.na(targetTaxaIDs[[row, i]])) {
        break
      } else {
        options(digits = 10)
        x <- as.double(targetTaxaIDs[[row, i]])
        y <- as.double(targetTaxaIDs[[row, i+1]])
        z <- paste("z", ((7 *(ceiling(x/146.2857) - 1)) + ceiling(y/146.2857)), sep = "")
        plate <- targetTaxaIDs[[row, "plate_id"]]
        aRow <- as.data.frame(allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), ])
        if (nrow(aRow) != 0) {
          if ((!(is.na(aRow[["SubjectID"]]))) && !("BrBryo_Amanthia" %in% c(as.character(aRow[["FinalIDs1"]]),as.character(aRow[["FinalIDs2"]]),as.character(aRow[["FinalIDs3"]])))) {
            if (as.character(aRow[["FinalIDs1"]]) == "BrBryo" || as.character(aRow[["FinalIDs1"]]) == "Nothing" || as.character(aRow[["FinalIDs1"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs1"] = "BrBryo_Amanthia"
            } else if (as.character(aRow[["FinalIDs2"]]) == "BrBryo" || as.character(aRow[["FinalIDs2"]]) == "Nothing" || as.character(aRow[["FinalIDs2"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs2"] = "BrBryo_Amanthia"
            } else if (as.character(aRow[["FinalIDs3"]]) == "BrBryo" || as.character(aRow[["FinalIDs3"]]) == "Nothing" || as.character(aRow[["FinalIDs3"]]) == "NONE") {
              allIDs[which(allIDs$zname == z & allIDs$PlateID == plate), "FinalIDs3"] = "BrBryo_Amanthia"
            }
          }
        } else {
          newRow <- data.frame("SubjectID" = NA, "zname" = z, "PlateID" = plate, 
                               "FinalIDs1" = "BrBryo_Amanthia", "FinalIDs2" = "NONE", "FinalIDs3" = "NONE")
          allIDs <- rbind(allIDs, newRow)
        }
      }
    }
  }
}

csvPath <- "FinalIDs_added_all_target_taxa_zdata.csv"
write_csv(allIDs, csvPath)