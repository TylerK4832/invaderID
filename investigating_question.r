# load necessary packages
library(filesstrings)
library(tidyverse)
library(ggplot2)

morphoNameVec <- c("Alg", "Anem", "Barn", "Biv", "BrBryo", "ColTuni",
                   "EnBryo", "Nothing", "SolTuni", "Spng", "TubeWm", 
                   "EnBryo_Watersipora", "ColTuni_Botrylid", "BrBryo_Amanthia", 
                   "BrBryo_Bugula", "NONE")

# collectors to import data
int <- col_integer()
fac <- col_factor(levels = morphoNameVec)
chr <- col_character()

# import original identifications dataset
allIDs <- read_csv("FinalIDs_added_all_target_taxa_zdata.csv",
                   col_types = list(int, chr, chr, fac, fac, fac))

# import environmental dataset
envData <- read_csv("Havards.Zooniverse.PanelInfoForNewcomerN.18Mar2020.csv")
envData <- envData[c(3,4,6,7,9,10,11)]






WatersiporaIDs <- allIDs %>%
  filter(FinalIDs1 == "EnBryo_Watersipora" | FinalIDs2 == "EnBryo_Watersipora" | FinalIDs3 == "EnBryo_Watersipora")

invaders <- c("ColTuni_Botrylid", "BrBryo_Amanthia", "BrBryo_Bugula")
names <- c("Botrylid", "Botrylid", "Amanthia", "Amanthia", "Bugula", "Bugula")
invader_vec <- c()
ws_present <- c()

abundance <- c()
ws_abundance <- c()
for (species in invaders) {
  abundance <- c(abundance, nrow(filter(allIDs, 
                                        FinalIDs1 == species | 
                                        FinalIDs2 == species | 
                                        FinalIDs3 == species))/nrow(allIDs),
                            nrow(filter(WatersiporaIDs,
                                         FinalIDs1 == species | 
                                           FinalIDs2 == species | 
                                           FinalIDs3 == species))/nrow(WatersiporaIDs))
  ws_present <- c(ws_present, "Not present", "Present")
}

speciesAbundanceWatersipora <- data.frame("Species" = names, "Watersipora" = ws_present, "Abundance" = abundance, stringsAsFactors = FALSE)

ggplot(data=speciesAbundanceWatersipora, aes(x=Species, y=Abundance, fill=Watersipora)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Reds") +
  theme_minimal() + 
  ylab("Abundance (%)")






BotrylidIDs <- allIDs %>%
  filter(FinalIDs1 == "ColTuni_Botrylid" | FinalIDs2 == "ColTuni_Botrylid" | FinalIDs3 == "ColTuni_Botrylid")

invaders <- c("EnBryo_Watersipora", "BrBryo_Amanthia", "BrBryo_Bugula")
names <- c("Watersipora", "Watersipora", "Amanthia", "Amanthia", "Bugula", "Bugula")
invader_vec <- c()
bot_present <- c()

abundance <- c()
bot_abundance <- c()
for (species in invaders) {
  abundance <- c(abundance, nrow(filter(allIDs, 
                                        FinalIDs1 == species | 
                                        FinalIDs2 == species | 
                                        FinalIDs3 == species))/nrow(allIDs),
                 nrow(filter(BotrylidIDs,
                             FinalIDs1 == species | 
                             FinalIDs2 == species | 
                             FinalIDs3 == species))/nrow(BotrylidIDs))
  bot_present <- c(bot_present, "Not present", "Present")
}

speciesAbundanceBotrylid <- data.frame("Species" = names, "Botrylid" = bot_present, "Abundance" = abundance, stringsAsFactors = FALSE)

ggplot(data=speciesAbundanceBotrylid, aes(x=Species, y=Abundance, fill=Botrylid)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Oranges") +
  theme_minimal() + 
  ylab("Abundance (%)")




taxaNames <- c("Watersipora", "Botrylid", "Amanthia", "Botrylid")
envVar <- "Sal"
taxa <- WatersiporaIDs

zcount <- c()
envVal <- c()
num_covered <- c()
    
tiles <- taxa %>% 
  select(PlateID) %>%
  distinct(PlateID) %>% 
  arrange()
    
tiles <- tiles$PlateID
    
for (tile in tiles) {
  tileEnvData <- filter(envData, Plate_ID == tile, Measure == envVar)$EnvValue[1]
  num_covered <- nrow(filter(taxa, PlateID == tile))/100
  if (!is.na(tileEnvData) & num_covered < 0.3) {
    envVal <- c(envVal, tileEnvData)
    zcount <- c(zcount, num_covered)
  }
}
  
coverEnvVar <- data.frame("cover" = zcount, "env" = envVal, stringsAsFactors = FALSE)
    
print(ggplot(coverEnvVar, aes(x=cover, y=env)) +
  geom_point() + 
  ggtitle(paste("Abundance with salinity"))) +
  xlab("Percent cover") +
  ylab("Salinity (ppt)") +
  geom_point(size = 1, color = "darkred")

