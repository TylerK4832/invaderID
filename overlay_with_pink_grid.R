#Using Dropbox on the Updated Loop Code

install.packages('rdrop2')
install.packages("httpuv")
install.packages("shiny")

library(rdrop2)
library(httpuv)
library(magick)
library(dplyr)
library(shiny)

imageList <- list.files('Individual Tiles from 2018 Launch')

#download the grid image file to your desktop and read it in
overlay1 <- image_read('7x7grid_transparent_resize_magenta.png') 

#change all these folder locations that are on newcomerk/desktop to your desktop
#make 2 empty desktop folders 'Trial Folder2' and 'Downloads'

dir.create("Individual Tiles from 2018 Launch - Overlayed")

for(i in 1:length(imageList)){
  BG <- image_read(file.path('C:', 'Users', 'tk900', 'Desktop', 'invader id project folder','Individual Tiles from 2018 Launch', imageList[i]))
  #and the background image gets flatted with each of these 50 overlays 
  ##to make 50 separate images with the same background file
  img1 <- c(BG, overlay1)
  z1 <- image_flatten(img1)
  mypath2 = file.path('C:', 'Users', 'tk900', 'Desktop', 'invader id project folder', 'Individual Tiles from 2018 Launch - Overlayed', imageList[i])
  image_write(z1, path = mypath2, format = "jpg")
}

# setwd('C:\Users\tk900\Desktop\invader id project folder')

