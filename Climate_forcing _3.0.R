rm(list=ls())
cat("\014")  
setwd("C:\\Users\\Paul Sandoval\\Desktop\\Ingeniería Civil\\VI año\\Memoria de título\\Modelación\\Climate_forcing\\pcraster_format")

#IMPROVE CHECK AND DELETE NEGATIVE NUMBERS ON PREC GRIDDED DATA

library("raster")
library("readxl")
library("gdalUtilities")

DEM <-raster("C:\\Users\\Paul Sandoval\\Desktop\\Ingeniería Civil\\VI año\\Memoria de título\\Modelación\\Climate_forcing\\dem.tif")

#1 Import data from excel

xlsx_doc <- "C:\\Users\\Paul Sandoval\\Desktop\\Ingeniería Civil\\VI año\\Memoria de título\\Modelación\\Climate_forcing\\Climate_forcing.xlsx"

data_prec <- read_xlsx(xlsx_doc, sheet = 1, guess_max = 30000)
data_tmax <- read_xlsx(xlsx_doc, sheet = 2, guess_max = 30000)
data_tmin <- read_xlsx(xlsx_doc, sheet = 3, guess_max = 30000)
data_tmean <- read_xlsx(xlsx_doc, sheet = 4, guess_max = 30000)
info <- read_xlsx(xlsx_doc, sheet = 5)

data_prec$Date <- as.Date(data_prec$Date)
data_tmax$Date <- as.Date(data_tmax$Date)
data_tmin$Date <- as.Date(data_tmin$Date)
data_tmean$Date <- as.Date(data_tmean$Date)

#2 Check NAs and replace them

data_prec[is.na(data_prec)] <- 0
data_tmax[is.na(data_tmax)] <- 0
data_tmin[is.na(data_tmin)] <- 0
data_tmean[is.na(data_tmean)] <- 0

summary(data_prec$Lago_Mascardi_Central_Frey) 
summary(data_tmax$Lago_Steffen_Muelle)
summary(data_tmin$Lago_Steffen_Muelle)
summary(data_tmean$Lago_Steffen_Muelle)

#3 Get elevations from info

elev_prec <- as.numeric(subset(info, Station == "Lago_Mascardi_Central_Frey", select = "Alt"))
elev_t3m <- as.numeric(subset(info, Station == "Lago_Steffen_Muelle", select = "Alt"))

#4 Assign lapse rates

lr_prec <- 0.1/100
lr_t3m <- -0.0065

#5 Create .tif and .map directories

tif_dir <- "C:\\Users\\Paul Sandoval\\Desktop\\Ingeniería Civil\\VI año\\Memoria de título\\Modelación\\Climate_forcing\\pcraster_format\\"
pcr_dir <- "C:\\Users\\Paul Sandoval\\Desktop\\Ingeniería Civil\\VI año\\Memoria de título\\Modelación\\Climate_forcing\\pcraster_format\\PCRASTER\\"

#6 Create prec series

gridded_prec<-brick(DEM, nl = length(data_prec$Date), values = FALSE)
gridded_prec<-setValues(gridded_prec,0)

for(i in 1:length(data_prec$Lago_Mascardi_Central_Frey)){
  if (data_prec$Lago_Mascardi_Central_Frey[i]>0) {
    gridded_prec[[i]]<-setValues(gridded_prec[[i]], data_prec$Lago_Mascardi_Central_Frey[i])
    gridded_prec[[i]]<-gridded_prec[[i]]*((lr_prec*(DEM-elev_prec))+1)
    gridded_prec[[i]][gridded_prec[[i]]<0]<- 0}
  names(gridded_prec[[i]]) <- data_prec$Date[i] #IDK what it do
  writeRaster(gridded_prec[[i]], paste0("prec_",i),format = "GTiff")
  tif_file <- paste0(tif_dir,"prec_",i,".tif")
  pcr_file <- paste0(pcr_dir,"prec000",format(i/1000,nsmall = 3))
  gdal_translate(tif_file,pcr_file,of="PCRaster",ot="Float32",mo="VS_SCALAR")
  print(i)}

#7 Create Tmax series

gridded_tmax <- stack(replicate(length(data_prec$Lago_Mascardi_Central_Frey), DEM))

for(i in 1:length(data_tmax$Lago_Steffen_Muelle)){
  gridded_tmax[[i]][gridded_tmax[[i]] > -999] <- data_tmax$Lago_Steffen_Muelle[i] #reference value
  gridded_tmax[[i]] <- gridded_tmax[[i]]+lr_t3m*(DEM-elev_t3m) #The magic 
  names(gridded_tmax[[i]]) <- data_tmax$Date[i] #IDK what it do
  writeRaster(gridded_tmax[[i]], paste0("tmax_",i),format = "GTiff")
  tif_file <- paste0(tif_dir,"tmax_",i,".tif")
  pcr_file <- paste0(pcr_dir,"tmax000",format(i/1000,nsmall = 3))
  gdal_translate(tif_file,pcr_file,of="PCRaster",ot="Float32",mo="VS_SCALAR")
  print(i)}

#8 Create Tmin series

gridded_tmin <- stack(replicate(length(data_prec$Lago_Mascardi_Central_Frey), DEM))

for(i in 1:length(data_tmin$Lago_Steffen_Muelle)){
  gridded_tmin[[i]][gridded_tmin[[i]] > -999] <- data_tmin$Lago_Steffen_Muelle[i] #reference value
  gridded_tmin[[i]]<-gridded_tmin[[i]]+lr_t3m*(DEM-elev_t3m) #The magic 
  names(gridded_tmin[[i]]) <- data_tmin$Date[i] #IDK what it do
  writeRaster(gridded_tmin[[i]], paste0("tmin_",i),format = "GTiff")
  tif_file <- paste0(tif_dir,"tmin_",i,".tif")
  pcr_file <- paste0(pcr_dir,"tmin000",format(i/1000,nsmall = 3))
  gdal_translate(tif_file,pcr_file,of="PCRaster",ot="Float32",mo="VS_SCALAR")
  print(i)}

#9 Create Tmean series

gridded_tmean <- stack(replicate(length(data_prec$Lago_Mascardi_Central_Frey), DEM))

for(i in 1:length(data_tmean$Lago_Steffen_Muelle)){
  gridded_tmean[[i]][gridded_tmean[[i]] > -999] <- data_tmean$Lago_Steffen_Muelle[i] #reference value
  gridded_tmean[[i]]<-gridded_tmean[[i]]+lr_t3m*(DEM-elev_t3m) #The magic 
  names(gridded_tmean[[i]]) <- data_tmean$Date[i] #IDK what it do
  writeRaster(gridded_tmean[[i]], paste0("tmean_",i),format = "GTiff")
  tif_file <- paste0(tif_dir,"tmean_",i,".tif")
  pcr_file <- paste0(pcr_dir,"tmean00",format(i/1000,nsmall = 3))
  gdal_translate(tif_file,pcr_file,of="PCRaster",ot="Float32",mo="VS_SCALAR")
  print(i)}

#END

