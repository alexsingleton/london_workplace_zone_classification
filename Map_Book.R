library(downloader)
library(dplyr)
library(XLConnect)
library(rgeos)
library(rgdal)
library(gdalUtils)
library(raster)
library(sqldf)
library(tmap)
library(ggplot2)
library(spatialEco)
library(tmaptools)
library(corrplot)
library(vegan)
library(psych)
library(car)
library(knitr)
library(skmeans)
library(plyr)
library(pander)
library(RColorBrewer)
library(colorspace)   ## hsv colorspace manipulations
library(RColorBrewer) ## For some example colors


## Function for desaturating colors by specified proportion (http://stackoverflow.com/questions/26314701/r-reducing-colour-saturation-of-a-colour-palette)
desat <- function(cols, sat=0.6) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}


# Set working directory
setwd("~/Dropbox/Projects/WZ_Classification/")

#Import Shapefile (WZ)
WZ_London <- readOGR(dsn = "./Shapefiles/", layer = "WZ_London_clip", verbose = FALSE)

#Import Shapefile (OA)
OA_London <- readOGR(dsn = "./Shapefiles/", layer = "OA_London", verbose = FALSE)

#Import lookup 
lookup <- read.csv("./Final_Deliverables/Final_Cluster_Lookup_With_Sub_Groups_With_Supression.csv")
lookupOA <- read.csv("OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv")

#WZ higher geog
h_geog <- read.csv("WZ11_BUASD11_BUA11_LAD11_RGN11_EW_LU.csv")

# Load OA Census data
load("./Rdata_Files/OA_Census.RData")

#OA Numerator / den. calcs
Num_Dem_OA_Census <- read.csv("Num_Dem_OA_Census.csv")


#Import Shapefile (WZ)
London_B <- readOGR(dsn = "./Shapefiles/", layer = "London_Borough_Excluding_MHW", verbose = FALSE)




# Setup output object
OA_Census_Variables <- data.frame(OA_CODE = oa_London[,"OA_CODE"])

#Calculate OA rates
for (i in 1:nrow(Num_Dem_OA_Census)) {
  numerator <- trimws(unlist(strsplit(as.character(Num_Dem_OA_Census[i,"Variable_Num"]),",")))
  denominator <- trimws(unlist(strsplit(as.character(Num_Dem_OA_Census[i,"Variable_Den"]),",")))
  
  # Extract and calculate values; The rep is necessary as ifelse will only return a length equiv. of input.
  N_Calc <- ifelse(rep(length(numerator) > 1,nrow(oa_London)),rowSums(oa_London[,numerator]),oa_London[,numerator])
  D_Calc <- ifelse(rep(length(denominator) > 1,nrow(oa_London)),rowSums(oa_London[,denominator]),oa_London[,denominator])
  # Calculate Rates
  tmp_calc <- data.frame(N_Calc / D_Calc * 100)
  colnames(tmp_calc) <- Num_Dem_OA_Census[i,"Name"]
  # Append Rates
  OA_Census_Variables <- cbind(OA_Census_Variables,tmp_calc)
  #Clean Up
  rm(list=c("N_Calc","D_Calc","tmp_calc"))
  
}

OA_Census_Variables[is.na(OA_Census_Variables)] <- 0 #replace NA with 0





#Colour ramp
splits <- c(2,2,2,3,2)# specify splits V2
#Get colours
my_colour <- brewer.pal(length(splits),"Set1")

# Creates a nested colour ramp
final_col <- NA

for (x in 1:length(my_colour)){
  
  tmp_col <- NA
  sat_tmp <- 1/splits[x]
  for (j in 1:splits[x]){
    o_col <- desat(my_colour[x], sat_tmp)
    sat_tmp <- sat_tmp + (1/splits[x])
    tmp_col <- c(tmp_col,o_col)
  }
  final_col <- c(final_col,tmp_col[-1])
}

final_col <- final_col[-1]
final_col <- c(final_col,"#AEB6BF")

#Calculate bands

library(Hmisc)

OA_Census_Variables_2 <- data.frame(apply(OA_Census_Variables[,-1], 2, function(x) as.numeric(cut2(x, g=10))))
OA_Census_Variables_2[OA_Census_Variables_2 != 10]<-NA #Max 10%
OA_Census_Variables_2 <- data.frame(OA_CODE = OA_Census_Variables$OA_CODE,OA_Census_Variables_2)


#Merge OA census data
OA_London <- merge(OA_London,OA_Census_Variables_2,by.x="code",by.y="OA_CODE",all.x=TRUE)#census OA
OA_London <- merge(OA_London,lookupOA,by.x="code",by.y="OA11CD",all.x=TRUE)#lookup


library(gtools)

# Create map
tmp9 <- WZ_London
tmp9 <- merge(tmp9,lookup,by.x="code",by.y="WZ_CODE",all.x=TRUE)#lookup
tmp9 <- merge(tmp9,h_geog,by.x="code",by.y="WZ11CD",all.x=TRUE)#higher geog
tmp9 <- merge(tmp9,Master_Input,by.x="code",by.y="WZ_CODE",all.x=TRUE)#residential vars


#writeOGR(OA_London, ".", "test", driver="ESRI Shapefile")
#writeOGR(tmp9, ".", "testWZ", driver="ESRI Shapefile")

#Get list of LAD
LAD_Lists <- unique(tmp9$LAD11CD)
Res_Vars <- Num_Dem_OA_Census$Name

for (i in 1:length(LAD_Lists)){
  
  tmpWZ <- tmp9[tmp9$LAD11CD == LAD_Lists[i],]
  
  pdf(paste0("./Final_Deliverables/Maps/",gsub(" ","_",unique(tmpWZ$LAD11NM)),".pdf"))
  for (x in 1:length(Res_Vars)) {
  
#Get Temp

tmpOA <- OA_London[OA_London$LAD11CD == LAD_Lists[i],]

#Removes any OA that are not in highest 10%
if (!all(is.na(tmpOA@data[,paste(Res_Vars[x])]))){
tmpOA <- tmpOA[!is.na(tmpOA@data[,paste(Res_Vars[x])]),]
}


m <- tm_shape(tmpWZ,projection = 27700) +
  tm_polygons("SubGroup",palette = final_col,border.col =NA, border.alpha = 0,title="Sub Group") +
  
  tm_shape(tmpOA,projection = 27700) +
  tm_borders(col ="black",lwd=0.4) +
  
  tm_legend(text.size=0.6,title.size=0.6,legend.outside = TRUE, legend.position = c("centre","bottom")) + 
  tm_layout(paste0(unique(tmpWZ$LAD11NM),"\n",Num_Dem_OA_Census[Num_Dem_OA_Census$Name == Res_Vars[x],"Description"]),frame = FALSE,title.size=0.6,title.snap.to.legend=TRUE) +
  tm_scale_bar(width=0.2) + tm_layout(attr.outside = TRUE)
print(m)

rm(tmpOA)

}
  rm(tmpWZ)
  dev.off()
}





# London Map
  
  pdf("./Final_Deliverables/Maps/All_London.pdf")
    
    m <- tm_shape(tmp9,projection = 27700) +
      tm_polygons("SubGroup",palette = final_col,border.col =NA, border.alpha = 0,title="Sub Group") +
      
      tm_shape(London_B,projection = 27700) +
      tm_polygons(border.col = "#FFFFFF",alpha = 0) +
      
      tm_legend(text.size=0.6,title.size=0.6,legend.outside = TRUE, legend.position = c("centre","bottom")) + 
      tm_layout(frame = FALSE,attr.outside = TRUE) +
      tm_scale_bar(width=0.2)
    print(m)
   
  dev.off()


