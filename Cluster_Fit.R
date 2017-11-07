# Load clustering input
load(file="./Rdata_Files/Master_Input_1_0_CUT_Final.RData")

Master_Input_1_0_CUT$WP102EW0001 <- NULL
Master_Input_1_0_CUT$WP102EW0003 <- NULL

# Import lookup tables
Lookup <- read.csv("./Final_Deliverables/Final_Cluster_Lookup_With_Sub_Groups.csv")

#Import Shapefile (WZ)
WZ_London <- readOGR(dsn = "./Shapefiles/", layer = "WZ_London_clip", verbose = FALSE)

Lookup <- merge(Lookup,Master_Input_1_0_CUT,by.x="WZ_CODE",by.y="WZ_CODE")
Lookup$WP102EW0001 <-NULL

SubGroup_Means <- aggregate(Lookup[,4:95], by=list(Lookup$SubGroup), FUN=mean)


#Split the data by the SuperGroup
Lookup <- split(Lookup,Lookup$SubGroup)

tmp_ALL <- NA

for (x in 1:length(Lookup)){

tmp_out <- NA

for (i in 1:nrow(Lookup[[x]])){
  tmp <- Lookup[[x]][i,4:95]
  tmp <- abs(tmp - SubGroup_Means[x,2:93])
  tmp_out <- rbind(tmp_out,tmp)
}

tmp_out <- data.frame(Lookup[[x]]$WZ_CODE,tmp_out[-1,])

tmp_ALL <- rbind(tmp_ALL,tmp_out)
rm(tmp_out)

}

tmp_ALL <- tmp_ALL[-1,]
tmp_ALL$SUM <- rowSums(tmp_ALL[,2:93])

colnames(tmp_ALL) <- c("WZ_CODE",colnames(tmp_ALL)[2:94])


WZ_London <- merge(WZ_London,tmp_ALL,by.x="code",by.y="WZ_CODE",all.x=TRUE)
Lookup <- read.csv("./Final_Deliverables/Final_Cluster_Lookup_With_Sub_Groups.csv")
WZ_London <- merge(WZ_London,Lookup,by.x="code",by.y="WZ_CODE",all.x=TRUE)



#Output Map
pdf("error_SubGroup.pdf")
m <- tm_shape(WZ_London,projection = 27700) +
  tm_polygons("SUM",border.col =NA, border.alpha = 0) +
  tm_legend(text.size=0.6,title.size=0.6,legend.outside = TRUE, legend.position = c("centre","bottom")) + 
  tm_layout(frame = FALSE, aes.palette = list(seq = "BuPu", div = "RdYlGn", cat = "Set3")
) +
  tm_scale_bar(width=0.2) + tm_layout(attr.outside = TRUE)
print(m)
dev.off()



