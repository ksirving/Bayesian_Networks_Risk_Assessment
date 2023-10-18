library(ncdf4); library(sp); library(raster)
library(sf)
#-------------------------------------------------------------------------------------
#SST data
#-------------------------------------------------------------------------------------
#---------------------------
#check content climate data
#---------------------------

# zipF<-file.choose() # lets you choose a file and save its file path in R (at least for windows)
# outDir<-"/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Data" # Define the folder where the zip file should be unzipped to 
# unzip(zipF,exdir=outDir)  # unzip your file 

getwd()
dat.fname <- "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Bayesian_Networks_Risk_Assessment/ignore/sst/ersst.v5.200001.nc"
#
dat.fname
getwd()

##get info layers
ncin <- nc_open(dat.fname)
ncin
nc_close(ncin)
#--------------------------
# reproject raster 

# Set the working directory
setwd("ignore/sst")
# List all .tif files
raster_files <- list.files(pattern=".nc")
raster_files

#set the new coordinat system (WGS 1984 World Mercator)
new_crs <- CRS("EPSG:3395")
file
# loop through the rasters and reprojec and save
for (file in raster_files) {
  # Load the rasters
  rast <- brick(file, varname="sst")
  rast
  # Reproject the raster
  rast_reprojected <- projectRaster(rast, crs=new_crs)
  
  # Save the reprojected raster
  writeRaster(rast_reprojected, filename=paste0("reprojected_", file), format="GTiff", overwrite=TRUE)
}

#--------------------------

#-------------------------
#extract data (loop over the years)
#-------------------------
getwd()
setwd("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Bayesian_Networks_Risk_Assessment")
b2 <- raster(dat.fname,varname="sst",level=1)   #just to initialize the loop
plot(rast_reprojected)

#upload data

sites <- read.csv("output_data/02_sites_coords.csv") %>%
  st_as_sf(coords=c("site_long", "site_lat"), crs=4326, remove=F) %>%
  st_transform(crs=3395)

sites

rast_reprojected

test <- raster::extract(rast_reprojected, sites)
test
dft <- data.frame(matrix( ncol = 3))
colnames(dft) <- c("Site", "Lat", "Lon")

dft$Site <- "General"
dft$Lat <- 34
dft$Lon <- 63

coordinates(dft) <-c("Lon", "Lat")
st_transform(dft, crs = st_crs(b2))
?st_transform
range(sites$begin) ##1991
range(sites$end) ## 2017


for(i in 2000:2023){
b1 <- brick(paste0("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Bayesian_Networks_Risk_Assessment/ignore/sst/ersst.v5.",i,"01.nc"),varname="sst",level=1)   
writeRaster(b1, "output_data/test_output1", format = "GTiff")

test <- raster::extract(b1, dft)
test
b1
plot(b1)

#if want to check the names
#aa = gsub("X","",names(b2))
#Year = sapply(strsplit(as.character(aa),".",fixed=T),'[',1)
#Month = sapply(strsplit(as.character(aa),".",fixed=T),'[',2)

#---------------------------------------------------------------
#calculating annual average based on monthly data
#--------------------------------------------------------------

tmin = cbind(extract(b1[[1]],sites),extract(b1[[2]],sites),extract(b1[[3]],sites),extract(b1[[4]],sites),extract(b1[[5]],sites),extract(b1[[6]],sites),extract(b1[[7]],sites),extract(b1[[8]],sites),extract(b1[[9]],sites),extract(b1[[10]],sites),extract(b1[[11]],sites),extract(b1[[12]],sites))
tmin_av = cbind(tmin_av,apply(tmin,1,mean))

tmax = cbind(extract(b2[[1]],sites),extract(b2[[2]],sites),extract(b2[[3]],sites),extract(b2[[4]],sites),extract(b2[[5]],sites),extract(b2[[6]],sites),extract(b2[[7]],sites),extract(b2[[8]],sites),extract(b2[[9]],sites),extract(b2[[10]],sites),extract(b2[[11]],sites),extract(b2[[12]],sites))
tmax_av = cbind(tmax_av,apply(tmax,1,mean))

print(i)
}

tmax_av
# warnings()
colnames(tmax_av) = 2004:2013
colnames(tmin_av) = 2004:2013
tmax_av
length(colnames(tmax_av))
tmin_av = data.frame(SiteID = sites$SiteID,tmin_av)
tmax_av = data.frame(SiteID = sites$SiteID,tmax_av)
tmax_av
tmean = (tmin_av[,2:11] + tmax_av[,2:11])/2
tmean = data.frame(SiteID = sites$SiteID,tmean)

tmean
write.csv(tmin_av,"/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Output/Sites_tmin_av_new_sites_2004_2013.csv",row.names=F)
write.csv(tmax_av,"/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Output/Sites_tmax_av_new_sites_2004_2013.csv",row.names=F)
write.csv(tmean,"/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Output/Sites_tmean_av_new_sites_2004_2013.csv",row.names=F)

#------------------------------------
#compute anomalies
#------------------------------------
# anom_tmean = (tmean[,2:12] - apply(tmean[,2:12],1,mean))
# anom_tmean = data.frame(SiteID = sites$SiteID,anom_tmean)
# 
# anom_tmin_av = (tmin_av[,2:12] - apply(tmin_av[,2:12],1,mean))
# anom_tmin_av = data.frame(SiteID = sites$SiteID,anom_tmin_av)
# 
# anom_tmax_av = (tmax_av[,2:12] - apply(tmax_av[,2:12],1,mean))
# anom_tmax_av = data.frame(SiteID = sites$SiteID,anom_tmax_av)
# 
# write.csv(anom_tmin_av,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_anomalies_tmin_av_new_sites.csv",row.names=F)
# write.csv(anom_tmax_av,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_anomalies_tmax_av_new_sites.csv",row.names=F)
# write.csv(anom_tmean,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_anomalies_tmean_av_new_sites.csv",row.names=F)

#-------------------------------------------------------------------------------------
#flow data
#-------------------------------------------------------------------------------------
#---------------------------
#check content flow data
#---------------------------
dat.fname <- "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Data/FLO1K.ts.1960.2015.qav.nc"
#
getwd()
str(dat.fname)
# ?download.file
outfile = "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Data"
# test <- download.file("/Users/katieirving/Library/Mobile Documents/com~apple~CloudDocs/Documents/sYNGEO/func_emp/data/.FLO1K.ts.1960.2015.qav.nc.icloud", outfile)
##get info layers
ncin <- nc_open(dat.fname)
ncin
nc_close(ncin)

# setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/func_sync_emp/input_data")
#--------------------------

#--------------------------
#upload site data
#--------------------------
# FishSites <- read.csv("/Users/katieirving/Documents/git/func_sync_emp/input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
# 
# FishSites <- FishSites[, c(9,11,12,14,15,16)]
# 
# FishSites <- FishSites[!duplicated(FishSites), ]
# write.csv(FishSites, "input_data/Bio/SiteID_Coords_locations.csv")

# sites <- sites[!duplicated(sites), ]
sites <- read.csv("Data/TableSites.csv")
coordinates(sites)<- c("Longitude","Latitude")
head(sites)
#-------------------------
#extract data 
#-------------------------

#create raster brick
# bmean <- brick("/Users/katieirving/Documents/sYNGEO/func_emp/data/FLO1K.ts.1960.2015.qav.nc",varname="qav",level=1)   #1960-2015

bmin <- brick("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Data/FLO1K.ts.1960.2015.qmi.nc",varname="qmi",level=1)   #1960-2015

bmax <- brick("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Data/FLO1K.ts.1960.2015.qma.nc",varname="qma",level=1)   #1960-2015

bav <-  brick("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/sYNGEO_Env_Extraction/Data/FLO1K.ts.1960.2015.qav.nc",varname="qav",level=1)   #1960-2015

Year = as.numeric(sapply(strsplit(gsub("X","",names(bmin)),".",fixed=T),'[',1))
Year
qmin_av = qmax_av = qmean = NULL
i=1

for(i in 1:nrow(sites)){
pts = sites[i,]
ii = which(Year %in% (1991:2015)) 
qmin_av=rbind(qmin_av,c(extract(bmin,pts)[ii]))
qmax_av=rbind(qmax_av,c(extract(bmax,pts)[ii]))
qmean=rbind(qmean,c(extract(bav,pts)[ii]))
print(i)
}


save(qmin_av, qmax_av, qmean, file="Output/flow_min_max_values_extracted_new_sites.RData")
# load(file="output_data/flow_min_max_values_extracted_new_sites.RData")
range(qmin_av)
# str(qmin_av)
qmin_av <- as.data.frame(qmin_av)
qmax_av <- as.data.frame(qmax_av)
qmean <- as.data.frame(qmean)
colnames(qmin_av) = 1991:2015
colnames(qmax_av) = 1991:2015
colnames(qmean) = 1991:2015
# names(qmax_av)
# dim(qmax_av)
# head(sites)
# dim(sites)

qmin_av = data.frame(SiteID = sites$sYNGEO_ID,qmin_av)
qmax_av = data.frame(SiteID = sites$sYNGEO_ID,qmax_av)
qmean = data.frame(SiteID = sites$sYNGEO_ID,qmean)

save(qmin_av, qmax_av, qmean, file="Output/flow_min_max_mean_extracted_new_sites_raw_V2.RData")

## get siteid

# siteSP <- shapefile("/Users/katieirving/Documents/git/func_sync_emp/input_data/Bio/Sites_funcsynchrony_10262020.shp")
# sites = read.csv("input_data/Bio/sites_selection_basins_same_time_window_10262020.csv",h=T)

# qmin_av = data.frame(SiteID = sites$SiteID,qmin_av)
# qmax_av = data.frame(SiteID = sites$SiteID,qmax_av)
# qmax_av <- qmax_av[, -13]
# qmin_av <- qmin_av[, -13]
# qmean = (qmin_av[,2:12] + qmax_av[,2:12])/2
# qmean = data.frame(SiteID = sites$SiteID,qmean)
## 4 NAs - sites with no flow value
## remove duplicates

# save(qmin_av, qmax_av, qmean, file="output_data/flow_min_max_mean_extracted_new_sites_raw.RData")

library(tidyverse)
#------------------------------------
#compute anomalies
#------------------------------------
anom_qmean = (qmean[,2:12] - apply(qmean[,2:12],1,mean))
anom_qmean = data.frame(SiteID = sites$SiteID,anom_qmean)

anom_qmin_av = (qmin_av[,2:12] - apply(qmin_av[,2:12],1,mean))
anom_qmin_av = data.frame(SiteID = sites$SiteID,anom_qmin_av)
anom_qmin_av$X2003
head(anom_qmin_av)

anom_qmax_av = (qmax_av[,2:12] - apply(qmax_av[,2:12],1,mean))
anom_qmax_av = data.frame(SiteID = sites$SiteID,anom_qmax_av)


save(anom_qmin_av,file="output_data/Sites_anomolies_qmin_av_flow_new_sites.RData")
save(anom_qmax_av,file="output_data/Sites_anomolies_qmax_av_flow_new_sites.RData")
save(anom_qmean,file="output_data/Sites_anomolies_qmean_av_flow_new_sites.RData")


names(qmean)
dim(qmin_av) ## 795
head(qmin_av)
head(anom_qmin_av)

save(qmin_av,file="output_data/Sites_qmin_av_flow_new_sites_raw.RData")
save(qmax_av,file="output_data/Sites_qmax_av_flow_new_sites_raw.RData")
save(qmean,file="output_data/Sites_qmean_flow_new_sites_raw.RData")


