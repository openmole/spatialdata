
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(sp)
library(sf)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpatialData/library/data'))

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

real <- as.tbl(read.csv('morpho/morpho_npoints10000_windowSize500_worldWidth50_seed-645997938.csv',sep=";"))
real = real[real$area>0,]
# some points have a very low density (quasi empty : closing steps at 1 -> dichotomy point clouds)
# -> a filter on density is necessary (idem for simulation results)
real = real[real$density>0.05,]


# sampling distrib
#plot(real$lon,real$lat)

params = c("lon","lat")

indics = c("avgBlockArea","avgComponentArea","avgDetour","avgDistance","components","density","fullClosingSteps","fullDilationSteps","fullErosionSteps","fullOpeningSteps","moran")
# "area" = alpha*density

# filter NAs
real=real[apply(real[,indics],1,function(r){length(which(is.na(r)))==0}),]

# corrs / effective dim
cor(real[,indics])

morph=real[,indics]
for(j in 1:ncol(morph)){morph[,j]=(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca = prcomp(morph)
summary(pca)
pcs = as.matrix(morph)%*%pca$rotation

real=cbind(real,pcs)

plots=list()
for(indic in indics){
  g=ggplot(real,aes_string(x='PC1',y='PC2',col=indic))
  plots[[indic]]=g+geom_point()
  #g=ggplot(real,aes_string(x='PC2',y='PC3',col=indic))
  #g+geom_point()
}
multiplot(plotlist = plots,cols = 4)

## TODO : clustering ?



#####
# maps


lonmin=-10;lonmax=40;latmin=30;latmax=80

g=ggplot(real[real$lon>lonmin&real$lon<lonmax&real$lat>latmin&real$lat<latmax,],
         aes(x=lon,y=lat,colour=PC1)
         )
g+geom_point()


# -> very high heterogeneity of morphologies within each urban area
# -> mapping makes sense with summary indicators such as diversities of morphologies

areas <- readOGR('.','cities_europe')
points <- spTransform(SpatialPointsDataFrame(coords=real[,c("lon","lat")],data=real,proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')),areas@proj4string)

points@data = cbind(points@data,over(points,areas))

pcsds = as.tbl(points@data) %>% group_by(ID_UMZ) %>% summarise(pc1sd = sd(PC1),pc2sd=sd(PC2),count=n()) %>% filter(count>2)

g=ggplot(st_bind_cols(st_as_sf(areas[areas$ID_UMZ%in%pcsds$ID_UMZ,]),as.data.frame(pcsds)),aes(fill=pc1sd,color=pc1sd))
g+geom_sf(size=5)

g=ggplot(st_bind_cols(st_as_sf(areas[areas$ID_UMZ%in%pcsds$ID_UMZ,]),as.data.frame(pcsds)),aes(fill=pc2sd,color=pc2sd))
g+geom_sf(size=5)




####
# Simu data

res <- as.tbl(read.csv('exploration/20190306_103425_LHS_GRID.csv'))
# filter NAs
res=res[apply(res[,indics],1,function(r){length(which(is.na(r)))==0}),]


params=c("blocksMaxSize","blocksMinSize","blocksNumber","expMixtureCenters","expMixtureRadius","expMixtureThreshold","generator","percolationBordPoints","percolationLinkWidth","percolationProba","randomDensity")

summary(res)

morph=res[,indics]
for(j in 1:ncol(morph)){morph[,j]=(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca = prcomp(morph)
summary(pca)
pcs = as.matrix(morph)%*%pca$rotation

res = cbind(res,pcs)




######
all = rbind(cbind(real[,c(indics,"lon","lat")],generator=rep("real",nrow(real))),cbind(res[,c(indics,"generator")],lon=rep(0,nrow(res)),lat=rep(0,nrow(res))))

morph=all[,indics]
for(j in 1:ncol(morph)){morph[,j]=(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca = prcomp(morph)
summary(pca)
pcs = as.matrix(morph)%*%pca$rotation
all = cbind(all,pcs)


g=ggplot(all,aes(x=PC1,y=PC2,color=generator,size=ifelse(generator=="real",0.01,0.0005),alpha=ifelse(generator=="real",1,0.1)))
g+geom_point()



## specific cases
all[all$PC1<0.4&all$PC2>0.75&all$generator=="real",]
#-4.247058 48.45855

all[all$PC1>1.25&all$generator=="real",]


