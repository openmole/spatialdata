
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(sp)
library(sf)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpatialData/library/data'))

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

realfiles = list.files('morpho/final')

real=data.frame()
for(realfile in realfiles){
  #show(realfile)
  currentdata=as.tbl(read.csv(paste0('morpho/final/',realfile),sep=";"))
  #show(summary(currentdata))
  real <- rbind(real,currentdata)
}

real = real[real$area>0,]
# some points have a very low density (quasi empty : closing steps at 1 -> dichotomy point clouds)
# -> a filter on density is necessary (idem for simulation results)
real = real[real$density>0.05&real$density<0.8,]
#real = real[real$moran<1,] # a strange point in Sicilia has a moran >1 -> ? - area seems full -> also remove too high density areas which make no sense

# sampling distrib
#plot(real$lon,real$lat)

params = c("lon","lat")

#indics = c("avgBlockArea","avgComponentArea","avgDetour","avgDistance","components","density","fullClosingSteps","fullDilationSteps","fullErosionSteps","fullOpeningSteps","moran")
# ! order for export : same as scala case class
indics = c("moran","avgDistance","density","components","avgDetour","avgBlockArea",
           "avgComponentArea","fullDilationSteps","fullErosionSteps") # fullClosingSteps,fullOpeningSteps
# "area" = alpha*density

# filter NAs
real=real[apply(real[,indics],1,function(r){length(which(is.na(r)))==0}),]

# corrs / effective dim
#cor(real[,indics])

morph=real[,indics]
maxs=apply(morph,2,max);mins=apply(morph,2,min)
write.table(data.frame(maxs,mins),file="calib/norm.csv",quote = F,sep=',',col.names = F,row.names=F)
for(j in 1:ncol(morph)){morph[,j]=(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca = prcomp(morph)
summary(pca)
pcs = as.matrix(morph)%*%pca$rotation
# export pc rotation
write.table(pca$rotation,file="calib/pca.csv",quote = F,sep=',',col.names = F,row.names=F)

real=cbind(real,pcs)

#plots=list()
for(indic in indics){
  g=ggplot(real,aes_string(x='PC1',y='PC2',col=indic))
  g+geom_point()+stdtheme
  ggsave(file=paste0('res/real/pc1-pc2_col',indic,'.png'),width=22,height=20,units='cm')
  #plots[[indic]]=g+geom_point()
  g=ggplot(real,aes_string(x='PC2',y='PC3',col=indic))
  g+geom_point()+stdtheme
  ggsave(file=paste0('res/real/pc2-pc3_col',indic,'.png'),width=22,height=20,units='cm')
}
#multiplot(plotlist = plots,cols = 3)


## clustering to have objective points for calibration

knums = 2:10

ccoef=c();cknums=c()
for(k in knums){
  show(k)
  km = kmeans(real[,c('PC1','PC2')],k,iter.max = 1000,nstart=5000)
  ccoef=append(ccoef,km$betweenss/km$totss);cknums=append(cknums,k)
}

# clustering coefficient
g=ggplot(data.frame(ccoef=ccoef,knums=cknums),aes(x=knums,y=ccoef))
g+geom_point()+geom_line()+xlab('Number of clusters')+ylab('Between-cluster variance proportion')
ggsave(file='res/real/clustPC1-PC2_ccoef-knum.png',width=15,height=10,units = 'cm')

cdcoef = diff(ccoef[(length(ccoef)-length(knums)+1):length(ccoef)]);cdknums=knums[2:length(knums)]

# delta clustering coefficient
g=ggplot(data.frame(cdcoef=cdcoef,knums=cdknums),aes(x=cdknums,y=cdcoef))
g+geom_point()+geom_line()+xlab('Number of clusters')+ylab('Between-cluster variance proportion increase')
ggsave(file='res/real/clustPC1-PC2_deltaccoef-knum.png',width=15,height=10,units = 'cm')


# -> select k = 5 or 4

# export centers -> closest point to centroid / or centroid ?
# closest to have representation in the point cloud

set.seed(0)
#k=5
k=4
km = kmeans(real[,c('PC1','PC2')],k,iter.max = 1000,nstart=5000)

g=ggplot(data.frame(real[,c('PC1','PC2')],cluster=km$cluster),aes(x=PC1,y=PC2,color=as.character(cluster)))
g+geom_point(alpha=0.6)+scale_color_discrete(name="Cluster")+stdtheme
ggsave(file=paste0('res/real/clustPC1-PC2_k',k,'_points-colcluster.png'),width=22,height=20,units='cm')

# export center coords : directly with models names
objdata=data.frame()
for(model in c("expMixture","blocks","calibration")){objdata=rbind(objdata,data.frame(num=1:nrow(km$centers),km$centers,rep(model,nrow(km$centers))))}
write.table(objdata,file='calib/objectives.csv',sep=" ",col.names = F,row.names = F,quote=F)


real$cluster = km$cluster

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



######
## Typology of urban areas ?

clustcount = as.tbl(points@data) %>% group_by(ID_UMZ) %>% summarise(count=n(),clust1 = length(which(cluster==1))/n(),clust2 = length(which(cluster==2))/n(),clust3 = length(which(cluster==3))/n(),clust4 = length(which(cluster==4))/n())
umzprofiles = left_join(clustcount[clustcount$count>=10,],areas@data)
umzprofiles$diversity = 1 - (umzprofiles$clust1^2 + umzprofiles$clust2^2 + umzprofiles$clust3^2 + umzprofiles$clust4^2)

cor(umzprofiles[,c("clust1","clust2","clust3","clust4","diversity","Area","Pop1961","Pop1971","Pop1981","Pop1991","Pop2001","Pop2011","X","Y")])
# -> correlations between X,Y and cluster profiles !
# TODO check Fisher intervals for interesting correlations

summary(as.factor(as.character(umzprofiles$Country)))
# TODO include

chisq.test(cut(umzprofiles$diversity,10),umzprofiles$Country)
# -> ultra shitty p-value




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
g+geom_point()+stdtheme
#ggsave()


## TODO : additional plots calibration objectives and fitted pops ; plus extreme points with close simulated.



## specific cases
all[all$PC1<0.4&all$PC2>0.75&all$generator=="real",]
#-4.247058 48.45855


all[all$PC1>1.25&all$generator=="real",]
#4.215393 51.95148

