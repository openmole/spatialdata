
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
realrotation = pca$rotation
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
g+geom_point(alpha=0.6)+
  geom_point(data=as.data.frame(km$centers),aes(x=PC1,y=PC2),inherit.aes = F,col='black',pch='+',size=2)+
  scale_color_discrete(name="Cluster")+stdtheme
ggsave(file=paste0('res/real/clustPC1-PC2_k',k,'_points-colcluster.png'),width=22,height=20,units='cm')

# export center coords : directly with models names
objdata=data.frame()
for(model in c("expMixture","blocks","percolation")){objdata=rbind(objdata,data.frame(num=1:nrow(km$centers),km$centers,rep(model,nrow(km$centers))))}
write.table(objdata,file='calib/objectives.csv',sep=" ",col.names = F,row.names = F,quote=F)


real$cluster = km$cluster
realcenters = km$centers

# save real
write.table(real,file='morpho/realpoints.csv',row.names = F,sep=',')

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
cor.test(umzprofiles$diversity,umzprofiles$X)

cor.test(umzprofiles$diversity,umzprofiles$Y)

summary(as.factor(as.character(umzprofiles$Country)))
# TODO include

chisq.test(cut(umzprofiles$diversity,50),umzprofiles$Country)
# -> ultra shitty p-value




####
# Simu data

#res <- as.tbl(read.csv('exploration/20190306_103425_LHS_GRID.csv'))
res <- as.tbl(read.csv('../openmole/exploration/20190311_181654_LHS_GRID.csv'))

resdir = 'res/20190311_181654_LHS_GRID/';dir.create(resdir)

# filter NAs
res=res[apply(res[,indics],1,function(r){length(which(is.na(r)))==0}),]
# filter as for real
res = res[res$density>0.05&res$density<0.8,]

params=c("generator","blocksMaxSize","blocksMinSize","blocksNumber","expMixtureCenters","expMixtureRadius","expMixtureThreshold","percolationBordPoints","percolationLinkWidth","percolationProba","randomDensity","replication","id")

summary(res)
#summary(res[res$generator=="percolation",params]) # for boundaries of calibration

morph=res[,indics]
for(j in 1:ncol(morph)){morph[,j]=(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca = prcomp(morph)
summary(pca)

# not useful
#pcs = as.matrix(morph)%*%pca$rotation
#res = cbind(res,pcs)


###### TODO

# intra-run variability

sres = res[,c(indics,'id','generator')] %>% group_by(id,generator) %>% summarize(
  sdMoran=sd(moran),meanMoran=mean(moran),sharpeMoran=abs(meanMoran/sdMoran),
  sdAvgDistance=sd(avgDistance),meanAvgDistance=mean(avgDistance),sharpeAvgDistance=meanAvgDistance/sdAvgDistance,
  sdDensity=sd(density),meanDensity=mean(density),sharpeDensity=meanDensity/sdDensity,
  sdComponents=sd(components),meanComponents=mean(components),sharpeComponents=meanComponents/sdComponents,
  sdAvgDetour=sd(avgDetour),meanAvgDetour=mean(avgDetour),sharpeAvgDetour=meanAvgDetour/sdAvgDetour,
  sdAvgBlockArea=sd(avgBlockArea),meanAvgBlockArea=mean(avgBlockArea),sharpeAvgBlockArea=meanAvgBlockArea/sdAvgBlockArea,
  sdAvgComponentArea=sd(avgComponentArea),meanAvgComponentArea=mean(avgComponentArea),sharpeAvgComponentArea=meanAvgComponentArea/sdAvgComponentArea,
  sdFullDilationSteps=sd(fullDilationSteps),meanFullDilationSteps=mean(fullDilationSteps),sharpeFullDilationSteps=meanFullDilationSteps/sdFullDilationSteps,
  sdFullErosionSteps=sd(fullErosionSteps),meanFullErosionSteps=mean(fullErosionSteps),sharpeFullErosionSteps=meanFullErosionSteps/sdFullErosionSteps
)

sres=sres[apply(sres,1,function(r){length(which(is.na(r)))==0}),]
#sres=sres[apply(sres,1,function(r){length(which(r<1e-8))==0}),]
summary(sres)




######
dummyParams = data.frame(matrix(0,nrow(real),length(params)-1));names(dummyParams)<-params[-1]
all = rbind(cbind(real[,c(indics,"lon","lat")],generator=rep("real",nrow(real)),dummyParams),cbind(res[,c(indics,params)],lon=rep(0,nrow(res)),lat=rep(0,nrow(res))))

morph=all[,indics]
for(j in 1:ncol(morph)){morph[,j]=(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca = prcomp(morph)
summary(pca)
pcs = as.matrix(morph)%*%pca$rotation
all = cbind(all,pcs)

#
rotcentroids = (as.matrix(cbind(km$centers,matrix(0,nrow = nrow(km$centers),ncol=length(indics)-ncol(km$centers))))%*%realrotation)%*%pca$rotation

g=ggplot(all,aes(x=PC1,y=PC2,color=generator,#size=ifelse(generator=="real",0.01,0.00005),
                 pch=ifelse(generator=="real",'+','.'),
                 alpha=ifelse(generator=="real",0.5,0.01)))
g+geom_point()+
  geom_point(data=as.data.frame(rotcentroids[,c('PC1','PC2')]),aes(x=PC1,y=PC2),inherit.aes = F,col='black',pch='+')+
  scale_shape_discrete(guide=FALSE)+scale_alpha_continuous(guide=FALSE)+stdtheme
ggsave(file=paste0(resdir,'lhscalib.png'),width=32,height=30,units='cm')



# plot projected in REAL principal component space
normalized = all[,indics]
for(j in 1:ncol(normalized)){normalized[,j]=(normalized[,j]-min(mins[j]))/(maxs[j]-mins[j])}
rotated=cbind(as.data.frame(as.matrix(normalized)%*%realrotation),generator = all[,c("generator")])
# reorder factor
#rotated$generator<-factor(rotated$generator,levels=rotated$generator)
#rotated=rotated[sample(1:nrow(rotated),1000),]
g=ggplot(rotated[rotated$generator!='real',],aes(x=PC1,y=PC2,color=generator)) #size=ifelse(generator=="real",0.01,0.00005),
                 #shape=ifelse(generator=="real","d","a"),
                 #alpha=ifelse(generator=="real",0.5,0.01)))
g+geom_point(alpha = 0.5,pch='+',size=1.6)+
  geom_point(data=rotated[rotated$generator=='real',],aes(x=PC1,y=PC2),inherit.aes = F,col='blue',size=1.5,alpha=0.8,pch='+')+
  geom_point(data=data.frame(realcenters[,c('PC1','PC2')]),aes(x=PC1,y=PC2,shape="a"),inherit.aes = F,fill='black',size=3)+
  geom_text(data=data.frame(realcenters[,c('PC1','PC2')],label=1:4),aes(x=PC1,y=PC2,label=label),hjust=2, vjust=2,inherit.aes = F,size=6)+
  scale_shape_discrete(guide=FALSE,solid=T)+scale_alpha_continuous(guide=FALSE)+
  scale_color_discrete()+
  #discrete_scale(aes(size=10),breaks=c("blocks","random","percolation","expMixture"),labels=c("blocks","random","percolation","expMixture"),scale_name="Generator")+
  stdtheme
ggsave(file=paste0(resdir,'lhscalib_projrealpcs.png'),width=32,height=30,units='cm')



## TODO : additional plots calibration objectives and fitted pops ; plus extreme points with close simulated.



###### closest sim points

for(objnum in 1:nrow(km$centers)){
  o1=km$centers[objnum,1];o2=km$centers[objnum,2]
  show(paste0(objnum,' pc1 = ',o1,' ; pc2 = ',o2))
  for(generator in unique(as.character(res$generator))){
  show(paste0('generator = ',generator))
  dists = apply(rotated[rotated$generator==generator,c('PC1','PC2')],1,function(r){sqrt((r[1]-o1)^2+(r[2]-o2)^2)})
  allgen = all[all$generator==generator,]
  allgen$dists = dists
  sallgens = allgen %>% group_by(id) %>% summarise(count=n(),distsd=sd(dists),dist=mean(dists))
  sallgens = sallgens[sallgens$count==100,]# !
  show(sallgens[sallgens$dist==min(sallgens$dist),])
}
}

#### closest real points
for(objnum in 1:nrow(km$centers)){
  o1=km$centers[objnum,1];o2=km$centers[objnum,2]
  show(paste0(objnum,' pc1 = ',o1,' ; pc2 = ',o2))
  dists = apply(rotated[rotated$generator=='real',c('PC1','PC2')],1,function(r){sqrt((r[1]-o1)^2+(r[2]-o2)^2)})
  allgen = all[all$generator=='real',]
  show(paste0(' min = ',min(dists)))
  show(allgen[dists<quantile(dists,0.001),c('lon','lat')])
}



##### closest in neighborhood of each objective
# for(objnum in 1:nrow(km$centers)){
#   o1=km$centers[objnum,1];o2=km$centers[objnum,2]
#   show(paste0(objnum,' pc1 = ',o1,' ; pc2 = ',o2))
#   dists = apply(rotated[rotated$generator=='real',c('PC1','PC2')],1,function(r){sqrt((r[1]-o1)^2+(r[2]-o2)^2)})
#   allgen = all[all$generator=='real',]
#   realpoints = allgen[dists<0.02,]
#   
#   dists = apply(rotated[rotated$generator!='real',c('PC1','PC2')],1,function(r){sqrt((r[1]-o1)^2+(r[2]-o2)^2)})
#   allgen = all[all$generator!='real',]
#   simpoints = allgen[dists<0.02,]
#   
#   full=rbind(realpoints,simpoints)
#   d = as.matrix(dist(full))
#   diag(d)<-Inf
#   
#   rowmins = apply(d[(nrow(realpoints)+1):nrow(full),1:nrow(realpoints)],1,min)
#   rowminind=which(rowmins==min(rowmins))
#   show(simpoints[rowminind,])
#   
#   colmin = d[(nrow(realpoints)+rowminind),1:nrow(realpoints)]
# }




######
# same plot with average on repetitions

sres = res %>% group_by(id,generator) %>% summarise(moran=mean(moran),avgDistance=mean(avgDistance),avgDistance=mean(avgDistance),
                                   density=mean(density),components=mean(components),avgDetour=mean(avgDetour),
                                   avgBlockArea=mean(avgBlockArea),avgComponentArea=mean(avgComponentArea),
                                   fullDilationSteps=mean(fullDilationSteps),fullErosionSteps=mean(fullErosionSteps),
                                   count=n()
                                   )
sres=sres[sres$count>75,]

all = rbind(cbind(real[,c(indics,"lon","lat")],generator=rep("real",nrow(real))),cbind(sres[,c(indics,"generator")],lon=rep(0,nrow(sres)),lat=rep(0,nrow(sres))))
morph=all[,indics]
for(j in 1:ncol(morph)){morph[,j]=(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca = prcomp(morph)
pcs = as.matrix(morph)%*%pca$rotation
all = cbind(all,pcs)


g=ggplot(all,aes(x=PC1,y=PC2,color=generator,size=ifelse(generator=="real",0.0001,0.00005),alpha=ifelse(generator=="real",0.1,0.01)))
g+geom_point()+scale_size_continuous(guide=FALSE)+scale_alpha_continuous(guide=FALSE)+stdtheme
ggsave(file=paste0(resdir,'lhscalib_averages.png'),width=32,height=30,units='cm')







###########

## specific cases
#all[all$PC1<0.4&all$PC2>0.75&all$generator=="real",]
#-4.247058 48.45855

#all[all$PC1>1.25&all$generator=="real",]
#4.215393 51.95148

