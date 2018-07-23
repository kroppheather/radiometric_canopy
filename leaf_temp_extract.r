##########################################################
########extraction of selected features        ###########
########from canopy thermal imagery            ###########
########collected from the TIM camera          ###########
########Heather Kropp started July 2018        ###########
##########################################################
##########################################################
library(plyr)

#######################################
#####set directories              ##### 
#######################################

#csvs of entire image temperature

dirI <- "z:\\data_repo\\field_data\\alaska_2018\\TIM\\csv"

#subset csv

dirS <- "z:\\projects\\thermal_canopy\\healy\\leaf_temp\\subset"

#output subsetted images
outS <- "z:\\projects\\thermal_canopy\\healy\\leaf_temp\\subset_out\\image"
#subset csv
outD <- "z:\\projects\\thermal_canopy\\healy\\leaf_temp\\subset_out"

#######################################
#####read in data                 ##### 
#######################################
#read in picture descriptions
descD <- read.csv("z:\\data_repo\\field_data\\alaska_2018\\TIM\\Picture_descriptions.csv")

#######################################
#####read in image data           ##### 
#######################################
#list image files
fileD <- list.files(paste0(dirI))
fileS <- list.files(paste0(dirS))
nameS <- gsub(".csv","",fileS)

#read in full files. Only read in names from subset since not interested in the other files
fullImage <- list()
subImage <- list()
for(i in 1:length(fileS)){
		fullImage[[i]] <- read.table(paste0(dirI,"\\",fileS[i]), sep=";",head=FALSE)
		subImage[[i]] <- read.table(paste0(dirS,"\\",fileS[i]),sep=",",head=TRUE)

}


#reformat data
datImage <- list()
for(i in 1:length(fileS)){
	datImage[[i]] <-	data.frame(Y=rep(seq(0,287),times=382),X=rep(seq(0,381),each=288),
			Temp=as.vector(data.matrix(fullImage[[i]][,1:382])))

}

#match on subset
subS <- list()
for(i in 1:length(fileS)){
	subS[[i]] <- join(datImage[[i]],subImage[[i]], by=c("Y","X"), type="inner")
}


#######################################
#####check subsets match in plot  ##### 
#######################################
		
#make a plot of the image

#b<-layout(matrix(c(1,2),ncol=2),width=c(lcm(15),lcm(5)), height=c(lcm(15),lcm(15))) 
#layout.show(b)
b<-layout(matrix(seq(1,3),ncol=3),width=c(lcm(15),lcm(15),lcm(5)), height=c(lcm(15),lcm(15),lcm(15))) 
#make a function
#inputs include the x and y coordinates, the temperature, and the number of temperature increments to show colors for
subThermal.plot<-function(fileP,x.inputFull,y.inputFull,T.inputFull, x.inputSub,y.inputSub,T.inputSub,ncolors){
jpeg(fileP, width=1200,height=700,
			quality=100,units="px")
			
b<-layout(matrix(seq(1,3),ncol=3),width=c(lcm(15),lcm(15),lcm(5)), height=c(lcm(15),lcm(15),lcm(15)))			
layout.show(b)

quantR <- seq(0,1,length.out=ncolors)

#set up color increments
crange<-quantile(T.inputFull,quantR)

ccol<-heat.colors(ncolors)
pcolFull<-character(0)
for(i in 1:length(x.inputFull)){
	for(j in 1:(ncolors-1)){
		if(T.inputFull[i]>=crange[j]&T.inputFull[i]<crange[j+1]){
			pcolFull[i]<-ccol[j]
			}
	
	}

}

pcolSub<-character(0)
for(i in 1:length(x.inputSub)){
	for(j in 1:(ncolors-1)){
		if(T.inputSub[i]>=crange[j]&T.inputSub[i]<crange[j+1]){
			pcolSub[i]<-ccol[j]
			}
	
	}

}



par(c(0,0,0,.5))
plot(c(0,max(x.inputFull)),c(0,max(y.inputFull)),xlim=c(0,max(x.inputFull)),
	ylim=c(max(y.inputFull),0),
	xlab=" " ,ylab=" ", type="n", axes=FALSE)

for(i in 1:length(y.inputFull)){
polygon(c(x.inputFull[i]-.5,x.inputFull[i]-.5,x.inputFull[i]+.5,x.inputFull[i]+.5),
	c(y.inputFull[i]-.5,y.inputFull[i]+.5,y.inputFull[i]+.5,y.inputFull[i]-.5), col=pcolFull[i],border="NA")

}
box(which="plot")

par(c(0,0,0,.5))
plot(c(0,max(x.inputFull)),c(0,max(y.inputFull)),xlim=c(0,max(x.inputFull)),
	ylim=c(max(y.inputFull),0),
	xlab=" " ,ylab=" ", type="n", axes=FALSE)

for(i in 1:length(y.inputSub)){
polygon(c(x.inputSub[i]-.5,x.inputSub[i]-.5,x.inputSub[i]+.5,x.inputSub[i]+.5),
	c(y.inputSub[i]-.5,y.inputSub[i]+.5,y.inputSub[i]+.5,y.inputSub[i]-.5), col=pcolSub[i],border="NA")

}
box(which="plot")

pp<-seq(1,ncolors)
xp<-rep(0,ncolors)
par(c(.5,0,0,0))

plot(c(0,1),c(0,ncolors+.5),type="n",axes=FALSE,xlab=" ", ylab=" ")
for(i in 1:ncolors){
	polygon( c(xp[i],xp[i],xp[i]+1,xp[i]+1),c(pp[i]-.5,pp[i]+.5,pp[i]+.5,pp[i]-.5),col=ccol[i])
}
axis(4,seq(1,ncolors),round(crange,3),las=2)
dev.off()
}		
for(i in 1:length(fileS)){

	subThermal.plot(paste0(outS,"\\",nameS[i],".jpg"),datImage[[i]]$X,datImage[[i]]$Y,datImage[[i]]$Temp,subS[[i]]$X,subS[[i]]$Y,subS[[i]]$Temp,20)	
}

#######################################
#####calculate stats on subset    ##### 
#######################################	
summaryStat <- list()
for(i in 1:length(fileS)){
	summaryStat[[i]] <- data.frame(photo_name=nameS[i],average=mean(subS[[i]]$Temp), SD=sd(subS[[i]]$Temp))
}
summaryS <- ldply(summaryStat,data.frame)

#join with photo descriptions
leafData <- join(summaryS,descD, by="photo_name",type="left")

aggregate(leafData$average,by=list(leafData$species1,leafData$doy),FUN="length")


write.table(leafData,paste0(outD,"\\leaf_temp_ir.csv"),sep=",", row.names=FALSE) 