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

dirS <- "z:\\projects\\thermal_canopy\\leaf_select"

#######################################
#####read in data                 ##### 
#######################################

#read in data
datD <- read.table(paste0(dirI,"\\h8.csv"), sep=";",head=FALSE)

#read in subset of pixels for shrub
datS <- read.table(paste0(dirS,"\\h8.csv"),sep=",",head=TRUE)


#reformat data

datD2 <- data.frame(Y=rep(seq(0,287),times=382),X=rep(seq(0,381),each=288),
		Temp=as.vector(data.matrix(datD[,1:382])))
		

#match on subset

subS <- join(datD2,datS, by=c("Y","X"), type="inner")

#######################################
#####check subsets match in plot  ##### 
#######################################
		
#make a plot of the image

#b<-layout(matrix(c(1,2),ncol=2),width=c(lcm(15),lcm(5)), height=c(lcm(15),lcm(15))) 
#layout.show(b)
b<-layout(matrix(c(1),ncol=1),width=c(lcm(15)), height=c(lcm(15))) 
#make a function
#inputs include the x and y coordinates, the temperature, and the number of temperature increments to show colors for
thermal.plot<-function(x.input,y.input,T.input, ncolors){

layout.show(b)

#set up color increments
crange<-seq(range(T.input)[1],range(T.input)[2],length.out=ncolors)

ccol<-heat.colors(ncolors)
pcol<-character(0)
for(i in 1:length(x.input)){
	for(j in 1:(ncolors-1)){
		if(T.input[i]>=crange[j]&T.input[i]<crange[j+1]){
			pcol[i]<-ccol[j]
			}
	
	}

}


par(c(0,0,0,.5))
plot(c(0,max(x.input)),c(0,max(y.input)),xlim=c(0,max(x.input)),
	ylim=c(max(y.input),0),
	xlab=" " ,ylab=" ", type="n", axes=FALSE)

for(i in 1:length(y.input)){
polygon(c(x.input[i]-.5,x.input[i]-.5,x.input[i]+.5,x.input[i]+.5),
	c(y.input[i]-.5,y.input[i]+.5,y.input[i]+.5,y.input[i]-.5), col=pcol[i],border="NA")

}
#pp<-seq(1,ncolors)
#xp<-rep(0,ncolors)
#par(c(.5,0,0,0))

#plot(c(0,1),c(0,ncolors+.5),type="n",axes=FALSE,xlab=" ", ylab=" ")
#for(i in 1:ncolors){
#polygon( c(xp[i],xp[i],xp[i]+1,xp[i]+1),c(pp[i]-.5,pp[i]+.5,pp[i]+.5,pp[i]-.5),col=ccol[i])
#}
#axis(4,seq(1,ncolors),round(crange,2),las=2)

}		
		
thermal.plot(datD2$X,datD2$Y,datD2$Temp,20)	
points(rep(0,288),seq(0,287),pch=19)	
points(seq(0,381),rep(0,382),pch=19,col="blue")	
points(datS$X,datS$Y, )		
thermal.plot(subS$X,subS$Y,subS$Temp,20)

#######################################
#####calculate stats on subset    ##### 
#######################################	


aveS <- mean(subS$Temp)	
sdS <- sd(subS$Temp)