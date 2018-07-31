##########################################################
########Script for generating tiff files from  ###########
########temperature data extracted from flir   ###########
######## jpeg to csv files using the script on ###########
###https://github.com/erickpeirson/radiometric-jpeg.git###
########Heather Kropp started July 2018        ###########
##########################################################
##########################################################
##########################################################

library(plyr)

############################################
###  set up directory                    ###
############################################
#get the directory of files to process
imgDIR <- "z:\\data_repo\\field_data\\alaska_2018\\flir_out\\csv"

#directory to create flight photos
outDIR <- "z:\\data_repo\\field_data\\alaska_2018\\flir_out\\tiff"

############################################
###  get list of flight directory        ###
############################################
#list all flight directories
flightDIRt <- list.dirs(imgDIR, full.names=FALSE)

#remove empty quote of parent directory
#each flight directory will at least have digits for the date
flightDIR <- flightDIRt[grepl("\\d",flightDIRt)] 

#make the output directory
for(i in 1:length(flightDIR)){

	dir.create(paste0(outDIR,"\\",flightDIR[i]))

}


############################################
###  read in csv files of data           ###
###  and make a tiff                     ###
############################################
#get a list of the files in each directory
flightFiles <- list()
for(i in 1:length(flightDIR)){
	flightFiles[[i]] <- list.files(paste0(imgDIR,"\\",flightDIR[i]))

}

#first get range of values for quantiles
#for colors


#declare the number of colors needed
Ncol <- 30


flightDataR <- list()
flightAll <- data.frame()
flightQ <- list()
for(i in 1:length(flightDIR)){
	for(j in 1:length(flightFiles[[i]])){
		#read in csv
		flightDataR[[j]] <- read.csv(paste0(imgDIR,"\\",flightDIR[i],"\\",flightFiles[[i]][j]),head=FALSE)

	}
	flightAll <- ldply(flightDataR, data.frame)
	#get data quantiles
	flightQ[[i]] <- quantile(as.vector(data.matrix(flightAll)), seq(0,1, length.out=Ncol))
	
}

#read in csvs and set up tiff for each one
flightDF <- list()
lyseq <- seq(0,Ncol-1)
#files are 120x 160 pixels
for(i in 1:length(flightDIR)){
	for(j in 1:length(flightFiles[[i]])){
		#read in data file and reorganize
		flightDataR[[j]] <- read.csv(paste0(imgDIR,"\\",flightDIR[i],"\\",flightFiles[[i]][j]),head=FALSE)
		
		flightDF[[j]] <- data.frame(Y=rep(seq(1,120),each=160),X=rep(seq(1,160),times=120),
								Temp=as.vector(data.matrix(flightDataR[[j]])))
		#assign a color for each pixel
		crange <- flightQ[[i]]
		ccol <- rainbow(Ncol)
		pcol <- character(0)
		for(k in 1:dim(flightDF[[j]])[1]){
			for(m in 1:(Ncol-1)){
				if(flightDF[[j]]$Temp[k]>=crange[m]&flightDF[[j]]$Temp[k]<crange[m+1]){
					pcol[k]<-ccol[m]
				}
			}	
	
		}
		flightDF[[j]]$pcol <- pcol
	
		jpeg(paste0(outDIR,"\\",flightDIR[i],"\\",gsub(".csv","",flightFiles[[i]][j]),".jpg"),
			width=640,height=480, units="px")
			
		par(mai=c(0,0,0,0))	
		plot(c(0,1),c(0,1), type="n", ylim=c(120,0),xlim=c(0,160), xaxs="i",yaxs="i",
			xlab=" ", ylab=" ")
			
			for(k in 1:dim(flightDF[[j]])[1]){
				polygon(c(flightDF[[j]]$X[k]-1,flightDF[[j]]$X[k]-1,flightDF[[j]]$X[k],flightDF[[j]]$X[k]),
						c(flightDF[[j]]$Y[k]-1,flightDF[[j]]$Y[k],flightDF[[j]]$Y[k],flightDF[[j]]$Y[k]-1),
						border=NA, col=flightDF[[j]]$pcol[k])
			}			
			
		dev.off()	
	}
	#make a legend for the flight color and temp
	jpeg(paste0(outDIR,"\\",flightDIR[i],"\\temperature_legend.jpg"),width=500,height=1000, units="px")
		layout(matrix(c(1),ncol=1), width=c(lcm(5)), height=c(lcm(30)))	
	
		plot(c(0,1),c(0,1),  type="n", ylim=c(0,Ncol),xlim=c(0,1), xaxs="i",yaxs="i",
			xlab=" ", ylab=" ",axes=FALSE)
			for(k in 1:(Ncol-1)){
				polygon(c(0,0,1,1), c(lyseq[k],lyseq[k]+1,lyseq[k]+1,lyseq[k]),col=ccol[k], border=NA)
			}
		
		axis(4,lyseq,round(flightQ[[i]],2),las=2,cex.axis=2)
	dev.off()
}	


############################################
### get photo times from rgb             ###
############################################
#get dji info
library(exifr)

taken <- read_exif("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_2_h11_04\\DJI_0019.jpg")[28]

#get gps position
pos <- read_exif("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_2_h11_04\\DJI_0019.jpg")[120]
