library(raster)
library(rgdal)
library(plyr)

flirT <- raster("z:\\users\\hkropp\\canopy\\20180704_110555_830.jpg")

#directory
dirD <- "z:\\data_repo\\field_data\\alaska_2018\\flir_out\\csv\\7_04"
dirO <- "z:\\data_repo\\field_data\\alaska_2018\\flir_out\\raster\\7_04"
#read in test file


files <-list.files(dirD)

img <- read.csv(paste0(dirD,"\\",files[1]),head=FALSE)


img2 <- t(data.matrix(img))

rIm <- raster(nrow=dim(img)[2],ncol=dim(img)[1], xmn=0,ymx=dim(img)[2],ymn=0,xmx=dim(img)[1])

rIm[] <-img2

fileN <- gsub(".ir.csv","",files)

writeRaster(rIm, paste0(dirO,"\\",fileN[1],".asc"))


flirT2R <- raster("z:\\users\\hkropp\\canopy\\7_04_ortho.tif", band=1)
flirT2G <- raster("z:\\users\\hkropp\\canopy\\7_04_ortho.tif", band=2)
flirT2B <- raster("z:\\users\\hkropp\\canopy\\7_04_ortho.tif", band=3)



############################################

###  set up directory                    ###

############################################

#get the directory of files to process

flightDIR <- "z:\\data_repo\\field_data\\alaska_2018\\flir_out\\csv\\7_04"







############################################

###  read in csv files of data           ###

###  and make a tiff                     ###

############################################

#get a list of the files in each directory


	flightFiles <- list.files(paste0(flightDIR))







#first get range of values for quantiles

#for colors





#declare the number of colors needed

Ncol <- 30





flightDataR <- list()

flightAll <- data.frame()

flightQ <- list()



	for(j in 1:length(flightFiles)){

		#read in csv

		flightDataR[[j]] <- read.csv(paste0(flightDIR,"\\",flightFiles[j]),head=FALSE)



	}

	flightAll <- ldply(flightDataR, data.frame)

	#get data quantiles

	flightQ <- quantile(as.vector(data.matrix(flightAll)), seq(0,1, length.out=Ncol))

	ccol <- rainbow(Ncol)
	ccrgb <- col2rgb(ccol)
	#make data frame
	colDF <- data.frame(temp=flightQ, red=ccrgb[1,], green=ccrgb[2,],
						blue=ccrgb[3,])
	colDF$colID <- seq(1,	dim(colDF)[1])	
	noDat <- c(temp=-999,red=255,green=255,blue=255, colID=31)
	colDF <- rbind(colDF,noDat)
	#get values goes by row
	dfT <- data.frame(red=getValues(flirT2R),green=getValues(flirT2G),blue=getValues(flirT2B))
	#find the color
	dfT$colID <- ifelse(dfT$red==colDF$red[1]&dfT$green==colDF$green[1]&dfT$blue==colDF$blue[1],colDF$temp[1],
					ifelse(dfT$red==colDF$red[2]&dfT$green==colDF$green[2]&dfT$blue==colDF$blue[2],colDF$temp[2],
					ifelse(dfT$red==colDF$red[3]&dfT$green==colDF$green[3]&dfT$blue==colDF$blue[3],colDF$temp[3],
					ifelse(dfT$red==colDF$red[4]&dfT$green==colDF$green[4]&dfT$blue==colDF$blue[4],colDF$temp[4],
					ifelse(dfT$red==colDF$red[5]&dfT$green==colDF$green[5]&dfT$blue==colDF$blue[5],colDF$temp[5],
					ifelse(dfT$red==colDF$red[6]&dfT$green==colDF$green[6]&dfT$blue==colDF$blue[6],colDF$temp[6],
					ifelse(dfT$red==colDF$red[7]&dfT$green==colDF$green[7]&dfT$blue==colDF$blue[7],colDF$temp[7],
					ifelse(dfT$red==colDF$red[8]&dfT$green==colDF$green[8]&dfT$blue==colDF$blue[8],colDF$temp[8],
					ifelse(dfT$red==colDF$red[9]&dfT$green==colDF$green[9]&dfT$blue==colDF$blue[9],colDF$temp[9],
					ifelse(dfT$red==colDF$red[10]&dfT$green==colDF$green[10]&dfT$blue==colDF$blue[10],colDF$temp[10],
					ifelse(dfT$red==colDF$red[11]&dfT$green==colDF$green[11]&dfT$blue==colDF$blue[11],colDF$temp[11],
					ifelse(dfT$red==colDF$red[12]&dfT$green==colDF$green[12]&dfT$blue==colDF$blue[12],colDF$temp[12],
					ifelse(dfT$red==colDF$red[13]&dfT$green==colDF$green[13]&dfT$blue==colDF$blue[13],colDF$temp[13],
					ifelse(dfT$red==colDF$red[14]&dfT$green==colDF$green[14]&dfT$blue==colDF$blue[14],colDF$temp[14],
					ifelse(dfT$red==colDF$red[15]&dfT$green==colDF$green[15]&dfT$blue==colDF$blue[15],colDF$temp[15],
					ifelse(dfT$red==colDF$red[16]&dfT$green==colDF$green[16]&dfT$blue==colDF$blue[16],colDF$temp[16],
					ifelse(dfT$red==colDF$red[17]&dfT$green==colDF$green[17]&dfT$blue==colDF$blue[17],colDF$temp[17],
					ifelse(dfT$red==colDF$red[18]&dfT$green==colDF$green[18]&dfT$blue==colDF$blue[18],colDF$temp[18],
					ifelse(dfT$red==colDF$red[19]&dfT$green==colDF$green[19]&dfT$blue==colDF$blue[19],colDF$temp[19],
					ifelse(dfT$red==colDF$red[20]&dfT$green==colDF$green[20]&dfT$blue==colDF$blue[20],colDF$temp[20],
					ifelse(dfT$red==colDF$red[21]&dfT$green==colDF$green[21]&dfT$blue==colDF$blue[21],colDF$temp[21],
					ifelse(dfT$red==colDF$red[22]&dfT$green==colDF$green[22]&dfT$blue==colDF$blue[22],colDF$temp[22],
					ifelse(dfT$red==colDF$red[23]&dfT$green==colDF$green[23]&dfT$blue==colDF$blue[23],colDF$temp[23],
					ifelse(dfT$red==colDF$red[24]&dfT$green==colDF$green[24]&dfT$blue==colDF$blue[24],colDF$temp[24],
					ifelse(dfT$red==colDF$red[25]&dfT$green==colDF$green[25]&dfT$blue==colDF$blue[25],colDF$temp[25],
					ifelse(dfT$red==colDF$red[26]&dfT$green==colDF$green[26]&dfT$blue==colDF$blue[26],colDF$temp[26],
					ifelse(dfT$red==colDF$red[27]&dfT$green==colDF$green[27]&dfT$blue==colDF$blue[27],colDF$temp[27],
					ifelse(dfT$red==colDF$red[28]&dfT$green==colDF$green[28]&dfT$blue==colDF$blue[28],colDF$temp[28],
					ifelse(dfT$red==colDF$red[29]&dfT$green==colDF$green[29]&dfT$blue==colDF$blue[29],colDF$temp[29],
					ifelse(dfT$red==colDF$red[30]&dfT$green==colDF$green[30]&dfT$blue==colDF$blue[30],colDF$temp[30],NA
					))))))))))))))))))))))))))))))

					
	flirR2 <- setValues(flirT2R, dfT$colID)				
	
	plot(flirR2)
	
	writeRaster(flirR2, paste0(dirO,"\\74_ortho.asc"))
	
	test <- unique(data.frame(red=dfT$red,green=dfT$green,blue=dfT$blue))