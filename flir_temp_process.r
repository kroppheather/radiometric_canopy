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
#files are 120x 160 pixels

############################################
### get photo times from rgb             ###
############################################
#get dji info
library(exifr)

taken <- read_exif("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_2_h11_04\\DJI_0019.jpg")[28]

#get gps position
pos <- read_exif("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_2_h11_04\\DJI_0019.jpg")[120]
