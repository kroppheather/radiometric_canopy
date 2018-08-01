##########################################################
##########################################################
########Script for pairing flir pictures with  ###########
########GPS coordinates from the rgb camera    ###########
########and subset relevant flir pictures      ###########
########that are not associated with take of   ###########
########and landing                            ###########
##########################################################
##########################################################
##########################################################
library(plyr)
library(zoo)
############################################
### set up directory                     ###
### and read in metadata files           ###
############################################
#input rgb paths
#must specify since multiple rgb flights are made around 
#thermal flights
rgbDIR <- c("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_4_p1",
			"z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_7_p1")
			
#get flir directories
flirDIR <- c("z:\\data_repo\\field_data\\alaska_2018\\flir_out\\tiff\\7_04",
			"z:\\data_repo\\field_data\\alaska_2018\\flir_out\\tiff\\7_07")


#read in files that have identified take offs and landings and 
#blury flir images from turning
takeoff <- read.csv("z:\\data_repo\\field_data\\alaska_2018\\flir_out\\flir_discards.csv")			

blurry <- 	read.csv("z:\\data_repo\\field_data\\alaska_2018\\flir_out\\flir_turning.csv")	

#specify output directory to copy flir photos so that only
#the flight path ones are included for agisoft

outDIR <- c("z:\\data_repo\\field_data\\alaska_2018\\flir_agisoft\\7_04",
			"z:\\data_repo\\field_data\\alaska_2018\\flir_agisoft\\7_07")

############################################
### extract times from file names        ###
### from flir and                        ###
### blurry and landing photos            ###
############################################
flirNames <- list()
#list file names and exclude temperature legend
for(i in 1:length(flirDIR)){
	
	flirNames[[i]] <- list.files(paste0(flirDIR[i]))
	flirNames[[i]] <- flirNames[[i]][flirNames[[i]]!= "temperature_legend.jpg"]
}

#set up a dataframe with all of the file info
#for time and the simple file name
flirInfo <- list()
for(i in 1:length(flirDIR)){

	flirInfo[[i]] <- data.frame(filename=flirNames[[i]],
						flight.date=gsub(".ir.jpg","",flirNames[[i]]),
						year=as.numeric(substr(flirNames[[i]],1,4)),
						month=as.numeric(substr(flirNames[[i]],5,6)),
						day=as.numeric(substr(flirNames[[i]],7,8)),
						hours=as.numeric(substr(flirNames[[i]],10,11)),
						minutes=as.numeric(substr(flirNames[[i]],12,13)),
						seconds=as.numeric(substr(flirNames[[i]],14,15)),
						milliseconds=as.numeric(substr(flirNames[[i]],17,19)))
						
	flirInfo[[i]]$hourT <- flirInfo[[i]]$hours+((flirInfo[[i]]$minutes+((flirInfo[[i]]$seconds+(flirInfo[[i]]$milliseconds*0.001))/60))/60)			
}

#get info from takeoff file
takeoff$year <- as.numeric(substr(takeoff$filename,1,4))
takeoff$month <- as.numeric(substr(takeoff$filename,5,6))
takeoff$day <- as.numeric(substr(takeoff$filename,7,8))
takeoff$hours <- as.numeric(substr(takeoff$filename,10,11))
takeoff$minutes <- as.numeric(substr(takeoff$filename,12,13))
takeoff$seconds <- as.numeric(substr(takeoff$filename,14,15))
takeoff$milliseconds <- as.numeric(substr(takeoff$filename,17,19))

takeoff$hourT <- takeoff$hours+((takeoff$minutes+((takeoff$seconds+(takeoff$milliseconds*0.001))/60))/60)		


blurry$year <- as.numeric(substr(blurry$filename,1,4))
blurry$month <- as.numeric(substr(blurry$filename,5,6))
blurry$day <- as.numeric(substr(blurry$filename,7,8))
blurry$hours <- as.numeric(substr(blurry$filename,10,11))
blurry$minutes <- as.numeric(substr(blurry$filename,12,13))
blurry$seconds <- as.numeric(substr(blurry$filename,14,15))
blurry$milliseconds <- as.numeric(substr(blurry$filename,17,19))

blurry$hourT <- blurry$hours+((blurry$minutes+((blurry$seconds+(blurry$milliseconds*0.001))/60))/60)
	

############################################
### flag take off and landings           ###
############################################
flights <- unique(data.frame(flight.date=takeoff$flight.date))


takeoffL <- list()
landingL <- list()
battery.startL <- list()
battery.endL <- list()
for(i in 1:dim(flights)[1]){

	takeoffL[[i]] <- takeoff[takeoff$flight.date==flights$flight.date[i]&takeoff$action=="takeoff",]
	landingL[[i]] <- takeoff[takeoff$flight.date==flights$flight.date[i]&takeoff$action=="landing",]
	battery.startL[[i]] <- takeoff[takeoff$flight.date==flights$flight.date[i]&takeoff$action=="battery.start",]
	battery.endL[[i]] <- takeoff[takeoff$flight.date==flights$flight.date[i]&takeoff$action=="battery.end",]
}

#generate take removal flag
for(i in 1:dim(flights)[1]){
	
	flirInfo[[i]]$flag.T <- ifelse(flirInfo[[i]]$hourT<=takeoffL[[i]]$hourT,1,0)
	flirInfo[[i]]$flag.L <- ifelse(flirInfo[[i]]$hourT>=landingL[[i]]$hourT,1,0)
	if(length(battery.endL[[i]]$hourT)!=0){
		flirInfo[[i]]$flag.B <- ifelse(flirInfo[[i]]$hourT>=battery.startL[[i]]$hourT&flirInfo[[i]]$hourT<=battery.endL[[i]]$hourT,1,0)
	}else{flirInfo[[i]]$flag.B <- rep(0, dim(flirInfo[[i]])[1])}
}

############################################
### flag blurry and turns                ###
############################################
#get a list of each type

blurryL <- list()
turn.startL <- list()
turn.endL <- list()
for(i in 1:dim(flights)[1]){

	blurryL[[i]] <- blurry[blurry$flight.date==flights$flight.date[i]&blurry$action=="blurry",]
	turn.startL[[i]] <- blurry[blurry$flight.date==flights$flight.date[i]&blurry$action=="turning.start",]
	turn.endL[[i]] <- blurry[blurry$flight.date==flights$flight.date[i]&blurry$action=="turning.end",]
}

#generate take removal flag for blurry
for(i in 1:dim(flights)[1]){
	if(length(blurryL[[i]]$hourT)!=0){
		flirInfo[[i]]$flag.BL <- ifelse(flirInfo[[i]]$hourT==blurryL[[i]]$hourT,1,0)
	}else{flirInfo[[i]]$flag.BL <- rep(0, dim(flirInfo[[i]])[1])}
}

#make turning flag
#compile a list of all turning files in dataframe
turnsL <- list()
turnT2 <- numeric(0)
turnT <- numeric(0)
for(i in 1:dim(flights)[1]){
	for(j in 1:dim(turn.endL[[i]])[1]){
		turnT <- which(flirInfo[[i]]$hourT>=turn.startL[[i]]$hourT[[j]]&flirInfo[[i]]$hourT<=turn.endL[[i]]$hourT[[j]])	
		turnT2 <- append(turnT2,turnT)
	
	}
	turnsL[[i]] <- turnT2
}	

#now add a flag to the data frame
for(i in 1:dim(flights)[1]){
	flirInfo[[i]]$flag.TU <- rep(0, dim(flirInfo[[i]])[1])
	for(j in 1:length(turnsL[[i]])){
		flirInfo[[i]]$flag.TU[turnsL[[i]][j]] <- 1
	}	
}


############################################
### remove flagged items and copy files  ###
############################################

#add up all flags into single flag
flirInfoSub <- list()
for(i in 1:dim(flights)[1]){
	flirInfo[[i]]$flag.All <- flirInfo[[i]]$flag.T+flirInfo[[i]]$flag.L+flirInfo[[i]]$flag.B+flirInfo[[i]]$flag.BL+flirInfo[[i]]$flag.TU
	flirInfoSub[[i]] <- flirInfo[[i]][flirInfo[[i]]$flag.All==0,]
}

#copy the files that are relevent for the flights
for(i in 1:dim(flights)[1]){
	for(j in 1:dim(flirInfoSub[[i]])[1]){
			file.copy(paste0(flirDIR[i],"\\",flirInfoSub[[i]]$filename[j])
					,paste0(outDIR[i],"\\",flirInfoSub[[i]]$filename[j]))
		}	
}
	
############################################
### get photo times from rgb             ###
############################################
#get dji info
library(exifr)

#list all rgb files
rgbInfo <- list()
for(i in 1:dim(flights)[1]){
	
	rgbInfo[[i]] <- data.frame(filename=list.files(paste0(rgbDIR[i]),".JPG"))
	
}

#read in photo information
photoInfo <- read_exif(paste0(rgbDIR[1],"\\",rgbInfo[[i]]$filename[1]))

for(i in 1:dim(flights)[1]){
	pos <- character(0)
	timestamp <- character(0)
	altitude <- numeric(0)
	
	
	for(j in 1:dim(rgbInfo[[i]])[1]){
	photoInfo <- read_exif(paste0(rgbDIR[i],"\\",rgbInfo[[i]]$filename[j]))
	pos <- append(pos,photoInfo[120])
	timestamp <- append(timestamp,photoInfo[28])
	altitude <- append(altitude,photoInfo[117])
	}
	rgbInfo[[i]]$pos <- pos
	rgbInfo[[i]]$timestamp <- timestamp
	rgbInfo[[i]]$altitude <- altitude

}

#extract information in character strings
for(i in 1:dim(flights)[1]){
	rgbInfo[[i]]$year <- as.numeric(substr(rgbInfo[[i]]$timestamp,1,4))
	rgbInfo[[i]]$month <- as.numeric(substr(rgbInfo[[i]]$timestamp,6,7))
	rgbInfo[[i]]$day <- as.numeric(substr(rgbInfo[[i]]$timestamp,9,10))
	rgbInfo[[i]]$hours <- as.numeric(substr(rgbInfo[[i]]$timestamp,12,13))
	rgbInfo[[i]]$minutes <- as.numeric(substr(rgbInfo[[i]]$timestamp,15,16))
	rgbInfo[[i]]$seconds <- as.numeric(substr(rgbInfo[[i]]$timestamp,18,19))
	
}

#extract gps data
gpsST <- list()

for(i in 1:dim(flights)[1]){
	lat <- character(0)
	long <- character(0)

	for(j in 1:dim(rgbInfo[[i]])[1]){
		gpsST <- strsplit(as.character(rgbInfo[[i]]$pos[j]),"\\s")
		lat <- append(lat,as.numeric(gpsST[[1]][1]))
		long <- append(long,as.numeric(gpsST[[1]][2]))
	
	}
	rgbInfo[[i]]$lat <- lat
	rgbInfo[[i]]$long <- long

}

rgbInfo[[1]]$altitude <- as.numeric(rgbInfo[[1]]$altitude)

#now join to flir data
dataAll <- list()
for(i in 1:dim(flights)[1]){
	colnames(flirInfoSub[[i]])[1] <- "filenameIR"
	dataAll[[i]] <- join(rgbInfo[[i]],flirInfoSub[[i]], by=c("hours","minutes","seconds","month","day","year"), type="full")
	dataAll[[i]] <- dataAll[[i]][order(dataAll[[i]]$hours,dataAll[[i]]$minutes,dataAll[[i]]$seconds),]
}
#only focus on first flight because time info on second is messed up
#approximate missing data

	dataAll[[1]]$lat.fill <- na.approx(dataAll[[1]]$lat)
	dataAll[[1]]$long.fill <- na.approx(dataAll[[1]]$long)
	dataAll[[1]]$altitude <- as.numeric(dataAll[[1]]$altitude)
	dataAll[[1]]$alt <- na.approx(dataAll[[1]]$altitude)

dataOut <- data.frame(filename=dataAll[[1]]$filenameIR,dataAll[[1]][,23:25]	)

dataOut <- na.omit(dataOut)

write.table(dataOut, "z:\\data_repo\\field_data\\alaska_2018\\flir_agisoft\\coord_flir_7_04.csv", sep=",", row.names=FALSE)
	