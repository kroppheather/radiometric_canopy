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
############################################
### set up directory                     ###
### and read in metadata files           ###
############################################
#input rgb paths
#must specify since multiple rgb flights are made around 
#thermal flights
rgbDIR <- c("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_4_p1",
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

taken <- read_exif("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_2_h11_04\\DJI_0019.jpg")[28]

#get gps position
pos <- read_exif("z:\\data_repo\\field_data\\alaska_2018\\rgb\\flight_7_2_h11_04\\DJI_0019.jpg")[120]
