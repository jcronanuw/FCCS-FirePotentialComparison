#Title: FCCS rate of spread comparison for Eglin Air Force Base custom fuelbeds.
#Purpose:
#1) To review evlauation of synchronicity between three datasets describing FCCS fuelbeds
#a) FCCS .xml files developed by Anne Andreu and delievered on August 9, 2016. These were submitted
#to Joint Fire Science Program and Eglin Air Force Base as part of the final report.
#b) Fuelbeds listed in state and transition models (STMs)
#c) fuelbeds listed in lookup tables used to run Fuelbed Dynamic Model (FDM) simulations.

#2) To compare predicted FCCS Benchmark ROS as probability of ignition (PIG) values against 
#expert opintion-derived PIG values and to identify new environmental scenarios and transformations to apply 
#to FCCS ROS predictions to create a realistic and objectively derived set of PIGS.
#Author: Jim Cronan
#Institution: Pacific Wildland Fire Sciences Laboratory (USDA Forest Service)
#Date: 18-Nov-2017

#Remove objects and reset functionS
rm(list=ls())
dev.off()


##############################################################################################################
##############################################################################################################
#STEP 1: ADMINISTRATIVE TASKS

#Set workinng directory
input_path <- "C:/Users/jcronan/Documents/GitHub/FCCS-FirePotentialComparison"     
setwd(input_path)

##############################################################################################################
##############################################################################################################
#STEP 2: CREATE FUNCTIONS

#Create a function that will recode the five digit fuelbed numbers as seven digit fuelbed numbers used
#in FDM
recode <- function(x) {
  as.numeric(paste(strsplit(x, "")[[1]][1], 
                   "0", 
                   strsplit(x, "")[[1]][2],
                   strsplit(x, "")[[1]][3],
                   strsplit(x, "")[[1]][4],
                   "0", 
                   strsplit(x, "")[[1]][5], sep = ""))
}

##############################################################################################################
##############################################################################################################
#STEP 3: LOAD & TEST DATA

#Load evaluation of synchronicity between STM, .xml, and FDM fuelbeds
fbsa <- read.table("inputs/fuelbed_synchronicty.csv", 
                  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Recode the $andreu_fuelbed_no integers so they are the same as the FDM inputs.
fbra <- as.character(fbsa$andreu_fuelbed_no)

#Apply function
fbrb <- sapply(fbra, recode)
fbsa$andreu_fuelbed_no <- fbrb

#strip out columns from sef_lut_all.csv and instead load that dataset from EglinAirForceBase repo.
fbsb <- fbsa[,1:4]

#Load evaluation of synchronicity between STM, .xml, and FDM fuelbeds
lut <- read.table("C:/Users/jcronan/Documents/GitHub/EglinAirForceBase/inputs/sef_lut_all.csv", 
                   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Compare fuelbed lists in scynchronicity file and lookup table. If they are different kill the script.
all.equal(fbsb$fuelbed, lut$fuelbed)

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 1
#Transformations -- None
fccs_fireBehavior <- read.table("inputs/fccs_ros_moistScenario_1_tansformation_none.csv", 
                          header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

##############################################################################################################
##############################################################################################################
#STEP 3: WRITE CODE

ros11 <- data.frame(number = potentials_1_1$Fuelbed_number, name = potentials_1_1$Fuelbed_name, 
                    ros = potentials_1_1$Custom_ROS)


barplot(height = ros11$ros, width = 1, names.arg = ros11$name)

?barplot



