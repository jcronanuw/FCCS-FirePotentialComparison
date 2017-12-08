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

#Libraries
library(ggplot2)#used for ggplot()
library(gridBase)#baseViewports
library(grid)#pusViewport

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
fbsb <- fbsa[,c(1:4,25)]

#Load evaluation of synchronicity between STM, .xml, and FDM fuelbeds
lut <- read.table("C:/Users/jcronan/Documents/GitHub/EglinAirForceBase/inputs/sef_lut_all.csv", 
                   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Compare fuelbed lists in scynchronicity file and lookup table. If they are different kill the script.
all.equal(fbsb$fuelbed, lut$fuelbed)

#Remove unused fuelbeds (these are fuelbeds that may exist in the fuelbed map, but are not in STMs).
fbsc <- fbsb[!is.na(fbsb$andreu_fuelbed_no) == T,]

#Before you apply the max Benchmark ROS to determine PIGs look at their distribution. 
barplot(fbsc$benchmark_ros)

#There is one Benchmark ROS that is way higher than the rest. This will suppress PIGs
max(fbsc$benchmark_ros)
fbsc[fbsc$benchmark_ros == max(fbsc$benchmark_ros),]

#This is fuelbed 1071000 -- This is salt marsh
#Use the next highest value
ros_test <- fbsc[order(fbsc$benchmark_ros),]
ros_test[(length(ros_test[,1]) -5):length(ros_test[,1]),]
max_ros <- ros_test$benchmark_ros[(length(ros_test[,1]) -1)]

#Convert benchmark ROS into probability of ignition (PIG)
bros_pig <- round(fbsc$benchmark_ros/max_ros,4)

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 1
#Transformations -- None
fccs_fireBehavior <- read.table("inputs/fccs_ros_moistScenario_1_tansformation_none.csv", 
                          header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Break fuelbeds into component parts and create a dataframe that will be source for visualizations.
fca <- as.character(fbsc$fuelbed)
fcb <- strsplit(fca, "")
fcc <- matrix(as.numeric(unlist(fcb)), nrow = length(fcb), ncol = length(fcb[[1]]), byrow = T)

#Calculate the difference between your expert opinion probability of ignition and the probability
#of ignition derived from FCCS benchmark rate of spread predictions.
pig_dif <- fbsc$cronan_pig - bros_pig

#Create a data frame that can be used for analysis.
fbsd <- data.frame(fuelbed = fbsc$fuelbed, andreu_fuelbed_no = fbsc$andreu_fuelbed_no, 
                   benchmark_ros = fbsc$benchmark_ros, fccs_pig = bros_pig, 
                   cronan_pig = fbsc$probability_of_ignition, 
                   diff_pig = fbsc$probability_of_ignition - bros_pig, 
                   topo = fcc[,1], 
                   cover = fcc[,3], treatment = fcc[,4], mfri = fcc[,5], age = fcc[,7])
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################


#                                       ITERATION 1
#EVALUATE PROBABILITY OF IGNITION VALUES IN THE CURRENT FDM STM/LOOKUP TABLES




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 4: 
#NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.

#Subset wet flatlands fuelbeds you want to look at
#Wet Flatlands
wet_flatlands <- fbsd[fbsd$fuelbed %in% c(1011101:1011106, 
                                  1011201:1011206, 
                                  1011301:1011306, 
                                  1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands
mesic_flatlands <- fbsd[fbsd$fuelbed %in% c(2011101:2011106, 
                                  2011201:2011206, 
                                  2011301:2011306, 
                                  2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

#Mesic Uplands
mesic_uplands <- fbsd[fbsd$fuelbed %in% c(3011101:3011106, 
                                  3011201:3011206, 
                                  3011301:3011306, 
                                  3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

#Xeric Uplands
xeric_uplands <- fbsd[fbsd$fuelbed %in% c(4011101:4011106, 
                                  4011201:4011206, 
                                  4011301:4011306, 
                                  4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]

#I'm starting with a subset of natural fuels and organizing by mean fire return intervals with stand 
#age nested in each mfri group. The predictions from Eglin staff should show a decrease in PIG as mfri 
#lengthens due to less grass/longleaf litter and higher fine fuel moisture. The Eglin staff predictions 
#increase with stand age for frequently burned fuel types as canopy and development of wiregrass 
#contributes to flammable fine fuels on the forest floor.

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
layout.show(nf)

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_cat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wet_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wet_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Flatlands
mf_cat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Flatlands", 
                               names(mesic_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(mf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN EACH TOPOGRAPHIC POSITION; START WITH WET FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 5: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands
wet_flatlands <- fbsd[fbsd$topo == 1 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wet_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wet_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 6: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds in mesic flatlands
mesic_flatlands <- fbsd[fbsd$topo == 2 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                mesic_flatlands$mfri), 
                   names.arg = NULL, 
                   main = paste("Mesic Flatlands", 
                                names(mesic_flatlands)[type], 
                                sep = " "), 
                   ylim = c(0, max(mesic_flatlands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 7: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds in mesic uplands
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                mesic_uplands$mfri), 
                   names.arg = NULL, 
                   main = paste("Mesic Uplands", 
                                names(mesic_uplands)[type], 
                                sep = " "), 
                   ylim = c(0, max(mesic_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN XERIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 8: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds in xeric uplands
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                xeric_uplands$mfri), 
                   names.arg = NULL, 
                   main = paste("Xeric Uplands", 
                                names(xeric_uplands)[type], 
                                sep = " "), 
                   ylim = c(0, max(xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN  PLANTATIONS

##############################################################################################################
##############################################################################################################
#STEP 9: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic region.
wet_flatlands <- fbsd[fbsd$topo == 1 & fbsd$treatment == 5,]
mesic_flatlands <- fbsd[fbsd$topo == 2 & fbsd$treatment == 5,]
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment == 5,]
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment == 5,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
layout.show(nf)

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_cat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wet_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wet_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Flatlands
mf_cat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Flatlands", 
                               names(mesic_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(mf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN NATURAL FUELS

##############################################################################################################
##############################################################################################################
#STEP 10: 
#ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from natural fuels by topographic position
#Note ---- there are no restoration fuelbeds in wet or mesic flatlands.
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment %in% c(2,3,4),]
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment %in% c(2,3,4),]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
dev.off()
nf <- layout(matrix(c(1,2), 2, 1, byrow = TRUE))
layout.show(nf)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Trends are not quite as expected.
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#PIGs should decline with lengthening mFRI and increase with stand age, they do not.


#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN PLANTATION FUELS

#############################################################################################################
##############################################################################################################
#STEP 11: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations.
#Note ---- there are no restoration fuelbeds in wet or mesic flatlands.
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment %in% c(6,7,8),]
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment %in% c(6,7,8),]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
dev.off()
nf <- layout(matrix(c(1,2), 2, 1, byrow = TRUE))
layout.show(nf)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Trends are not quite as expected.
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#PIGs should decline with lengthening mFRI and increase with stand age, they do not.

#############################################################################################################
##############################################################################################################
#STEP 12: 
#CONCLUSIONS
#The idealized "expert opnion" PIGs you coded into the FDM input file is overly simplistic. Probably because
#you developed this quickly as a way to test early versions of FDM. You cannot use these values to compare
#with Benchmark ROS-dervived PIG values. 

#RESPONSE:
#Develop coefficients attached to each fuelbed factor (sys_pigs)
#and apply them to come up with a more systematic way of assigning PIGs

#Load probability of ignition (PIG) coefficients you just developed. 
#These are numbers attached to each fuelbed factor
#that will yield an idealized (based on Eglin staff interviews) PIG for each fuelbed
pig_coef <- read.table("inputs/pig_coefficients.csv", 
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Create a string of coefficients for each fuelbed in FDM inputs
new_pigs <- mapply(function(y) {
  a1 <- pig_coef[,2][pig_coef[,1] == fbsd[y,7]]
  b1 <- pig_coef[,4][pig_coef[,3] == fbsd[y,8]]
  c1 <- pig_coef[,6][pig_coef[,5] == fbsd[y,9]]
  d1 <- pig_coef[,8][pig_coef[,7] == fbsd[y,10]]
  e1 <- pig_coef[,10][pig_coef[,9] == fbsd[y,11]]
  c(a1, b1, c1, d1, e1)
}, 1:length(fbsd[,1]))

#Rotate matrix
sys_pig_comps <- t(new_pigs)

#Calculate product of coefficients.
sys_pigs <- sys_pig_comps[,1] *sys_pig_comps[,2] *sys_pig_comps[,3] *sys_pig_comps[,4] *sys_pig_comps[,5]



#Create a data frame that can be used for analysis.
fbsd <- data.frame(fuelbed = fbsc$fuelbed, andreu_fuelbed_no = fbsc$andreu_fuelbed_no, 
                   benchmark_ros = fbsc$benchmark_ros, fccs_pig = bros_pig, 
                   cronan_pig = sys_pigs, 
                   diff_pig = fbsc$probability_of_ignition - bros_pig, 
                   topo = fcc[,1], 
                   cover = fcc[,3], treatment = fcc[,4], mfri = fcc[,5], age = fcc[,7])

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################


#                                       ITERATION 2
#EVALUATE PROBABILITY OF IGNITION VALUES FOR THE SYSTEMATICALLY DERIVED PIGS




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#STEP 13: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.

#Subset selected natural fuelbeds by topographic position.
#Wet Flatlands
wet_flatlands <- fbsd[fbsd$fuelbed %in% c(1011101:1011106, 
                                          1011201:1011206, 
                                          1011301:1011306, 
                                          1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands
mesic_flatlands <- fbsd[fbsd$fuelbed %in% c(2011101:2011106, 
                                            2011201:2011206, 
                                            2011301:2011306, 
                                            2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

#Mesic Uplands
mesic_uplands <- fbsd[fbsd$fuelbed %in% c(3011101:3011106, 
                                          3011201:3011206, 
                                          3011301:3011306, 
                                          3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

#Xeric Uplands
xeric_uplands <- fbsd[fbsd$fuelbed %in% c(4011101:4011106, 
                                          4011201:4011206, 
                                          4011301:4011306, 
                                          4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
layout.show(nf)

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_cat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wet_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wet_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Flatlands
mf_cat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Flatlands", 
                               names(mesic_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(mf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first itteration (step 4).
#As in step 4, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN EACH TOPOGRAPHIC POSITION; START WITH WET FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 14: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands fuelbeds.
wet_flatlands <- fbsd[fbsd$topo == 1 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                wet_flatlands$mfri), 
                   names.arg = NULL, 
                   main = paste("Wet Flatlands", 
                                names(wet_flatlands)[type], 
                                sep = " "), 
                   ylim = c(0, max(wet_flatlands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 5).
#As in step 5, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 15: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
mesic_flatlands <- fbsd[fbsd$topo == 2 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                mesic_flatlands$mfri), 
                   names.arg = NULL, 
                   main = paste("Mesic Flatlands", 
                                names(mesic_flatlands)[type], 
                                sep = " "), 
                   ylim = c(0, max(mesic_flatlands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 6).
#As in step 6, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 16: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                mesic_uplands$mfri), 
                   names.arg = NULL, 
                   main = paste("Mesic Uplands", 
                                names(mesic_uplands)[type], 
                                sep = " "), 
                   ylim = c(0, max(mesic_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 7).
#As in step 7, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN XERIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 17: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                xeric_uplands$mfri), 
                   names.arg = NULL, 
                   main = paste("Xeric Uplands", 
                                names(xeric_uplands)[type], 
                                sep = " "), 
                   ylim = c(0, max(xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 8).
#As in step 8, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN  PLANTATIONS

##############################################################################################################
##############################################################################################################
#STEP 18: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
wet_flatlands <- fbsd[fbsd$topo == 1 & fbsd$treatment == 5,]
mesic_flatlands <- fbsd[fbsd$topo == 2 & fbsd$treatment == 5,]
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment == 5,]
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment == 5,]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
layout.show(nf)

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_cat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wet_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wet_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Flatlands
mf_cat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Flatlands", 
                               names(mesic_flatlands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(mf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 9).
#As in step 9, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN NATURAL FUELS

##############################################################################################################
##############################################################################################################
#STEP 19: ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment %in% c(2,3,4),]
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment %in% c(2,3,4),]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
dev.off()
nf <- layout(matrix(c(1,2), 2, 1, byrow = TRUE))
layout.show(nf)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 9).
#As in step 9, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#Trends are not quite as expected in step 10 (first iteration)
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#Systematic PIG Improvements
#1) Post-thinning PIGs are now very low. As expected.
#2) No change. these were fine in the first iteration.
#3) No change. these were fine in the first iteration.

#Although not a major driver (restoration treatment is), trends for mFRI and stand age are 
#improved over the first iteration.

#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN PLANTATION FUELS

#############################################################################################################
##############################################################################################################
#STEP 20: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
mesic_uplands <- fbsd[fbsd$topo == 3 & fbsd$treatment %in% c(6,7,8),]
xeric_uplands <- fbsd[fbsd$topo == 4 & fbsd$treatment %in% c(6,7,8),]

#Column number of fuelbed parameter you want to look at
type <-5 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
dev.off()
nf <- layout(matrix(c(1,2), 2, 1, byrow = TRUE))
layout.show(nf)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(0, max(wet_flatlands[,type], 
                                  mesic_flatlands[,type], 
                                  mesic_uplands[,type], 
                                  xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 9).
#As in step 9, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#Trends are not quite as expected in step 10 (first iteration)
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#Systematic PIG Improvements
#1) Post-thinning PIGs are now very low. As expected.
#2) No change. these were fine in the first iteration.
#3) No change. these were fine in the first iteration.

#Although not a major driver (restoration treatment is), trends for mFRI and stand age are 
#improved over the first iteration.

#############################################################################################################
##############################################################################################################
#STEP 21: 
#CONCLUSIONS
#The systematically derived PIGs are much better than the first version of PIGS in the FDM input file.
#While these are also derived from "expert opinion" the expected trends are applied uniformly
#across all fuelbeds using the PIG coefficients developed for iteration 2.

#RESPONSE:
#Compare systematically derived PIGs with Benchmark ROS-derived PIGs

#Calculate the deviation between the two
pig_dev <- fbsd$cronan_pig - fbsd$fccs_pig

#Create a data frame that can be used for analysis.
fbse <- data.frame(fuelbed = fbsd$fuelbed, andreu_fuelbed_no = fbsd$andreu_fuelbed_no, 
                   benchmark_ros = fbsd$benchmark_ros, fccd_pig = bros_pig, 
                   cronan_pig = sys_pigs, 
                   pig_dev = pig_dev, 
                   topo = fcc[,1], 
                   cover = fcc[,3], treatment = fcc[,4], mfri = fcc[,5], age = fcc[,7])

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################


#                                       ITERATION 3
#SUBTRACT BENCHMARK ROS-DERIVED PIGS FROM SYSTEMATICALLY-DERIVED PIGS AND EVALUATE DEGREE
#OF CORRELATION




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 22: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.

#Subset selected natural fuelbeds by topographic position.
#Wet Flatlands
wet_flatlands <- fbse[fbse$fuelbed %in% c(1011101:1011106, 
                                          1011201:1011206, 
                                          1011301:1011306, 
                                          1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands
mesic_flatlands <- fbse[fbse$fuelbed %in% c(2011101:2011106, 
                                            2011201:2011206, 
                                            2011301:2011306, 
                                            2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

#Mesic Uplands
mesic_uplands <- fbse[fbse$fuelbed %in% c(3011101:3011106, 
                                          3011201:3011206, 
                                          3011301:3011306, 
                                          3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

#Xeric Uplands
xeric_uplands <- fbse[fbse$fuelbed %in% c(4011101:4011106, 
                                          4011201:4011206, 
                                          4011301:4011306, 
                                          4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]

#Column number of fuelbed parameter you want to look at
type <-6 #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
layout.show(nf)

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_cat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wet_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wet_flatlands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Flatlands
mf_cat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Flatlands", 
                               names(mesic_flatlands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(mf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN EACH TOPOGRAPHIC POSITION; START WITH WET FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 23: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands fuelbeds.
wet_flatlands <- fbse[fbse$topo == 1 & fbse$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-type #5 = Eglin staff predictions for probability of ignition.

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
dev.off()
par(cex = font*mult)
wf_acat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                wet_flatlands$mfri), 
                   names.arg = NULL, 
                   main = paste("Wet Flatlands", 
                                names(wet_flatlands)[type], 
                                sep = " "), 
                   ylim = c(min(wet_flatlands[,type]), 
                            max(wet_flatlands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected. The only exception are titi swamps.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 24: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
mesic_flatlands <- fbse[fbse$topo == 2 & fbse$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-type

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                mesic_flatlands$mfri), 
                   names.arg = NULL, 
                   main = paste("Mesic Flatlands", 
                                names(mesic_flatlands)[type], 
                                sep = " "), 
                   ylim = c(min(mesic_flatlands[,type]), 
                            max(mesic_flatlands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 25: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
mesic_uplands <- fbse[fbse$topo == 3 & fbse$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-type

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                mesic_uplands$mfri), 
                   names.arg = NULL, 
                   main = paste("Mesic Uplands", 
                                names(mesic_uplands)[type], 
                                sep = " "), 
                   ylim = c(min(mesic_uplands[,type]), 
                            max(mesic_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN XERIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 26: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
xeric_uplands <- fbse[fbse$topo == 4 & fbse$treatment == 1,]

#Column number of fuelbed parameter you want to look at
type <-type

#Font size for all text in plot
font <- 0.7
mult <- 1

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_acat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                   col = mapply(function(y) colors_fr[y+1], 
                                xeric_uplands$mfri), 
                   names.arg = NULL, 
                   main = paste("Xeric Uplands", 
                                names(xeric_uplands)[type], 
                                sep = " "), 
                   ylim = c(min(xeric_uplands[,type]), 
                            max(xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(wf_acat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN  PLANTATIONS

##############################################################################################################
##############################################################################################################
#STEP 27: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
wet_flatlands <- fbse[fbse$topo == 1 & fbse$treatment == 5,]
mesic_flatlands <- fbse[fbse$topo == 2 & fbse$treatment == 5,]
mesic_uplands <- fbse[fbse$topo == 3 & fbse$treatment == 5,]
xeric_uplands <- fbse[fbse$topo == 4 & fbse$treatment == 5,]

#Column number of fuelbed parameter you want to look at
type <-type

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
layout.show(nf)

#Colors from yellow to red for fire frequency (high to low)
colors_fr <- c("grey", "yellow", "orange", "red", "dark red")

#Wet Flatlands
par(cex = font*mult)
wf_cat <- barplot(wet_flatlands[,type], wet_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wet_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wet_flatlands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wet_flatlands$fuelbed),
          x = unit(wf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Flatlands
mf_cat <- barplot(mesic_flatlands[,type], mesic_flatlands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_flatlands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Flatlands", 
                               names(mesic_flatlands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(mesic_flatlands$fuelbed),
          x = unit(mf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN NATURAL FUELS

##############################################################################################################
##############################################################################################################
#STEP 28: ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
mesic_uplands <- fbse[fbse$topo == 3 & fbse$treatment %in% c(2,3,4),]
xeric_uplands <- fbse[fbse$topo == 4 & fbse$treatment %in% c(2,3,4),]

#Column number of fuelbed parameter you want to look at
type <-type

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
dev.off()
nf <- layout(matrix(c(1,2), 2, 1, byrow = TRUE))
layout.show(nf)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 9).
#As in step 9, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#Trends are not quite as expected in step 10 (first iteration)
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#Systematic PIG Improvements
#1) Post-thinning PIGs are now very low. As expected.
#2) No change. these were fine in the first iteration.
#3) No change. these were fine in the first iteration.

#Although not a major driver (restoration treatment is), trends for mFRI and stand age are 
#improved over the first iteration.

#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN PLANTATION FUELS

#############################################################################################################
##############################################################################################################
#STEP 29: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
mesic_uplands <- fbse[fbse$topo == 3 & fbse$treatment %in% c(6,7,8),]
xeric_uplands <- fbse[fbse$topo == 4 & fbse$treatment %in% c(6,7,8),]

#Column number of fuelbed parameter you want to look at
type <-type

#Font size for all text in plot
font <- 0.7
mult <- 1

#Layout panels
dev.off()
nf <- layout(matrix(c(1,2), 2, 1, byrow = TRUE))
layout.show(nf)

#Mesic Uplands
mu_cat <- barplot(mesic_uplands[,type], mesic_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mesic_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mesic_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mesic_uplands$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xeric_uplands[,type], xeric_uplands$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xeric_uplands$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xeric_uplands)[type], 
                               sep = " "), 
                  ylim = c(min(wet_flatlands[,type], 
                               mesic_flatlands[,type], 
                               mesic_uplands[,type], 
                               xeric_uplands[,type]), 
                           max(wet_flatlands[,type],
                               mesic_flatlands[,type],
                               mesic_uplands[,type],
                               xeric_uplands[,type])))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xeric_uplands$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#This output is much better than the first iteration (step 9).
#As in step 9, there is the expected declining PIG as mfri lengthens, but there is also
#the expected low PIG in young stands.

#Trends are not quite as expected in step 10 (first iteration)
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#Systematic PIG Improvements
#1) Post-thinning PIGs are now very low. As expected.
#2) No change. these were fine in the first iteration.
#3) No change. these were fine in the first iteration.

#Although not a major driver (restoration treatment is), trends for mFRI and stand age are 
#improved over the first iteration.

############
#Start here
#1) Redo systematic PIG calcs so max is one -- currently they can multiply to > 1
#2) Fix salt march PIG in fccs-derived values
#3) Use deviation from expected values to identify patterns in fccs-derived values that
#   are largest.

plot(fbse$cronan_pig, fbse$fccd_pig)





