#BEGIN

#Title: Fuelbed Testing for the Fuelbed Dynamics Model (FDM) and Eglin Air Force Fuelbed Mapping
#Project.

#Author: Jim Cronan
#Institution: Pacific Wildland Fire Sciences Laboratory (USDA Forest Service)
#Date: 18-Nov-2017

#Purpose 1
#1) To review evlauation of synchronicity between three datasets describing FCCS fuelbeds

#a) FCCS Fuelbeds -- .xml files developed by Anne Andreu and delievered on August 9, 2016. 
#These were submitted to Joint Fire Science Program and Eglin Air Force Base as part of the 
#final report.

#b) FDM/State and Transition Model (STM) Fuelbeds -- fuelbed numbers listed in the tabular STM
#that is run in FDM.

#c) Graphical STM Fuelbeds. Fuelbeds depicted graphically in PowerPoint file. Used as a iluustrative
#reference of FDM/STM fuelbeds listed above. Not required to run FDM

#This analysis is described here:
#C:\Users\jcronan\Documents\Projects_Incomplete_Files\2012_Fuelbed_Characterization_and_Mapping\...
#Fuelbed_Crosswalk\20180105_Fuelbed_Crosswalk_Cronan.docx


#Purpose 2: To develop probability of igntion (PIG) values derived from Fuelbed Characteristic
#Classification System (FCCS) Fire Bhevaior Model rate of spread (ROS) predictions to paramterize 
#the Fuelbed Dynamics Model (FDM) fire spread sub-model.

#This analysis is described here:
#C:\Users\jcronan\Documents\Projects_Incomplete_Files\2012_Fuelbed_Characterization_and_Mapping\...
#Probability_of_Ignition\20171222_PIG_Methods_Cronan.docx

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

#Create a function for a 1-panel bar chart
x1plot <- function(a,x,z)
{
  
  #Column number of fuelbed parameter you want to look at
  type <-x #5 = Eglin staff predictions for probability of ignition.
  
  #Font size for all text in plot
  font <- 0.7
  mult <- 1
  
  #Colors from yellow to red for fire frequency (high to low)
  colors_fr <- c("grey", "yellow", "orange", "red", "dark red")
  
  #Plot 1
  par(cex = font*mult)
  xx_acat <- barplot(a[,type], a$fuelbed, 
                     col = mapply(function(y) colors_fr[y+1], 
                                  a$mfri), 
                     names.arg = NULL, 
                     main = paste(z, 
                                  names(a)[type], 
                                  sep = " "), 
                     ylim = c(min(0,
                                  a[,type]), 
                              max(a[,type])))
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  grid.text(as.character(a$fuelbed),
            x = unit(xx_acat, "native"), y=unit(-1, "lines"),
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
}

#Create a function for a 2-panel bar chart
x2plot <- function(a,b,x,z)
{
  
  #Column number of fuelbed parameter you want to look at
  type <-x #5 = Eglin staff predictions for probability of ignition.
  
  #Font size for all text in plot
  font <- 0.7
  mult <- 1
  
  
  #Colors from yellow to red for fire frequency (high to low)
  colors_fr <- c("grey", "yellow", "orange", "red", "dark red")
  
  #Layout panels
  nf <- layout(matrix(c(1,2), 2, 1, byrow = TRUE))
  layout.show(nf)
  
  #Plot 1
  x1_cat <- barplot(a[,type], a$fuelbed, 
                    col = mapply(function(y) colors_fr[y+1], 
                                 a$mfri), 
                    names.arg = NULL, 
                    main = paste(z[1], 
                                 names(a)[type], 
                                 sep = " "), 
                    ylim = c(min(0, 
                                 a[,type], 
                                 b[,type]), 
                             max(a[,type],
                                 b[,type])))
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  grid.text(as.character(a$fuelbed),
            x = unit(x1_cat, "native"), y=unit(-1, "lines"),
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
  
  #Plot 2
  x2_cat <- barplot(b[,type], b$fuelbed, 
                    col = mapply(function(y) colors_fr[y+1], 
                                 b$mfri), 
                    names.arg = NULL, 
                    main = paste(z[2], 
                                 names(b)[type], 
                                 sep = " "), 
                    ylim = c(min(0, 
                                 a[,type], 
                                 b[,type]), 
                             max(a[,type],
                                 b[,type])))
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  
  grid.text(as.character(b$fuelbed),
            x = unit(x2_cat, "native"), y=unit(-1, "lines"),
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
}

#Create a function for a 4-panel bar chart
x4plot <- function(a,b,c,d,x,z)
{
  
  #Column number of fuelbed parameter you want to look at
  type <-x #5 = Eglin staff predictions for probability of ignition.
  
  #Font size for all text in plot
  font <- 0.7
  mult <- 1
  
  #Layout panels
  nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  layout.show(nf)
  
  #Colors from yellow to red for fire frequency (high to low)
  colors_fr <- c("grey", "yellow", "orange", "red", "dark red")
  
  #Upper Left
  par(cex = font*mult)
  wf_cat <- barplot(a[,type], a$fuelbed, 
                    col = mapply(function(y) colors_fr[y+1], 
                                 a$mfri), 
                    names.arg = NULL, 
                    main = paste(z[1], 
                                 names(a)[type], 
                                 sep = " "), 
                    ylim = c(min(0, 
                                 a[,type], 
                                 b[,type], 
                                 c[,type], 
                                 d[,type]), 
                             max(a[,type],
                                 b[,type],
                                 c[,type],
                                 d[,type])))
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  grid.text(as.character(a$fuelbed),
            x = unit(wf_cat, "native"), y=unit(-1, "lines"),
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
  
  #Upper Right
  mf_cat <- barplot(b[,type], b$fuelbed, 
                    col = mapply(function(y) colors_fr[y+1], 
                                 b$mfri), 
                    names.arg = NULL, 
                    main = paste(z[2], 
                                 names(b)[type], 
                                 sep = " "), 
                    ylim = c(min(0, 
                                 a[,type], 
                                 b[,type], 
                                 c[,type], 
                                 d[,type]), 
                             max(a[,type],
                                 b[,type],
                                 c[,type],
                                 d[,type])))
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  
  grid.text(as.character(b$fuelbed),
            x = unit(mf_cat, "native"), y=unit(-1, "lines"),
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
  
  #Lower Left
  mu_cat <- barplot(c[,type], c$fuelbed, 
                    col = mapply(function(y) colors_fr[y+1], 
                                 c$mfri), 
                    names.arg = NULL, 
                    main = paste(z[3], 
                                 names(c)[type], 
                                 sep = " "), 
                    ylim = c(min(0, 
                                 a[,type], 
                                 b[,type], 
                                 c[,type], 
                                 d[,type]), 
                             max(a[,type],
                                 b[,type],
                                 c[,type],
                                 d[,type])))
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  grid.text(as.character(c$fuelbed),
            x = unit(mu_cat, "native"), y=unit(-1, "lines"),
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
  
  #Lower Right
  xu_cat <- barplot(d[,type], d$fuelbed, 
                    col = mapply(function(y) colors_fr[y+1], 
                                 d$mfri), 
                    names.arg = NULL, 
                    main = paste(z[4], 
                                 names(d)[type], 
                                 sep = " "), 
                    ylim = c(min(0, 
                                 a[,type], 
                                 b[,type], 
                                 c[,type], 
                                 d[,type]), 
                             max(a[,type],
                                 b[,type],
                                 c[,type],
                                 d[,type])))
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  
  grid.text(as.character(d$fuelbed),
            x = unit(xu_cat, "native"), y=unit(-1, "lines"),
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
}

x1_comparison <- function(a, v, w, z)
{
  #a = object with deviation from expected PIG values
  #v = previous iteration of deviation from expected PIGS (i.e., baseline
  #you are making comparision with)
  #w = current iteration of deviation from expected PIG values.
  #z = plot title
  
  #Font size for all text in plot
  font <- 0.7
  mult <- 1
  
  #Colors from yellow to red for fire frequency (high to low)
  
  #Current deviation from expected PIGS (Plot 1)
  colors_fr <- c("grey", "yellow", "orange", "red", "dark red")
  par(cex = font*mult)
  plot <- barplot(a[,w], 
                  a$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], a$mfri), 
                  names.arg = NULL, 
                  main = z, 
                  ylim = c(-1,1))
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  grid.text(as.character(a$fuelbed), 
            x = unit(plot, "native"), y=unit(-1, "lines"), 
            just="right", rot=50, gp = gpar(cex = font))
  popViewport(3)
  
  #Previous iteration of deviation from expected PIGS (overlay plot)
  colors_fr <- c("clear")
  par(new = T)
  plot <- barplot(a[,v], a$fuelbed, 
                  density = 10, 
                  angle = 45, 
                  border = "black", 
                  names.arg = NULL, 
                  ylim = c(-1,1))
}

##############################################################################################################
##############################################################################################################
#STEP 3: LOAD, FORMAT, AND CHECK DATA

#Load crosswalk between FCCS, Graphical STM, and FDM/STM fuelbeds
fbsa <- read.table("inputs/fuelbed_synchronicty.csv", 
                  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Recode the $andreu_fuelbed_no integers so they are the same length as the FDM/STM fuelbeds.
#First convert integers to characters
fbra <- as.character(fbsa$andreu_fuelbed_no)
#Second, recode fuelbeds to remove zeros at the 2 and 6 positions. This reduces the FCCS Fuelbed
#numbers from 7 to 5 digit integers
fbrb <- sapply(fbra, recode)
#Third replace 7-digit integers with recoded 5-digit integers.
#warning -- N/As will be generated for dummy STM fuelbeds
fbsa$andreu_fuelbed_no <- fbrb

#Load the most recent STM from the FDM GitHub Repository.
lut <- read.table("C:/Users/jcronan/Documents/GitHub/EglinAirForceBase/inputs/sef_lut_all.csv", 
                   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Compare fuelbeds from your synchroncity table with the most recent copy from the FDM repository.
#This ensures you are not working with a dated copy.
all.equal(fbsb$fuelbed, lut$fuelbed)

#Remove dummy fuelbeds
fbsb <- fbsa[!is.na(fbsa$andreu_fuelbed_no) == T,]

#Break fuelbeds into component parts and create a dataframe that will be source for visualizations.
fca <- as.character(fbsb$fuelbed)
fcb <- strsplit(fca, "")
fcc <- matrix(as.numeric(unlist(fcb)), nrow = length(fcb), ncol = length(fcb[[1]]), byrow = T)

#Create a data frame that can be used for analysis.
pig.df <- data.frame(fuelbed = fbsb$fuelbed, 
                           andreu_fuelbed_no = fbsb$andreu_fuelbed_no, 
                           topo = fcc[,1], 
                           cover = fcc[,3], 
                           treatment = fcc[,4], 
                           mfri = fcc[,5], 
                           age = fcc[,7], 
                           expected_pig = fbsb$probability_of_ignition)

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



#SECTION 1
#TEST 1
#EVALUATE THE PROBABILITY OF IGNITION VALUES FOR FDM/STM FUELBEDS THAT WERE DETERMINED CASUALLY
#FROM EXPERT OPINION DURING THE DEVELOPMENT AND TESTING PHASE OF FDM V2.
#s1t1



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
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 4 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.
dev.off()

#Object to handle title names
plotTitle <- vector()

#Subset wet flatlands fuelbeds you want to look at
plotTitle[1] <- "Wet Flatlands"
wet_flatlands <- pig.df[pig.df$fuelbed %in% c(1011101:1011106, 
                                          1011201:1011206, 
                                          1011301:1011306, 
                                          1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

plotTitle[2] <- "Mesic Flatlands"
mesic_flatlands <- pig.df[pig.df$fuelbed %in% c(2011101:2011106, 
                                            2011201:2011206, 
                                            2011301:2011306, 
                                            2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

plotTitle[3] <- "Mesic Uplands"
mesic_uplands <- pig.df[pig.df$fuelbed %in% c(3011101:3011106, 
                                          3011201:3011206, 
                                          3011301:3011306, 
                                          3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

plotTitle[4] <- "Xeric Uplands"
xeric_uplands <- pig.df[pig.df$fuelbed %in% c(4011101:4011106, 
                                          4011201:4011206, 
                                          4011301:4011306, 
                                          4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]


#Barchart of PIGs
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 8, plotTitle)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

##############################################################################################################
##############################################################################################################
#STEP 5: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.
dev.off()

#Subset all natural fuelbeds in wet flatlands
plotTitle <- "Wet Flatlands -- Natural Fuelbeds"
wet_flatlands <- pig.df[pig.df$topo == 1 & pig.df$treatment == 1,]

x1plot(wet_flatlands, 8, plotTitle)
  
#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

##############################################################################################################
##############################################################################################################
#STEP 6: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.
dev.off()

#Subset all natural fuelbeds in mesic flatlands
plotTitle <- "Mesic Flatlands -- Natural Fuelbeds"
mesic_flatlands <- pig.df[pig.df$topo == 2 & pig.df$treatment == 1,]

x1plot(mesic_flatlands, 8, plotTitle)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

##############################################################################################################
##############################################################################################################
#STEP 7: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.
dev.off()

#Subset all natural fuelbeds in mesic uplands
plotTitle <- "Mesic Uplands -- Natural Fuelbeds"
mesic_uplands <- pig.df[pig.df$topo == 3 & pig.df$treatment == 1,]

x1plot(mesic_uplands, 8, plotTitle)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

##############################################################################################################
##############################################################################################################
#STEP 8: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds in xeric uplands
plotTitle <- "Xeric Uplands -- Natural Fuelbeds"
xeric_uplands <- pig.df[pig.df$topo == 4 & pig.df$treatment == 1,]

x1plot(xeric_uplands, 8, plotTitle)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

##############################################################################################################
##############################################################################################################
#STEP 9: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()

#Object to handle title names
plotTitle <- c("Wet Flatlands", 
               "Mesic Flatlands", 
               "Mesic Uplands", 
               "Xeric Uplands")

#Subset all plantation fuelbeds by topographic region.
wet_flatlands <- pig.df[pig.df$topo == 1 & pig.df$treatment == 5,]
mesic_flatlands <- pig.df[pig.df$topo == 2 & pig.df$treatment == 5,]
mesic_uplands <- pig.df[pig.df$topo == 3 & pig.df$treatment == 5,]
xeric_uplands <- pig.df[pig.df$topo == 4 & pig.df$treatment == 5,]

x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 8, plotTitle)

#As we can see in the output there is the expected trend across mfri, but not
#stand age. This is likely due to the fact that you did not put a lot of effort into
#setting up the initial PIG values in sef_lut_all.csv.

##############################################################################################################
##############################################################################################################
#STEP 10: 
#ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
dev.off()

#Object to handle title names
plotTitle <- c("Mesic Uplands", 
               "Xeric Uplands")

#Subset restoration fuelbeds derived from natural fuels by topographic position
#Note ---- there are no restoration fuelbeds in wet or mesic flatlands.
mesic_uplands <- pig.df[pig.df$topo == 3 & pig.df$treatment %in% c(2,3,4),]
xeric_uplands <- pig.df[pig.df$topo == 4 & pig.df$treatment %in% c(2,3,4),]

x2plot(mesic_uplands, xeric_uplands, 8, plotTitle)

#Trends are not quite as expected.
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#PIGs should decline with lengthening mFRI and increase with stand age, they do not.

#############################################################################################################
##############################################################################################################
#STEP 11: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.
dev.off()

#Object to handle title names
plotTitle <- c("Mesic Uplands", 
               "Xeric Uplands")

#Subset restoration fuelbeds derived from plantations.
#Note ---- there are no restoration fuelbeds in wet or mesic flatlands.
mesic_uplands <- pig.df[pig.df$topo == 3 & pig.df$treatment %in% c(6,7,8),]
xeric_uplands <- pig.df[pig.df$topo == 4 & pig.df$treatment %in% c(6,7,8),]

x2plot(mesic_uplands, xeric_uplands, 8, plotTitle)

#Trends are not quite as expected.
#1) Post-thinning should have low PIGs >> they are too high
#2) Post-herbicide should have PIGs near zero >> this is the case.
#3) Herbicide recovery should have high PIGs >> this is the case.

#PIGs should decline with lengthening mFRI and increase with stand age, they do not.

#############################################################################################################
##############################################################################################################
#CONCLUSIONS
#The casually assigned "expert opnion" PIGs you coded into the FDM input file is overly simplistic. Probably 
#because you developed this quickly as a way to develop and test early versions of FDM. You cannot use these 
#values to compare with Benchmark ROS-dervived PIG values. 

#RESPONSE:
#Develop coefficients attached to each fuelbed factor (sys_pigs)
#and apply them to come up with a more systematic way of assigning PIGs
#See step 12 in test 2

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



#SECTION 1
#TEST 2
#EVALUATE THE PROBABILITY OF IGNITION VALUES FOR FDM/STM FUELBEDS THAT WERE DETERMINED SYSTEMATICALLY
#USING PIG COEFFICIENTS VERSION 1
#s1t2



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

#############################################################################################################
##############################################################################################################
#STEP 12: 
#Develop coefficients attached to each fuelbed factor (sys_pigs)
#and apply them to come up with a more systematic way of assigning PIGs

#Load probability of ignition (PIG) coefficients you just developed. 
#These are numbers attached to each fuelbed factor
#that will yield an idealized (based on Eglin staff interviews) PIG for each fuelbed
pig_coef <- read.table("inputs/pig_coefficients_v1.csv", 
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Create a string of coefficients for each fuelbed in FDM inputs
new_pigs <- mapply(function(y) {
  a1 <- pig_coef[,2][pig_coef[,1] == pig.df[y,3]]
  b1 <- pig_coef[,4][pig_coef[,3] == pig.df[y,4]]
  c1 <- pig_coef[,6][pig_coef[,5] == pig.df[y,5]]
  d1 <- pig_coef[,8][pig_coef[,7] == pig.df[y,6]]
  e1 <- pig_coef[,10][pig_coef[,9] == pig.df[y,7]]
  c(a1, b1, c1, d1, e1)
}, 1:length(pig.df[,1]))

#Rotate matrix
sys_pig_comps <- t(new_pigs)

#Calculate product of coefficients.
expected_pigs_v1 <- sys_pig_comps[,1] *sys_pig_comps[,2] *sys_pig_comps[,3] *sys_pig_comps[,4] *sys_pig_comps[,5]

#Stanardize values of version 2 expected PIGs to 1.0
expected_pigs_v1 <- expected_pigs_v1/max(expected_pigs_v1)

#Create a data frame that can be used for analysis.
pig.df_v1 <- data.frame(pig.df[,1:7], 
                        expected_pigvo = pig.df$expected_pig, 
                        expected_pigv1 = expected_pigs_v1) 


##############################################################################################################
##############################################################################################################
#STEP 13: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in mesic uplands
plotTitle <- "Wet Flatlands -- Natural Fuelbeds"
wet_flatlands <- pig.df_v1[pig.df_v1$topo == 1 & pig.df_v1$treatment == 1,]

#Compare version 1 of expected PIGs (colored) with casually determined PIGs (clear)
dev.off()
x1_comparison(wet_flatlands, 8, 9, plotTitle)

#This output is much better than the first iteration (step 5).
#As in step 5, there is the expected declining PIG as mfri lengthens, but the PIGs are much lower, especially
#for fire-inhibiting fuel types like closed canopy hardwood bottomlands where values are now near zero. There
#is also now a increasing PIG with stand age for all fuel types, especially fire-adapted forest types where
#we would expect to see this.

##############################################################################################################
##############################################################################################################
#STEP 14: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- "Mesic Flatlands -- Natural Fuelbeds"
mesic_flatlands <- pig.df_v1[pig.df_v1$topo == 2 & pig.df_v1$treatment == 1,]

dev.off()
x1_comparison(mesic_flatlands, 8, 9, plotTitle)

#This output is much better than the first iteration (step 6).
#As in step 6, there is the expected declining PIG as mfri lengthens, but the PIGs are much lower, especially
#for fire-inhibiting fuel types like closed canopy hardwood bottomlands where values are now near zero. There
#is also now a increasing PIG with stand age for all fuel types, especially fire-adapted forest types where
#we would expect to see this.

##############################################################################################################
##############################################################################################################
#STEP 15: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- "Mesic Uplands -- Natural Fuelbeds"
mesic_uplands <- pig.df_v1[pig.df_v1$topo == 3 & pig.df_v1$treatment == 1,]

dev.off()
x1_comparison(mesic_uplands, 8, 9, plotTitle)

#This output is much better than the first iteration (step 7).
#As in step 7, there is the expected declining PIG as mfri lengthens, but the PIGs are lower, especially
#for fire-inhibiting fuel types like sand pine and oak upldands where values are now near zero. There
#is also now a increasing PIG with stand age for all fuel types, especially fire-adapted forest types where
#we would expect to see this.

##############################################################################################################
##############################################################################################################
#STEP 16: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- "Xeric Uplands -- Natural Fuelbeds"
xeric_uplands <- pig.df_v1[pig.df_v1$topo == 4 & pig.df_v1$treatment == 1,]

dev.off()
x1_comparison(xeric_uplands, 8, 9, plotTitle)

#This output is much better than the first iteration (step 8).
#As in step 8, there is the expected declining PIG as mfri lengthens, but the PIGs are lower, especially
#for fire-inhibiting fuel types like sand pine and oak upldands where values are now near zero. There
#is also now a increasing PIG with stand age for all fuel types, especially fire-adapted forest types where
#we would expect to see this.

##############################################################################################################
##############################################################################################################
#STEP 17:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- pig.df_v1[pig.df_v1$topo == 1 & pig.df_v1$treatment == 5,]
mesic_flatlands <- pig.df_v1[pig.df_v1$topo == 2 & pig.df_v1$treatment == 5,]
mesic_uplands <- pig.df_v1[pig.df_v1$topo == 3 & pig.df_v1$treatment == 5,]
xeric_uplands <- pig.df_v1[pig.df_v1$topo == 4 & pig.df_v1$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x1_comparison(wet_flatlands, 8, 9, plotTitle[1])
dev.off()
x1_comparison(mesic_flatlands, 8, 9, plotTitle[2])
dev.off()
x1_comparison(xeric_uplands, 8, 9, plotTitle[3])
dev.off()
x1_comparison(xeric_uplands, 8, 9, plotTitle[4])


#This output is much better than the first iteration (step 9).
#As in step 9, there is the expected declining PIG as mfri lengthens, but the PIGs are lower, especially
#for fire-inhibiting fire-excluded long-nnedle plantations where values are now near zero. There
#is also now a increasing PIG with stand age for all fuel types, especially fire-adapted forest types where
#we would expect to see this.

##############################################################################################################
##############################################################################################################
#STEP 18: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
mesic_uplands <- pig.df_v1[pig.df_v1$topo == 3 & pig.df_v1$treatment %in% c(2,3,4),]
xeric_uplands <- pig.df_v1[pig.df_v1$topo == 4 & pig.df_v1$treatment %in% c(2,3,4),]

dev.off()
x1_comparison(xeric_uplands, 8, 9, plotTitle[1])
dev.off()
x1_comparison(xeric_uplands, 8, 9, plotTitle[2])

#This output is much better than the step 10. 
#1) PIGs for sand pine removal fuelbeds are much closer to zero whereas in the casually
#assigned PIGs they were near one. Early stage oak herbicide PIGs are a little higher,
#but still near zero. This remains realistics. Late stage oak herbicide fuelbeds are
#lower, but casually assigned values were all near one and new values are highest for 
#older stands representing the increased fine fuel of these stands.

#############################################################################################################
##############################################################################################################
#STEP 19: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- pig.df_v1[pig.df_v1$topo == 3 & pig.df_v1$treatment %in% c(6,7,8),]
xeric_uplands <- pig.df_v1[pig.df_v1$topo == 4 & pig.df_v1$treatment %in% c(6,7,8),]

dev.off()
x1_comparison(xeric_uplands, 8, 9, plotTitle[1])
dev.off()
x1_comparison(xeric_uplands, 8, 9, plotTitle[2])

#This output is much better than the step 10. 
#1) PIGs for sand pine removal fuelbeds are much closer to zero whereas in the casually
#assigned PIGs they were near one. Early stage oak herbicide PIGs are a little higher,
#but still near zero. This remains realistics. Late stage oak herbicide fuelbeds are
#lower, but casually assigned values were all near one and new values are highest for 
#older stands representing the increased fine fuel of these stands.

#############################################################################################################
##############################################################################################################
#CONCLUSIONS
#The systematically derived PIGs are much better than the first version of PIGS in the FDM input file.
#While these are also derived from "expert opinion" the expected trends are applied uniformly
#across all fuelbeds using the PIG coefficients developed for iteration 2.

#RESPONSE:
#Develop a second version of systematically derived PIGs. One problem with expected PIGs is that
#upland fire-inhibiting forest types do not have the expected increase in PIG during the early stages 
#of canopy development. 

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



#SECTION 1
#TEST 3
#EVALUATE THE PROBABILITY OF IGNITION VALUES FOR FDM/STM FUELBEDS THAT WERE DETERMINED SYSTEMATICALLY
#USING PIG COEFFICIENTS VERSION 2
#s1t3



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

#############################################################################################################
##############################################################################################################
#STEP 20: 
#Develop second version of coefficients attached to each fuelbed factor (sys_pigs)
#and apply them to come up with a more systematic way of assigning PIGs

#Load probability of ignition (PIG) coefficients you just developed. 
#These are numbers attached to each fuelbed factor
#that will yield an idealized (based on Eglin staff interviews) PIG for each fuelbed
pig_coef_v2 <- read.table("inputs/pig_coefficients_v2.csv", 
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Create a string of coefficients for each fuelbed in FDM inputs
new_pigs_v2 <- mapply(function(y) {
  a1 <- pig_coef_v2[,2][pig_coef_v2[,1] == pig.df[y,3]]
  b1 <- pig_coef_v2[,4][pig_coef_v2[,3] == pig.df[y,4]]
  b2 <- pig_coef_v2[,5][pig_coef_v2[,3] == pig.df[y,4]]
  c1 <- pig_coef_v2[,7][pig_coef_v2[,6] == pig.df[y,5]]
  c2 <- pig_coef_v2[,8][pig_coef_v2[,6] == pig.df[y,5]]
  d1 <- pig_coef_v2[,10][pig_coef_v2[,9] == pig.df[y,6]]
  d2 <- pig_coef_v2[,11][pig_coef_v2[,9] == pig.df[y,6]]
  d3 <- pig_coef_v2[,12][pig_coef_v2[,9] == pig.df[y,6]]
  e1 <- pig_coef_v2[,14][pig_coef_v2[,13] == pig.df[y,7]]
  e2 <- pig_coef_v2[,15][pig_coef_v2[,13] == pig.df[y,7]]
  e3 <- pig_coef_v2[,16][pig_coef_v2[,13] == pig.df[y,7]]
  c(a1, b1, b2, c1, c2, d1, d2, d3, e1, e2, e3)
}, 1:length(pig.df[,1]))

#Rotate matrix
sys_pig_comps_v2 <- t(new_pigs_v2)

#Calculate product of coefficients.
expected_pigs_v2 <- mapply(function(y) { 
  sys_pig_comps_v2[y,1] * sys_pig_comps_v2[y,2] * sys_pig_comps_v2[y,4] * sys_pig_comps_v2[y,min(sys_pig_comps_v2[y,3] + sys_pig_comps_v2[y,5], 3) + 5] * sys_pig_comps_v2[y,min(sys_pig_comps_v2[y,3]  + sys_pig_comps_v2[y,5], 3) + 8] 
  }, 1:length(new_pigs_v2[1,]))

#Stanardize values of version 2 expected PIGs to 1.0
expected_pigs_v2 <- expected_pigs_v2/max(expected_pigs_v1)

#Create a data frame that can be used for analysis.
pig.df_v2 <- data.frame(pig.df[,1:7], 
                        expected_pigvo = pig.df$expected_pig, 
                        expected_pigv1 = expected_pigs_v1, 
                        expected_pigv2 = expected_pigs_v2) 

##############################################################################################################
##############################################################################################################
#STEP 21: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in mesic uplands
plotTitle <- "Wet Flatlands -- Natural Fuelbeds"
wet_flatlands <- pig.df_v2[pig.df_v2$topo == 1 & pig.df_v2$treatment == 1,]

#Compare version 1 of expected PIGs (colored) with casually determined PIGs (clear)
x1_comparison(wet_flatlands, 8, 10, plotTitle)
x1_comparison(wet_flatlands, 9, 10, plotTitle)

#Same as version 1. This was expected. Version 2 was created to address issues with upland fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 22: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- "Mesic Flatlands -- Natural Fuelbeds"
mesic_flatlands <- pig.df_v2[pig.df_v2$topo == 2 & pig.df_v2$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 10, plotTitle)
x1_comparison(mesic_flatlands, 9, 10, plotTitle)

#Same as version 1. This was expected. Version 2 was created to address issues with upland fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 23: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- "Mesic Uplands -- Natural Fuelbeds"
mesic_uplands <- pig.df_v2[pig.df_v2$topo == 3 & pig.df_v2$treatment == 1,]

x1_comparison(mesic_uplands, 8, 10, plotTitle)
x1_comparison(mesic_uplands, 9, 10, plotTitle)

#Great, converting the switch had th desired effect on sand pine and oak/pine mixes. These fuelbeds
#now have a marginal PIG in middle stages of stand succession (peaks at 20-40 years old).

##############################################################################################################
##############################################################################################################
#STEP 24: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- "Xeric Uplands -- Natural Fuelbeds"
xeric_uplands <- pig.df_v2[pig.df_v2$topo == 4 & pig.df_v2$treatment == 1,]

x1_comparison(xeric_uplands, 8, 10, plotTitle)
x1_comparison(xeric_uplands, 9, 10, plotTitle)

#Great, converting the switch had th desired effect on sand pine and oak/pine mixes. These fuelbeds
#now have a marginal PIG in middle stages of stand succession (peaks at 20-40 years old).

##############################################################################################################
##############################################################################################################
#STEP 25:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- pig.df_v2[pig.df_v2$topo == 1 & pig.df_v2$treatment == 5,]
mesic_flatlands <- pig.df_v2[pig.df_v2$topo == 2 & pig.df_v2$treatment == 5,]
mesic_uplands <- pig.df_v2[pig.df_v2$topo == 3 & pig.df_v2$treatment == 5,]
xeric_uplands <- pig.df_v2[pig.df_v2$topo == 4 & pig.df_v2$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
x1_comparison(wet_flatlands, 8, 10, plotTitle[1])
x1_comparison(wet_flatlands, 9, 10, plotTitle[1])
x1_comparison(mesic_flatlands, 8, 10, plotTitle[2])
x1_comparison(mesic_flatlands, 9, 10, plotTitle[2])
x1_comparison(xeric_uplands, 8, 10, plotTitle[3])
x1_comparison(xeric_uplands, 9, 10, plotTitle[3])
x1_comparison(xeric_uplands, 8, 10, plotTitle[4])
x1_comparison(xeric_uplands, 9, 10, plotTitle[4])

#Great, converting the switch had th desired effect on sand pine and oak/pine mixes. These fuelbeds
#now have a marginal PIG in middle stages of stand succession (peaks at 20-40 years old).

##############################################################################################################
##############################################################################################################
#STEP 26: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- pig.df_v2[pig.df_v2$topo == 3 & pig.df_v2$treatment == aa,]
xeric_uplands <- pig.df_v2[pig.df_v2$topo == 4 & pig.df_v2$treatment == aa,]

x1_comparison(xeric_uplands, 8, 10, plotTitle[1])
x1_comparison(xeric_uplands, 9, 10, plotTitle[1])
x1_comparison(xeric_uplands, 8, 10, plotTitle[2])
x1_comparison(xeric_uplands, 9, 10, plotTitle[2])

#Good, this was unexpected, but version 2 evens out the difference in PIGs between 
#longleaf pine and oak/pine mix overstory types.

#############################################################################################################
##############################################################################################################
#STEP 27: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- pig.df_v2[pig.df_v2$topo == 3 & pig.df_v2$treatment %in% c(6,7,8),]
xeric_uplands <- pig.df_v2[pig.df_v2$topo == 4 & pig.df_v2$treatment %in% c(6,7,8),]

x1_comparison(xeric_uplands, 8, 10, plotTitle[1])
x1_comparison(xeric_uplands, 9, 10, plotTitle[1])
x1_comparison(xeric_uplands, 8, 10, plotTitle[2])
x1_comparison(xeric_uplands, 9, 10, plotTitle[2])

#No change

#############################################################################################################
##############################################################################################################
#CONCLUSIONS
#Version 2 is better than version 1. The switches create more realistic PIG curves over stand age for
#upland natural stands of sand pine and oak/pine mixes without affecting other vegetation types.
#The second version also evens out differences in PIGs between longleaf and oak/pine late-stage
#oak herbicide stands so they all have a PIG near one as stand age increases.

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



#SECTION 1
#TEST 3
#EVALUATE THE PROBABILITY OF IGNITION VALUES FOR FDM/STM FUELBEDS THAT WERE DETERMINED SYSTEMATICALLY
#USING PIG COEFFICIENTS VERSION 2
#s1t3



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



#Create a data frame that can be used for analysis.
benchmark.df <- data.frame(fuelbed = fbsb$fuelbed, 
                           andreu_fuelbed_no = fbsb$andreu_fuelbed_no, 
                           topo = fcc[,1], 
                           cover = fcc[,3], 
                           treatment = fcc[,4], 
                           mfri = fcc[,5], 
                           age = fcc[,7], 
                           expected_pig = fbsb$probability_of_ignition, 
                           benchmark_ros = fbsb$benchmark_ros, 
                           benchmark_pig = benchmark_pig, 
                           benchmark_dif = benchmark_pig - fbsb$probability_of_ignition)

#STEP 22: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.

#Subset selected natural fuelbeds by topographic position.
#Wet Flatlands
plotTitle <- c("Wet Flatlands", 
               "Mesic Flatlands", 
               "Mesic Uplands", 
               "Xeric Uplands")
wet_flatlands <- pig.df_v1[pig.df_v1$fuelbed %in% c(1011101:1011106, 
                                          1011201:1011206, 
                                          1011301:1011306, 
                                          1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands
mesic_flatlands <- pig.df_v1[pig.df_v1$fuelbed %in% c(2011101:2011106, 
                                            2011201:2011206, 
                                            2011301:2011306, 
                                            2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

#Mesic Uplands
mesic_uplands <- pig.df_v1[pig.df_v1$fuelbed %in% c(3011101:3011106, 
                                          3011201:3011206, 
                                          3011301:3011306, 
                                          3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

#Xeric Uplands
xeric_uplands <- pig.df_v1[pig.df_v1$fuelbed %in% c(4011101:4011106, 
                                          4011201:4011206, 
                                          4011301:4011306, 
                                          4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]


#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 11, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN EACH TOPOGRAPHIC POSITION; START WITH WET FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 23: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- benchmark.df_v2[benchmark.df_v2$topo == 1 & benchmark.df_v2$treatment == 1,]

dev.off()
x1plot(wet_flatlands, 11, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected. The only exception are titi swamps.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 24: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- benchmark.df_v2[benchmark.df_v2$topo == 2 & benchmark.df_v2$treatment == 1,]

dev.off()
x1plot(mesic_flatlands, 11, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 25: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 3 & benchmark.df_v2$treatment == 1,]

dev.off()
x1plot(mesic_uplands, 11, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN XERIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 26: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 4 & benchmark.df_v2$treatment == 1,]

dev.off()
x1plot(xeric_uplands, 11, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN  PLANTATIONS

##############################################################################################################
##############################################################################################################
#STEP 27: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- benchmark.df_v2[benchmark.df_v2$topo == 1 & benchmark.df_v2$treatment == 5,]
mesic_flatlands <- benchmark.df_v2[benchmark.df_v2$topo == 2 & benchmark.df_v2$treatment == 5,]
mesic_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 3 & benchmark.df_v2$treatment == 5,]
xeric_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 4 & benchmark.df_v2$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 11, plotTitle)

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
plotTitle <- c("Mesic Uplands -- Restoration/Natural", 
               "Xeric Uplands -- Restoration/Natural")
mesic_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 3 & benchmark.df_v2$treatment %in% c(2,3,4),]
xeric_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 4 & benchmark.df_v2$treatment %in% c(2,3,4),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 11, plotTitle)

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
plotTitle <- c("Mesic Uplands -- Restoration/Plantations", 
               "Xeric Uplands -- Restoration/Plantations")
mesic_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 3 & benchmark.df_v2$treatment %in% c(6,7,8),]
xeric_uplands <- benchmark.df_v2[benchmark.df_v2$topo == 4 & benchmark.df_v2$treatment %in% c(6,7,8),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 11, plotTitle)

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

#############################################################################################################
##############################################################################################################
#STEP 30: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
#figure 1
plot(benchmark.df_v2$expected_pig, benchmark.df_v2$benchmark_pig)

#Show the linear model. 
#If FCCS/Predicted ROS-derived PIGS lined up perfectly with expected values the model parameters
#would be:
#Slope:       1
#Intercept:   0
#R-Squared:   1
#p-value:     0
#The goal here is not to fit the model-derived PIGs to expected pigs, but to use the expected PIGS
#as a standard to judge improvements in the model-derived PIGS. We know there are major problems
#with the initial system of using Benchmark fuel moistures for all FCCS fuelbed runs to derive
#PIGS because fine fuels in a swamp will have much wetter fuels than fine fuels in an upland 
#stand of longleaf pine under weather conditions suitable for prescribed burninng.
benchmark.lm <- lm(benchmark.df_v2$benchmark_pig ~ benchmark.df_v2$expected_pig)
summary(benchmark.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(benchmark.df_v2$benchmark_dif)

#Create a data frame to track total deviation from expected PIGS
ms <- data.frame(Moisture_Scenario = c(1,2,3,4,5,6,7,8,9), 
                 WetFlatlands_Nat = seq(0,1,9), 
                 MesicFlatlands_Nat = seq(0,1,9), 
                 MesicUplands_Nat = seq(0,1,9), 
                 XericUplands_Nat = seq(0,1,9), 
                 WetFlatlands_Other = seq(0,1,9), 
                 MesicFlatlands_Other = seq(0,1,9), 
                 MesicUplands_Other = seq(0,1,9), 
                 XericUplands_Other = seq(0,1,9), 
                 Total = seq(0,1,9))

#Populate data frame
scenario <- 1
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 1 & benchmark.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 2 & benchmark.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 3 & benchmark.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 4 & benchmark.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 1 & benchmark.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 2 & benchmark.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 3 & benchmark.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(benchmark.df$benchmark_dif[benchmark.df$topo == 4 & benchmark.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(benchmark.df$benchmark_dif)),1)

ms


sum(abs(benchmark.df_v2$benchmark_dif))

#CONCLUSION: FIGURE 1 SHOWS THAT THE SALT MARSH FUELBED IS AN EXTREME OUTLIER
#SOLUTION: CREATE TWO FUEL MOISTURE GROUPS
#1 - High for the salt march
#2 - Everything else stays at Benchmark


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


#                                       FUEL MOISTURE SCENARIO 2 -- ITERATION 1
#s2i1




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
#STEP 31: LOAD & TEST DATA

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 1
#Transformations -- None
fccs_fireBehavior <- read.table("inputs/fccs_ros_moistScenario_2_tansformation_none.csv", 
                                header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(fccs_fireBehavior$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms2_ros = fccs_fireBehavior$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsd <- merge(fbsc, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbse <- fbsd[order(fbsd$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbse$ms2_ros)
#Looks good. The last fuelbed (Rangeland) has the highest ROS and it is not dramatically higher than
#others. Use rangeland as the max ROS to set PIGS

#Convert benchmark ROS into probability of ignition (PIG)
ms2_pig <- round(fbse$ms2_ros/max(fbse$ms2_ros),4)

#Create a data frame with data through fuel moisture scenario 3.
ms2.df <- data.frame(fuelbed = fbsc$fuelbed, 
                     andreu_fuelbed_no = fbsc$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsc$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms2_ros = fbse$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 32: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.

#Subset selected natural fuelbeds by topographic position.
#Wet Flatlands
plotTitle <- c("Wet Flatlands", 
               "Mesic Flatlands", 
               "Mesic Uplands", 
               "Xeric Uplands")
wet_flatlands <- ms2.df[ms2.df$fuelbed %in% c(1011101:1011106, 
                                              1011201:1011206, 
                                              1011301:1011306, 
                                              1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands
mesic_flatlands <- ms2.df[ms2.df$fuelbed %in% c(2011101:2011106, 
                                                2011201:2011206, 
                                                2011301:2011306, 
                                                2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

#Mesic Uplands
mesic_uplands <- ms2.df[ms2.df$fuelbed %in% c(3011101:3011106, 
                                              3011201:3011206, 
                                              3011301:3011306, 
                                              3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

#Xeric Uplands
xeric_uplands <- ms2.df[ms2.df$fuelbed %in% c(4011101:4011106, 
                                              4011201:4011206, 
                                              4011301:4011306, 
                                              4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]


#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN EACH TOPOGRAPHIC POSITION; START WITH WET FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 33: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms2.df[ms2.df$topo == 1 & ms2.df$treatment == 1,]

dev.off()
x1plot(wet_flatlands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected. The only exception are titi swamps.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 34: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms2.df[ms2.df$topo == 2 & ms2.df$treatment == 1,]

dev.off()
x1plot(mesic_flatlands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 35: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms2.df[ms2.df$topo == 3 & ms2.df$treatment == 1,]

dev.off()
x1plot(mesic_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN XERIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 36: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms2.df[ms2.df$topo == 4 & ms2.df$treatment == 1,]

dev.off()
x1plot(xeric_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN  PLANTATIONS

##############################################################################################################
##############################################################################################################
#STEP 37: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms2.df[ms2.df$topo == 1 & ms2.df$treatment == 5,]
mesic_flatlands <- ms2.df[ms2.df$topo == 2 & ms2.df$treatment == 5,]
mesic_uplands <- ms2.df[ms2.df$topo == 3 & ms2.df$treatment == 5,]
xeric_uplands <- ms2.df[ms2.df$topo == 4 & ms2.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN NATURAL FUELS

##############################################################################################################
##############################################################################################################
#STEP 38: ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Natural", 
               "Xeric Uplands -- Restoration/Natural")
mesic_uplands <- ms2.df[ms2.df$topo == 3 & ms2.df$treatment %in% c(2,3,4),]
xeric_uplands <- ms2.df[ms2.df$topo == 4 & ms2.df$treatment %in% c(2,3,4),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 14, plotTitle)

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
#STEP 39: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Plantations", 
               "Xeric Uplands -- Restoration/Plantations")
mesic_uplands <- ms2.df[ms2.df$topo == 3 & ms2.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms2.df[ms2.df$topo == 4 & ms2.df$treatment %in% c(6,7,8),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 14, plotTitle)

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

#############################################################################################################
##############################################################################################################
#STEP 40: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms2.df$expected_pig, ms2.df$ms2_pig)

#Show the linear model. 
#If FCCS/Predicted ROS-derived PIGS lined up perfectly with expected values the model parameters
#would be:
#Slope:       1
#Intercept:   0
#R-Squared:   1
#p-value:     0
#The goal here is not to fit the model-derived PIGs to expected pigs, but to use the expected PIGS
#as a standard to judge improvements in the model-derived PIGS. We know there are major problems
#with the initial system of using Benchmark fuel moistures for all FCCS fuelbed runs to derive
#PIGS because fine fuels in a swamp will have much wetter fuels than fine fuels in an upland 
#stand of longleaf pine under weather conditions suitable for prescribed burninng.
ms2.lm <- lm(ms2.df$ms2_pig ~ ms2.df$expected_pig)
summary(ms2.lm)
plot(ms2.df$expected_pig, ms2.df$ms2_pig)
abline(ms2.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms2.df$ms2_dif)

#Populate data frame
scenario <- 2
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 1 & ms2.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 2 & ms2.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 3 & ms2.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 4 & ms2.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 1 & ms2.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 2 & ms2.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 3 & ms2.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms2.df$ms2_dif[ms2.df$topo == 4 & ms2.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms2.df$ms2_dif)),1)

ms
#RESULT: FUEL MOISTURE SCENARIO 2 WORKS A LITTLE BETTER THAN SCENARIO 1
#SOLUTION: CREATE MULTIPLE FUEL MOISTURE GROUPS BASED ON TOPOGRAPHIC POSITION AND MFRI

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


#                                       FUEL MOISTURE SCENARIO 3 -- ITERATION 1
#SUBTRACT BENCHMARK ROS-DERIVED PIGS FROM SYSTEMATICALLY-DERIVED PIGS AND EVALUATE DEGREE
#OF CORRELATION
#s3i1



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
#STEP 41: LOAD & TEST DATA

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 1
#Transformations -- None
fccs_fireBehavior <- read.table("inputs/fccs_ros_moistScenario_3_tansformation_none.csv", 
                                header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(fccs_fireBehavior$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms3_ros = fccs_fireBehavior$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsd <- merge(fbsc, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbse <- fbsd[order(fbsd$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbse$ms3_ros)
#Looks good. The last fuelbed (Rangeland) has the highest ROS and it is not dramatically higher than
#others. Use rangeland as the max ROS to set PIGS

#Convert benchmark ROS into probability of ignition (PIG)
ms3_pig <- round(fbse$ms3_ros/max(fbse$ms3_ros),4)

#Create a data frame with data through fuel moisture scenario 3.
ms3.df <- data.frame(fuelbed = fbsc$fuelbed, 
                     andreu_fuelbed_no = fbsc$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsc$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms3_ros = fbsd$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 42: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.

#Subset selected natural fuelbeds by topographic position.
#Wet Flatlands
plotTitle <- c("Wet Flatlands", 
               "Mesic Flatlands", 
               "Mesic Uplands", 
               "Xeric Uplands")
wet_flatlands <- ms3.df[ms3.df$fuelbed %in% c(1011101:1011106, 
                                          1011201:1011206, 
                                          1011301:1011306, 
                                          1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands
mesic_flatlands <- ms3.df[ms3.df$fuelbed %in% c(2011101:2011106, 
                                            2011201:2011206, 
                                            2011301:2011306, 
                                            2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

#Mesic Uplands
mesic_uplands <- ms3.df[ms3.df$fuelbed %in% c(3011101:3011106, 
                                          3011201:3011206, 
                                          3011301:3011306, 
                                          3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

#Xeric Uplands
xeric_uplands <- ms3.df[ms3.df$fuelbed %in% c(4011101:4011106, 
                                          4011201:4011206, 
                                          4011301:4011306, 
                                          4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]


#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN EACH TOPOGRAPHIC POSITION; START WITH WET FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 43: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms3.df[ms3.df$topo == 1 & ms3.df$treatment == 1,]

dev.off()
x1plot(wet_flatlands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected. The only exception are titi swamps.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC FLATLANDS

##############################################################################################################
##############################################################################################################
#STEP 44: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms3.df[ms3.df$topo == 2 & ms3.df$treatment == 1,]

dev.off()
x1plot(mesic_flatlands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN MESIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 45: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment == 1,]

dev.off()
x1plot(mesic_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN XERIC UPLANDS

##############################################################################################################
##############################################################################################################
#STEP 46: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment == 1,]

dev.off()
x1plot(xeric_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT WIDER RANGE OF FUELBEDS IN  PLANTATIONS

##############################################################################################################
##############################################################################################################
#STEP 47: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms3.df[ms3.df$topo == 1 & ms3.df$treatment == 5,]
mesic_flatlands <- ms3.df[ms3.df$topo == 2 & ms3.df$treatment == 5,]
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment == 5,]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 14, plotTitle)

#Generally speaking those fuelbeds with low expected PIGs have FCCS-dervived PIGs that are higher than
#expected (negative values) and those fuelbeds with high expected PIGs have FCCS-derived PIGs that
#are higher than expected.

#NEXT STEP, LOOK AT HOW PIGS CHANGE ACROSS RESTORATION TREATMENTS IN NATURAL FUELS

##############################################################################################################
##############################################################################################################
#STEP 48: ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Natural", 
               "Xeric Uplands -- Restoration/Natural")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment %in% c(2,3,4),]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment %in% c(2,3,4),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 14, plotTitle)

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
#STEP 49: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Plantations", 
               "Xeric Uplands -- Restoration/Plantations")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment %in% c(6,7,8),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 14, plotTitle)

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

#############################################################################################################
##############################################################################################################
#STEP 50: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms3.df$expected_pig, ms3.df$ms3_pig)

#Show the linear model. 
#If FCCS/Predicted ROS-derived PIGS lined up perfectly with expected values the model parameters
#would be:
#Slope:       1
#Intercept:   0
#R-Squared:   1
#p-value:     0
#The goal here is not to fit the model-derived PIGs to expected pigs, but to use the expected PIGS
#as a standard to judge improvements in the model-derived PIGS. We know there are major problems
#with the initial system of using Benchmark fuel moistures for all FCCS fuelbed runs to derive
#PIGS because fine fuels in a swamp will have much wetter fuels than fine fuels in an upland 
#stand of longleaf pine under weather conditions suitable for prescribed burninng.
ms3.lm <- lm(ms3.df$ms3_pig ~ ms3.df$expected_pig)
summary(ms3.lm)
plot(ms3.df$expected_pig, ms3.df$ms3_pig)
abline(ms3.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms3.df$ms3_dif)

#Populate data frame
scenario <- 3
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 1 & ms3.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 2 & ms3.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 3 & ms3.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 4 & ms3.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 1 & ms3.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 2 & ms3.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 3 & ms3.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms3.df$ms3_dif[ms3.df$topo == 4 & ms3.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms3.df$ms3_dif)),1)

ms
#RESULT: FUEL MOISTURE SCENARIO 3 IS BETTER THAN 2 (SLOPE INCREASES, INTERCEPT DECREASES, 
#R-SQUARED INCREASES, AND DIFFERENCES FROM EXPECTED DECREASE FOR ALL GROUPS EXCEPT MANAGED
#UPLANDS)
#SOLUTION: REVIEW DIFFERENCES MORE CLOSELY AND THEN CONSIDER REVISING GROUPS TO DECREASE
#THE DIFFERENCE FROM EXPECTED FURTHER.

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


#                                       FUEL MOISTURE SCENARIO 3 -- ITERATION 2
#s3i2




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
#STEP 51: CALCULATE DIFFERENCE BETWEEN MS3 AND MS4 PIGS

#Create a data frame with data through fuel moisture scenario 3.
ms3.df_v2 <- data.frame(fuelbed = fbsc$fuelbed, 
                     andreu_fuelbed_no = fbsc$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsc$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms3_ros = fbsd$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2,
                     ms2_dif = ms2.df$ms2_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 52: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS FLATLANDS.

#Subset selected natural fuelbeds by topographic position.

plotTitle <- c("Wet Flatlands--ms2", 
               "Wet Flatlands--ms3",
               "Mesic Flatlands--ms2", 
               "Mesic Flatlands--ms3")

#Wet Flatlands -- ms 2
wet_flatlands <- ms3.df_v2[ms3.df_v2$fuelbed %in% c(1011101:1011106, 
                                              1011201:1011206, 
                                              1011301:1011306, 
                                              1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands -- m2 2
mesic_flatlands <- ms3.df_v2[ms3.df_v2$fuelbed %in% c(2011101:2011106, 
                                                2011201:2011206, 
                                                2011301:2011306, 
                                                2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]


dev.off()
x4plot_compare(wet_flatlands, mesic_flatlands, 15, 14, plotTitle)

#Wet Flatlands
#Improvement over ms2 comes in large gains in pigs for unburned longleaf pine and
#gains for young stands of 4-8 and 9-20 mfri longleaf pine
#Declines occur in older stands with frequent to some burning and younger frequently 
#burned stands. 
#The group number for 1-3 and 4-8 mfri longleaf is too high. Fire behavior in older stands
#should have increased, but you decreased it by increasing fuel moistures. Must find a 
#way to do this while continuing to reduce fire behavior in younger stands, which should not
#have enough fuel to burn. Perhaps do the them what you did to uplands. They have dense
#fine fuel bulk density

#Mesic flatlands
#Similar trends to wet flatlands, but not as pronounced.



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 53: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms3.df_v2[ms3.df_v2$topo == 1 & ms3.df_v2$treatment == 1,]


dev.off()
x1_comparison(wet_flatlands, 15, 14, plotTitle)

#Conslusion: There has been an improvement in all fuelbeds, especially those with fire
#exclusion.

#Response
#1-3, 4-8, and 9-20 years mFRI longleaf pine > 40 years old have lower PIGs than expected and MS2.
#All other fuelbeds are an improvement over MS2, but 9-20 mfri titi swamps still have PIGs that are
#much higher than expected.
#Young (40 years old) 1-3, 4-8, and 9-20 mfri longleaf still have PIGs that are greater than expected.

#Next iteration
#Move older longleaf pine (> 40 years old) fuelbeds with mfri < 20 years down one moisture scenario group.
#Move all fuelbeds with > 20 mfri up one fuel moisture scenario group.
#Move all non-longleaf fuelbeds with < 20 mfri up one fuel moisture scenario group.

##############################################################################################################
##############################################################################################################
#STEP 54: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms3.df_v2[ms3.df_v2$topo == 2 & ms3.df_v2$treatment == 1,]

dev.off()
x1_comparison(mesic_flatlands, 15, 14, plotTitle)

#Response
#1-3 and 4-8 years mFRI longleaf pine > 40 years old have similar to lower PIGs than MS2 and lower
#PIGs than expected.
#Most other fuelbeds are an improvement over MS2, but 9-20 mfri titi swamps still have PIGs that are
#much higher than expected.
#Young (40 years old) 1-3, 4-8, and 9-20 mfri longleaf still have PIGs that are greater than expected.

#Next iteration
#Move older longleaf pine (> 40 years old) fuelbeds with mfri < 20 years down one moisture scenario group.
#Move all fuelbeds with > 20 mfri up one fuel moisture scenario group.
#Move all non-longleaf fuelbeds with < 20 mfri up one fuel moisture scenario group.


##############################################################################################################
##############################################################################################################
#STEP 55: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms3.df_v2[ms3.df_v2$topo == 3 & ms3.df_v2$treatment == 1,]

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle)

#Response
#1-3, 4-8, and 9-20 years mFRI longleaf pine > 40 years old have similar to lower PIGs than MS2 and lower
#PIGs than expected.
#Most other fuelbeds are an improvement over MS2, but fire-excluded sand pine still have PIGs that are
#higher than expected.
#Young (40 years old) 1-3, 4-8, and 9-20 mfri longleaf still have PIGs that are greater than expected.


#Next iteration
#Move younger longleaf pine (< 40 years old) fuelbeds with mfri < 20 years up one moisture scenario group.
#Move older longleaf pine (> 40 years old) fuelbeds with mfri < 20 years down one moisture scenario group.
#Move all fuelbeds with > 20 mfri up one fuel moisture scenario group.
#Move all non-longleaf fuelbeds with < 20 mfri up one fuel moisture scenario group.

##############################################################################################################
##############################################################################################################
#STEP 56: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms3.df_v2[ms3.df_v2$topo == 4 & ms3.df_v2$treatment == 1,]

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle)

#Response
#1-3, 4-8, and 9-20 years mFRI longleaf pine > 40 years old have similar to lower PIGs than MS2 and lower
#PIGs than expected.
#Most other fuelbeds are an improvement over MS2, but fire-excluded sand pine and oak forests still have 
#PIGs that are higher than expected.
#Young (40 years old) 1-3, 4-8, and 9-20 mfri longleaf still have PIGs that are greater than expected.

#Next iteration
#Move younger longleaf pine (< 40 years old) fuelbeds with mfri < 20 years up one moisture scenario group.
#Move older longleaf pine (> 40 years old) fuelbeds with mfri < 20 years down one moisture scenario group.
#Move all fuelbeds with > 20 mfri up one fuel moisture scenario group.
#Move all non-longleaf fuelbeds with < 20 mfri up one fuel moisture scenario group.

##############################################################################################################
##############################################################################################################
#STEP 57: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms3.df_v2[ms3.df_v2$topo == 1 & ms3.df_v2$treatment == 5,]
mesic_flatlands <- ms3.df_v2[ms3.df_v2$topo == 2 & ms3.df_v2$treatment == 5,]
mesic_uplands <- ms3.df_v2[ms3.df_v2$topo == 3 & ms3.df_v2$treatment == 5,]
xeric_uplands <- ms3.df_v2[ms3.df_v2$topo == 4 & ms3.df_v2$treatment == 5,]

dev.off()
x1_comparison(wet_flatlands, 15, 14, plotTitle[1])

dev.off()
x1_comparison(mesic_flatlands, 15, 14, plotTitle[2])

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle[3])

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle[4])

#Response (wet flatlands)
#Accept for 1-3 mfri plantations which have higher PIGS than expected and ms2 the other are all an
#improvement, although it would be helpful to move fuel moisture scenarios up one for all
#1-3, 4-8, and 9-20 mfri fuelbeds

#Response (mesic flatlands)
#Accept for 1-3 mfri plantations which have higher PIGS than expected and ms2 the other are all an
#improvement, although it would be helpful to move fuel moisture scenarios up one for all
#1-3, 4-8, and 9-20 mfri fuelbeds

#Response (mesic uplands)
#Accept for 1-3 mfri plantations which have higher PIGS than expected and ms2 the other are all an
#improvement, although it would be helpful to move fuel moisture scenarios up one for all
#mfri fuelbeds

#Response (xeric uplands)
#All fuelbeds except sand pine plantations have PIGs that are close to expected and MS2. MS3 in sand
#pine is a slight improvement over MS2, but still higher than expected. Shift moisture scenario
#up for sand pine plantations.

##############################################################################################################
##############################################################################################################
#STEP 58: ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Natural", 
               "Xeric Uplands -- Restoration/Natural")
mesic_uplands <- ms3.df_v2[ms3.df_v2$topo == 3 & ms3.df_v2$treatment %in% c(2,3,4),]
xeric_uplands <- ms3.df_v2[ms3.df_v2$topo == 4 & ms3.df_v2$treatment %in% c(2,3,4),]

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle[1])

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle[2])


#Response (mesic Uplands)
#All 21-40 year old restoration fuelbeds have PIGs that are much higher than expected and higher than MS2.
#All post-thinning and immediate-post herbicide fuelbeds have PIGs that are much higher than expected and 
#higher than MS2.
#All 1-yr post herbicide longleaf pine fuelbeds > 40 years old have PIGs that are closer than expected
#relative to MS2 , but are still lower than expected. This is not the case for mixed oak/pine
#1-yr post-herbicide fuelbeds where PIGs are less accurate than MS2 and are higher than expected, a trend
#that increases with lengthening mfri.

#Response (xeric Uplands)
#All 21-40 year old restoration fuelbeds have PIGs that are much higher than expected and higher than MS2.
#All post-thinning and immediate-post herbicide fuelbeds have PIGs that are much higher than expected and 
#higher than MS2.
#All 1-yr post herbicide longleaf pine fuelbeds > 40 years old have PIGs that are closer than expected
#relative to MS2 , but are still lower than expected. This is not the case for mixed oak/pine
#1-yr post-herbicide fuelbeds where PIGs are less accurate than MS2 and are higher than expected, a trend
#that increases with lengthening mfri.

#############################################################################################################
##############################################################################################################
#STEP 59: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Plantations", 
               "Xeric Uplands -- Restoration/Plantations")
mesic_uplands <- ms3.df_v2[ms3.df_v2$topo == 3 & ms3.df_v2$treatment %in% c(6,7,8),]
xeric_uplands <- ms3.df_v2[ms3.df_v2$topo == 4 & ms3.df_v2$treatment %in% c(6,7,8),]

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle[1])

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle[2])

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


#                                       FUEL MOISTURE SCENARIO 4 -- ITERATION 1
#s4i1




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
#STEP 60: LOAD & TEST DATA
#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 4
#Transformations -- None
fccs_fireBehavior <- read.table("inputs/fccs_ros_moistScenario_4_tansformation_none.csv", 
                                header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(fccs_fireBehavior$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms4_ros = fccs_fireBehavior$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsd <- merge(fbsc, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbse <- fbsd[order(fbsd$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbse$ms4_ros)
#Looks good. The last fuelbed (Rangeland) has the highest ROS and it is not dramatically higher than
#others. Use rangeland as the max ROS to set PIGS

#Convert benchmark ROS into probability of ignition (PIG)
ms4_pig <- round(fbse$ms4_ros/max(fbse$ms4_ros),4)

#Create a data frame with data through fuel moisture scenario 3.
ms4.df <- data.frame(fuelbed = fbsc$fuelbed, 
                     andreu_fuelbed_no = fbsc$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsc$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms4_ros = fbsd$ms4_ros, 
                     ms4_pig = ms4_pig, 
                     ms4_dif = ms4_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 61: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS ALL TOPOGRAPHIC POSITIONS.

#Subset selected natural fuelbeds by topographic position.
#Wet Flatlands
plotTitle <- c("Wet Flatlands", 
               "Mesic Flatlands", 
               "Mesic Uplands", 
               "Xeric Uplands")
wet_flatlands <- ms4.df[ms4.df$fuelbed %in% c(1011101:1011106, 
                                              1011201:1011206, 
                                              1011301:1011306, 
                                              1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands
mesic_flatlands <- ms4.df[ms4.df$fuelbed %in% c(2011101:2011106, 
                                                2011201:2011206, 
                                                2011301:2011306, 
                                                2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]

#Mesic Uplands
mesic_uplands <- ms4.df[ms4.df$fuelbed %in% c(3011101:3011106, 
                                              3011201:3011206, 
                                              3011301:3011306, 
                                              3031401:3031406),]
mesic_uplands <- mesic_uplands[order(mesic_uplands$fuelbed),]

#Xeric Uplands
xeric_uplands <- ms4.df[ms3.df$fuelbed %in% c(4011101:4011106, 
                                              4011201:4011206, 
                                              4011301:4011306, 
                                              4031401:4031406),]
xeric_uplands <- xeric_uplands[order(xeric_uplands$fuelbed),]


#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 14, plotTitle)


##############################################################################################################
##############################################################################################################
#STEP 62: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms4.df[ms4.df$topo == 1 & ms4.df$treatment == 1,]

dev.off()
x1plot(wet_flatlands, 14, plotTitle)


##############################################################################################################
##############################################################################################################
#STEP 63: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms3.df[ms3.df$topo == 2 & ms3.df$treatment == 1,]

dev.off()
x1plot(mesic_flatlands, 14, plotTitle)

##############################################################################################################
##############################################################################################################
#STEP 64: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment == 1,]

dev.off()
x1plot(mesic_uplands, 14, plotTitle)

##############################################################################################################
##############################################################################################################
#STEP 65: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment == 1,]

dev.off()
x1plot(xeric_uplands, 14, plotTitle)


##############################################################################################################
##############################################################################################################
#STEP 66: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms3.df[ms3.df$topo == 1 & ms3.df$treatment == 5,]
mesic_flatlands <- ms3.df[ms3.df$topo == 2 & ms3.df$treatment == 5,]
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment == 5,]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
dev.off()
x4plot(wet_flatlands, mesic_flatlands, mesic_uplands, xeric_uplands, 14, plotTitle)


##############################################################################################################
##############################################################################################################
#STEP 67: ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Natural", 
               "Xeric Uplands -- Restoration/Natural")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment %in% c(2,3,4),]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment %in% c(2,3,4),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 14, plotTitle)

#############################################################################################################
##############################################################################################################
#STEP 68: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Plantations", 
               "Xeric Uplands -- Restoration/Plantations")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment %in% c(6,7,8),]

dev.off()
x2plot(mesic_uplands, xeric_uplands, 14, plotTitle)

#############################################################################################################
##############################################################################################################
#STEP 69: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms4.df$expected_pig, ms4.df$ms4_pig)

#Show the linear model. 
#If FCCS/Predicted ROS-derived PIGS lined up perfectly with expected values the model parameters
#would be:
#Slope:       1
#Intercept:   0
#R-Squared:   1
#p-value:     0
#The goal here is not to fit the model-derived PIGs to expected pigs, but to use the expected PIGS
#as a standard to judge improvements in the model-derived PIGS. We know there are major problems
#with the initial system of using Benchmark fuel moistures for all FCCS fuelbed runs to derive
#PIGS because fine fuels in a swamp will have much wetter fuels than fine fuels in an upland 
#stand of longleaf pine under weather conditions suitable for prescribed burninng.
ms4.lm <- lm(ms4.df$ms4_pig ~ ms4.df$expected_pig)
summary(ms4.lm)
plot(ms4.df$expected_pig, ms4.df$ms4_pig)
abline(ms4.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms4.df$ms4_dif)

#Populate data frame
scenario <- 4
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 1 & ms4.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 2 & ms4.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 3 & ms4.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 4 & ms4.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 1 & ms4.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 2 & ms4.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 3 & ms4.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms4.df$ms4_dif[ms4.df$topo == 4 & ms4.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms4.df$ms4_dif)),1)

ms

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


#                                       FUEL MOISTURE SCENARIO 4 -- ITERATION 2
#s4i2




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
#STEP 70: CALCULATE DIFFERENCE BETWEEN MS3 AND MS4 PIGS

#Create a data frame with data through fuel moisture scenario 3.
ms4.df_v2 <- data.frame(fuelbed = fbsc$fuelbed, 
                        andreu_fuelbed_no = fbsc$andreu_fuelbed_no, 
                        topo = fcc[,1], 
                        cover = fcc[,3], 
                        treatment = fcc[,4], 
                        mfri = fcc[,5], 
                        age = fcc[,7], 
                        expected_pig = expected_pigs_v2, 
                        benchmark_ros = fbsc$benchmark_ros, 
                        benchmark_pig = benchmark_pig, 
                        benchmark_dif = benchmark_pig - expected_pigs_v2, 
                        ms4_ros = fbsd$ms4_ros, 
                        ms4_pig = ms4_pig, 
                        ms4_dif = ms4_pig - expected_pigs_v2,
                        ms3_dif = ms3.df$ms3_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 71: 
#SELECTED NATURAL FUELS FOR NATURAL FUELBEDS ACROSS FLATLANDS.

#Subset selected natural fuelbeds by topographic position.

plotTitle <- c("Wet Flatlands--ms3", 
               "Wet Flatlands--ms4",
               "Mesic Flatlands--ms3", 
               "Mesic Flatlands--ms4")

#Wet Flatlands -- ms 2
wet_flatlands <- ms4.df_v2[ms4.df_v2$fuelbed %in% c(1011101:1011106, 
                                                    1011201:1011206, 
                                                    1011301:1011306, 
                                                    1031401:1031406),]
wet_flatlands <- wet_flatlands[order(wet_flatlands$fuelbed),]

#Mesic Flatlands -- m2 2
mesic_flatlands <- ms4.df_v2[ms4.df_v2$fuelbed %in% c(2011101:2011106, 
                                                      2011201:2011206, 
                                                      2011301:2011306, 
                                                      2031401:2031406),]
mesic_flatlands <- mesic_flatlands[order(mesic_flatlands$fuelbed),]


dev.off()
x4plot_compare(wet_flatlands, mesic_flatlands, 15, 14, plotTitle)


##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#STEP 72: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms4.df_v2[ms4.df_v2$topo == 1 & ms4.df_v2$treatment == 1,]


dev.off()
x1_comparison(wet_flatlands, 15, 14, plotTitle)

##############################################################################################################
##############################################################################################################
#STEP 73: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms4.df_v2[ms4.df_v2$topo == 2 & ms4.df_v2$treatment == 1,]

dev.off()
x1_comparison(mesic_flatlands, 15, 14, plotTitle)

##############################################################################################################
##############################################################################################################
#STEP 74: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms4.df_v2[ms4.df_v2$topo == 3 & ms4.df_v2$treatment == 1,]

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle)

##############################################################################################################
##############################################################################################################
#STEP 75: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms4.df_v2[ms4.df_v2$topo == 4 & ms4.df_v2$treatment == 1,]

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle)

##############################################################################################################
##############################################################################################################
#STEP 76: 
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms4.df_v2[ms4.df_v2$topo == 1 & ms4.df_v2$treatment == 5,]
mesic_flatlands <- ms4.df_v2[ms4.df_v2$topo == 2 & ms4.df_v2$treatment == 5,]
mesic_uplands <- ms4.df_v2[ms4.df_v2$topo == 3 & ms4.df_v2$treatment == 5,]
xeric_uplands <- ms4.df_v2[ms4.df_v2$topo == 4 & ms4.df_v2$treatment == 5,]

dev.off()
x1_comparison(wet_flatlands, 15, 14, plotTitle[1])

dev.off()
x1_comparison(mesic_flatlands, 15, 14, plotTitle[2])

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle[3])

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle[4])

##############################################################################################################
##############################################################################################################
#STEP 77: ALL RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Natural", 
               "Xeric Uplands -- Restoration/Natural")
mesic_uplands <- ms4.df_v2[ms4.df_v2$topo == 3 & ms4.df_v2$treatment %in% c(2,3,4),]
xeric_uplands <- ms4.df_v2[ms4.df_v2$topo == 4 & ms4.df_v2$treatment %in% c(2,3,4),]

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle[1])

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle[2])

#############################################################################################################
##############################################################################################################
#STEP 78: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration/Plantations", 
               "Xeric Uplands -- Restoration/Plantations")
mesic_uplands <- ms4.df_v2[ms4.df_v2$topo == 3 & ms4.df_v2$treatment %in% c(6,7,8),]
xeric_uplands <- ms4.df_v2[ms4.df_v2$topo == 4 & ms4.df_v2$treatment %in% c(6,7,8),]

dev.off()
x1_comparison(mesic_uplands, 15, 14, plotTitle[1])

dev.off()
x1_comparison(xeric_uplands, 15, 14, plotTitle[2])
