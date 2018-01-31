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

x1_comparison <- function(a, v, w, z, ymin, ymax)
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
                  ylim = c(ymin,ymax))
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
                  ylim = c(ymin,ymax))
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
all.equal(fbsa$fuelbed, lut$fuelbed)

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
pig_coef_v1 <- read.table("inputs/pig_coefficients_v1.csv", 
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Create a string of coefficients for each fuelbed in FDM inputs
new_pigs <- mapply(function(y) {
  a1 <- pig_coef_v1[,2][pig_coef_v1[,1] == pig.df[y,3]]
  b1 <- pig_coef_v1[,4][pig_coef_v1[,3] == pig.df[y,4]]
  c1 <- pig_coef_v1[,6][pig_coef_v1[,5] == pig.df[y,5]]
  d1 <- pig_coef_v1[,8][pig_coef_v1[,7] == pig.df[y,6]]
  e1 <- pig_coef_v1[,10][pig_coef_v1[,9] == pig.df[y,7]]
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
                        expected_pigv0 = pig.df$expected_pig, 
                        expected_pigv1 = expected_pigs_v1) 


##############################################################################################################
##############################################################################################################
#STEP 13: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in mesic uplands
plotTitle <- "Wet Flatlands -- Natural Fuelbeds"
wet_flatlands <- pig.df_v1[pig.df_v1$topo == 1 & pig.df_v1$treatment == 1,]

#Compare version 1 of expected PIGs (colored) with casually determined PIGs (clear)
x1_comparison(wet_flatlands, 8, 9, plotTitle, 0, 1)

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

x1_comparison(mesic_flatlands, 8, 9, plotTitle, 0, 1)

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

x1_comparison(mesic_uplands, 8, 9, plotTitle, 0, 1)

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

x1_comparison(xeric_uplands, 8, 9, plotTitle, 0, 1)

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
x1_comparison(wet_flatlands, 8, 9, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 9, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 9, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 9, plotTitle[4], 0, 1)


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

x1_comparison(xeric_uplands, 8, 9, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 9, plotTitle[2], 0, 1)

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

x1_comparison(xeric_uplands, 8, 9, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 9, plotTitle[2], 0, 1)

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
                        expected_pigv0 = pig.df$expected_pig, 
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
x1_comparison(wet_flatlands, 8, 10, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 9, 10, plotTitle, 0, 1)

#Same as version 1. This was expected. Version 2 was created to address issues with upland fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 22: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- "Mesic Flatlands -- Natural Fuelbeds"
mesic_flatlands <- pig.df_v2[pig.df_v2$topo == 2 & pig.df_v2$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 10, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 9, 10, plotTitle, 0, 1)

#Same as version 1. This was expected. Version 2 was created to address issues with upland fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 23: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- "Mesic Uplands -- Natural Fuelbeds"
mesic_uplands <- pig.df_v2[pig.df_v2$topo == 3 & pig.df_v2$treatment == 1,]

x1_comparison(mesic_uplands, 8, 10, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 9, 10, plotTitle, 0, 1)

#Great, converting the switch had th desired effect on sand pine and oak/pine mixes. These fuelbeds
#now have a marginal PIG in middle stages of stand succession (peaks at 20-40 years old).

##############################################################################################################
##############################################################################################################
#STEP 24: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- "Xeric Uplands -- Natural Fuelbeds"
xeric_uplands <- pig.df_v2[pig.df_v2$topo == 4 & pig.df_v2$treatment == 1,]

x1_comparison(xeric_uplands, 8, 10, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 9, 10, plotTitle, 0, 1)

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
x1_comparison(wet_flatlands, 8, 10, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 9, 10, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 10, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 9, 10, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 10, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 9, 10, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 10, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 9, 10, plotTitle[4], 0, 1)

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

x1_comparison(xeric_uplands, 8, 10, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 9, 10, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 10, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 9, 10, plotTitle[2], 0, 1)

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

x1_comparison(xeric_uplands, 8, 10, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 9, 10, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 10, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 9, 10, plotTitle[2], 0, 1)

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



#SECTION 2
#TEST 1
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING BENCHMARK FUEL MOISTURE
#SCENARIO FOR ALL FUELBEDS
#s2t1



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
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
#STEP 28: 
#CREATE DATA FRAME FOR BENCHMARK FUEL MOISTURE SCENARIO PIGS

#Calculate PIGs from Benchmark Fuel Moisture Scenario ROS predictions
#Use feature scaling to scale all values from 0 to 1.
benchmark_pig <- round(((fbsb$benchmark_ros - min(fbsb$benchmark_ros))/
  (max(fbsb$benchmark_ros) - min(fbsb$benchmark_ros))), 4)

#Create a data frame that can be used for analysis.
benchmark.df <- data.frame(fuelbed = fbsb$fuelbed, 
                           andreu_fuelbed_no = fbsb$andreu_fuelbed_no, 
                           topo = fcc[,1], 
                           cover = fcc[,3], 
                           treatment = fcc[,4], 
                           mfri = fcc[,5], 
                           age = fcc[,7], 
                           expected_pig = pig.df_v2$expected_pigv2, 
                           benchmark_ros = fbsb$benchmark_ros, 
                           benchmark_pig = benchmark_pig, 
                           benchmark_dif = benchmark_pig - pig.df_v2$expected_pigv2)

##############################################################################################################
##############################################################################################################
#STEP 29: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- benchmark.df[benchmark.df$topo == 1 & benchmark.df$treatment == 1,]

x1plot(wet_flatlands, 10, plotTitle)

#This brings up a problem. The PIGs for all fuelbeds in wet flatlands are much lower than expected
#because the FCCS fuelbed for salt marshes has a predicted ROS that is much higher than other fuelbeds
#in this topographic position. Check to see how many other fuelbeds have a high predicted ROS.
barplot(sort(fbsb$benchmark_ros))

#############################################################################################################
##############################################################################################################
#STEP 30: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
#figure 1
plot(benchmark.df$expected_pig, benchmark.df$benchmark_pig)

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
benchmark.lm <- lm(benchmark.df$benchmark_pig ~ benchmark.df$expected_pig)
summary(benchmark.lm)
plot(benchmark.df$expected_pig, benchmark.df$benchmark_pig)
abline(benchmark.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(benchmark.df$benchmark_dif)

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



#SECTION 2
#TEST 2
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING BENCHMARK FUEL MOISTURE
#SCENARIO FOR ALL FUELBEDS EXCEPT SALT MARSH
#s2t2



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
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
ms1_ros <- read.table("inputs/fccs_ros_moistureScenario_1.csv", 
                                header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(ms1_ros$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms1_ros = ms1_ros$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsc <- merge(fbsb, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbsd <- fbsc[order(fbsc$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbsd$ms1_ros)

#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
ms1_pig <- round(((fbsd$ms1_ros - min(fbsd$ms1_ros))/
                          (max(fbsd$ms1_ros) - min(fbsd$ms1_ros))), 4)

#Create a data frame with data through fuel moisture scenario 3.
ms1.df <- data.frame(fuelbed = fbsd$fuelbed, 
                     andreu_fuelbed_no = fbsd$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsd$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 32: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in mesic uplands
plotTitle <- "Wet Flatlands -- Natural Fuelbeds"
wet_flatlands <- ms1.df[ms1.df$topo == 1 & ms1.df$treatment == 1,]

#Compare version 1 of expected PIGs (colored) with casually determined PIGs (clear)
x1_comparison(wet_flatlands, 8, 13, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 10, 13, plotTitle, 0, 1)

#Generally speaking FCCS-derived PIGs are higher than expected for fire-inhibiting fuelbeds and lower
#than expected for fire-facilitating fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 33: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- "Mesic Flatlands -- Natural Fuelbeds"
mesic_flatlands <- ms1.df[ms1.df$topo == 2 & ms1.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 13, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 10, 13, plotTitle, 0, 1)

#Generally speaking FCCS-derived PIGs are higher than expected for fire-inhibiting fuelbeds and lower
#than expected for fire-facilitating fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 34: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- "Mesic Uplands -- Natural Fuelbeds"
mesic_uplands <- ms1.df[ms1.df$topo == 3 & ms1.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 13, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 10, 13, plotTitle, 0, 1)

#Generally speaking FCCS-derived PIGs are higher than expected for fire-inhibiting fuelbeds and lower
#than expected for fire-facilitating fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 35: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- "Xeric Uplands -- Natural Fuelbeds"
xeric_uplands <- ms1.df[ms1.df$topo == 4 & ms1.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 13, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 10, 13, plotTitle, 0, 1)

#Generally speaking FCCS-derived PIGs are higher than expected for fire-inhibiting fuelbeds and lower
#than expected for fire-facilitating fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 36:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms1.df[ms1.df$topo == 1 & ms1.df$treatment == 5,]
mesic_flatlands <- ms1.df[ms1.df$topo == 2 & ms1.df$treatment == 5,]
mesic_uplands <- ms1.df[ms1.df$topo == 3 & ms1.df$treatment == 5,]
xeric_uplands <- ms1.df[ms1.df$topo == 4 & ms1.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
x1_comparison(wet_flatlands, 8, 13, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 10, 13, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 13, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 10, 13, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 13, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 10, 13, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 13, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 10, 13, plotTitle[4], 0, 1)

#Generally speaking FCCS-derived PIGs are higher than expected for flatlands and lower
#than expected for fire-facilitating fuelbeds in uplands and higher than expected for 
#fire inhibiting fuelbeds in uplands.

##############################################################################################################
##############################################################################################################
#STEP 37: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms1.df[ms1.df$topo == 3 & ms1.df$treatment == aa,]
xeric_uplands <- ms1.df[ms1.df$topo == 4 & ms1.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 13, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 10, 13, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 13, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 10, 13, plotTitle[2], 0, 1)

#Sand Pine Removal (aa = 2)
#FCCS-derived PIGs are higher than expected for all fuelbeds.
#Early Stage Oak Herbicide (aa = 3)
#FCCS-derived PIGs are higher than expected for all fuelbeds.
#Late Stage Oak Herbicide (aa = 4)
#Generally speaking FCCS-derived PIGs are higher than expected for fire-inhibiting fuelbeds and lower
#than expected for fire-facilitating fuelbeds.

#############################################################################################################
##############################################################################################################
#STEP 38: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms1.df[ms1.df$topo == 3 & ms1.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms1.df[ms1.df$topo == 4 & ms1.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 13, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 10, 13, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 13, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 10, 13, plotTitle[2], 0, 1)

#With one exception, FCCS-derived PIGs are higher than expected for these fuelbeds.

#############################################################################################################
##############################################################################################################
#STEP 39: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms1.df$expected_pig, ms1.df$ms1_pig)

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
ms1.lm <- lm(ms1.df$ms1_pig ~ ms1.df$expected_pig)
summary(ms1.lm)
plot(ms1.df$expected_pig, ms1.df$ms1_pig)
abline(ms1.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms1.df$ms1_dif)

#Populate data frame
scenario <- 2
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 1 & ms1.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 2 & ms1.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 3 & ms1.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 4 & ms1.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 1 & ms1.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 2 & ms1.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 3 & ms1.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms1.df$ms1_dif[ms1.df$topo == 4 & ms1.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms1.df$ms1_dif)),1)

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


#SECTION 2
#TEST 3
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING 9 FUEL MOISTURE GROUPS
#FUEL MOISTURE ~ TOPOGRAPHIC POSITION + CANOPY COVER
#s2t3



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
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
ms2_ros <- read.table("inputs/fccs_ros_moistureScenario_2.csv", 
                                header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(ms2_ros$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms2_ros = ms2_ros$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbse <- merge(fbsb, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbsf <- fbse[order(fbse$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbsf$ms2_ros)

#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
ms2_pig <- round(((fbsf$ms2_ros - min(fbsf$ms2_ros))/
                    (max(fbsf$ms2_ros) - min(fbsf$ms2_ros))), 4)

#Create a data frame with data through fuel moisture scenario 3.
ms2.df <- data.frame(fuelbed = fbsf$fuelbed, 
                     andreu_fuelbed_no = fbsf$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsf$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2, 
                     ms2_ros = fbsf$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 42: 
#ALL NATURAL FUELBEDS IN WET FLATLANDS.

#Subset all natural fuelbeds in mesic uplands
plotTitle <- "Wet Flatlands -- Natural Fuelbeds"
wet_flatlands <- ms2.df[ms2.df$topo == 1 & ms2.df$treatment == 1,]

#Compare version 1 of expected PIGs (colored) with casually determined PIGs (clear)
x1_comparison(wet_flatlands, 8, 16, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 13, 16, plotTitle, 0, 1)

#Slight improvement over MS 1, but in general PIGs for fire-inhibiting fuelbeds are higher than expected and 
#fuelbeds for fire-facilitating fuelbeds are lower than expected.

##############################################################################################################
##############################################################################################################
#STEP 43: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- "Mesic Flatlands -- Natural Fuelbeds"
mesic_flatlands <- ms2.df[ms2.df$topo == 2 & ms2.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 16, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 13, 16, plotTitle, 0, 1)

#Slight improvement over MS 1, but in general PIGs for fire-inhibiting fuelbeds are higher than expected and 
#fuelbeds for fire-facilitating fuelbeds are lower than expected.

##############################################################################################################
##############################################################################################################
#STEP 44: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- "Mesic Uplands -- Natural Fuelbeds"
mesic_uplands <- ms2.df[ms2.df$topo == 3 & ms2.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 16, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 13, 16, plotTitle, 0, 1)

#Very slight improvement over MS 1, but in general PIGs for fire-inhibiting fuelbeds are higher than expected and 
#fuelbeds for fire-facilitating fuelbeds are lower than expected.

##############################################################################################################
##############################################################################################################
#STEP 45: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- "Xeric Uplands -- Natural Fuelbeds"
xeric_uplands <- ms2.df[ms2.df$topo == 4 & ms2.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 16, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 13, 16, plotTitle, 0, 1)

#Very slight improvement over MS 1, but in general PIGs for fire-inhibiting fuelbeds are higher than expected and 
#fuelbeds for fire-facilitating fuelbeds are lower than expected.

##############################################################################################################
##############################################################################################################
#STEP 46:
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
x1_comparison(wet_flatlands, 8, 16, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 13, 16, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 16, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 13, 16, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 16, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 13, 16, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 16, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 13, 16, plotTitle[4], 0, 1)

#Slight improvement for all fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 47: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms2.df[ms2.df$topo == 3 & ms2.df$treatment == aa,]
xeric_uplands <- ms2.df[ms2.df$topo == 4 & ms2.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 16, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 13, 16, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 16, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 13, 16, plotTitle[2], 0, 1)

#Sand Pine Removal (aa = 2)
#FCCS-derived PIGs are higher than expected for all fuelbeds and further from expected than MS 1.
#Early Stage Oak Herbicide (aa = 3)
#FCCS-derived PIGs are higher than expected for all fuelbeds and further from expected than MS 1.
#Late Stage Oak Herbicide (aa = 4)
#Generally speaking FCCS-derived PIGs are higher than expected for fire-inhibiting fuelbeds, this trend
#increased from MS 1 to MS 2 (i.e. accuracy was worse) and lower
#than expected for fire-facilitating fuelbeds, but this trend decreased from MS1 to MS 2 (i.e. got better).

#############################################################################################################
##############################################################################################################
#STEP 48: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms2.df[ms2.df$topo == 3 & ms2.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms2.df[ms2.df$topo == 4 & ms2.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 16, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 13, 16, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 16, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 13, 16, plotTitle[2], 0, 1)

#FCCS-derived PIGs are higher than expected for these fuelbeds. This trend increased (i.e., got worse)
#from MS 1 to MS 2.

#############################################################################################################
##############################################################################################################
#STEP 49: 
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
scenario <- 3
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

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################



#SECTION 2
#TEST 4
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING 18 FUEL MOISTURE GROUPS
#FUEL MOISTURE ~ TOPOGRAPHIC POSITION/2 + CANOPY COVER + STAND AGE | COVER + TREATMENT
#s2t4 




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
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

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 1
#Transformations -- None
ms3_ros <- read.table("inputs/fccs_ros_moistureScenario_3.csv", 
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(ms3_ros$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms3_ros = ms3_ros$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsg <- merge(fbsb, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbsh <- fbsg[order(fbsg$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbsh$ms3_ros)

#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
ms3_pig <- round(((fbsh$ms3_ros - min(fbsh$ms3_ros))/
                    (max(fbsh$ms3_ros) - min(fbsh$ms3_ros))), 4)

#Create a data frame with data through fuel moisture scenario 3.
ms3.df <- data.frame(fuelbed = fbsh$fuelbed, 
                     andreu_fuelbed_no = fbsh$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsh$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2, 
                     ms2_ros = fbsf$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2, 
                     ms3_ros = fbsh$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 52: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms3.df[ms3.df$topo == 1 & ms3.df$treatment == 1,]

x1_comparison(wet_flatlands, 8, 19, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 16, 19, plotTitle, 0, 1)

#The trends for FCCS-derived PIGs nearly match those in expected PIGs v2, but fire faciltating fuelbeds
#have PIGs that are lower than expected and 9-20 year mFRI titi swamps have PIGs that are about 2x higher 
#than expected.

#MS 3 PIGs are much improved over MS 2 PIGs

##############################################################################################################
##############################################################################################################
#STEP 53: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms3.df[ms3.df$topo == 2 & ms3.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 19, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 16, 19, plotTitle, 0, 1)

#The trends for FCCS-derived PIGs nearly match those in expected PIGs v2, but fire faciltating fuelbeds
#have PIGs that are lower than expected and 9-20 year mFRI titi swamps have PIGs that are about 2x higher 
#than expected.

#MS 3 PIGs are much improved over MS 2 PIGs

##############################################################################################################
##############################################################################################################
#STEP 54: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 19, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 16, 19, plotTitle, 0, 1)

#The trends for FCCS-derived PIGs nearly match those in expected PIGs v2, but fire faciltating fuelbeds
#have PIGs that are lower than expected and the expected effect of stand age on PIG in sand pine and 
#mixed oak/pine stands does not occur in FCCS-derived PIGs.

#MS 3 PIGs are improved over MS 2 PIGs

##############################################################################################################
##############################################################################################################
#STEP 55: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 10, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 16, 19, plotTitle, 0, 1)

#The trends for FCCS-derived PIGs nearly match those in expected PIGs v2, but most fuelbeds
#have PIGs that are lower than expected at higher stand ages, especially 9-20 year mFRI sandhill which has
#PIGs near zero. Additionally, the expected effect of stand age on PIG in sand pine and 
#mixed oak/pine stands does not occur in FCCS-derived PIGs.

##############################################################################################################
##############################################################################################################
#STEP 56:
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
x1_comparison(wet_flatlands, 8, 19, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 16, 19, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 19, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 16, 19, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 19, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 16, 19, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 19, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 16, 19, plotTitle[4], 0, 1)

#Improvement for all fuelbeds.
#Except for 9-20 year mFRI uplands where PIGs are near zero, this is below expected PIGs, especially for
#older stands.

##############################################################################################################
##############################################################################################################
#STEP 57: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment == aa,]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 19, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 16, 19, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 19, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 16, 19, plotTitle[2], 0, 1)

#Sand Pine Removal (aa = 2)
#FCCS-derived PIGs basically zero for all fuelbeds. Expected PIGs are close to zero so this is an 
#improvement over MS 2.
#Early Stage Oak Herbicide (aa = 3)
#FCCS-derived PIGs basically zero for all fuelbeds. Expected PIGs are close to zero so this is an 
#improvement over MS 2.
#Late Stage Oak Herbicide (aa = 4)
#Generally speaking FCCS-derived PIGs are quite close to expected PIGs, this is a large improvement 
#over MS 2.

#############################################################################################################
##############################################################################################################
#STEP 58: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms3.df[ms3.df$topo == 3 & ms3.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms3.df[ms3.df$topo == 4 & ms3.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 19, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 16, 19, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 19, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 16, 19, plotTitle[2], 0, 1)

#Sand Pine Removal (aa = 2)
#FCCS-derived PIGs basically zero for all fuelbeds. Expected PIGs are close to zero so this is an 
#improvement over MS 2.
#Early Stage Oak Herbicide (aa = 3)
#FCCS-derived PIGs basically zero for all fuelbeds. Expected PIGs are close to zero so this is an 
#improvement over MS 2.
#Late Stage Oak Herbicide (aa = 4)
#Generally speaking FCCS-derived PIGs are quite close to expected PIGs, this is a large improvement 
#over MS 2.

#############################################################################################################
##############################################################################################################
#STEP 59: 
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
scenario <- 4
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


##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################



#SECTION 2
#TEST 5
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING 18 FUEL MOISTURE GROUPS
#FUEL MOISTURE ~ TOPOGRAPHIC POSITION/2 + CANOPY COVER + STAND AGE | COVER + TREATMENT
#VARIANT OF EQUATION 2
#s2t5 




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#STEP 60: CALCULATE DIFFERENCE BETWEEN MS3 AND ms4 PIGS

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 1
#Transformations -- None
ms4_ros <- read.table("inputs/fccs_ros_moistureScenario_4.csv", 
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(ms4_ros$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms4_ros = ms4_ros$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsi <- merge(fbsb, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbsj <- fbsi[order(fbsi$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbsj$ms4_ros)

#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
ms4_pig <- round(((fbsj$ms4_ros - min(fbsj$ms4_ros))/
                    (max(fbsj$ms4_ros) - min(fbsj$ms4_ros))), 4)

#Create a data frame with data through fuel moisture scenario 3.
ms4.df <- data.frame(fuelbed = fbsj$fuelbed, 
                     andreu_fuelbed_no = fbsj$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsj$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2, 
                     ms2_ros = fbsf$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2, 
                     ms3_ros = fbsh$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2, 
                     ms4_ros = fbsj$ms4_ros, 
                     ms4_pig = ms4_pig, 
                     ms4_dif = ms4_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 61: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms4.df[ms4.df$topo == 1 & ms4.df$treatment == 1,]

x1_comparison(wet_flatlands, 8, 22, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 19, 22, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 62: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms4.df[ms4.df$topo == 2 & ms4.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 22, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 19, 22, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 63: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms4.df[ms4.df$topo == 3 & ms4.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 22, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 19, 22, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 64: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms4.df[ms4.df$topo == 4 & ms4.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 22, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 19, 22, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 65:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms4.df[ms4.df$topo == 1 & ms4.df$treatment == 5,]
mesic_flatlands <- ms4.df[ms4.df$topo == 2 & ms4.df$treatment == 5,]
mesic_uplands <- ms4.df[ms4.df$topo == 3 & ms4.df$treatment == 5,]
xeric_uplands <- ms4.df[ms4.df$topo == 4 & ms4.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
x1_comparison(wet_flatlands, 8, 22, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 19, 22, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 22, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 19, 22, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 22, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 19, 22, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 22, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 19, 22, plotTitle[4], 0, 1)

#PIG trends are reflective of natural fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 66: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms4.df[ms4.df$topo == 3 & ms4.df$treatment == aa,]
xeric_uplands <- ms4.df[ms4.df$topo == 4 & ms4.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 22, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 19, 22, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 22, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 19, 22, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 67: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms4.df[ms4.df$topo == 3 & ms4.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms4.df[ms4.df$topo == 4 & ms4.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 22, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 19, 22, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 22, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 19, 22, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 68: 
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
scenario <- 5
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



#SECTION 2
#TEST 6
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING 18 FUEL MOISTURE GROUPS
#FUEL MOISTURE ~ TOPOGRAPHIC POSITION/2 + CANOPY COVER + STAND AGE | COVER + TREATMENT
#VARIANT OF EQUATION 2
#s2t6 




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#STEP 69: CALCULATE DIFFERENCE BETWEEN MS3 AND ms4 PIGS

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 1
#Transformations -- None
ms5_ros <- read.table("inputs/fccs_ros_moistureScenario_5.csv", 
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(ms5_ros$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms5_ros = ms5_ros$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsk <- merge(fbsb, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbsl <- fbsk[order(fbsk$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbsl$ms5_ros)

#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
#Change from test 5. Use 90th percentile predicted ROS, rather than max(predicted ros).
ms5_pig <- round(((fbsl$ms5_ros - min(fbsl$ms5_ros))/
                    ((fbsl$ms5_ros[round((length(fbsl[,1]) * 0.9),0)]) - min(fbsl$ms5_ros))), 4)

#Create a data frame with data through fuel moisture scenario 3.
ms5.df <- data.frame(fuelbed = fbsl$fuelbed, 
                     andreu_fuelbed_no = fbsl$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsl$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2, 
                     ms2_ros = fbsf$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2, 
                     ms3_ros = fbsh$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2, 
                     ms4_ros = fbsj$ms4_ros, 
                     ms4_pig = ms4_pig, 
                     ms4_dif = ms4_pig - expected_pigs_v2, 
                     ms5_ros = fbsl$ms5_ros, 
                     ms5_pig = ms5_pig, 
                     ms5_dif = ms5_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 70: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms5.df[ms5.df$topo == 1 & ms5.df$treatment == 1,]

x1_comparison(wet_flatlands, 8, 25, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 22, 25, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 71: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms5.df[ms5.df$topo == 2 & ms5.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 25, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 22, 25, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 72: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms5.df[ms5.df$topo == 3 & ms5.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 25, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 22, 25, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 73: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms5.df[ms5.df$topo == 4 & ms5.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 25, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 22, 25, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 74:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms5.df[ms5.df$topo == 1 & ms5.df$treatment == 5,]
mesic_flatlands <- ms5.df[ms5.df$topo == 2 & ms5.df$treatment == 5,]
mesic_uplands <- ms5.df[ms5.df$topo == 3 & ms5.df$treatment == 5,]
xeric_uplands <- ms5.df[ms5.df$topo == 4 & ms5.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
x1_comparison(wet_flatlands, 8, 25, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 22, 25, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 25, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 22, 25, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 25, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 22, 25, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 25, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 22, 25, plotTitle[4], 0, 1)

#PIG trends are reflective of natural fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 75: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms5.df[ms5.df$topo == 3 & ms5.df$treatment == aa,]
xeric_uplands <- ms5.df[ms5.df$topo == 4 & ms5.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 25, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 22, 25, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 25, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 22, 25, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 76: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms5.df[ms5.df$topo == 3 & ms5.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms5.df[ms5.df$topo == 4 & ms5.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 25, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 22, 25, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 25, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 22, 25, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 77: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms5.df$expected_pig, ms5.df$ms5_pig)

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
ms5.lm <- lm(ms5.df$ms5_pig ~ ms5.df$expected_pig)
summary(ms5.lm)
plot(ms5.df$expected_pig, ms5.df$ms5_pig)
abline(ms5.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms5.df$ms5_dif)

#Populate data frame
scenario <- 6
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 1 & ms5.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 2 & ms5.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 3 & ms5.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 4 & ms5.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 1 & ms5.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 2 & ms5.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 3 & ms5.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms5.df$ms5_dif[ms5.df$topo == 4 & ms5.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms5.df$ms5_dif)),1)

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



#SECTION 2
#TEST 7
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING 18 FUEL MOISTURE GROUPS
#FUEL MOISTURE ~ TOPOGRAPHIC POSITION/2 + CANOPY COVER + STAND AGE | COVER + TREATMENT
#VARIANT OF EQUATION 2
#s2t7 




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#STEP 78: CALCULATE DIFFERENCE BETWEEN MS3 AND ms4 PIGS

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 6
#Transformations -- None
ms6_ros <- read.table("inputs/fccs_ros_moistureScenario_6.csv", 
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(ms6_ros$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms6_ros = ms6_ros$Custom_ROS)

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbsm <- merge(fbsb, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbsn <- fbsm[order(fbsm$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbsn$ms6_ros)

#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
#Change from test 5. Use 90th percentile predicted ROS, rather than max(predicted ros).
ms6_pig <- round(((fbsn$ms6_ros - min(fbsn$ms6_ros))/
                    ((fbsn$ms6_ros[round((length(fbsn[,1]) * 0.9),0)]) - min(fbsn$ms6_ros))), 4)
ms6_pig[ms6_pig > 1] <- 1

#Create a data frame with data through fuel moisture scenario 3.
ms6.df <- data.frame(fuelbed = fbsn$fuelbed, 
                     andreu_fuelbed_no = fbsn$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsn$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2, 
                     ms2_ros = fbsf$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2, 
                     ms3_ros = fbsh$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2, 
                     ms4_ros = fbsj$ms4_ros, 
                     ms4_pig = ms4_pig, 
                     ms4_dif = ms4_pig - expected_pigs_v2, 
                     ms5_ros = fbsl$ms5_ros, 
                     ms5_pig = ms5_pig, 
                     ms5_dif = ms5_pig - expected_pigs_v2, 
                     ms6_ros = fbsn$ms6_ros, 
                     ms6_pig = ms6_pig, 
                     ms6_dif = ms6_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 79: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms6.df[ms6.df$topo == 1 & ms6.df$treatment == 1,]

x1_comparison(wet_flatlands, 8, 28, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 25, 28, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 80: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms6.df[ms6.df$topo == 2 & ms6.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 28, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 25, 28, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 81: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms6.df[ms6.df$topo == 3 & ms6.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 28, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 25, 28, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 82: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms6.df[ms6.df$topo == 4 & ms6.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 28, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 25, 28, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 83:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms6.df[ms6.df$topo == 1 & ms6.df$treatment == 5,]
mesic_flatlands <- ms6.df[ms6.df$topo == 2 & ms6.df$treatment == 5,]
mesic_uplands <- ms6.df[ms6.df$topo == 3 & ms6.df$treatment == 5,]
xeric_uplands <- ms6.df[ms6.df$topo == 4 & ms6.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
x1_comparison(wet_flatlands, 8, 28, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 25, 28, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 28, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 25, 28, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 28, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 25, 28, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 28, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 25, 28, plotTitle[4], 0, 1)

#PIG trends are reflective of natural fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 84: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms6.df[ms6.df$topo == 3 & ms6.df$treatment == aa,]
xeric_uplands <- ms6.df[ms6.df$topo == 4 & ms6.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 28, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 19, 28, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 28, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 19, 28, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 85: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms6.df[ms6.df$topo == 3 & ms6.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms6.df[ms6.df$topo == 4 & ms6.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 28, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 25, 28, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 28, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 25, 28, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 86: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms6.df$expected_pig, ms6.df$ms6_pig)

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
ms6.lm <- lm(ms6.df$ms6_pig ~ ms6.df$expected_pig)
summary(ms6.lm)
plot(ms6.df$expected_pig, ms6.df$ms6_pig)
abline(ms6.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms6.df$ms6_dif)

#Populate data frame
scenario <- 7
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 1 & ms6.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 2 & ms6.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 3 & ms6.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 4 & ms6.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 1 & ms6.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 2 & ms6.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 3 & ms6.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms6.df$ms6_dif[ms6.df$topo == 4 & ms6.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms6.df$ms6_dif)),1)

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



#SECTION 2
#TEST 8
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING 18 FUEL MOISTURE GROUPS
#FUEL MOISTURE ~ TOPOGRAPHIC POSITION/2 + CANOPY COVER + STAND AGE | COVER + TREATMENT
#VARIANT OF EQUATION 2
#ADD NEW FUELBEDS FOR RESTORATION FUEL TYPES
#s2t8




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#STEP 87: CALCULATE DIFFERENCE BETWEEN MS3 AND ms4 PIGS

#Break Anne's fuelbeds into component parts and create a dataframe.
aa_fca <- as.character(fbsb$andreu_fuelbed_no)
aa_fcb <- strsplit(aa_fca, "")
aa_fcc <- matrix(as.numeric(unlist(aa_fcb)), nrow = length(aa_fcb), ncol = length(aa_fcb[[1]]), byrow = T)

#Look at fuelbeds in xeric uplands where a single fire regime in Anne's fuelbeds is used
#to represent fuelbeds of different fire regimes in the FDM model. This is the source of
#the poor relationship between expected and predicted PIGs in test 7, mainly for the
#restoration fuelbeds.
fbsb[,c(1,4)][(fcc[,5] - aa_fcc[,5]) != 0 & fcc[,4] == 4,]

#I used this list to create new FCCS fuelbeds. I only changed the names, not parameters.
#This way the correct fuel moisture scenarios can be assigned to each fuelbed.

#RELEAD FUELBED SYNCHRONICITY TABLE BECAUSE YOU HAVE NEW FUELBEDS
#Load crosswalk between FCCS, Graphical STM, and FDM/STM fuelbeds
fbsa_t8 <- read.table("inputs/fuelbed_synchronicty_test8.csv", 
                   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Recode the $andreu_fuelbed_no integers so they are the same length as the FDM/STM fuelbeds.
#First convert integers to characters
fbra_t8 <- as.character(fbsa_t8$andreu_fuelbed_no)

#Second, recode fuelbeds to remove zeros at the 2 and 6 positions. This reduces the FCCS Fuelbed
#numbers from 7 to 5 digit integers
fbrb_t8 <- sapply(fbra_t8, recode)

#Third replace 7-digit integers with recoded 5-digit integers.
#warning -- N/As will be generated for dummy STM fuelbeds
fbsa_t8$andreu_fuelbed_no <- fbrb_t8

#Compare fuelbeds from your synchroncity table with the most recent copy from the FDM repository.
#This ensures you are not working with a dated copy.
all.equal(fbsa_t8$fuelbed, lut$fuelbed)

#Remove dummy fuelbeds
fbsb_t8 <- fbsa_t8[!is.na(fbsa_t8$andreu_fuelbed_no) == T,]

#Load initial FCCS fire behavior predictions.
#Fuel Moisture scenario -- 7
#Transformations -- None
ms7_ros <- read.table("inputs/fccs_ros_moistureScenario_7.csv", 
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Check for any duplicate fuelbeds in fuel moisture scenario 7 FCCS Outputs
#length(sort(unique(ms7_ros$Fuelbed_number)))
#duplicated(ms7_ros$Fuelbed_number)
#Vector output should contain all FALSE elements.

#Condense this table to fuelbeds and rate of spread predictions
#First recode the five digit fuelbed number to a seven digit number.
ch.fb <- as.character(ms7_ros$Fuelbed_number)
ch.fb_re <- mapply(function(y) {recode(y)}, ch.fb)
ch.fb_reNu <- as.numeric(ch.fb_re)
ros <- data.frame(fuelbed = ch.fb_reNu, ms7_ros = ms7_ros$Custom_ROS)

#Check to make sure there are no fuelbeds in the fuelbed synchronicity table that don't match with 
#fuelbeds in FCCS output table
#aa <- match(fbsb_t8$andreu_fuelbed_no, ros$fuelbed)
#bb <- cbind(fbsb_t8$andreu_fuelbed_no, aa)
#bb[is.na(bb[,2] == T)]
#Output should be "numeric(0)" i.e., not unmatched fuelbeds.

#Merge Andreu's fuelbeds and model-derived ROS predictions into the complete list of Eglin Fuelbeds
fbso <- merge(fbsb_t8, ros, by.x = "andreu_fuelbed_no", by.y = "fuelbed")
fbsp <- fbso[order(fbso$fuelbed),]

#Before you apply the max Moisture Scenario 3 ROS to determine PIGs look at their distribution. 
barplot(fbsp$ms7_ros)

#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
#Change from test 5. Use 90th percentile predicted ROS, rather than max(predicted ros).
ms7_pig <- round(((fbsp$ms7_ros - min(fbsp$ms7_ros))/
                    ((fbsp$ms7_ros[round((length(fbsp[,1]) * 0.9),0)]) - min(fbsp$ms7_ros))), 4)
ms7_pig[ms7_pig > 1] <- 1

#Create a data frame with data through fuel moisture scenario 3.
ms7.df <- data.frame(fuelbed = fbsn$fuelbed, 
                     andreu_fuelbed_no = fbsn$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsn$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2, 
                     ms2_ros = fbsf$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2, 
                     ms3_ros = fbsh$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2, 
                     ms4_ros = fbsj$ms4_ros, 
                     ms4_pig = ms4_pig, 
                     ms4_dif = ms4_pig - expected_pigs_v2, 
                     ms5_ros = fbsl$ms5_ros, 
                     ms5_pig = ms5_pig, 
                     ms5_dif = ms5_pig - expected_pigs_v2, 
                     ms6_ros = fbsn$ms6_ros, 
                     ms6_pig = ms6_pig, 
                     ms6_dif = ms6_pig - expected_pigs_v2, 
                     ms7_ros = fbsp$ms7_ros, 
                     ms7_pig = ms7_pig, 
                     ms7_dif = ms7_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 88: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms7.df[ms7.df$topo == 1 & ms7.df$treatment == 1,]

x1_comparison(wet_flatlands, 8, 31, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 28, 31, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 80: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms7.df[ms7.df$topo == 2 & ms7.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 31, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 28, 31, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 81: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms7.df[ms7.df$topo == 3 & ms7.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 31, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 28, 31, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 82: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms7.df[ms7.df$topo == 4 & ms7.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 31, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 28, 31, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 83:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms7.df[ms7.df$topo == 1 & ms7.df$treatment == 5,]
mesic_flatlands <- ms7.df[ms7.df$topo == 2 & ms7.df$treatment == 5,]
mesic_uplands <- ms7.df[ms7.df$topo == 3 & ms7.df$treatment == 5,]
xeric_uplands <- ms7.df[ms7.df$topo == 4 & ms7.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
x1_comparison(wet_flatlands, 8, 31, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 28, 31, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 31, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 28, 31, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 31, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 28, 31, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 31, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 28, 31, plotTitle[4], 0, 1)

#PIG trends are reflective of natural fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 84: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms7.df[ms7.df$topo == 3 & ms7.df$treatment == aa,]
xeric_uplands <- ms7.df[ms7.df$topo == 4 & ms7.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 31, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 28, 31, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 31, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 28, 31, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 85: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms7.df[ms7.df$topo == 3 & ms7.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms7.df[ms7.df$topo == 4 & ms7.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 31, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 28, 31, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 31, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 28, 31, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 86: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms7.df$expected_pig, ms7.df$ms7_pig)

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
ms7.lm <- lm(ms7.df$ms7_pig ~ ms7.df$expected_pig)
summary(ms7.lm)
plot(ms7.df$expected_pig, ms7.df$ms7_pig)
abline(ms7.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms7.df$ms7_dif)

#Populate data frame
scenario <- 8
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 1 & ms7.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 2 & ms7.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 3 & ms7.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 4 & ms7.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 1 & ms7.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 2 & ms7.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 3 & ms7.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms7.df$ms7_dif[ms7.df$topo == 4 & ms7.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms7.df$ms7_dif)),1)

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



#SECTION 2
#TEST 9
#USE SYSTEMATICALLY DETERMINED PIGS TO TEST ACCURACY OF ROS-DERIVED PIGS USING 18 FUEL MOISTURE GROUPS
#FUEL MOISTURE ~ TOPOGRAPHIC POSITION/2 + CANOPY COVER + STAND AGE | COVER + TREATMENT
#VARIANT OF EQUATION 2
#ADD NEW FUELBEDS FOR RESTORATION FUEL TYPES
#s2t9




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################


#Use feature scaling to onvert moisture scenario 1 predicted ROS into probability of ignition (PIG)
#values for each fuelbed.
#Change from test 5. Use 90th percentile predicted ROS, rather than max(predicted ros).
ms8_pig <- round(((fbsp$ms7_ros - min(fbsp$ms7_ros))/
                    ((fbsp$ms7_ros[round((length(fbsp[,1]) * 0.8),0)]) - min(fbsp$ms7_ros))), 4)
ms8_pig[ms8_pig > 1] <- 1

#Create a data frame with data through fuel moisture scenario 3.
ms8.df <- data.frame(fuelbed = fbsn$fuelbed, 
                     andreu_fuelbed_no = fbsn$andreu_fuelbed_no, 
                     topo = fcc[,1], 
                     cover = fcc[,3], 
                     treatment = fcc[,4], 
                     mfri = fcc[,5], 
                     age = fcc[,7], 
                     expected_pig = expected_pigs_v2, 
                     benchmark_ros = fbsn$benchmark_ros, 
                     benchmark_pig = benchmark_pig, 
                     benchmark_dif = benchmark_pig - expected_pigs_v2, 
                     ms1_ros = fbsd$ms1_ros, 
                     ms1_pig = ms1_pig, 
                     ms1_dif = ms1_pig - expected_pigs_v2, 
                     ms2_ros = fbsf$ms2_ros, 
                     ms2_pig = ms2_pig, 
                     ms2_dif = ms2_pig - expected_pigs_v2, 
                     ms3_ros = fbsh$ms3_ros, 
                     ms3_pig = ms3_pig, 
                     ms3_dif = ms3_pig - expected_pigs_v2, 
                     ms4_ros = fbsj$ms4_ros, 
                     ms4_pig = ms4_pig, 
                     ms4_dif = ms4_pig - expected_pigs_v2, 
                     ms5_ros = fbsl$ms5_ros, 
                     ms5_pig = ms5_pig, 
                     ms5_dif = ms5_pig - expected_pigs_v2, 
                     ms6_ros = fbsn$ms6_ros, 
                     ms6_pig = ms6_pig, 
                     ms6_dif = ms6_pig - expected_pigs_v2, 
                     ms7_ros = fbsp$ms7_ros, 
                     ms7_pig = ms7_pig, 
                     ms7_dif = ms7_pig - expected_pigs_v2, 
                     ms8_ros = fbsp$ms7_ros, 
                     ms8_pig = ms8_pig, 
                     ms8_dif = ms8_pig - expected_pigs_v2)

##############################################################################################################
##############################################################################################################
#STEP 88: 
#COMPARE WET FLATLANDS -- NATURAL FUELBEDS

#Subset all natural fuelbeds in wet flatlands fuelbeds.
plotTitle <- c("Wet Flatlands -- Natural Fuelbeds")
wet_flatlands <- ms8.df[ms8.df$topo == 1 & ms8.df$treatment == 1,]

x1_comparison(wet_flatlands, 8, 34, plotTitle, 0, 1)
x1_comparison(wet_flatlands, 31, 34, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 80: 
#ALL NATURAL FUELBEDS IN MESIC FLATLANDS.

#Subset all natural fuelbeds for mesic flatlands
plotTitle <- c("Mesic Flatlands -- Natural Fuelbeds")
mesic_flatlands <- ms8.df[ms8.df$topo == 2 & ms8.df$treatment == 1,]

x1_comparison(mesic_flatlands, 8, 34, plotTitle, 0, 1)
x1_comparison(mesic_flatlands, 31, 34, plotTitle, 0, 1)

#PIGs for older stand ages are slightly lower across the board

##############################################################################################################
##############################################################################################################
#STEP 81: 
#ALL NATURAL FUELBEDS IN MESIC UPLANDS.

#Subset all natural fuelbeds for mesic uplands
plotTitle <- c("Mesic Uplands -- Natural Fuelbeds")
mesic_uplands <- ms8.df[ms8.df$topo == 3 & ms8.df$treatment == 1,]

x1_comparison(mesic_uplands, 8, 34, plotTitle, 0, 1)
x1_comparison(mesic_uplands, 31, 34, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 82: 
#ALL NATURAL FUELBEDS IN XERIC UPLANDS.

#Subset all natural fuelbeds for xeric uplands
plotTitle <- c("Xeric Uplands -- Natural Fuelbeds")
xeric_uplands <- ms8.df[ms8.df$topo == 4 & ms8.df$treatment == 1,]

x1_comparison(xeric_uplands, 8, 34, plotTitle, 0, 1)
x1_comparison(xeric_uplands, 31, 34, plotTitle, 0, 1)

#PIGs for 1-3 longleaf > 40 years old are slightly higher, but PIGs for other older longleaf are slightly
#lower.

##############################################################################################################
##############################################################################################################
#STEP 83:
#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS

#Subset all plantation fuelbeds by topographic position.
plotTitle <- c("Wet Flatlands -- Plantations", 
               "Mesic Flatlands -- Plantations", 
               "Mesic Uplands -- Plantations", 
               "Xeric Uplands -- Plantations")
wet_flatlands <- ms8.df[ms8.df$topo == 1 & ms8.df$treatment == 5,]
mesic_flatlands <- ms8.df[ms8.df$topo == 2 & ms8.df$treatment == 5,]
mesic_uplands <- ms8.df[ms8.df$topo == 3 & ms8.df$treatment == 5,]
xeric_uplands <- ms8.df[ms8.df$topo == 4 & ms8.df$treatment == 5,]

#ALL PLANTATION FUELBEDS IN ALL TOPOGRAPHIC POSITIONS
x1_comparison(wet_flatlands, 8, 34, plotTitle[1], 0, 1)
x1_comparison(wet_flatlands, 31, 34, plotTitle[1], 0, 1)
x1_comparison(mesic_flatlands, 8, 34, plotTitle[2], 0, 1)
x1_comparison(mesic_flatlands, 31, 34, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 8, 34, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 31, 34, plotTitle[3], 0, 1)
x1_comparison(xeric_uplands, 8, 34, plotTitle[4], 0, 1)
x1_comparison(xeric_uplands, 31, 34, plotTitle[4], 0, 1)

#PIG trends are reflective of natural fuelbeds.

##############################################################################################################
##############################################################################################################
#STEP 84: ALL POST-RESTORATION FUELBEDS IN ALL NATURAL FUELBEDS FOR ALL TOPOGRAPHIC POSITIONS.
#Evaluate the degree to which probability of ignition derived from FCCS rate of spread predictions
#correlates with probability of ignition derived from Eglin staff meetings.

#Subset restoration fuelbeds derived from natural fuels
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Restoration", 
               "Xeric Uplands -- Restoration")
aa <- 4
mesic_uplands <- ms8.df[ms8.df$topo == 3 & ms8.df$treatment == aa,]
xeric_uplands <- ms8.df[ms8.df$topo == 4 & ms8.df$treatment == aa,]

x1_comparison(mesic_uplands, 8, 34, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 31, 34, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 34, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 31, 34, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 85: 
#ALL RESTORATION FUELBEDS IN ALL PLANTATION FOR ALL TOPOGRAPHIC POSITIONS.

#Subset restoration fuelbeds derived from plantations
#Note ----- there are no restoration fuelbeds in wet or mesic flatlands
plotTitle <- c("Mesic Uplands -- Plantations with Restoration", 
               "Xeric Uplands -- Plantations with Restoration")
mesic_uplands <- ms8.df[ms8.df$topo == 3 & ms8.df$treatment %in% c(6,7,8),]
xeric_uplands <- ms8.df[ms8.df$topo == 4 & ms8.df$treatment %in% c(6,7,8),]

x1_comparison(mesic_uplands, 8, 34, plotTitle[1], 0, 1)
x1_comparison(mesic_uplands, 31, 34, plotTitle[1], 0, 1)
x1_comparison(xeric_uplands, 8, 34, plotTitle[2], 0, 1)
x1_comparison(xeric_uplands, 31, 34, plotTitle[2], 0, 1)

#In general PIGs are slightly lower.

#############################################################################################################
##############################################################################################################
#STEP 86: 
#QUANTIFY THE DEGREE OF CORRESPONDANCE BETWEEN FCCS/PREDICTED ROS-DERIVED PIGS AND EXPECTED PIGS.
#THIS WILL BE THE BASIS FOR EVALUATING ALL FUTURE FCCS/PREDICTED ROS-DERIVED PIGS
plot(ms8.df$expected_pig, ms8.df$ms8_pig)

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
ms8.lm <- lm(ms8.df$ms8_pig ~ ms8.df$expected_pig)
summary(ms8.lm)
plot(ms8.df$expected_pig, ms8.df$ms8_pig)
abline(ms8.lm)
#Not even close

#Quantify deviation from expected PIGS
plot(ms8.df$ms8_dif)

#Populate data frame
scenario <- 8
ms$WetFlatlands_Nat[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 1 & ms8.df$treatment == 1])),1)
ms$MesicFlatlands_Nat[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 2 & ms8.df$treatment == 1])),1)
ms$MesicUplands_Nat[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 3 & ms8.df$treatment == 1])),1)
ms$XericUplands_Nat[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 4 & ms8.df$treatment == 1])),1)
ms$WetFlatlands_Other[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 1 & ms8.df$treatment > 1])),1)
ms$MesicFlatlands_Other[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 2 & ms8.df$treatment > 1])),1)
ms$MesicUplands_Other[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 3 & ms8.df$treatment > 1])),1)
ms$XericUplands_Other[scenario] <- round(sum(abs(ms8.df$ms8_dif[ms8.df$topo == 4 & ms8.df$treatment > 1])),1)
ms$Total[scenario] <- round(sum(abs(ms8.df$ms8_dif)),1)

ms
#END---------------------------------------------------------------------------------------
