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

#Break fuelbeds into component parts and create a dataframe that will be source for visualizations.
fca <- as.character(fbsb$fuelbed)
fcb <- strsplit(fca, "")
fcc <- matrix(as.numeric(unlist(fcb)), nrow = length(fcb), ncol = length(fcb[[1]]), byrow = T)

fbsc <- data.frame(fuelbed = fbsb$fuelbed, andreu_fuelbed_no = fbsb$andreu_fuelbed_no, 
                   benchmark_ros = fbsb$benchmark_ros, topo = fcc[,1], cover = fcc[,3], 
                   treatment = fcc[,4], mfri = fcc[,5], age = fcc[,7])
##############################################################################################################
##############################################################################################################
#STEP 3: CREATE 3-D BARCHARTS TO VISUALIZE ROS OVER NATURAL/PLANTATION FUELBEDS IN EACH TOPOGRAPHIC 
#POSITION

#Subset wet flatlands fuelbeds you want to look at
#Wet Flatlands
wf_ns <- fbsc[fbsc$fuelbed %in% c(1011101:1011106, 
                                  1011201:1011206, 
                                  1011301:1011306, 
                                  1031401:1031406),]
wf_ns <- wf_ns[order(wf_ns$fuelbed),]

#Mesic Flatlands
mf_ns <- fbsc[fbsc$fuelbed %in% c(2011101:2011106, 
                                  2011201:2011206, 
                                  2011301:2011306, 
                                  2031401:2031406),]
mf_ns <- mf_ns[order(mf_ns$fuelbed),]

#Mesic Uplands
mu_ns <- fbsc[fbsc$fuelbed %in% c(3011101:3011106, 
                                  3011201:3011206, 
                                  3011301:3011306, 
                                  3031401:3031406),]
mu_ns <- mu_ns[order(mu_ns$fuelbed),]

#Xeric Uplands
xu_ns <- fbsc[fbsc$fuelbed %in% c(4011101:4011106, 
                                  4011201:4011206, 
                                  4011301:4011306, 
                                  4031401:4031406),]
xu_ns <- xu_ns[order(xu_ns$fuelbed),]

#Column number of fuelbed parameter you want to look at
type <-3

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
wf_cat <- barplot(wf_ns[,type], wf_ns$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               wf_ns$mfri), 
                  names.arg = NULL, 
                  main = paste("Wet Flatlands", 
                               names(wf_ns)[type], 
                               sep = " "), 
                  ylim = c(0, max(wf_ns$benchmark_ros, 
                                  mf_ns$benchmark_ros, 
                                  mu_ns$benchmark_ros, 
                                  xu_ns$benchmark_ros)))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(wf_ns$fuelbed),
          x = unit(wf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Flatlands
mf_cat <- barplot(mf_ns[,type], mf_ns$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mf_ns$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Flatlands", 
                               names(mf_ns)[type], 
                               sep = " "), 
                  ylim = c(0, max(wf_ns$benchmark_ros, 
                                  mf_ns$benchmark_ros, 
                                  mu_ns$benchmark_ros, 
                                  xu_ns$benchmark_ros)))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(mf_ns$fuelbed),
          x = unit(mf_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Mesic Uplands
mu_cat <- barplot(mu_ns[,type], mu_ns$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               mu_ns$mfri), 
                  names.arg = NULL, 
                  main = paste("Mesic Uplands", 
                               names(mu_ns)[type], 
                               sep = " "), 
                  ylim = c(0, max(wf_ns$benchmark_ros, 
                                  mf_ns$benchmark_ros, 
                                  mu_ns$benchmark_ros, 
                                  xu_ns$benchmark_ros)))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(as.character(mu_ns$fuelbed),
          x = unit(mu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)

#Xeric Uplands
xu_cat <- barplot(xu_ns[,type], xu_ns$fuelbed, 
                  col = mapply(function(y) colors_fr[y+1], 
                               xu_ns$mfri), 
                  names.arg = NULL, 
                  main = paste("Xeric Uplands", 
                               names(xu_ns)[type], 
                               sep = " "), 
                  ylim = c(0, max(wf_ns$benchmark_ros, 
                                  mf_ns$benchmark_ros, 
                                  mu_ns$benchmark_ros, 
                                  xu_ns$benchmark_ros)))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)


grid.text(as.character(xu_ns$fuelbed),
          x = unit(xu_cat, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp = gpar(cex = font))
popViewport(3)










#Create a 3d barchart
print(ggplot(fb.sub, aes(age, benchmark_ros)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ mfri, ncol = 2))




# Generate some sample data, then compute mean and standard deviation
# in each group
df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)
ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y))

# The summary data frame ds is used to plot larger red points on top
# of the raw data. Note that we don't need to supply `data` or `mapping`
# in each layer because the defaults from ggplot() are used.
ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

# Same plot as above, declaring only the data frame in ggplot().
# Note how the x and y aesthetics must now be declared in
# each geom_point() layer.
ggplot(df) +
  geom_point(aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3)

# Alternatively we can fully specify the plot in each layer. This
# is not useful here, but can be more clear when working with complex
# mult-dataset graphics
ggplot() +
  geom_point(data = df, aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
  geom_errorbar(
    data = ds,
    aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
    colour = 'red',
    width = 0.4
  )










