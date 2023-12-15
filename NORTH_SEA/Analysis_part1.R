# Setting up New library for this analysis 
myPaths <- .libPaths()
myPaths <- c(myPaths, 'D:/ALL OLEUM/Shaly Sand Analysis/R-research project/R_Library')
.libPaths(myPaths)  # add new path
myPaths <- c(myPaths[3], myPaths[2], myPaths[1])  # switch them to make our temporary path to access our packages

# Our required packages
library(readxl) 
library(crayon) 
library(stringr) 
library(stringi) 
library(openxlsx) 
library(janitor)
library(ggrepel)
library(tidyr)
library(lubridate)
library(leaflet)
library(dplyr) 
library(ggplot2) 
library(plotly) 
library(plotrix) 
library(viridisLite) 
library(viridis)
library(hrbrthemes)
library(scales)
library(formattable)
library(showtext)
library(cowplot)
library(readr)
library(htmlwidgets)
library(mapview)
library(raster)
library(png)
# Turn on showtext
showtext_auto()


# Knowing Well data
## Going through well information, drilling reports, mud reports and survey reports can reveal whole scenario of the operation that went on this well. After, gathering mentioned
## reports, the following data has been taken into account for our calulations:

# Data from Mud Report: Gel-Lignosulphate mud
Rho_m = 9.88 #ppg
Cl_ions = 21000.00 #ppm concentration - Mud Report
Ca_ions = 1080.00 #ppm concentration
SO4_ions = 22.00 # Sulphonate ions - Lignites
OW_ratio = 0 #percentage
T_surface = 75.0 #°F 
T_bottom = 248 #°F

## The following expression is used to calculate equivalent Salinity of Mud
## the following graphs must be referred:
library(png)
dev.new()
Graph1 <- readPNG("D:/ALL OLEUM/Shaly Sand Analysis/R-research project/NORTH_SEA/Analysis/TotalSolidsconc.png",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(Graph1,0,0,1,1)

## Thus, applying multipliers

Eq_NaCl_sal = (Cl_ions*1.0)+(Ca_ions*1.07)+(SO4_ions*0.8) # These multipliers change based on mud ionic concentration

Graph2 <- readPNG("D:/ALL OLEUM/Shaly Sand Analysis/R-research project/NORTH_SEA/Analysis/RmudfromNaCl.png",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(Graph2,0,0,1,1)

## From the above graph, we can estimate the resistivity of the mud based on Salinity of the mud estimated above at surface temperature 75°C
R_mud_surf = 0.39 # Ohm-m @ 75°F and 22173.2 Naclppm 

dev.off()

## Correcting resistivity of mud fro formation temperatures: Arp's equation

R_mud = R_mud_surf * ((T_surface+6.77)/(T_bottom+6.77)) # Ohm-m

## It is imperative to calculate resistivities of mud filterate and mud cake
R_mf = 0.75 * R_mud #0hm-m @248°F
R_mc = 1.5 * R_mud #0hm-m @248°F


## We need to know resistivity of mud to start our shaly-sand analysis. From there, we will have to calculate resistivities of mud cake and mud filterate, and establish the value of 
## resistivity of formation water (Rw). There are number of ways to calculate Rw. However, in this case we have data of Spontaneous Potential log which will help us to accurately 
## calculate Rw at formation conditions.

# Calculation of Rw
## Establishing the SSP is vital for this calculation of Rw. From the logs it is seen that there is a shift of base line as we go deeper. Usually, we have to correct for the base line shift
## always as we enter into new formations. However, we are interested specifically in depths of 2700m till TD.


