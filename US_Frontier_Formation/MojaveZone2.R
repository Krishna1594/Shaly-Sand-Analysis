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

# Analysis (continued)
## Lets load Nearest Shale data:
Shaledata <- read_csv(file.choose())
Zone2 <- read_csv(file.choose()) # for the depth range of 9110ft till 9232ft

## Using the same parameters set for this well,
R_sh = 18.0   # Ohm-m

Sal_FF_zone2 = 13000.0 #mg/L or ppm * Source USGS
T_bottomhole = 192.0 #°F
T_formation_Zone2 = 75 + (T_bottomhole - 75)/12802 * 9171 # Surface temperature (75°F) and formation depth mid-point = 9171t. Depth@192 = 12802ft.
R_w_zone2 = (400000/(T_formation_Zone2 * Sal_FF_zone2))^0.88 #ohm-m

## 1. Simandoux Equation

Zone2$Sw_sim <- (0.4*R_w_zone2/(Zone2$PHI_e)^2)*(((((Zone2$V_sh_corr_St/(100*R_sh))^2)+(5*(Zone2$PHI_e^2)/(Zone2$Res90IN*R_w)))^0.5)-(Zone2$V_sh_corr_St/(R_sh*100))) 

# Both parameters are from Humble equation for Shale Formation Factor (F)
a = 0.62 # unconsolidated sandstones
m = 2.15 # unconsolidated sandstones 
n = 2 #assumption
Zone2$f <- a/(Zone2$PHI_e^m)

### The Indonesia equation

Zone2$Sw_ind <- (((1/Zone2$Res90IN)^0.5)/((((0.01*Zone2$V_sh_corr_St)^(1-(0.5*Zone2$V_sh_corr_St/100)))/R_sh^0.5)+((1/(Zone2$f*R_w_zone2))^0.5)))^(2/n)

## Plotting our curves

par(mar=c(0.25, 0.25, 0.25, 0.25),mfrow = c(1,5),oma = c(5, 3, 3, 0.4))
#Plotting Gamma-caliper
par(xpd=F)
plot(Zone2$DCAL,Zone2$Depth, type = "l", xaxs = "i",
     yaxs = "i", xlim = c(0,150), ylim = c(9250,9110), col = "black",
     ylab = "Depth", xlab = "", lwd = 1)
axis(3, seq(0,150,15))
axis(2, at= pretty(Zone2$Depth), labels = pretty(Zone2$Depth))
points(Zone2$GR,Zone2$Depth, type = "l", col ="dark green", lwd = 1)
grid(nx = 5, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "DCAL",
       border = NULL,
       text.col = "black",
       bty = "n",
       horiz = T , 
       inset = c(0, 0))
legend("bottom", 
       legend = "GR",
       border = NULL,
       text.col = "darkgreen", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.02))

# Resistivity Track
plot(Zone2$Res10IN, Zone2$Depth, type = "l", xaxs = "i",
     yaxs = "i", yaxt = "n" , xlim = c(0.1,1000), log = 'x', ylim = c(9250,9110), col = "magenta",
     xlab = "", lwd = 1)
at.x <- outer(1:9, 10^(0:3))
lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
axis(3, at=at.x, labels=lab.x, las=1)
points(Zone2$Res20IN, Zone2$Depth, type = "l", col ="green", lwd = 1)
points(Zone2$Res30IN, Zone2$Depth, type = "l", col ="blue", lwd = 1)
points(Zone2$Res60IN, Zone2$Depth, type = "l", col ="red", lwd = 1)
points(Zone2$Res90IN, Zone2$Depth, type = "l", col ="gray40", lwd = 1)
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "10in Res",
       border = NULL,
       text.col = "magenta",
       bty = "n",
       horiz = T , 
       inset = c(0, 0))
legend("bottom", 
       legend = "20in Res",
       border = NULL,
       text.col = "green", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.02))
legend("bottom", 
       legend = "30in Res",
       border = NULL,
       text.col = "blue",
       bty = "n",
       horiz = T , 
       inset = c(0, 0.04))
legend("bottom", 
       legend = "60in Res",
       border = NULL,
       text.col = "red",
       bty = "n",
       horiz = T , 
       inset = c(0, 0.06))
legend("bottom", 
       legend = "90in Res",
       border = NULL,
       text.col = "gray40",
       bty = "n",
       horiz = T , 
       inset = c(0, 0.08))

#Plotting Density-Neutron porosity and PEF
plot(Zone2$RHOB,Zone2$Depth, type = "l", xaxt = "n",
     yaxt = "n",xaxs = "i", yaxs = "i", xlim = c(1.8,2.8), ylim = c(9250,9110), col = "red",
     xlab = "", lwd = 1)
axis(3, seq(1.8,2.8,0.2), col="red",col.ticks="red",col.axis="red")
par(new=T)
plot(Zone2$CNSS, Zone2$Depth, type = "l", col ="#2a4a9b", lwd = 1, xlim = c(54,-6),
     ylim = c(9250,9110), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
axis(1, xlim=c(-54,6),line=0,col="#3a4a9b",col.ticks="#2a4a9b",col.axis="#2a4a9b")
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "RHOB",
       border = NULL,
       text.col = "red",
       bty = "n",
       horiz = F , 
       inset = c(0, 0))
legend("bottom", 
       legend = "CNSS",
       border = NULL,
       text.col = "#2a4a9b", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.02))
par(new=T)
plot(Zone2$PEF,Zone2$Depth, type = "l", col ="#660010", lwd = 1, xlim = c(0,20),
     ylim = c(9250,9110), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylab = "")
axis(1, xlim=c(0,20),line=2.2,col="#660010",col.ticks="#660010",col.axis="#660010")
legend("bottom", 
       legend = "PEF",
       border = NULL,
       text.col = "#660010",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.04))

## Plotting shale volume and Water Saturation 

plot(Zone2$V_sh_corr_St,Zone2$Depth, type = "l", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", xlim = c(0,100), ylim = c(9250,9110), col = "gray30",
     xlab = "", lwd = 2)
#polygon(c(FrontierForm$V_sh_corr_St, rev(FrontierForm$V_sh_corr_St)), c(rep(min(FrontierForm$Depth), length(FrontierForm$V_sh_corr_St)), rev(FrontierForm$Depth)), col = 'gray20', border = NA)
axis(1, xlim=c(0,100),line=0,col="gray20",col.ticks="black",col.axis="gray20")
axis(3, xlim=c(0,100),line=0,col="gray20",col.ticks="black",col.axis="gray20")
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "V_sh %",
       border = NULL,
       text.col = "black",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.04))

## Plotting water Saturations from both Simandoux and Indonesia methods 
plot(Zone2$Sw_sim, Zone2$Depth, type = "l", col ="blue", lwd = 1, xlim = c(0,1),
     ylim = c(9250,9110), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylab = "")
points(Zone2$Sw_ind, Zone2$Depth, type = "l", col ="red", lwd = 1)
axis(1, xlim=c(0,1),line=0,col="blue",col.ticks="blue",col.axis="blue")
axis(3, xlim=c(0,1),line=0,col="blue",col.ticks="blue",col.axis="blue")
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "Sw (Simandoux)",
       border = NULL,
       text.col = "blue",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.02))
legend("bottom", 
       legend = "Sw (Indonesia)",
       border = NULL,
       text.col = "red",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.04))



## Exporting our Data
write.csv(FrontierForm, "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/US_FRONTIER/Zone2_final.csv")
