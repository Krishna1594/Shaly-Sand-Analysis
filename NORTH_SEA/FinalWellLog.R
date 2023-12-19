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


# Final Well Log of 15_9-12

# Importing core data
Core_dat <- read_csv(file.choose())

summary(Core_dat)

par(mar=c(0.5, 0.5, 0.5, 0.5),mfrow = c(1,6),oma = c(4, 4, 3, 0.2))
## Plotting Gamma-caliper
par(xpd=F)
plot(condensate_dat$GR,condensate_dat$Depth, type = "l", xaxs = "i",
     yaxs = "i", xlim = c(0,150), ylim = c(3800,3400), col = "darkgreen",
     ylab = "Depth", xlab = "", lwd = 1)
axis(3, seq(0,150,15))
axis(2, at= pretty(condensate_dat$Depth), labels = pretty(condensate_dat$Depth))
points(condensate_dat$CALI,condensate_dat$Depth, type = "l", col ="black", lwd = 1)
legend("bottom", 
       legend = "CALI",
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
       inset = c(0, 0.01))
par(new=T)
plot(condensate_dat$SP,condensate_dat$Depth, type = "l", col ="green", lwd = 1, lty = "dashed",xaxs = "i",
     yaxs = "i", xaxt = "n", yaxt = "n", xlim = c(-20,100), ylim = c(3800,3400), ylab = "Depth", xlab = "")
axis(1, xlim=c(-20,100),line=2.2, col="green",col.ticks="green",col.axis="green")
grid(nx = 5, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "SP",
       border = NULL,
       text.col = "green", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.02))

# Resistivity Track
plot(condensate_dat$RSHA, condensate_dat$Depth, type = "l", xaxs = "i",
     yaxs = "i", yaxt = "n" , xlim = c(0.1,1000), log = 'x', ylim = c(3800,3400), col = "magenta",
     xlab = "", lwd = 1)
at.x <- outer(1:9, 10^(0:3))
lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
axis(3, at=at.x, labels=lab.x, las=1)
points(condensate_dat$RMED, condensate_dat$Depth, type = "l", col ="blue", lwd = 1)
points(condensate_dat$RDEP, condensate_dat$Depth, type = "l", col ="red", lwd = 1)
points(condensate_dat$Rxo, condensate_dat$Depth, type = "l", col ="green", lwd = 1)
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "Shallow Res",
       border = NULL,
       text.col = "magenta",
       bty = "n",
       horiz = T , 
       inset = c(0, 0))
legend("bottom", 
       legend = "Medium Res",
       border = NULL,
       text.col = "blue", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.01))
legend("bottom", 
       legend = "Deep Res",
       border = NULL,
       text.col = "red",
       bty = "n",
       horiz = T , 
       inset = c(0, 0.02))
legend("bottom", 
       legend = "Rxo - Flushed",
       border = NULL,
       text.col = "green",
       bty = "n",
       horiz = T , 
       inset = c(0, 0.03))

#Plotting Density-Neutron porosity and DRHO
plot(condensate_dat$RHOB,condensate_dat$Depth, type = "l", xaxt = "n",
     yaxt = "n",xaxs = "i", yaxs = "i", xlim = c(1,3), ylim = c(3800,3400), col = "red",
     xlab = "", lwd = 1)
axis(3, seq(1,3,0.5), col="red",col.ticks="red",col.axis="red")
par(new=T)
plot(condensate_dat$NPHI,condensate_dat$Depth, type = "l", col ="#3a7a9b", lwd = 1, xlim = c(1.05,-0.15),
     ylim = c(3800,3400), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
axis(1, xlim=c(1.0,-0.15),line=0,col="#3a7a9b",col.ticks="#3a7a9b",col.axis="#3a7a9b")
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "RHOB",
       border = NULL,
       text.col = "red",
       bty = "n",
       horiz = F , 
       inset = c(0, 0))
legend("bottom", 
       legend = "NPHI",
       border = NULL,
       text.col = "#3a7a9b", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.01))
par(new=T)
plot(condensate_dat$DRHO,condensate_dat$Depth, type = "l", col ="#660010", lwd = 1, xlim = c(0,5),
     ylim = c(3800,3400), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylab = "")
axis(1, xlim=c(0,5),line=2.2,col="#660010",col.ticks="#660010",col.axis="#660010")
legend("bottom", 
       legend = "DRHO",
       border = NULL,
       text.col = "#660010",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.02))

## Plotting Sonic velocities 
plot(condensate_dat$DTC,condensate_dat$Depth, type = "l", xaxt = "n",
     yaxt = "n",xaxs = "i", yaxs = "i", xlim = c(240,40), ylim = c(3800,3400), col = "black",
     xlab = "", lwd = 1)
axis(3, xlim=c(240,40), col="black",col.ticks="black",col.axis="black")
axis(1, xlim=c(240,40),line=0,col="black",col.ticks="black",col.axis="black")
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "DTC",
       border = NULL,
       text.col = "black",
       bty = "n",
       horiz = F , 
       inset = c(0, 0))

## Plotting shale volume
plot(condensate_dat$Vsh_corr_St,condensate_dat$Depth, type = "l", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", xlim = c(0,1), ylim = c(3800,3400), col = "gray30",
     xlab = "", lwd = 0.5)
polygon(c(condensate_dat$Vsh_corr_St, rev(condensate_dat$Vsh_corr_St)), c(rep(min(condensate_dat$Depth), length(condensate_dat$Vsh_corr_St)), rev(condensate_dat$Depth)), col = 'gray20', border = NA)
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

## Plotting the Sw from both models and comparing with core data
plot(condensate_dat$Sw_sim, condensate_dat$Depth, type = "l", xaxs = "i",
     yaxs = "i", yaxt = "n" , xlim = c(0,1), ylim = c(3800,3400), col = "blue",
     xlab = "", lwd = 1)
axis(1, xlim=c(0,1),line=0,col="red",col.ticks="red",col.axis="red")
axis(3, xlim=c(0,1),line=0,col="blue",col.ticks="blue",col.axis="blue")
points(condensate_dat$Sw_Ind, condensate_dat$Depth, type = "l", col ="red", lwd = 1)
points(Core_dat$Pore_Sw_decimal, Core_dat$Depth, col ="black", lwd = 1)
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "Sw Simandoux",
       border = NULL,
       text.col = "blue",
       bty = "n",
       horiz = T , 
       inset = c(0, 0))
legend("bottom", 
       legend = "Sw Indonesia",
       border = NULL,
       text.col = "red", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.01))
legend("bottom", 
       legend = "SW Core data",
       border = NULL,
       text.col = "black",
       bty = "n",
       horiz = T , 
       inset = c(0, 0.02))
