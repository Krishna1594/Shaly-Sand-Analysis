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

# Well Information
## The '15_9-12' is one of the wells located in North Sea as part of oil & gas exploration and production purposes. The well information has been gathered from
## North Sea directoreate of oil and gas wells. The data is made available by GeoLink services as volve dataset for scientific purposes.

## Lets load well data:
#ASCIIdata <- read.xlsx(file.choose(), sheet = "Sheet1")
ASCIIwell <- read_table(file.choose(), 
                        col_names = FALSE,
                        skip = 40)

C_Headers <- c('Depth','Lith_geo','CALI','DRHO','NPHI','RHOB','GR','DTC','DTE','RDEP','SP','RSHA','Rxo','RMED')
names(ASCIIwell) <- C_Headers
NSwell1 <- ASCIIwell

## Getting well information from the file
#WellInfo <- ASCIIdata[1:82]

# marking our wells with custom icons
Derrick <- makeIcon(iconUrl = "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/drilling-rig.png", 30, 30)
Rig <- makeIcon(iconUrl = "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/offshore-rig.png", 32, 32)


NSWells <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=1.717781, lat=58.456436, popup="Well: 15_9-12",icon = Derrick) %>%
  htmlwidgets::onRender("
      function(el, x) {
        console.log(this);
        var myMap = this;
        var imageUrl = 'D:/ALL OLEUM/Shaly Sand Analysis/R-research project/NORTH_SEA/NS_Jurassic_and_Carboniferous_Basins.png';
        var imageBounds = [[50.1000, -4.55000], [61.70000, 9.500000]];

        L.imageOverlay(imageUrl, imageBounds).addTo(myMap);
      }
      ")

## Displaying our well locations on map
NSWells  # Print the map

## Summary of well data
summary(NSwell1)

# Well Log of 15_9-12

par(mar=c(0.5, 0.5, 0.5, 0.5),mfrow = c(1,4),oma = c(4, 4, 3, 0.2))
## Plotting Gamma-caliper
par(xpd=F)
plot(NSwell1$GR,NSwell1$Depth, type = "l", xaxs = "i",
     yaxs = "i", xlim = c(0,150), ylim = c(3900,450), col = "darkgreen",
     ylab = "Depth", xlab = "", lwd = 1)
axis(3, seq(0,150,15))
axis(2, at= pretty(NSwell1$Depth), labels = pretty(NSwell1$Depth))
points(NSwell1$CALI,NSwell1$Depth, type = "l", col ="black", lwd = 1)
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
plot(NSwell1$SP,NSwell1$Depth, type = "l", col ="green", lwd = 1, lty = "dashed",xaxs = "i",
     yaxs = "i", xaxt = "n", yaxt = "n", xlim = c(-20,100), ylim = c(3900,450), ylab = "Depth", xlab = "")
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
plot(NSwell1$RSHA, NSwell1$Depth, type = "l", xaxs = "i",
     yaxs = "i", yaxt = "n" , xlim = c(0.1,1000), log = 'x', ylim = c(3900,450), col = "magenta",
     xlab = "", lwd = 1)
at.x <- outer(1:9, 10^(0:3))
lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
axis(3, at=at.x, labels=lab.x, las=1)
points(NSwell1$RMED, NSwell1$Depth, type = "l", col ="blue", lwd = 1)
points(NSwell1$RDEP, NSwell1$Depth, type = "l", col ="red", lwd = 1)
points(NSwell1$Rxo, NSwell1$Depth, type = "l", col ="green", lwd = 1)
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
plot(NSwell1$RHOB,NSwell1$Depth, type = "l", xaxt = "n",
     yaxt = "n",xaxs = "i", yaxs = "i", xlim = c(1,3), ylim = c(3900,450), col = "red",
     xlab = "", lwd = 1)
axis(3, seq(1,3,0.5), col="red",col.ticks="red",col.axis="red")
par(new=T)
plot(NSwell1$NPHI,NSwell1$Depth, type = "l", col ="#3a7a9b", lwd = 1, xlim = c(1.05,-0.15),
     ylim = c(3900,450), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
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
plot(NSwell1$DRHO,NSwell1$Depth, type = "l", col ="#660010", lwd = 1, xlim = c(0,5),
     ylim = c(3900,450), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylab = "")
axis(1, xlim=c(0,5),line=2.2,col="#660010",col.ticks="#660010",col.axis="#660010")
legend("bottom", 
       legend = "DRHO",
       border = NULL,
       text.col = "#660010",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.02))

## Plotting Sonic velocities 
plot(NSwell1$DTC,NSwell1$Depth, type = "l", xaxt = "n",
     yaxt = "n",xaxs = "i", yaxs = "i", xlim = c(240,40), ylim = c(3900,450), col = "black",
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

write.csv(NSwell1, "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/NORTH_SEA/Analysis/NSwell1.csv")
