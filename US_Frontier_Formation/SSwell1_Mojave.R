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
# Turn on showtext
showtext_auto()

## Lets load well data:
#ASCIIdata <- read.xlsx(file.choose(), sheet = "Sheet1")
ASCIIData <- read_table(file.choose(), 
                        col_names = FALSE,
                        skip = 76)


## Getting well information from the file
#WellInfo <- ASCIIdata[1:82]

# marking our wells with custom icons
Derrick <- makeIcon(iconUrl = "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/drilling-rig.png", 30, 30)
Rig <- makeIcon(iconUrl = "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/offshore-rig.png", 26, 26)

Well_Locs <- leaflet() %>% 
  addTiles() %>% addMarkers(lat = 43.588704, lng = -106.093986,  
                            popup="Well: MOJAVE FEDERAL 4277 27-44F-H",
                            icon = Derrick) %>% addMarkers(lat = 43.60568, lng = -106.02121,  
                            popup="Well: IBERLIN RANCH FEDERAL1726-2FH",
                            icon = Derrick)

## Displaying our well locations on map
Well_Locs

# Working on Well: Mojave Federal 4277 27-44F-H
# Well Logs
## We don't need the headers included as data now. Thus we will remove it and keep what we need:
SSwell1 <- ASCIIData[-(1 * 1), ]
Headers <- ASCIIData[1,2:27]       # "~A" will be excluded from becoming a column
names(SSwell1) <- Headers
# Rename column where names is "Sepal.Length"
names(SSwell1)[names(SSwell1) == "10IN_2FT_R"] <- "Res10IN"
names(SSwell1)[names(SSwell1) == "20IN_2FT_R"] <- "Res20IN"
names(SSwell1)[names(SSwell1) == "30IN_2FT_R"] <- "Res30IN"
names(SSwell1)[names(SSwell1) == "60IN_2FT_R"] <- "Res60IN"
names(SSwell1)[names(SSwell1) == "90IN_2FT_R"] <- "Res90IN"

SSwell1 <- as.data.frame(SSwell1)

## Excluding last two unwanted columns:
SSwell1 <- SSwell1[,-(27:28)]
#SSwell1 <- SSwell1[,-29]

par(mar=c(0.5, 0.5, 0.5, 0.5),mfrow = c(1,4),oma = c(4, 4, 3, 0.2))
#Plotting Gamma-caliper
par(xpd=F)
plot(SSwell1$DCAL,SSwell1$Depth, type = "l", xaxs = "i",
     yaxs = "i", xlim = c(0,150), ylim = c(14400,400), col = "black",
     ylab = "Depth", xlab = "", lwd = 1)
axis(3, seq(0,150,15))
axis(2, at= pretty(SSwell1$Depth), labels = pretty(SSwell1$Depth))
points(SSwell1$GR,SSwell1$Depth, type = "l", col ="dark green", lwd = 1)
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
plot(SSwell1$Res10IN, SSwell1$Depth, type = "l", xaxs = "i",
     yaxs = "i", yaxt = "n" , xlim = c(0.1,1000), log = 'x', ylim = c(14400,400), col = "magenta",
     xlab = "", lwd = 1)
at.x <- outer(1:9, 10^(0:3))
lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
axis(3, at=at.x, labels=lab.x, las=1)
points(SSwell1$Res20IN, SSwell1$Depth, type = "l", col ="green", lwd = 1)
points(SSwell1$Res30IN, SSwell1$Depth, type = "l", col ="blue", lwd = 1)
points(SSwell1$Res60IN, SSwell1$Depth, type = "l", col ="red", lwd = 1)
points(SSwell1$Res90IN, SSwell1$Depth, type = "l", col ="gray40", lwd = 1)
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
plot(SSwell1$RHOB,SSwell1$Depth, type = "l", xaxt = "n",
     yaxt = "n",xaxs = "i", yaxs = "i", xlim = c(1,3), ylim = c(14400,400), col = "red",
     xlab = "", lwd = 1)
axis(3, seq(1,3,0.5), col="red",col.ticks="red",col.axis="red")
par(new=T)
plot(SSwell1$CNLS,SSwell1$Depth, type = "l", col ="#3a7a9b", lwd = 1, xlim = c(60,-10),
       ylim = c(14400,400), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
axis(1, xlim=c(-60,10),line=0,col="#3a7a9b",col.ticks="#3a7a9b",col.axis="#3a7a9b")
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "RHOB",
       border = NULL,
       text.col = "red",
       bty = "n",
       horiz = F , 
       inset = c(0, 0))
legend("bottom", 
       legend = "CNLS",
       border = NULL,
       text.col = "#3a7a9b", 
       bty = "n",
       horiz = T, 
       inset = c(0, 0.02))
par(new=T)
plot(SSwell1$PEF,SSwell1$Depth, type = "l", col ="#660010", lwd = 1, xlim = c(0,20),
     ylim = c(14400,400), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylab = "")
axis(1, xlim=c(0,20),line=2.2,col="#660010",col.ticks="#660010",col.axis="#660010")
legend("bottom", 
       legend = "PEF",
       border = NULL,
       text.col = "#660010",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.04))

#Let's Calculate Volume of Shale
## As per geological information, 'Frontier' formation is from late cretaceous comprising of White to brown sandstone and 
## dark-gray shale; oyster coquina in upper part; coal and lignite in lower part. (North and South Wyoming) - Gray sandstone and sandy shale.
## For this reason, just using V(sh) or I(GR) index is not enough as it overestimates the volume of shale and moreover, the reservoir has proven
## to be producing both oil and gas but predominantly gas. Looking at the geological setting (tertiary to old rocks) it is decided to use Clavier's and Steiber's
## correction for Shale volume and then select which estimates the least Vsh.
## But, let us find the raw shale volume by identifying clean sand and clean shale lines near the zone of interest.

GR_Clean_Sand = 45.0 # °API
GR_Clean_Shale =120.0 # °API

SSwell1$V_sh_raw = (as.numeric(SSwell1$GR) - GR_Clean_Sand)/(GR_Clean_Shale - GR_Clean_Sand)   # Raw Shale Volume
SSwell1$V_sh_corr_C = (1.7 - (3.38 - ((SSwell1$V_sh_raw + 0.7)^2))^0.5)*100
SSwell1$V_sh_corr_St = (SSwell1$V_sh_raw/(3 -(2 * SSwell1$V_sh_raw)))*100

SSwell1$V_sh_corr_C[is.nan(SSwell1$V_sh_corr_c) | SSwell1$V_sh_corr_C < 0] <- 0
SSwell1$V_sh_corr_St[is.nan(SSwell1$V_sh_corr_St) | SSwell1$V_sh_corr_St < 0] <- 0

plot(SSwell1$V_sh_corr_C,SSwell1$Depth, type = "l", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", xlim = c(0,100), ylim = c(14400,400), col = "gray30",
     xlab = "", lwd = 0.5)
#polygon(c(SSwell1$V_sh_corr_C, rev(SSwell1$V_sh_corr_C)), c(rep(min(SSwell1$Depth), length(SSwell1$V_sh_corr_C)), rev(SSwell1$Depth)), col = 'gray20', border = NA)
axis(1, xlim=c(0,100),line=0,col="gray20",col.ticks="black",col.axis="gray20")
axis(3, xlim=c(0,100),line=0,col="gray20",col.ticks="black",col.axis="gray20")
points(SSwell1$V_sh_corr_St, SSwell1$Depth, type = "l", col ="#907910", lwd = 1)
grid(nx = 4, ny = NULL, lty = 2, col = 'gray', lwd = 1)
legend("bottom", 
       legend = "V_sh % Clavier ",
       border = NULL,
       text.col = "black",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.04))
legend("bottom", 
       legend = "V_sh % Steiber ",
       border = NULL,
       text.col = "#907910",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.05))

# Applying Corrections based on Sandstone matrix
## Computing Nuetron porosity and applying corrections to convert CN based on limestone matrix to sandstone matrix can be done either by referring to
## conversion charts if we want at specific zone precisely otherwise, we add 0.04 to the CNL/CNPOR values to change into sandstone matrix for the whole log.
## The follwoing image is the source: Crain's Petrophysical handbook
library(jpeg)
dev.new()
CPH_LogCorrVals <- readJPEG("D:/ALL OLEUM/Shaly Sand Analysis/Porosity_Scales_for_Log.jpeg",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(CPH_LogCorrVals,0,0,1,1)
dev.off()

## Applying the necessary corrections:
SSwell1$CNLS <- as.numeric(as.character(SSwell1$CNLS))
SSwell1$CNSS <- ifelse(SSwell1$CNLS != -999.25, as.numeric(SSwell1$CNLS) + 4.0,SSwell1$CNLS)

## Now plot the new Sandstone porosity logs along with others

par(mar=c(0.5, 0.5, 0.5, 0.5),mfrow = c(1,4),oma = c(4, 4, 3, 0.2))
#Plotting Gamma-caliper
par(xpd=F)
plot(SSwell1$DCAL,SSwell1$Depth, type = "l", xaxs = "i",
     yaxs = "i", xlim = c(0,150), ylim = c(14400,400), col = "black",
     ylab = "Depth", xlab = "", lwd = 1)
axis(3, seq(0,150,15))
axis(2, at= pretty(SSwell1$Depth), labels = pretty(SSwell1$Depth))
points(SSwell1$GR,SSwell1$Depth, type = "l", col ="dark green", lwd = 1)
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
plot(SSwell1$Res10IN, SSwell1$Depth, type = "l", xaxs = "i",
     yaxs = "i", yaxt = "n" , xlim = c(0.1,1000), log = 'x', ylim = c(14400,400), col = "magenta",
     xlab = "", lwd = 1)
at.x <- outer(1:9, 10^(0:3))
lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
axis(3, at=at.x, labels=lab.x, las=1)
points(SSwell1$Res20IN, SSwell1$Depth, type = "l", col ="green", lwd = 1)
points(SSwell1$Res30IN, SSwell1$Depth, type = "l", col ="blue", lwd = 1)
points(SSwell1$Res60IN, SSwell1$Depth, type = "l", col ="red", lwd = 1)
points(SSwell1$Res90IN, SSwell1$Depth, type = "l", col ="gray40", lwd = 1)
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
plot(SSwell1$RHOB,SSwell1$Depth, type = "l", xaxt = "n",
     yaxt = "n",xaxs = "i", yaxs = "i", xlim = c(1.8,2.8), ylim = c(14400,400), col = "red",
     xlab = "", lwd = 1)
axis(3, seq(1.8,2.8,0.2), col="red",col.ticks="red",col.axis="red")
par(new=T)
plot(SSwell1$CNSS,SSwell1$Depth, type = "l", col ="#2a4a9b", lwd = 1, xlim = c(54,-6),
     ylim = c(14400,400), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
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
plot(SSwell1$PEF,SSwell1$Depth, type = "l", col ="#660010", lwd = 1, xlim = c(0,20),
     ylim = c(14400,400), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylab = "")
axis(1, xlim=c(0,20),line=2.2,col="#660010",col.ticks="#660010",col.axis="#660010")
legend("bottom", 
       legend = "PEF",
       border = NULL,
       text.col = "#660010",
       bty = "n",
       horiz = F , 
       inset = c(0, 0.04))
plot(SSwell1$V_sh_corr_St,SSwell1$Depth, type = "l", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", xlim = c(0,100), ylim = c(14400,400), col = "gray30",
     xlab = "", lwd = 0.5)
polygon(c(SSwell1$V_sh_corr_St, rev(SSwell1$V_sh_corr_St)), c(rep(min(SSwell1$Depth), length(SSwell1$V_sh_corr_St)), rev(SSwell1$Depth)), col = 'gray20', border = NA)
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

# Exporting the data in csv file
write.csv(SSwell1, "D:/ALL OLEUM/Shaly Sand Analysis/R-research project/US_FRONTIER/SSwell1.csv")
